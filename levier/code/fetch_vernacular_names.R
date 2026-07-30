# Looks up French (falling back to English) common names for every species in
# data/gbif_occurrences_levier.csv, via a handful of batched Wikidata SPARQL
# queries (one taxon-name -> Wikidata item match per species, using wdt:P225).
# Species with no Wikidata match (common for fungi/insects) are left NA and
# the dashboard falls back to the scientific name for those.

library(dplyr)
library(httr)
library(jsonlite)

in_csv  <- "data/gbif_occurrences_levier.csv"
out_csv <- "data/gbif_vernacular_names.csv"
chunk_size <- 250

species <- read.csv(in_csv, stringsAsFactors = FALSE) %>%
  filter(!is.na(species), species != "") %>%
  distinct(species) %>%
  pull(species)

cat(sprintf("Looking up vernacular names for %d species via Wikidata\n", length(species)))

chunks <- split(species, ceiling(seq_along(species) / chunk_size))

sparql_query <- function(names) {
  values <- paste(sprintf('"%s"', gsub('"', '\\\\"', names)), collapse = " ")
  sprintf('
    SELECT ?taxonName ?nameFr ?nameEn WHERE {
      VALUES ?taxonName { %s }
      ?item wdt:P225 ?taxonName .
      OPTIONAL { ?item rdfs:label ?nameFr . FILTER(lang(?nameFr) = "fr") }
      OPTIONAL { ?item rdfs:label ?nameEn . FILTER(lang(?nameEn) = "en") }
    }', values)
}

fetch_chunk <- function(names) {
  resp <- POST(
    "https://query.wikidata.org/sparql",
    body = list(query = sparql_query(names), format = "json"),
    encode = "form",
    add_headers(
      "User-Agent" = "LevierDashboardScript/1.0",
      "Accept" = "application/sparql-results+json"
    ),
    timeout(60)
  )
  stop_for_status(resp)
  parsed <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
  bindings <- parsed$results$bindings
  if (is.null(bindings) || length(bindings) == 0 || is.null(bindings$taxonName)) return(tibble())
  tibble(
    species = bindings$taxonName$value,
    nameFr = if (!is.null(bindings$nameFr)) bindings$nameFr$value else NA_character_,
    nameEn = if (!is.null(bindings$nameEn)) bindings$nameEn$value else NA_character_
  )
}

results <- vector("list", length(chunks))
for (i in seq_along(chunks)) {
  cat(sprintf("Chunk %d / %d (%d species)\n", i, length(chunks), length(chunks[[i]])))
  results[[i]] <- tryCatch(fetch_chunk(chunks[[i]]), error = function(e) {
    warning(sprintf("Chunk %d failed: %s", i, conditionMessage(e)))
    tibble()
  })
  Sys.sleep(1)
}

vernacular <- bind_rows(results) %>%
  mutate(
    # Wikidata items without a real vernacular name often fall back to the
    # scientific name as the label -- that's not a common name, drop it.
    nameFr = na_if(nameFr, ""),
    nameEn = na_if(nameEn, ""),
    nameFr = ifelse(tolower(nameFr) == tolower(species), NA_character_, nameFr),
    nameEn = ifelse(tolower(nameEn) == tolower(species), NA_character_, nameEn)
  ) %>%
  group_by(species) %>%
  summarise(
    commonName = coalesce(first(na.omit(nameFr)), first(na.omit(nameEn)), NA_character_),
    .groups = "drop"
  )

out <- tibble(species = species) %>%
  left_join(vernacular, by = "species")

write.csv(out, out_csv, row.names = FALSE, fileEncoding = "UTF-8")

cat(sprintf(
  "Matched %d / %d species with a common name -> %s\n",
  sum(!is.na(out$commonName)), nrow(out), out_csv
))
