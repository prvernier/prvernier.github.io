# Downloads all GBIF occurrence records that fall within the Levier commune
# boundary (data/levier.geojson).
#
# Strategy: GBIF's occurrence/search "geometry" filter rejects complex
# polygons (the commune outline has ~2600 vertices), so we query a bounding
# box instead and then clip the results down to the exact commune shape
# locally with sf. The public search API caps pagination at 100,000 records;
# for areas with more occurrences than that, use rgbif::occ_download() with a
# free GBIF account instead.

library(sf)
library(dplyr)
library(httr)
library(jsonlite)

commune_path  <- "data/levier.geojson"
out_csv       <- "data/gbif_occurrences_levier.csv"
out_geojson   <- "data/gbif_occurrences_levier.geojson"
page_size     <- 300
max_offset    <- 100000

# 1. Commune boundary and search bounding box --------------------------------

levier <- st_read(commune_path, quiet = TRUE)
bbox <- st_bbox(levier)

wkt_bbox <- sprintf(
  "POLYGON((%f %f, %f %f, %f %f, %f %f, %f %f))",
  bbox["xmin"], bbox["ymin"],
  bbox["xmax"], bbox["ymin"],
  bbox["xmax"], bbox["ymax"],
  bbox["xmin"], bbox["ymax"],
  bbox["xmin"], bbox["ymin"]
)

gbif_search <- function(offset, limit) {
  resp <- GET(
    "https://api.gbif.org/v1/occurrence/search",
    query = list(geometry = wkt_bbox, limit = limit, offset = offset),
    user_agent("levier-dashboard (R script)")
  )
  stop_for_status(resp)
  fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
}

# 2. Total record count in the bounding box -----------------------------------

total <- gbif_search(offset = 0, limit = 1)$count
cat(sprintf("GBIF reports %d occurrences in the bounding box\n", total))

if (total > max_offset) {
  warning(sprintf(
    "Bounding box has %d occurrences, above the %d search-API pagination limit. ",
    total, max_offset
  ), "Only the first ", max_offset, " will be fetched; use rgbif::occ_download() for a complete pull.")
}

# 3. Page through the search API ----------------------------------------------

n_pages <- ceiling(min(total, max_offset) / page_size)
pages <- vector("list", n_pages)

for (i in seq_len(n_pages)) {
  offset <- (i - 1) * page_size
  page <- gbif_search(offset = offset, limit = page_size)
  pages[[i]] <- page$results
  cat(sprintf("Fetched %d / %d\n", min(offset + page_size, total), total))
  Sys.sleep(0.2)
}

occ_all <- bind_rows(pages)
cat(sprintf("Downloaded %d raw records from the bounding box\n", nrow(occ_all)))

# 4. Clip to the exact commune polygon -----------------------------------------

occ_all <- occ_all %>% filter(!is.na(decimalLatitude), !is.na(decimalLongitude))

occ_sf <- st_as_sf(
  occ_all,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326,
  remove = FALSE
)

occ_levier <- occ_sf[st_intersects(occ_sf, levier, sparse = FALSE)[, 1], ]

cat(sprintf(
  "%d of %d occurrences fall within the Levier commune boundary\n",
  nrow(occ_levier), nrow(occ_sf)
))

# 5. Look up dataset titles for the datasets actually represented -------------

dataset_keys <- unique(occ_levier$datasetKey)
dataset_titles <- vapply(dataset_keys, function(key) {
  resp <- tryCatch(GET(paste0("https://api.gbif.org/v1/dataset/", key)), error = function(e) NULL)
  if (is.null(resp) || http_error(resp)) return(NA_character_)
  fromJSON(content(resp, as = "text", encoding = "UTF-8"))$title
}, character(1))
dataset_lookup <- tibble(datasetKey = dataset_keys, datasetTitle = dataset_titles)

occ_levier <- occ_levier %>% left_join(dataset_lookup, by = "datasetKey")

# 6. Keep the useful columns and save ------------------------------------------

keep_cols <- intersect(c(
  "gbifID", "occurrenceID", "scientificName", "species", "taxonRank",
  "kingdom", "phylum", "class", "order", "family", "genus",
  "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters",
  "eventDate", "year", "month", "day", "basisOfRecord", "individualCount",
  "recordedBy", "datasetKey", "datasetTitle", "references"
), names(occ_levier))

occ_out <- occ_levier %>% select(all_of(keep_cols))

dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
write.csv(st_drop_geometry(occ_out), out_csv, row.names = FALSE, fileEncoding = "UTF-8")
st_write(occ_out, out_geojson, delete_dsn = TRUE, quiet = TRUE)

cat(sprintf(
  "\nSaved %d occurrence records (%d distinct species) to:\n  %s\n  %s\n",
  nrow(occ_out), n_distinct(occ_out$species, na.rm = TRUE), out_csv, out_geojson
))
