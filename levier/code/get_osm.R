# ============================================================
# OSM Data Download, Save & Interactive Map for AOI (Levier, Doubs)
# ============================================================
# Packages required:
#   sf, osmdata, mapview, dplyr
#
# Install if needed:
#   install.packages(c("sf", "osmdata", "mapview", "dplyr"))
# ============================================================

library(sf)
library(osmdata)
library(mapview)
library(dplyr)

# ─── 1. Load the Area of Interest ────────────────────────────────────────────

message("Loading AOI from GeoPackage...")

# Adjust this path to the location of your aoi.gpkg file
aoi_path <- "aoi.gpkg"

aoi <- st_read(aoi_path, layer = "aoi", quiet = TRUE)

# Reproject to WGS84 (EPSG:4326) for OSM queries
aoi_wgs84 <- st_transform(aoi, crs = 4326)

# Extract bounding box for the OSM query
bbox <- st_bbox(aoi_wgs84)
cat("AOI bounding box (WGS84):\n")
print(bbox)
cat("Commune(s):", paste(aoi$NOM_COM, collapse = ", "), "\n\n")


# ─── 2. Define OSM Feature Keys to Download ──────────────────────────────────

# We query a broad set of OSM feature categories
osm_keys <- list(
  amenity       = "amenity",        # schools, hospitals, restaurants, etc.
  building      = "building",       # all buildings
  highway       = "highway",        # roads, paths, footways
  waterway      = "waterway",       # rivers, streams, canals
  natural       = "natural",        # forests, water bodies, peaks
  landuse       = "landuse",        # farmland, residential, commercial
  leisure       = "leisure",        # parks, sports areas
  shop          = "shop",           # retail shops
  tourism       = "tourism",        # hotels, viewpoints, museums
  historic      = "historic",       # castles, ruins, monuments
  railway       = "railway",        # rail lines and stations
  public_transport = "public_transport"  # stops, platforms
)


# ─── 3. Download OSM Data ────────────────────────────────────────────────────

message("Querying OSM Overpass API — this may take a minute...")

# Helper: safely query one key, returning NULL on error
query_osm <- function(key) {
  tryCatch({
    q <- opq(bbox = bbox) |>
      add_osm_feature(key = key)
    result <- osmdata_sf(q)
    result
  }, error = function(e) {
    message("  [skipped] ", key, ": ", conditionMessage(e))
    NULL
  })
}

osm_results <- lapply(names(osm_keys), function(k) {
  message("  Fetching: ", k)
  query_osm(k)
})
names(osm_results) <- names(osm_keys)


# ─── 4. Extract sf Layers ────────────────────────────────────────────────────

# Helper: pull a geometry type from a result, clip to AOI, add source key
extract_layer <- function(result, key, geom_type) {
  slot_name <- paste0("osm_", geom_type)   # e.g. "osm_points"
  if (is.null(result)) return(NULL)
  layer <- result[[slot_name]]
  if (is.null(layer) || nrow(layer) == 0) return(NULL)
  
  # Ensure CRS matches AOI
  layer <- st_transform(layer, crs = st_crs(aoi_wgs84))
  
  # Clip to AOI polygon (suppress warnings about attribute agr)
  suppressWarnings(
    layer <- st_intersection(layer, st_union(aoi_wgs84))
  )
  if (nrow(layer) == 0) return(NULL)
  
  layer$osm_key <- key
  layer
}

# Collect points, lines, polygons for each key
collect_layers <- function(geom_type) {
  layers <- lapply(names(osm_results), function(k) {
    extract_layer(osm_results[[k]], k, geom_type)
  })
  layers <- Filter(Negate(is.null), layers)
  if (length(layers) == 0) return(NULL)
  
  # Bind rows — keep only a small common set of columns to avoid mismatches
  # (OSM features can have hundreds of ad-hoc tags)
  bind_and_trim <- function(lst) {
    # Keep columns present in all layers, plus geometry
    common_cols <- Reduce(intersect, lapply(lst, function(l) {
      setdiff(names(l), attr(l, "sf_column"))
    }))
    # Always keep osm_id and osm_key
    keep <- unique(c("osm_id", "osm_key", common_cols))
    lst2 <- lapply(lst, function(l) {
      present <- intersect(keep, names(l))
      l[, present, drop = FALSE]
    })
    do.call(rbind, lst2)
  }
  
  bind_and_trim(layers)
}

message("\nAssembling layers...")
points_sf   <- collect_layers("points")
lines_sf    <- collect_layers("lines")
polygons_sf <- collect_layers("polygons")   # includes multipolygons

# Summarise
summarise_layer <- function(lyr, name) {
  if (is.null(lyr)) {
    cat(sprintf("  %-20s: 0 features\n", name))
  } else {
    cat(sprintf("  %-20s: %d features\n", name, nrow(lyr)))
  }
}
cat("\nFeature counts:\n")
summarise_layer(points_sf,   "Points")
summarise_layer(lines_sf,    "Lines")
summarise_layer(polygons_sf, "Polygons")


# ─── 5. Save to GeoPackage ───────────────────────────────────────────────────

output_gpkg <- "osm_data_aoi.gpkg"

message("\nSaving to GeoPackage: ", output_gpkg)

# Remove existing file to avoid layer conflicts
if (file.exists(output_gpkg)) file.remove(output_gpkg)

write_if_exists <- function(layer, layer_name, gpkg_path) {
  if (!is.null(layer) && nrow(layer) > 0) {
    # Ensure all list columns are converted to character (sf write limitation)
    for (col in names(layer)) {
      if (is.list(layer[[col]]) && !inherits(layer[[col]], "sfc")) {
        layer[[col]] <- vapply(layer[[col]], function(x) {
          if (is.null(x)) NA_character_ else paste(x, collapse = "; ")
        }, character(1))
      }
    }
    st_write(layer, gpkg_path, layer = layer_name,
             append = TRUE, quiet = TRUE)
    message("  Saved layer: ", layer_name)
  }
}

write_if_exists(aoi_wgs84,   "aoi",            output_gpkg)
write_if_exists(points_sf,   "osm_points",     output_gpkg)
write_if_exists(lines_sf,    "osm_lines",      output_gpkg)
write_if_exists(polygons_sf, "osm_polygons",   output_gpkg)

# Verify saved layers
saved_layers <- st_layers(output_gpkg)
cat("\nLayers saved to", output_gpkg, ":\n")
print(saved_layers)


# ─── 6. Interactive Map ──────────────────────────────────────────────────────

message("\nBuilding interactive map...")

# Base: AOI boundary
m <- mapview(aoi_wgs84,
             layer.name  = "AOI (Levier)",
             col.regions = "transparent",
             color       = "red",
             lwd         = 3,
             alpha.regions = 0)

# Add polygons (land use, buildings, natural areas)
if (!is.null(polygons_sf) && nrow(polygons_sf) > 0) {
  m <- m + mapview(polygons_sf,
                   zcol        = "osm_key",
                   layer.name  = "Polygons",
                   alpha.regions = 0.35,
                   lwd         = 0.5)
}

# Add lines (roads, waterways, railways)
if (!is.null(lines_sf) && nrow(lines_sf) > 0) {
  m <- m + mapview(lines_sf,
                   zcol       = "osm_key",
                   layer.name = "Lines",
                   lwd        = 1.5)
}

# Add points (amenities, tourism, shops, etc.)
if (!is.null(points_sf) && nrow(points_sf) > 0) {
  m <- m + mapview(points_sf,
                   zcol       = "osm_key",
                   layer.name = "Points",
                   cex        = 4)
}

# Display the map (opens in browser / RStudio Viewer)
message("Displaying interactive map...")
m

# ─── Done ────────────────────────────────────────────────────────────────────

message("\nAll done!")
message("  • OSM data saved to : ", output_gpkg)
message("  • Interactive map   : displayed in viewer")
