library(sf)
library(osmdata)
library(dplyr)
library(mapview)
library(ggplot2)


# Read levier if not present
if (!exists("levier")) levier <- st_read("vernier.gpkg", "levier")

available_features()

available_tags("amenity")

poly <- levier
poly_wgs84 <- poly |> st_transform(4326) |> st_make_valid()
levier_bb <- st_bbox(poly_wgs84)

#bbox |> opq()

levier_bank <- levier_bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf()

levier_bank
levier_bank$bbox
levier_bank$meta
levier_bank$osm_points
levier_bank$osm_polygons

ggplot() +
  geom_sf(data = levier_bank$osm_points)

library(ggmap)
levier_map <- get_map(levier_bb, maptype = "roadmap")


#fetch_osm_layers <- function(poly, keys = c("highway", "building", "waterway", "landuse", "natural", "amenity")) {
fetch_osm_layers <- function(poly, keys = c("highway", "building")) {
  poly_wgs84 <- poly |> st_transform(4326) |> st_make_valid()
  bbox <- st_bbox(poly_wgs84)
  out <- list()

  for (k in keys) {
    q <- opq(bbox = bbox) |> add_osm_feature(key = k)
    od <- osmdata_sf(q)

    layers <- list(
      points = od$osm_points,
      lines = od$osm_lines,
      polygons = od$osm_polygons,
      multilines = od$osm_multilines,
      multipolygons = od$osm_multipolygons
    )

    # Clip each non-empty layer to the polygon (returns original if empty or NULL)
    clipped <- lapply(layers, function(x) {
      if (is.null(x) || nrow(x) == 0) return(x)
      st_intersection(x, poly_wgs84)
    })

    out[[k]] <- clipped
  }

  out
}

# Fetch OSM layers for levier and return the result
osm_layers <- fetch_osm_layers(levier)
osm_layers

mapview(levier) + mapview(osm_layers$polygons)
