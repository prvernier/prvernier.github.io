library(sf)
library(dplyr)
library(mapsf)
library(maposm)
library(mapview)

#-- Get cadastral data ---------------------------------------------------------

x <- st_read("D:/gisdata/france/parcelles/commune.shp") |>
  filter(NOM_COM=="Levier")
y <- st_read("D:/gisdata/france/parcelles/parcelle.shp") |>
  filter(NOM_COM=="Levier")

# Vie-de-Salin
z1 <- y |> filter(SECTION=="ZM" & NUMERO %in% c("0036","0038","0040")) |>
  st_cast("MULTIPOLYGON")
# Mesjean
z2a <- y |> filter(SECTION=="0A" & NUMERO %in% c("0040","0041","0042","0049","0180") & FEUILLE==1 & COM_ABS=="000")
z2b <- y |> filter(SECTION=="ZD" & NUMERO %in% c("0050","0089")) 
z2 <- rbind(z2a, z2b) |> 
  st_cast("MULTIPOLYGON")
# House
z3 <- y |> filter(SECTION=="AC" & NUMERO %in% c("0063","0065","0276","0278","0279","0320","0322")) |> 
  st_cast("MULTIPOLYGON")
z <- rbind(z1, z2, z3)

# Save the layers
st_write(x, "levier.gpkg", "commune", delete_layer=TRUE)
st_write(y, "levier.gpkg", "parcelles", delete_layer=TRUE)
st_write(z, "vernier.gpkg", "parcelles", delete_layer=TRUE)

# View the layers
mapview(x) + mapview(z)

# Calculate areas
cat("House: ", round(sum(st_area(z3))/10000,2), "\n")
cat("Mesjean: ", round(sum(st_area(z2))/10000,2), "\n")
cat("Vie de Salin: ", round(sum(st_area(z1))/10000,2), "\n")

#-- Create grids ---------------------------------------------------------------

grd1 <- st_make_grid(z1, cellsize=5)
grd1 <- grd1[z1,] |>
  st_as_sf() |>
  st_cast("MULTIPOLYGON") |>
  mutate(id=row_number())
mapview(grd1)
join1 <- st_join(grd1, z1, join = st_intersects)
mapview(join1, zcol="NUMERO", alpha=0.5)
st_write(grd1, "vernier.gpkg", "vie-de-salin-5k", delete_layer=TRUE)

grd2 <- st_make_grid(z24, cellsize=5)
grd2 <- grd2[z2,] |>
  st_as_sf() |>
  st_cast("MULTIPOLYGON") |>
  mutate(id=row_number())
mapview(grd2, col.regions="yellow")
join2 <- st_join(grd2, z2, join = st_intersects)
mapview(join2, zcol="NUMERO", alpha=0.5)
st_write(grd2, "vernier.gpkg", "mesjean-5k", delete_layer=TRUE)

grd3 <- st_make_grid(z3, cellsize=1)
grd3 <- grd3[z3,] |>
  st_as_sf() |>
  st_cast("MULTIPOLYGON") |>
  mutate(id=row_number())
mapview(grd3)
join3 <- st_join(grd3, z3, join = st_intersects)
mapview(join3, zcol="NUMERO", alpha=0.5)
st_write(grd3, "vernier.gpkg", "maison-1k", delete_layer=TRUE)

join123 <- rbind(join1, join2, join3)
st_write(join123, "levier.gpkg", "grid1m", delete_layer=TRUE)

#-- Get OSM data ---------------------------------------------------------------

x <- st_read("levier.gpkg", "commune")
osm_layers = om_get(x)

# Preset maps
om_map(x = osm_layers, title = "Levier, Doubs", theme = "light")
om_map(x = osm_layers, title = "Levier, Doubs", theme = "grey")

# More control on individual layers
mf_map(osm_layers$zone, col = "#f2efe9", border = NA, add = FALSE)
mf_map(osm_layers$urban, col = "#e0dfdf", border = "#e0dfdf", lwd = .5, add = TRUE)
mf_map(osm_layers$green, col = "#c8facc", border = "#c8facc", lwd = .5, add = TRUE)
mf_map(osm_layers$water, col = "#aad3df", border = "#aad3df", lwd = .5, add = TRUE)
mf_map(osm_layers$railway, col = "grey50", lty = 2, lwd = .2, add = TRUE)
mf_map(osm_layers$road, col = "white", border = "white", lwd = .5, add = TRUE)
mf_map(osm_layers$street, col = "white", border = "white", lwd = .5, add = TRUE)
mf_map(osm_layers$building, col = "#d9d0c9", border = "#c6bab1", lwd = .5, add = TRUE)
mf_map(osm_layers$zone, col = NA, border = "#c6bab1", lwd = 3, add = TRUE)
mf_credits(txt = "\ua9 OpenStreetMap contributors")
mf_scale(size = 100, scale_units = "m")
mf_title("Levier, Doubs")

om_write(osm_layers, "levier_osm.gpkg")
om_read(x = "levier_osm.gpkg")
