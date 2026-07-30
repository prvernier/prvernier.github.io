library(sf)
library(stars)
library(dplyr)
library(mapview)


# Levier house
y <- st_read('C:/Users/PIVER37/Documents/gisdata/grids/france/L93_1x1.shp')
x <- st_read('madre/vernier.gpkg') |>
  st_transform(crs=st_crs(y)) |>
  filter(numero %in% c('0063', '0065', '0276', '0278', '0279', '0320', '0322')) |>
  st_union() |>
  st_sf()
y <- y[x,]
grid = st_as_stars(st_bbox(y), dx = 10, dy = 10)
grid = st_as_sf(grid) %>% st_cast('MULTIPOLYGON')
grid = grid[x, ]
grid2 = st_as_stars(st_bbox(grid), dx = 1, dy = 1)
grid2 = st_as_sf(grid2) %>% st_cast('MULTIPOLYGON')
grid2 = grid2[x, ]
mapview(x) + mapview(grid2, alpha.regions=0)
grid2x = st_intersection(grid2, y)
st_write(grid2, 'madre/vernier.gpkg','house_grid_1m', delete_layer=TRUE)
st_write(grid2x, 'madre/vernier.gpkg','house_grid_1m_clipped', delete_layer=TRUE)

# Vie de Salin 0038
y <- st_read('C:/Users/PIVER37/Documents/gisdata/grids/france/L93_1x1.shp')
x <- st_read('madre/vernier.gpkg') |>
  st_transform(crs=st_crs(y)) |>
  filter(numero %in% c('0038')) |>
  st_union() |>
  st_sf()
y <- y[x,]
grid = st_as_stars(st_bbox(y), dx = 10, dy = 10)
grid = st_as_sf(grid) %>% st_cast('MULTIPOLYGON')
grid = grid[x, ]
mapview(grid, alpha.regions=0) + mapview(x, alpha.regions=0)
gridx = st_intersection(grid, y)
st_write(grid, 'madre/vernier.gpkg','salin38_grid_5m', delete_layer=TRUE)
st_write(gridx, 'madre/vernier.gpkg','salin38_grid_5m_clipped', delete_layer=TRUE)

# Vie de Salin 0040
y <- st_read('C:/Users/PIVER37/Documents/gisdata/grids/france/L93_1x1.shp')
x <- st_read('madre/vernier.gpkg') |>
  st_transform(crs=st_crs(y)) |>
  filter(numero=='0040' & section=='ZM') |>
  st_union() |>
  st_sf()
y <- y[x,]
grid = st_as_stars(st_bbox(y), dx = 5, dy = 5)
grid = st_as_sf(grid) %>% st_cast('MULTIPOLYGON')
grid = grid[x, ]
mapview(grid, alpha.regions=0) + mapview(x, alpha.regions=0)
gridx = st_intersection(grid, y)
st_write(grid, 'madre/vernier.gpkg','salin40_grid_5m', delete_layer=TRUE)
st_write(gridx, 'madre/vernier.gpkg','salin40_grid_5m_clipped', delete_layer=TRUE)

# Mesjean
y <- st_read('C:/Users/PIVER37/Documents/gisdata/grids/france/L93_1x1.shp')
x <- st_read('madre/vernier.gpkg') |>
  st_transform(crs=st_crs(y)) |>
  filter(numero=='0040' & section=='0A' | numero %in% c('0041','0042','0050','0089', '0180')) |> # add 0049
  st_union() |>
  st_sf()
y <- y[x,]
grid = st_as_stars(st_bbox(y), dx = 5, dy = 5)
grid = st_as_sf(grid) %>% st_cast('MULTIPOLYGON')
grid = grid[x, ]
mapview(grid, alpha.regions=0) + mapview(x, alpha.regions=0)
gridx = st_intersection(grid, y)
st_write(grid, 'madre/vernier.gpkg','mesjean_grid_5m', delete_layer=TRUE)
st_write(gridx, 'madre/vernier.gpkg','mesjean_grid_5m_clipped', delete_layer=TRUE)

# Lemuy
y <- st_read('C:/Users/PIVER37/Documents/gisdata/grids/france/L93_1x1.shp')
x <- st_read('madre/vernier.gpkg') |>
  st_transform(crs=st_crs(y)) |>
  filter(numero %in% c('0021')) |>
  st_union() |>
  st_sf()
y <- y[x,]
grid = st_as_stars(st_bbox(y), dx = 5, dy = 5)
grid = st_as_sf(grid) %>% st_cast('MULTIPOLYGON')
grid = grid[x, ]
mapview(grid, alpha.regions=0) + mapview(x, alpha.regions=0)
gridx = st_intersection(grid, y)
st_write(grid, 'madre/vernier.gpkg','lemuy_grid_5m', delete_layer=TRUE)
st_write(gridx, 'madre/vernier.gpkg','lemuy_grid_5m_clipped', delete_layer=TRUE)
