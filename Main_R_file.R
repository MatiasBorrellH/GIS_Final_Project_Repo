library(sf) 
library(spData) 
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(terra)
library(raster)

#Setting folder:
getwd()
setwd(getwd())


# Data Loading:
# Administrative level 3 data from Peru
admin_lvl3_peru <- st_read("data/Lima/cities_geom/gadm41_PER_3.json")

# Peru Complete road network
peru_roads <- st_read('data/hotosm_per_roads_lines_shp/hotosm_per_roads_lines_shp.shp')

# Night Lights Raster (Used as population density proxy):
raster_night_lights <- rast("data/night_lights/F182013.v4/F182013.v4c_web.avg_vis.tif")

# Slope Raster (Lima Specific):
raster_slope_lima <- rast("data/Lima/Rasters/SRTM_GL3/viz.SRTMGL3_slope.tif")

# Roughness Raster (Lima Specific):
raster_roughness_lima <- rast("data/Lima/Rasters/SRTM_GL3/viz.SRTMGL3_roughness.tif")

# Relief Raster (Lima Specific):
raster_relief_lima <- rast("data/Lima/Rasters/SRTM_GL3/viz.SRTMGL3_color-relief.tif")

# Aspect Raster (Lima Specific):
raster_aspect_lima <- rast("data/Lima/Rasters/SRTM_GL3/viz.SRTMGL3_aspect.tif")



# Data Filtering:
# Getting Lima Geometry
lima_geoms <- admin_lvl3_peru %>%
  filter(NAME_2 == 'Lima') %>% 
  dplyr::select(NAME_3, geometry) 

# Getting lima road Network:
unique(peru_roads$highway)

peru_roads_filtered <- peru_roads %>% 
  dplyr::select(name, highway, geometry) %>%
  filter(!is.na(name)) %>%
  distinct(geometry, .keep_all = TRUE)

# Removing Peru roads to save some RAM.
rm(peru_roads)

# Getting the inner road network of Lima.
lima_roads <- peru_roads_filtered %>% 
  filter(lengths(st_within(peru_roads_filtered, lima_geoms)) > 0)

# Removing peru_roads_filtered to save some RAM.
rm(peru_roads_filtered)

# Getting Lima district geographical centroids:
lima_centroids <- st_centroid(lima_geoms)

centroids_distance_matrix <- st_distance(lima_centroids)



# Creating road density raster: ( This part still under development, to see the initial plot, do not execute, just go to plotting part)
# Transforming into a terra object.
lima_roads <- vect(lima_roads)
# Assigning a value to each road in Lima:
lima_roads$value <- 1

#Creating a raster template:
raster_road_density <- rast(ext(lima_roads), resolution = 100)

r_count <- rasterize(lima_roads, raster_road_density, field = "value", fun = sum, background = 0)

plot(r_count)


lima_geoms <- vect(lima_geoms)


zonal_stats <- extract(r_count, lima_geoms, fun = sum, na.rm = TRUE)

lima_geoms$line_count <- zonal_stats[, 2]

plot(lima_geoms["line_count"])


print(lima_geoms)



# Plotting:
ggplot() +
  geom_sf(data = lima_geoms, color = 'black') +
  geom_sf(data = lima_roads, color = 'black') +
  geom_sf(data = lima_centroids, color = 'red') +
  ggtitle('Lima Road Network + District geografical centroids')
  theme_minimal()
