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



# Sub-setting lima_geometries to match our area of interest:

# Transforming one of the rasters into polygons:
raster_extent_pol <- as.polygons(ext(raster_slope_lima))
crs(raster_extent_pol) <- crs(raster_slope_lima)

# Transforming it into a sf_object
raster_extent_sf <- st_as_sf(raster_extent_pol)

# If neccesary transforming the CRS.
if (st_crs(lima_geoms) != st_crs(raster_extent_sf)) {
  lima_geoms <- st_transform(lima_geoms, st_crs(raster_extent_sf))
}

# Intersecting both geometries
lima_geoms_cropped <- st_intersection(lima_geoms, raster_extent_sf)

# Visualizing:
plot(raster_aspect_lima)
plot(lima_geoms_cropped$geometry, add = TRUE, col = "red", lwd = 2)




# Cropping rasters into lima_cropped polygons:
raster_aspect_lima_crop <- crop(raster_aspect_lima, lima_geoms_cropped)
raster_night_lights_crop <- crop(raster_night_lights, lima_geoms_cropped)
raster_roughness_lima <- crop(raster_roughness_lima, lima_geoms_cropped)
raster_slope_lima_crop <- crop(raster_slope_lima, lima_geoms_cropped)

# Masking every cropped raster:
raster_aspect_lima_mask <- mask(raster_aspect_lima_crop, lima_geoms_cropped)
raster_night_lights_mask <- mask(raster_night_lights_crop, lima_geoms_cropped)
raster_roughness_lima_mask <- mask(raster_roughness_lima, lima_geoms_cropped)
raster_slope_lima_mask <- mask(raster_slope_lima_crop, lima_geoms_cropped)



# Visualize every mask
plot(raster_aspect_lima_mask, main = "Aspect Masked")
plot(st_geometry(lima_geoms_cropped),add=T)

plot(raster_night_lights_mask, main = "Night Lights Masked")
plot(st_geometry(lima_geoms_cropped),add=T)

plot(raster_roughness_lima_mask, main = "Roughness Masked")
plot(st_geometry(lima_geoms_cropped),add=T)

plot(raster_slope_lima_mask, main = "Slope Masked")
plot(st_geometry(lima_geoms_cropped),add=T)

plot(lima_geoms_cropped$geometry, add = TRUE, col = "red", lwd = 2)


# Resampling night_light to match the other rasters:
raster_night_lights_resampled <- resample(raster_night_lights_mask, raster_aspect_lima_mask, method = "bilinear")


# Turning rasters into vectors:
rast_aspect_val <- values(raster_aspect_lima_mask)[,1]
rast_night_lights_val <- values(raster_night_lights_resampled)[,1]
rast_roughness_val <- values(raster_roughness_lima_mask)[,1]
rast_slope_val <- values(raster_slope_lima_mask)[,1]


# Creating a dataframe for raster values manipulations:

rasters_dataframe <- data.frame(
  aspect = rast_aspect_val,
  night_lights = rast_night_lights_val,
  roughness = rast_roughness_val,
  slope = rast_slope_val
)

# Creating a new combination_raster:
rasters_dataframe$combined <- rowSums(rasters_dataframe, na.rm = TRUE)

# Reconstructing the raster from a dataframe colummn into a spat raster using the slope raster as reference:
raster_combined <- raster_slope_lima_mask
values(raster_combined) <- rasters_dataframe$combined


# Locating the NA rows.
celdas_validas <- which(!is.na(values(raster_combined)))

# Mapping the new values into the non-NA rows
vals <- values(raster_combined)
vals[celdas_validas] <- rasters_dataframe$combined
values(raster_combined) <- vals

length(rasters_dataframe$combined) 

# Visualizing:
plot(raster_combined, main = "Combined Raster + Roads + Centroids")
plot(lima_roads,add=T)
plot(st_geometry(lima_geoms_cropped), add=T)
plot(st_geometry(lima_centroids), add = TRUE, col = "red", pch = 20, cex = 1.2)


# Storing the combined raster:
writeRaster(raster_suma, "combined_raster.tif", overwrite = TRUE)


