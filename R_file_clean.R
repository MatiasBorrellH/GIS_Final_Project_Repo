#### Final Project Geospatial ###

# Importing Packages
  library(sf) 
  library(spData) 
  library(tidyverse) 
  library(ggplot2)
  library(dplyr)
  library(terra)
  library(raster)
  library(viridis)
  library(gdistance)
  


# Data Loading:

  # Selecting data folder
  data_folder <- "C:/Users/aleja/OneDrive/Escritorio/Term_2/Geospatial/data/final_project"
  
  # Administrative level 3 data from Peru
  admin_lvl3_peru <- st_read(file.path(data_folder, "data", "Lima", "cities_geom", "gadm41_PER_3.json"))
  
  # Peru Complete road network
  peru_roads <- st_read(file.path(data_folder, "data", "hotosm_per_roads_lines_shp", "hotosm_per_roads_lines_shp.shp"))
  
  # Night Lights Raster (Used as population density proxy)
  raster_night_lights <- rast(file.path(data_folder, "data", "night_lights", "F182013.v4", "F182013.v4c_web.avg_vis.tif"))
  
  # Slope Raster (Lima Specific)
  raster_slope_lima <- rast(file.path(data_folder, "data", "Lima", "Rasters", "SRTM_GL3", "viz.SRTMGL3_slope.tif"))
  
  # Roughness Raster (Lima Specific)
  raster_roughness_lima <- rast(file.path(data_folder, "data", "Lima", "Rasters", "SRTM_GL3", "viz.SRTMGL3_roughness.tif"))
  
  # Aspect Raster (Lima Specific)
  raster_aspect_lima <- rast(file.path(data_folder, "data", "Lima", "Rasters", "SRTM_GL3", "viz.SRTMGL3_aspect.tif"))
  
# Cropping necessary rasters
  
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
  lima_geoms_cropped <- st_make_valid(lima_geoms_cropped)
  
  # Cropping rasters into lima_cropped polygons:
  raster_aspect_lima_crop <- crop(raster_aspect_lima, lima_geoms_cropped)
  raster_night_lights_crop <- crop(raster_night_lights, lima_geoms_cropped)
  raster_roughness_lima_crop <- crop(raster_roughness_lima, lima_geoms_cropped)
  raster_slope_lima_crop <- crop(raster_slope_lima, lima_geoms_cropped)
  
  # Masking every cropped raster:
  raster_aspect_lima_mask <- mask(raster_aspect_lima_crop, lima_geoms_cropped)
  raster_night_lights_mask <- mask(raster_night_lights_crop, lima_geoms_cropped)
  raster_roughness_lima_mask <- mask(raster_roughness_lima_crop, lima_geoms_cropped)
  raster_slope_lima_mask <- mask(raster_slope_lima_crop, lima_geoms_cropped)
  
# Plot of our Raw data
  
  plot(raster_aspect_lima_mask, main = "Aspect Masked")
  plot(st_geometry(lima_geoms_cropped),add=T)
  
  plot(raster_night_lights_mask, main = "Night Lights Masked")
  plot(st_geometry(lima_geoms_cropped),add=T)
  
  plot(raster_roughness_lima_mask, main = "Roughness Masked")
  plot(st_geometry(lima_geoms_cropped),add=T)
  
  plot(raster_slope_lima_mask, main = "Slope Masked")
  plot(st_geometry(lima_geoms_cropped),add=T)
  
# Creating Population Density Index from Night lights
  
  # Extract values from the original raster
  vals <- values(raster_night_lights_mask)
  
  # Apply Min-Max normalization (scale from 0 to 1)
  vals_norm <- (vals - min(vals, na.rm = TRUE)) / (max(vals, na.rm = TRUE) - min(vals, na.rm = TRUE))
  
  # Create a new raster object based on the original
  raster_night_lights_normalized <- raster_night_lights_mask
  
  # Assign normalized values to the new raster
  values(raster_night_lights_normalized) <- vals_norm
  
  # Set the power level to increase influence of brighter areas
  power_alpha <- 20  # level of pull
  
  # Function to compute weighted centroid with a fallback to geometric centroid
  get_weighted_centroid <- function(polygon, raster, alpha = 1) {
    # Crop and mask the raster to the polygon
    r_crop <- crop(raster, polygon)
    r_mask <- mask(r_crop, polygon)
    
    # Check if there are any non-NA values
    if (all(is.na(values(r_mask)))) {
      return(st_centroid(st_geometry(polygon)))  # fallback
    }
    
    # Extract coordinates and values
    coords <- xyFromCell(r_mask, which(!is.na(values(r_mask))))
    vals <- values(r_mask)[!is.na(values(r_mask))]
    
    # Check again in case no values were left
    if (length(vals) == 0) {
      return(st_centroid(st_geometry(polygon)))
    }
    
    # Apply more aggressive weighting
    vals_transformed <- vals^alpha
    
    # Weighted centroid calculation
    x_w <- sum(coords[, 1] * vals_transformed) / sum(vals_transformed)
    y_w <- sum(coords[, 2] * vals_transformed) / sum(vals_transformed)
    
    return(st_point(c(x_w, y_w)))
  }
  
  # Apply to each district with alpha
  weighted_centroids <- lapply(1:nrow(lima_geoms_cropped), function(i) {
    get_weighted_centroid(lima_geoms_cropped[i, ], raster_night_lights_normalized, alpha = power_alpha)
  })
  
  # Combine into an sf object
  lima_centroids_weighted <- st_sf(
    district_name = lima_geoms_cropped$NAME_3,
    geometry = st_sfc(weighted_centroids, crs = st_crs(lima_geoms_cropped))
  )
  
  # Plot of the Weighted Centroids
  
  # Plot normalized night lights raster
  plot(raster_night_lights_normalized, 
       main = paste("Normalized Night Lights + Weighted Centroids"),
       col = viridis(100))
  
  # Add Lima district polygons
  plot(st_geometry(lima_geoms_cropped), 
       add = TRUE, border = "black", lwd = 1)
  
  # Add weighted centroids (in red)
  plot(st_geometry(lima_centroids_weighted), 
       add = TRUE, col = "red", pch = 20, cex = 1.5)
  
# Adding Roads
  
  # Filtering Primary Roads Only
  lima_roads_primary <- lima_roads %>% 
    filter(highway == "primary")
  
  raster_template <- raster_slope_lima_mask 
  
  # Rasterize roads - assign value 1 to any cell intersected by a primary road
  raster_primary_roads <- terra::rasterize(lima_roads_primary, raster_template, field = 1, background = NA)
  
  
  # Plot of the Weighted Centroids + Roads
  
  # Plot normalized night lights raster
  plot(raster_night_lights_normalized, 
       main = paste("Normalized Night Lights + W. Centroids + Primary Roads"),
       col = viridis(100))
  
  # Add Lima district polygons
  plot(st_geometry(lima_geoms_cropped), 
       add = TRUE, border = "black", lwd = 1)
  
  # Add weighted centroids (in red)
  plot(st_geometry(lima_centroids_weighted), 
       add = TRUE, col = "red", pch = 20, cex = 1.5)
  
  # Plot clean roads as lines
  plot(st_geometry(lima_roads_primary), add = TRUE, col = "blue", lwd = 2.0)
  
# Now create Index of difficulty of construction
  
  # 1. Normalize roughness
  vals_roughness <- values(raster_roughness_lima_mask)
  vals_roughness_norm <- (vals_roughness - min(vals_roughness, na.rm = TRUE)) / 
    (max(vals_roughness, na.rm = TRUE) - min(vals_roughness, na.rm = TRUE))
  
  raster_roughness_norm <- raster_roughness_lima_mask
  values(raster_roughness_norm) <- vals_roughness_norm
  
  # 2. Normalize slope
  vals_slope <- values(raster_slope_lima_mask)
  vals_slope_norm <- (vals_slope - min(vals_slope, na.rm = TRUE)) / 
    (max(vals_slope, na.rm = TRUE) - min(vals_slope, na.rm = TRUE))
  
  raster_slope_norm <- raster_slope_lima_mask
  values(raster_slope_norm) <- vals_slope_norm
  
  # 3. Weighted average (50% slope, 50% roughness)
  construction_difficulty_vals <- 0.5 * values(raster_slope_norm) + 
    0.5 * values(raster_roughness_norm)
  
  # 4. Create new raster and assign values
  raster_construction_difficulty <- raster_slope_lima_mask  # use as template
  values(raster_construction_difficulty) <- construction_difficulty_vals
  
  
# Plot of Difficulty of construction + main roads + weigthed centroids
  
  # Plot everything in a single combined plot
  plot(raster_construction_difficulty, 
       main = "Construction Difficulty Index + W. Centroids + Roads",
       col = viridis::viridis(100))
  
  # Add Lima district boundaries
  plot(st_geometry(lima_geoms_cropped), add = TRUE, lwd = 1)
  
  # Add weighted centroids (in red)
  plot(st_geometry(lima_centroids_weighted), 
       add = TRUE, col = "red", pch = 20, cex = 1.5)
  
  # Plot clean roads as lines
  plot(st_geometry(lima_roads_primary), add = TRUE, col = "blue", lwd = 2.0)

# Calculating Most efficient train lines
  
  # Convert cost raster from terra to raster
  cost_raster <- raster(raster_construction_difficulty)  # now a RasterLayer
  
  # Compute conductance
  conductance <- 1 / (cost_raster + 1e-6)
  
  # Create transition matrix (now it will work)
  tr <- transition(conductance, transitionFunction = mean, directions = 8)
  tr <- geoCorrection(tr, type = "c")
  
  # 3. Prepare coordinates of centroids
  coords <- st_coordinates(lima_centroids_weighted)
  
  # 4. Calculate least-cost path between all centroids
  # This will create a cost matrix
  cost_matrix <- costDistance(tr, coords, coords)
  
  # 5. Optional: Extract actual paths (lines between each pair)
  paths <- list()
  counter <- 1
  for (i in 1:(nrow(coords) - 1)) {
    for (j in (i + 1):nrow(coords)) {
      path <- shortestPath(tr, coords[i, ], coords[j, ], output = "SpatialLines")
      paths[[counter]] <- st_as_sf(path)
      paths[[counter]]$from <- lima_centroids_weighted$district_name[i]
      paths[[counter]]$to <- lima_centroids_weighted$district_name[j]
      counter <- counter + 1
    }
  }
  
  # 6. Combine all paths into one sf object
  paths_sf <- do.call(rbind, paths)
  
  
  # Plot everything in a single combined plot
  plot(raster_construction_difficulty, 
       main = "W. Centroids + Roads + Best Possible Connection",
       col = viridis::viridis(100))
  
  # Add Lima district boundaries
  plot(st_geometry(lima_geoms_cropped), add = TRUE, lwd = 1)
  
  # Plot clean roads as lines
  plot(st_geometry(lima_roads_primary), add = TRUE, col = "blue", lwd = 2.0)
  
  # Plot best possible path
  plot(st_geometry(paths_sf), add = TRUE, col = "white", lwd = 2.0)
  
  # Add weighted centroids (in red)
  plot(st_geometry(lima_centroids_weighted), 
       add = TRUE, col = "red", pch = 20, cex = 2.0)
  
# Data for Train station lines in Lima
  
  peru_train <- st_read(file.path(data_folder, "data", "train", "gis_osm_railways_free_1.shp"))
  # Filter only codes 6102 and 6103
  lima_train_filtered <- peru_train %>%
    filter(code %in% c(6103))
  
  lima_train_cropped <- st_intersection(lima_train_filtered, st_union(lima_geoms_cropped))
  
  # Plot Lima districts
  plot(st_geometry(lima_geoms_cropped),
       main = "Lima Districts + Rail (Code 6102)",
       col = NA, border = "black", lwd = 1)

  # Add weighted centroids (optional)
  plot(st_geometry(lima_centroids_weighted), add = TRUE, col = "red", pch = 20, cex = 2)
  
  # Plot best possible path
  plot(st_geometry(paths_sf), add = TRUE, col = "green", lwd = 2.0)
  
  # Add filtered railway lines
  plot(st_geometry(lima_train_cropped), add = TRUE, col = "purple", lwd = 2)
  
# Creating the best line possible (usign construction difficulty index)
# that Joins Villa Maria del Triunfo Y San Juan de Lurigancho
  
  # 1. Convert SpatRaster to RasterLayer for gdistance
  cost_raster <- raster(raster_construction_difficulty)
  conductance <- 1 / (cost_raster + 1e-6)
  
  # 2. Create transition matrix and apply geo-correction
  tr <- transition(conductance, transitionFunction = mean, directions = 8)
  tr <- geoCorrection(tr, type = "c")
  
  # 3. Extract coordinates of origin and destination centroids
  origin <- st_coordinates(lima_centroids_weighted[lima_centroids_weighted$district_name == "SanJuandeLurigancho", ])
  dest   <- st_coordinates(lima_centroids_weighted[lima_centroids_weighted$district_name == "VillaMariadelTriunfo", ])
  
  # 4. Compute least-cost path between the two centroids
  lcp <- shortestPath(tr, origin, dest, output = "SpatialLines")
  
  # 5. Convert path to sf
  lcp_sf <- st_as_sf(lcp)
  
  # 6. Plot everything
  par(mar = c(4, 4, 6, 2))  # Add space for title
  plot(raster_construction_difficulty, 
       main = "Least-Cost Path Calculation",
       col = viridis(100))
  
  # Plot clean roads as lines
  plot(lima_roads,add = TRUE)
  
  # Add Lima district boundaries
  plot(st_geometry(lima_geoms_cropped), add = TRUE, border = "black", lwd = 1)
  
  # Add centroids
  plot(st_geometry(lima_centroids_weighted), add = TRUE, col = "red", pch = 20, cex = 2)

  # Add district names next to centroids
  text(st_coordinates(lima_centroids_weighted),
       labels = lima_centroids_weighted$district_name,
       cex = 0.6, col = "white", pos = 3)
  
  # Add filtered railway lines
  plot(st_geometry(lima_train_cropped), add = TRUE, col = "purple", lwd = 4)

  # Add least-cost path
  plot(st_geometry(lcp_sf), add = TRUE, col = "white", lwd = 2)
  
  
  