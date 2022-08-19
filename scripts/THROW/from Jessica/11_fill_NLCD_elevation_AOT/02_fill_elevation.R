path_data = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction/data/"

library(FNN)
library(sf)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# Fill Elevation by Nearest Non-Missing Neighbor
# Written by Jessica
# Last edited February 2022
#-------------------------------------------------------------------------------
max_distance_degrees = 1

#-------------------------------------------------------------------------------
#### 1 km grid ####
# Load 1 km grid
grid_1km = read_sf(paste0(path_data, "1_grids/1km_aod_grid/"))
path_filled = paste0(path_data, "2_from_EE/elevation_1km_subgrid_filled/")
if (!dir.exists(path_filled)) dir.create(path_filled)
files = list.files(paste0(path_data, "2_from_EE/elevation_1km_subgrid"), full.names = T)
for (i in 1:length(files)) {
  print(i)
  file = files[i]
  elevation = read.csv(file)
  if (!anyNA(elevation)) next
  elevation_filled = grid_1km %>% 
    right_join(elevation, by = "grid_id")
  
  cell_1km_na = which(is.na(elevation_filled$mean))
  nn = get.knnx(st_coordinates(st_centroid(elevation_filled[-cell_1km_na,])),
                st_coordinates(st_centroid(elevation_filled[cell_1km_na,])),
                k = 1)
  elevation_filled[cell_1km_na, "ID_nna"] = elevation_filled[-cell_1km_na,][nn$nn.index,]$grid_id
  # meters to kilometers to degrees
  elevation_filled[cell_1km_na, "distance_nna"] = nn$nn.dist/1000/100
  
  # Assign matched non-NA values
  elevation_filled = elevation_filled %>% 
    left_join(elevation %>% select(ID_nna = grid_id, mean, stdDev_stdDev), 
              by = "ID_nna") %>% 
    mutate(mean = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, mean.y, mean.x),
           stdDev_stdDev = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, stdDev_stdDev.y, stdDev_stdDev.x))
  
  # Follow original format
  elevation_filled = elevation_filled %>% select(names(elevation))
  
  # Save
  write.csv(elevation_filled, 
            file = paste0(path_filled, basename(file)),
            row.names = F)
}

#-------------------------------------------------------------------------------
#### 10 km grid ####
# Load 10 km grid
grid_10km = read_sf(paste0(path_data, "1_grids/10km_grid/"))

# Load elevation
elevation = read.csv(paste0(path_data, "2_from_EE/elevation_avg_sd_10km_grid.csv"))
elevation_filled = full_join(grid_10km, elevation)

# Get ID of NN non-NA 10 km grid cell
cell_10km_na = which(is.na(elevation_filled$mean))
nn = get.knnx(st_coordinates(st_centroid(elevation_filled[-cell_10km_na,])),
              st_coordinates(st_centroid(elevation_filled[cell_10km_na,])),
              k = 1)
elevation_filled[cell_10km_na, "ID_nna"] = elevation_filled[-cell_10km_na,][nn$nn.index,]$ID
# meters to kilometers to degrees
elevation_filled[cell_10km_na, "distance_nna"] = nn$nn.dist/1000/100

# Assign matched non-NA values
elevation_filled = elevation_filled %>% 
  left_join(elevation %>% select(ID_nna = ID, mean, stdDev_stdDev), 
            by = "ID_nna") %>% 
  mutate(mean = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, mean.y, mean.x),
         stdDev_stdDev = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, stdDev_stdDev.y, stdDev_stdDev.x))

# Follow original format
elevation_filled = elevation_filled %>% select(names(elevation))

# Save
write.csv(elevation_filled, 
          file = paste0(path_data, "2_from_EE/elevation_avg_sd_10km_grid_filled.csv"),
          row.names = F)
