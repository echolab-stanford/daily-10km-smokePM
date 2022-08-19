source("work/06_complex_MERRA_model/00_utils.R")
path_project = paste0(path_dropbox, "../Projects/smoke_PM_prediction/")
path_results = paste0(path_github, "work/09_test_HYSPLIT/results/")
path_seagate = "/Volumes/Seagate PD JBL/"

library(rgeos)
library(sf)
library(dplyr)

#-------------------------------------------------------------------------------
# Prepare 10 km Grid for Gridding Trajectory Point Counts
# Written by Jessica
# Last edited December 2021
#-------------------------------------------------------------------------------
# Choose distance buffer (meters)
distance_buffer = 50*1000

# Load grid
project_grid = readRDS(paste0("~/Documents/GitHub/purple-air-infiltration/data/", "grid.RDS"))
project_grid = gBuffer(project_grid, 
                       byid = T, 
                       width = distance_buffer, 
                       capStyle = "ROUND", 
                       quadsegs = 30)

# Transform
traj_points = readRDS(list.files(paste0(path_seagate, "HYSPLIT/6-day trajectories/trajectory points/"), full.names = T)[1])
project_grid = project_grid %>% 
  st_as_sf() %>% 
  st_transform(st_crs(traj_points)) %>% 
  rename(id_grid = ID)

# Save
saveRDS(project_grid, paste0(path_seagate, "HYSPLIT/10km_grid_HYSPLIT_crs.rds"))
