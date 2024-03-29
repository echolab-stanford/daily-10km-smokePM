source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Drops 10 km grid cells that we do not make predictions for. Saves predictions 
# and 10 km grid in root folder. 
# ------------------------------------------------------------------------------
# Load 10 km grid
grid_10km = read_sf(file.path(path_data, "1_grids", "grid_10km_wgs84"))

# Get grid cell IDs for which predictions are not made
no_pred_cells = readRDS(list.files(file.path(
  path_data, "ERA5_variables", "Land", "surface_pressure", "USA", "10km_grid", 
  "UTC-0600", "daily_mean_of_1-hourly"), full.names = T)[1]) %>% 
  filter(is.na(surface_pressure)) %>% 
  pull(id_grid) %>% 
  unique()

# Subset grid
grid_10km = grid_10km %>% filter(!(ID %in% no_pred_cells))

# Save
if (!dir.exists(file.path(path_final, "10km_grid", "10km_grid_wgs84"))) dir.create(file.path(path_final, "10km_grid", "10km_grid_wgs84"))
write_sf(grid_10km, file.path(path_final, "10km_grid", "10km_grid_wgs84", "10km_grid_wgs84.shp"))
