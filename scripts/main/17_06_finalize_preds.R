source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Drops 10 km grid cells that we do not make predictions for. Saves predictions 
# and 10 km grid in root folder. 
# ------------------------------------------------------------------------------
# Load predictions
preds = readRDS(file.path(path_output, "smokePM_predictions_20060101_20201231.rds"))

# Save
saveRDS(preds, file.path(path_final, "smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.rds"))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, "smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.csv"), row.names = F)

# ------------------------------------------------------------------------------
# Load predictions aggregated to county level
preds = readRDS(file.path(path_output, "county_smokePM_predictions_20060101_20201231.rds"))

# Save
saveRDS(preds, file.path(path_final, "smokePM2pt5_predictions_on_smokedays_daily_county_20060101-20201231.rds"))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, "smokePM2pt5_predictions_on_smokedays_daily_county_20060101-20201231.csv"), row.names = F)

# ------------------------------------------------------------------------------
# Load predictions aggregated to census tract level
preds = readRDS(file.path(path_output, "tract_smokePM_predictions_20060101_20201231.rds"))

# Save
saveRDS(preds, file.path(path_final, "smokePM2pt5_predictions_on_smokedays_daily_tract_20060101-20201231.rds"))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, "smokePM2pt5_predictions_on_smokedays_daily_tract_20060101-20201231.csv"), row.names = F)

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
if (!dir.exists(file.path(path_final, "10km_grid_wgs84"))) dir.create(file.path(path_final, "10km_grid_wgs84"))
write_sf(grid_10km, file.path(path_final, "10km_grid_wgs84", "10km_grid_wgs84.shp"))
