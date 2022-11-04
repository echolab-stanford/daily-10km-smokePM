source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Saves smokePM aggregates in final folder.
# ------------------------------------------------------------------------------
# Load predictions
preds = readRDS(file.path(path_output, "smokePM", "predictions", "combined", "smokePM_predictions_20060101_20201231.rds"))

# Save
saveRDS(preds, file.path(path_final, "10km_grid", "smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.rds"))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, "10km_grid", "smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.csv"), row.names = F)

# ------------------------------------------------------------------------------
# Load predictions aggregated to county level
preds = readRDS(file.path(path_output, "smokePM", "predictions", "combined", "county_smokePM_predictions_20060101_20201231.rds"))

# Save
saveRDS(preds, file.path(path_final, "county", "smokePM2pt5_predictions_daily_county_20060101-20201231.rds"))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, "county", "smokePM2pt5_predictions_daily_county_20060101-20201231.csv"), row.names = F)

# ------------------------------------------------------------------------------
# Load predictions aggregated to zip level
preds = readRDS(file.path(path_output, "smokePM", "predictions", "combined", "zcta_smokePM_predictions_20060101_20201231.rds"))

# Save
saveRDS(preds, file.path(path_final, "zcta", "smokePM2pt5_predictions_daily_zcta_20060101-20201231.rds"))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, "zcta", "smokePM2pt5_predictions_daily_zcta_20060101-20201231.csv"), row.names = F)

# ------------------------------------------------------------------------------
# Load predictions aggregated to census tract level
preds = readRDS(file.path(path_output, "smokePM", "predictions", "combined", "tract_smokePM_predictions_20060101_20201231.rds"))

# Save
saveRDS(preds, file.path(path_final, "tract", "smokePM2pt5_predictions_daily_tract_20060101-20201231.rds"))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, "tract", "smokePM2pt5_predictions_daily_tract_20060101-20201231.csv"), row.names = F)
