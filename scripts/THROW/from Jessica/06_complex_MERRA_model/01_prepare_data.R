source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(tidyr)
library(rgdal)

#-------------------------------------------------------------------------------
# Prepare Data
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
# Read in merged data
dat_merged <- readRDS(paste0(path_dropbox, "PM25/epa_station_covariates.rds"))
dat_merged <- dat_merged %>% 
  mutate(
    # Anomalize PM2.5 and AOT
    smokePM = pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0),
    aot_anom = aot - aot_med_3yr, 
    # Convert from Kelvin to Celsius
    temperature = temperature - 273.15
  ) %>% 
  drop_na(smokePM, pm25)

# Get EPA station lon/lat
epa_loc <- readOGR(paste0(path_dropbox, "PM25/epa_station_locations"), "epa_station_locations")
epa_loc <- epa_loc@data
dat_merged <- dat_merged %>% left_join(epa_loc, by = c("epa_id" = "id"))

# Subset to a more manageable size: California 2017-2020
dat_merged <- dat_merged %>% filter(state == "06", year > 2016)

# Subset to relevant columns and complete observations
dat_merged <- dat_merged %>% 
  select(smokePM, smoke_day, aot_anom, precipitation, temperature, pbl_min, 
         km_dist, epa_id, month, year, elevation, wind_u, wind_v, lon, lat, 
         county, date) %>% 
  drop_na()

# Split train/test
set.seed(3278)
dat_train <- dat_merged %>% 
  slice_sample(prop = 0.75) %>% 
  ungroup()
dat_test <- dat_merged %>% setdiff(dat_train)

# Save data
saveRDS(dat_train, paste0(path_project, "dat_train.rds"))
saveRDS(dat_test, paste0(path_project, "dat_test.rds"))
