source("work/05_get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(tidyr)
library(stringr)

#-------------------------------------------------------------------------------
# Describe HYSPLPIT Trajectory Point-EPA Station-Day Matching
# Written by Jessica
# Last edited August 2021
#-------------------------------------------------------------------------------
# Read in EPA station-days and get identifiers for observations
dat_epa <- readRDS(paste0(path_dropbox, "PM25/EPA/epa_station_level_pm25_data.rds")) %>% 
  # group_by(id, date) %>% 
  # slice_head(n = 1) %>% 
  # ungroup() %>% 
  mutate(date = as.Date(str_pad(date, 8, "left", 0), format = "%m%d%Y"))
obs <- paste(dat_epa$id, dat_epa$date) %>% unique()

# Get EPA station-days in CA 2020 that we observe
dat_covariates <- readRDS(paste0(path_dropbox, "PM25/epa_station_covariates.rds")) %>% 
  filter(paste(epa_id, date) %in% obs, 
         state == "06", 
         year == 2020) %>% 
  mutate(smokePM = pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0), 
         date = strftime(date))

rm(dat_epa)

# Get HYSPLIT data in configuration with most possible matches
dat_epa_traj <- readRDS(paste0(path_project, "reshaped_aggregated_72hr_traj_matched.rds")) %>% 
  filter(cutoff == 10, 
         agg_method == "idw", 
         injection_heights == "500+1500") %>% 
  select(-pm25) %>% 
  rename(epa_id = id_epa,
         date = date_station,
         lon = lon_epa,
         lat = lat_epa)

# Merge station-days with HYSPLIT data
dat_merged <- dat_epa_traj %>% full_join(dat_covariates)

# How many station-days match to at least one HYSPLIT trajectory point?
count(dat_merged, is.na(height))
count(dat_merged, smoke_day, is.na(height))
count(dat_merged, smoke_day)
count(dat_merged, smoke_day, n_traj_points) %>% View()
