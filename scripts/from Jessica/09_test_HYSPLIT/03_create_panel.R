source("work/10_predict_AOD/00_utils.R")

library(lubridate)
library(sp)
library(rgeos)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)

#-------------------------------------------------------------------------------
# Create Panel of EPA Station-Smoke Days
# Written by Jessica
# Last edited November 2021
# 
# Check if we get enough variation in num_traj_points or height
# Output: EPA station-days smoke day cells w/ smokePM, fold, and all stage 2 predictors + HYSPLIT predictors
#-------------------------------------------------------------------------------
# Read in EPA station-days with smoke PM and fold (id_epa, lon, lat, fold, date, smokePM)
# Longitude and latitude
epa = readRDS("INSERT FILE PATH") %>% 
  drop_na(fold, smokePM) %>% 
  select(id_epa = epa_id)

# Crosswalk between EPA stations and 10 km grid cells
epa_loc = read_sf("~/BurkeLab Dropbox/Data/PM25/epa_station_locations/epa_station_locations.shp")
project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS") %>% 
  gBuffer(byid = T, width = 5000, capStyle = "SQUARE") %>% 
  st_as_sf() %>% 
  st_transform(st_crs(epa_loc))
crosswalk = epa_loc %>% 
  st_join(project_grid) %>% 
  select(id_epa = id, lon, lat, id_grid = ID)

# Elevation
elevation = read.csv("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/2_from_EE/elevation_avg_sd_10km_grid.csv")

# NLCD
nlcd = read.csv("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/2_from_EE/NLCD_areas_10km_grid.csv")


# AOD
aod = list.files("/Volumes/Seagate PD JBL/smoke_PM_prediction/output/AOD_predictions/10km_smoke_days/", pattern = "^AOD_predictions_10km") %>% map_dfr(readRDS)
dat_merged = epa %>% left_join(aod, by = c("id_grid" = "grid_id_10km", "date"))



aod_dates = gsub("AOD_predictions_10km | \\.rds", "", aod_files)

for (m in 1:length(year_months)) {
  year_month = year_months[m]
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  dates_m_str = grep(paste0(y_str, m_str), all_dates_str, value = T)
  # dates_m = 
  # dates_m = grep(gsub("_", "-", year_month), all_dates, value = T)
  # dates_m_str = format(dates_m, "%Y%m%d")
  
  # Smoke days
  smoke_days = readRDS(paste0("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/smoke_days/grid_smoke_day_", year_month, ".rds"))
  
  # Anomalous AOT
  aot = readRDS(paste0("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/3_intermediate/aot_anom_smoke_days_", m_str, ".rds"))
  
  # ERA5
  era5_global = list.files("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/ERA5_variables/Global/",
                           full.names = T) %>% 
    paste0("/USA/10km_grid/UTC-0600") %>% 
    list.files(full.names = T) %>% 
    map(function(x) {
      x_name = str_split(x, pattern = "\\/")[[1]]
      l_x_name = length(x_name)
      old_name = x_name[l_x_name - 4]
      new_name = x_name[c(l_x_name - 4, l_x_name)] %>% paste0(collapse = "_")
      list.files(x, full.names = TRUE, pattern = year_month) %>%
        map_dfr(function(x) readRDS(x)) %>%
        rename(!!new_name := !!old_name)
    }) %>%
    reduce(.f = full_join, by = c("id_grid", "date"))
  
  era5_land = list.files(path_data, "/Volumes/Seagate PD JBL/smoke_PM_prediction/data/ERA5_variables/Land/",
                         full.names = T) %>% 
    {paste0(., "/USA/10km_grid/", ifelse(grepl("precipitation", .), "UTC+0000", "UTC-0600"))} %>%
    list.files(full.names = TRUE) %>%
    map(function(x) {
      x_name = str_split(x, pattern = "\\/")[[1]]
      l_x_name = length(x_name)
      old_name = x_name[l_x_name - 4]
      new_name = x_name[c(l_x_name - 4, l_x_name)] %>% paste0(collapse = "_")
      list.files(x, full.names = TRUE, pattern = year_month) %>%
        map_dfr(function(x) readRDS(x)) %>%
        rename(!!new_name := !!old_name)
    }) %>%
    reduce(.f = full_join, by = c("id_grid", "date"))
  
  # Fire
  fire = readRDS(paste0("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/distance_to_fire_cluster/grid_distance_to_fire_cluster_", year_month, ".rds"))
  
  # Month
  
  # HYSPLIT
  traj_points = readRDS(paste0("~/BurkeLab Dropbox/Data/hms_hysplit/10km_grid/grid_trajectory_points_", year_month, ".rds"))
  traj_points = traj_points %>% mutate(date = ymd(date))
  
  # AOD
  aod = grep(paste0("^", y_str, m_str), aod_dates)
  aod = aod_files[aod] %>% map_dfr(readRDS)

dat_merged = epa %>% left_join(crosswalk)

# Month
dat_merged = dat_merged %>% mutate(month = month(date))















# Set time period
start_date = "20100607" # "20060107" # "20060101"
end_date = "20201230" # "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(substr(all_dates, 1, 7))
year_months = gsub("-", "_", year_months)

# Crosswalk between EPA stations and 10 km grid cells
epa_loc = read_sf("~/BurkeLab Dropbox/Data/PM25/epa_station_locations/epa_station_locations.shp")
project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS") %>% 
  gBuffer(byid = T, width = 5000, capStyle = "SQUARE") %>% 
  st_as_sf() %>% 
  st_transform(st_crs(epa_loc))
crosswalk = epa_loc %>% 
  st_join(project_grid) %>% 
  select(id_epa = id, lon, lat, id_grid = ID)

# EPA station-days that are smoke days (incl. smokePM and fold)
# THIS PANEL IS NO LONGER VALID BECAUSE SOME MISSING SMOKE DATES WERE TREATED AS NON-SMOKE DAY
epa = readRDS("~/BurkeLab Dropbox/Data/PM25/epa_station_smokePM_full_panel_extendedCovariates.rds")
epa = epa %>% 
  select(id_epa = epa_id, lon, lat, date, smokePM, fold) %>% 
  drop_na(smokePM, fold)
df = epa %>% left_join(crosswalk)
df1 = epa %>% left_join(crosswalk, by = c("lon", "lat"))
nrow(df)
nrow(df1)
all(df1$id_epa.x == df1$id_epa.y)

#-------------------------------------------------------------------------------
#### Cross-sectional predictors ####
# Longitude and latitude


# Elevation
elevation = read.csv("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/2_from_EE/elevation_avg_sd_10km_grid.csv")

# NLCD
nlcd = read.csv("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/2_from_EE/NLCD_areas_10km_grid.csv")

#-------------------------------------------------------------------------------
#### Time-varying predictors ####

# AOD files
aod_files = list.files("/Volumes/Seagate PD JBL/smoke_PM_prediction/output/AOD_predictions/10km_smoke_days/", pattern = "^AOD_predictions_10km")
aod_dates = gsub("AOD_predictions_10km | \\.rds", "", aod_files)

for (m in 1:length(year_months)) {
  year_month = year_months[m]
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  dates_m_str = grep(paste0(y_str, m_str), all_dates_str, value = T)
  # dates_m = 
  # dates_m = grep(gsub("_", "-", year_month), all_dates, value = T)
  # dates_m_str = format(dates_m, "%Y%m%d")
  
  # Smoke days
  smoke_days = readRDS(paste0("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/smoke_days/grid_smoke_day_", year_month, ".rds"))
  
  # Anomalous AOT
  aot = readRDS(paste0("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/3_intermediate/aot_anom_smoke_days_", m_str, ".rds"))
  
  # ERA5
  era5_global = list.files("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/ERA5_variables/Global/",
                           full.names = T) %>% 
    paste0("/USA/10km_grid/UTC-0600") %>% 
    list.files(full.names = T) %>% 
    map(function(x) {
      x_name = str_split(x, pattern = "\\/")[[1]]
      l_x_name = length(x_name)
      old_name = x_name[l_x_name - 4]
      new_name = x_name[c(l_x_name - 4, l_x_name)] %>% paste0(collapse = "_")
      list.files(x, full.names = TRUE, pattern = year_month) %>%
        map_dfr(function(x) readRDS(x)) %>%
        rename(!!new_name := !!old_name)
    }) %>%
    reduce(.f = full_join, by = c("id_grid", "date"))
  
  era5_land = list.files(path_data, "/Volumes/Seagate PD JBL/smoke_PM_prediction/data/ERA5_variables/Land/",
                         full.names = T) %>% 
    {paste0(., "/USA/10km_grid/", ifelse(grepl("precipitation", .), "UTC+0000", "UTC-0600"))} %>%
    list.files(full.names = TRUE) %>%
    map(function(x) {
      x_name = str_split(x, pattern = "\\/")[[1]]
      l_x_name = length(x_name)
      old_name = x_name[l_x_name - 4]
      new_name = x_name[c(l_x_name - 4, l_x_name)] %>% paste0(collapse = "_")
      list.files(x, full.names = TRUE, pattern = year_month) %>%
        map_dfr(function(x) readRDS(x)) %>%
        rename(!!new_name := !!old_name)
    }) %>%
    reduce(.f = full_join, by = c("id_grid", "date"))
  
  # Fire
  fire = readRDS(paste0("/Volumes/Seagate PD JBL/smoke_PM_prediction/data/distance_to_fire_cluster/grid_distance_to_fire_cluster_", year_month, ".rds"))
  
  # Month
  
  # HYSPLIT
  traj_points = readRDS(paste0("~/BurkeLab Dropbox/Data/hms_hysplit/10km_grid/grid_trajectory_points_", year_month, ".rds"))
  traj_points = traj_points %>% mutate(date = ymd(date))
  
  # AOD
  aod = grep(paste0("^", y_str, m_str), aod_dates)
  aod = aod_files[aod] %>% map_dfr(readRDS)
  
  # for (d in 1:length(dates_m_str)) {
  #   ymd_str = dates_m_str[d]
  #   ymd_d = ymd(ymd_str)
  #   smoke_days_d = smoke_days %>% filter(date == ymd_d)
  #   era5_global_d = era5_global %>% filter(date == ymd_d)
  #   era5_land_d = era5_land %>% filter(date == ymd_d)
  #   fire_d = fire %>% filter(date == ymd_d)
  #   traj_points_d = traj_points %>% filter(date == ymd_d)
  #   
  #   # AOD
  #   aod = readRDS(paste0("/Volumes/Seagate PD JBL/smoke_PM_prediction/output/AOD_predictions/10km_smoke_days/AOD_predictions_10km_", ymd_str, ".rds"))
  #   
  #   
  # }
}










# Get EPA station-days and spatial folds
epa = readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates.rds"))
# Limit to smoke days
# Limit to West Coast

#### Get predictor values where and when we have output (smoke PM) ####
# Get HYSPLIT info

# Get AOD 10 km aggregations

# Get other predictors in daily 10 km grid


