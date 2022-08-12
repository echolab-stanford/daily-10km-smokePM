source("work/05_get_HYSPLIT_height/00_utils.R")
path_github <- "~/Documents/GitHub/purple-air-infiltration/" # delete once move repos

# library(ecmwfr)
# cds_uid <- readRDS("../../Registrations/Copernicus Climate Data Store/CDS_UID.rds")
# wf_set_key(user = cds_uid,
#            key = readRDS("../../Registrations/Copernicus Climate Data Store/CDS_API_KEY.rds"),
#            service = "cds")

env_name = "era5"
if (!(env_name %in% reticulate::conda_list()$name)) {
  reticulate::conda_create(env_name)
}
reticulate::use_condaenv(env_name, required = T)
library(reticulate)
if (!py_module_available("cdsapi")) py_install("cdsapi", forge = T)
if (!py_module_available("tenacity")) py_install("tenacity", forge = T)
source_python(paste0(path_github, "work/01_process_aot_smoke_pm/14_define_CDS_API_request.py"))

library(stringr)
library(lubridate)

#-------------------------------------------------------------------------------
# Retrieve ERA5(-Land) Daily Aggregates
# Written by Jessica
# Last edited October 2021
# 
# Download ERA5(-Land) daily aggregates using the "Daily statistics calculated 
# from ERA5 data" application 
# (https://cds.climate.copernicus.eu/cdsapp#!/software/app-c3s-daily-era5-statistics?tab=app)
# accessed via the CDS Python API.
#-------------------------------------------------------------------------------
#### Explore data products ####
# Explore ECMWF data sets if desired
# View(wf_datasets(user = cds_uid,
#                  service = "cds"))

# Explore ERA5(-Land) daily data if desired
# This does not actually work in current version of ecmwfr package
# View(wf_product_info(dataset = "app-c3s-daily-era5-statistics",
#                      user = cds_uid,
#                      service = "cds"))

#-------------------------------------------------------------------------------
#### Set time zone and area ####
# year and month must be passed in as integers, e.g. 2020L, 3L
years <- 2006:2020
months <- 1:12

# Contiguous US time zones range UTC-04:00 (EDT) to UTC-08:00 (PST), so we use
# UTC-06:00 (CST/MDT)
time_zone <- "UTC-06:00"

# Contiguous US
north <- 49.92
south <- 24.43
east <- -66.62
west <- -125.5

# Output directory
path_out <- paste0(path_dropbox, "ERA5/")

#-------------------------------------------------------------------------------
#### Retrieve data ####
#-------------------------------------------------------------------------------
retrieve_era5 <- function(variable, statistic, dataset, path_log = "~/Desktop/") {
  if (variable == "total_precipitation") stopifnot(time_zone == "UTC+00:00")
  time_zone_dir <- gsub(":", "", time_zone)
  
  stopifnot(dataset %in% c("global", "land"))
  dataset_dir <- str_to_title(dataset)
  area <- list(lat = c(south, north), lon = c(west, east))
  statistic_run <- statistic
  # daily_sum <- statistic == "daily_sum"
  # if (daily_sum) statistic_run <- "daily_mean"
  
  # Full dataset name
  dataset <- ifelse(dataset == "global", "reanalysis-era5-single-levels", "reanalysis-era5-land")
  
  # Start logging output and messages to console
  warn_option <- getOption("warn")
  options(warn = 1)
  log_file_name <- file.path(path_log, sprintf("%s_%s_%s.log", dataset, variable, statistic))
  try(log_file <- file(log_file_name, open="at")) # open file for appending in text mode
  sink(log_file, type="output", append = TRUE, split = TRUE)
  sink(log_file, type="message", append = TRUE)
  
  for (year in years) {
    for (month in months) {
      start_time <- get_start_time(paste("Started:", variable, year, month))
      retrieve_era5_daily(
        dataset = dataset,
        variable = variable,
        statistic = statistic_run,
        year = year,
        month = month,
        time_zone = time_zone,
        area = area,
        folder = file.path(path_out, dataset_dir, variable, "USA", "raw", time_zone_dir,
                           paste0(statistic, "_of_1-hourly"))
      )

      # Multiply by 24 to get daily_sum
      # if (daily_sum) {
      #   file_name_run <- file.path(
      #     path_out, dataset_dir, variable, "USA", "raw", time_zone_dir, paste0(statistic, "_of_1-hourly"),
      #     sprintf("%s_%s_%s_%s_%s.nc", dataset, variable, statistic_run, year, str_pad(month, 2, "left", 0))
      #   )
      #   file_name <- file.path(
      #     path_out, dataset_dir, variable, "USA", "raw", time_zone_dir, paste0(statistic, "_of_1-hourly"),
      #     sprintf("%s_%s_%s_%s_%s.nc", dataset, variable, statistic, year, str_pad(month, 2, "left", 0))
      #   )
      #   file_data <- brick(file_name_run)
      #   file_data_output <- capture.output(x)
      #   vn <- trimws(gsub("(.*)\\:","", file_data_output[9], perl=TRUE))
      #   values(file_data) <- values(file_data)*24
      #   writeRaster(file_data, file_name, varname = vn, overwrite = TRUE)
      #   unlink(file_name_run)
      # }
      
      run_time <- print_time(start_time)
      
      # Wait a bit before next retrieval if current retrieval ran fast/from cache
      if (run_time < minutes(1)) Sys.sleep(10)
    }
  }
  # Stop logging
  sink(type = "output")
  sink(type = "message")
  close(log_file)
  options(warn = warn_option)
}

# Set temporary working directory
path_temp <- paste0(path_out, "temp/")
if (!dir.exists(path_temp)) dir.create(path_temp)
setwd(path_temp)

#-------------------------------------------------------------------------------
#### ERA5 ####
# Takes 4.5-6.5 minutes per month

# PBL (mean)
retrieve_era5("boundary_layer_height", "daily_mean", "global")

# PBL (min)
retrieve_era5("boundary_layer_height", "daily_minimum", "global")

# PBL (max)
retrieve_era5("boundary_layer_height", "daily_maximum", "global")

# Mean sea level pressure
retrieve_era5("mean_sea_level_pressure", "daily_mean", "global")

#-------------------------------------------------------------------------------
#### ERA5-Land ####
# Takes 15-90 minutes per month

# 2-meter dew point temperature
retrieve_era5("2m_dewpoint_temperature", "daily_mean", "land")

# 2-meter temperature
retrieve_era5("2m_temperature", "daily_mean", "land")

# 10-meter u-component of wind
retrieve_era5("10m_u_component_of_wind", "daily_mean", "land")

# 10-meter v-component of wind
retrieve_era5("10m_v_component_of_wind", "daily_mean", "land")

# Surface pressure
retrieve_era5("surface_pressure", "daily_mean", "land")

# Total precipitation
# ERA5-Land total_precipitation is cumulative
time_zone <- "UTC+00:00"
retrieve_era5("total_precipitation", "daily_maximum", "land")

# Remove temporary working directory
setwd(path_github)
unlink(path_temp, recursive = TRUE)
