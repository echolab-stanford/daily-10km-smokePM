source("work/get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(foreach)
library(doParallel)

num_cores <- 6

#-------------------------------------------------------------------------------
# Generate 48-hour trajectories using parallel splitr
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Set path for directories
# Output directory where trajectories get stored
out_dir <- paste0(path_dropbox, "hms_hysplit/trajectories/raw/")
# Meteorology directory
met_dir <- paste0(path_dropbox, "meteorology/gdas1/")
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/HYSPLIT_CA_2020/"

# Set trajectories to run for 48 hours
duration <- 48

# Read in HYSPLIT points in NOAA HMS data
dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct.rds"))

# Define function for running splitr in parallel
source("work/get_HYSPLIT_height/01_parallelize_splitr.R")

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  out_dir = out_dir,
  exec_rm = FALSE,
  lat = dat_hms_hysplit$lat,
  lon = dat_hms_hysplit$lon,
  height = dat_hms_hysplit$height,
  duration = duration,
  days = dat_hms_hysplit$ymd,
  daily_hours = dat_hms_hysplit$hour,
  met_type = "gdas1",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")
