library(dplyr)
library(foreach)
library(doParallel)
library(splitr)
library(ff)

#-------------------------------------------------------------------------------
# Define function for running splitr in parallel
source("01_parallelize_splitr.R")

# Get number of cores
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

# Get file paths from bash script
scratch <- Sys.getenv("SCRATCH")
run_dir <- Sys.getenv("SLURM_JOB_NAME")
# Output directory where trajectories get stored
out_dir <- sprintf("%s/%s/consolidated/", scratch, run_dir)
# Meteorology directory
met_dir <- sprintf("%s/gdas1/", scratch)
# Directory in which working directories will be made
exec_dir <- sprintf("%s/%s/working/", scratch, run_dir)

# Set trajectory duration
duration <- 72

# Read in initialization points
dat_hms_hysplit <- readRDS(sprintf("%s/hms_hysplit/hms_hysplit_initialization_distinct.rds", scratch))

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
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
stopImplicitCluster()
