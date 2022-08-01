library(dplyr)
library(foreach)
library(doParallel)
library(splitr)
library(ff)

#-------------------------------------------------------------------------------
# Define function for running splitr in parallel
source("01_parallelize_splitr.R")

# Get number of cores
# Use fewer than max available to avoid segfaults
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")) - 7

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
duration <- 24*7*6

# Read in initialization points
hysplit_points_file_name <- "hms_hysplit_initialization_rerun02.rds" # "hms_hysplit_initialization_distinct.rds"
hysplit_points_file_path <- file.path(scratch, "hms_hysplit", hysplit_points_file_name)
dat_hms_hysplit <- readRDS(hysplit_points_file_path)

# Filter
run_year_month <- commandArgs(trailingOnly = TRUE)
if (length(run_year_month) > 0) {
  run_year <- as.integer(run_year_month[1])
  if (length(run_year_month) > 1) {
    run_month <- as.integer(run_year_month[2:length(run_year_month)])
  } else {
    run_month <- dat_hms_hysplit %>% filter(year == run_year) %>% pull(month) %>% unique() %>% sort()
  }
} else {
  run_year <- dat_hms_hysplit %>% pull(year) %>% unique() %>% sort()
  run_month <- dat_hms_hysplit %>% pull(month) %>% unique() %>% sort()
}
dat_hms_hysplit <- dat_hms_hysplit %>% filter(year %in% run_year, month %in% run_month)
stopifnot(nrow(dat_hms_hysplit) > 0)

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
  exec_dir = exec_dir
)
stopImplicitCluster()
