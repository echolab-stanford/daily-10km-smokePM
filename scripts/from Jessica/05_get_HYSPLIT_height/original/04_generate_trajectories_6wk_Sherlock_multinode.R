library(dplyr)
library(foreach)
library(doParallel)
library(splitr)
library(ff)

#-------------------------------------------------------------------------------
scratch <- Sys.getenv("SCRATCH")
run_dir <- Sys.getenv("SLURM_JOB_NAME")
node_dir <- Sys.getenv("SLURM_ARRAY_TASK_ID")

# Define function for running splitr in parallel
source("01_parallelize_splitr.R")

# Get number of cores
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

# Final output directory across nodes
final_dir <- sprintf("%s/%s/final/", scratch, run_dir)
if (!dir.exists(final_dir)) dir.create(final_dir)

# Create node directories
node_path <- sprintf("%s/%s/%s/", scratch, run_dir, node_dir)
if (!dir.exists(node_path)) dir.create(node_path)

# Output directory where trajectories get stored
out_dir <- sprintf("%s/%s/%s/consolidated/", scratch, run_dir, node_dir)
# Meteorology directory
met_dir <- sprintf("%s/gdas1/", scratch)
# Directory in which working directories will be made
exec_dir <- sprintf("%s/%s/%s/working/", scratch, run_dir, node_dir)

# Set trajectories to run for 6 weeks
duration <- 24*7*6

# Read in initialization points
dat_hms_hysplit <- readRDS(sprintf("%s/hms_hysplit/hms_hysplit_initialization_distinct_%s.rds",
                                   scratch, node_dir))

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores - 1)
hysplit_trajectory_parallel(
  out_dir = out_dir,
  exec_rm = TRUE,
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

#-------------------------------------------------------------------------------
# Copy trajectories to final directory
out_dir %>% 
  lapply(list.files, full.names = TRUE) %>% 
  lapply(file.copy, final_dir)
