source("work/get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(foreach)
library(doParallel)

num_cores <- 6

runtimes <- vector("list", 12)

# Set path for directories
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/sequential/100/consolidated/"
# Meteorology directory
met_dir <- "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/sequential/100/exec/"

# Set trajectories to run for 48 hours
duration <- 48

# Read in HYSPLIT points in NOAA HMS data
dat_hms_hysplit1 <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct.rds"))

# REMOVE LATER
dat_hms_hysplit1 <- dat_hms_hysplit1 %>% filter(state == "California", year == 2020)
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 100)

# Define function for running splitr in parallel
source("work/get_HYSPLIT_height/01_parallelize_splitr_byday_seq.R")

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  lat = dat_hms_hysplit$lat,
  lon = dat_hms_hysplit$lon,
  height = dat_hms_hysplit$height,
  duration = duration,
  days = dat_hms_hysplit$ymd,
  daily_hours = dat_hms_hysplit$hour,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[1]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")

#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/sequential/500/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/sequential/500/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 500)

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  lat = dat_hms_hysplit$lat,
  lon = dat_hms_hysplit$lon,
  height = dat_hms_hysplit$height,
  duration = duration,
  days = dat_hms_hysplit$ymd,
  daily_hours = dat_hms_hysplit$hour,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[2]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")

#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/sequential/1000/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/sequential/1000/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 1000)

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  lat = dat_hms_hysplit$lat,
  lon = dat_hms_hysplit$lon,
  height = dat_hms_hysplit$height,
  duration = duration,
  days = dat_hms_hysplit$ymd,
  daily_hours = dat_hms_hysplit$hour,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[3]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_day/100/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_day/100/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 100)

# Define function for running splitr in parallel
source("work/get_HYSPLIT_height/01_parallelize_splitr_byday.R")

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  lat = dat_hms_hysplit$lat,
  lon = dat_hms_hysplit$lon,
  height = dat_hms_hysplit$height,
  duration = duration,
  days = dat_hms_hysplit$ymd,
  daily_hours = dat_hms_hysplit$hour,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[4]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")
#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_day/500/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_day/500/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 500)

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  lat = dat_hms_hysplit$lat,
  lon = dat_hms_hysplit$lon,
  height = dat_hms_hysplit$height,
  duration = duration,
  days = dat_hms_hysplit$ymd,
  daily_hours = dat_hms_hysplit$hour,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[5]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")
#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_day/1000/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_day/1000/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 1000)

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  lat = dat_hms_hysplit$lat,
  lon = dat_hms_hysplit$lon,
  height = dat_hms_hysplit$height,
  duration = duration,
  days = dat_hms_hysplit$ymd,
  daily_hours = dat_hms_hysplit$hour,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[6]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk/100/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk/100/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 100)

# Define function for running splitr in parallel
source("work/get_HYSPLIT_height/01_parallelize_splitr_bychunk.R")

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  df = dat_hms_hysplit,
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  duration = duration,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[7]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")

#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk/500/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk/500/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 500)

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  df = dat_hms_hysplit,
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  duration = duration,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[8]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")
#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk/1000/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk/1000/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 1000)

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  df = dat_hms_hysplit,
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  duration = duration,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[9]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk_file/100/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk_file/100/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 100)

# Define function for running splitr in parallel
source("work/get_HYSPLIT_height/01_parallelize_splitr_bychunk_fromfile.R")

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  df = dat_hms_hysplit,
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  duration = duration,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[10]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")
#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk_file/500/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk_file/500/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 500)

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  df = dat_hms_hysplit,
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  duration = duration,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[11]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")

#-------------------------------------------------------------------------------
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk_file/1000/consolidated/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/testing/test26/parallel_chunk_file/1000/exec/"

# REMOVE LATER
set.seed(123)
dat_hms_hysplit <- dat_hms_hysplit1 %>% slice_sample(n = 1000)

#-------------------------------------------------------------------------------
# Run splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  df = dat_hms_hysplit,
  num_cores = num_cores,
  out_dir = out_dir,
  exec_rm = FALSE,
  duration = duration,
  met_type = "gdas1",
  # traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
end_time <- Sys.time()
runtimes[[12]] <- difftime(end_time, start_time, units = "mins")
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories parallel")

#-------------------------------------------------------------------------------
print(unlist(runtimes))

runtimes <- c(0.4367149, 2.3959187, 6.2410806, 0.1241074, 0.7627830, 1.9210244, 
              0.1707760, 2.7981653, 10.2145787, 0.1733421, 2.6865658, 10.9063011)
df <- data.frame(x = 1:12, 
                 y = runtimes,
                 g = rep(c("sequential by day", "parallel by day", 
                           "parallel by chunk", "parallel by chunk from file"), 
                         each = 3),
                 n = rep(c(100, 500, 1000), 4))
library(ggplot2)
ggplot() +
  geom_point(data = df, mapping = aes(g, y, color = n)) + 
  geom_line(data = df, mapping = aes(g, y, group = g)) +
  ylab("runtime (minutes)")
ggsave(paste0(path_github, "work/get_HYSPLIT_height/runtimes_1.png"))
