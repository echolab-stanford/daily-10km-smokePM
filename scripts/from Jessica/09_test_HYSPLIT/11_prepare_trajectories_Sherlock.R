# Timers
print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}

# Decide if using $GROUP_SCRATCH too
# Decide whether to remove temporary files
use_group = T
remove_temp = F

# Paths
path_scratch = Sys.getenv("SCRATCH")
run_dir = Sys.getenv("SLURM_JOB_NAME")
path_run = paste0(path_scratch, "/", run_dir, "/")
path_data = paste0(path_run, "data/")
traj_dir = paste0("/scratch/users/jessssli/prepare_trajectories/data/", "traj/")
temp_dir = paste0(path_data, "temp/")
date_dir = paste0(path_data, "date/")
misc_dir = paste0(path_data, "misc/")
if (!dir.exists(temp_dir)) dir.create(temp_dir)
if (!dir.exists(date_dir)) dir.create(date_dir)
if (!dir.exists(misc_dir)) dir.create(misc_dir)
if (use_group) {
  path_group_scratch = Sys.getenv("GROUP_SCRATCH")
  path_group_run = paste0(path_group_scratch, "/", run_dir, "/")
  path_group_data = paste0(path_group_run, "data/")
  group_temp_dir = paste0(path_group_data, "temp/")
  if (!dir.exists(group_temp_dir)) dir.create(group_temp_dir)
}

library(sp)
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(tools)
library(foreach)
library(doParallel)

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1

#-------------------------------------------------------------------------------
# Prepare HYSPLIT Trajectories for Gridding
# Written by Jessica
# Last edited December 2021
# 
# Trajectories can tell us about smoke plume height and/or smoke intensity/density.
#-------------------------------------------------------------------------------
duration_days = 6

# Set time period
args = commandArgs(trailingOnly = T)
start_date = args[1]
end_date = args[2]
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")

# Get trajectory file paths
traj_files = system(sprintf("find %s", traj_dir), intern = T)
traj_files = grep("\\.rds$", traj_files, value = T)
initialization_dates = file_path_sans_ext(basename(traj_files))
initialization_dates = gsub("traj-traj-fwd-", "", initialization_dates)
initialization_dates = paste0(20, substr(initialization_dates, 1, 8))
initialization_dates = ymd(initialization_dates)
all_dates_ext = (ymd(start_date) - days(duration_days)) %--% (ymd(end_date) + days(1))
w = which(initialization_dates %within% all_dates_ext)
traj_files = traj_files[w]
initialization_dates = initialization_dates[w]

# Load smoke plumes
smoke = readRDS(paste0("/scratch/users/jessssli/prepare_trajectories/", "smoke_plumes_sfdf.RDS")) %>% select(date)
crs_hms = st_crs(smoke)
rm(smoke)

# Read in crosswalk
crosswalk = readRDS(paste0("/scratch/users/jessssli/prepare_trajectories/", "hms_hysplit_initialization_duplicates_20060419-20201231.rds")) %>%
  mutate(datetime = ymd_hm(paste(ymd, hour, minute), tz = "UTC")) %>%
  select(id_hysplit, id_height, lon, lat, height, datetime)

#-------------------------------------------------------------------------------
#### Get qualifying trajectory points and save by operational day ####
print("STARTED PART 1-------------------------------------------------------------------------------")
registerDoParallel(num_cores)
start_time = get_start_time()
l_traj_files = length(traj_files)
l_traj_files_2 = l_traj_files/2
traj_out = foreach(i = 1:l_traj_files) %dopar% {
  print(paste("Started file", i, "at:", Sys.time()))
  traj_file = traj_files[i]
  traj = readRDS(traj_file) %>%
    select(hour_along, traj_dt, lat, lon, height, pressure,
           traj_dt_i, lat_i, lon_i, height_i) %>%
    # Assume HYSPLIT points have same CRS as smoke plumes
    st_as_sf(coords = c("lon", "lat"), remove = F, crs = crs_hms)
  
  #### Discard trajectory points after trajectory hits ground
  hit_ground = traj$height == 0
  if (sum(hit_ground) > 0) {
    hit_ground_index = min(which(hit_ground))
    traj = traj[1:hit_ground_index, ]
  }
  
  dates_in_range = traj$traj_dt %>% with_tz("Etc/GMT+6") %>% format("%Y%m%d") %>% unique()
  dates_in_range = any(dates_in_range %in% all_dates_str)
  if (!dates_in_range) return(1)
  
  #### Get consecutive distance and traveled distance
  traj = traj %>% arrange(hour_along)
  if (nrow(traj) > 1){
    traj_points = traj$geometry
    consecutive_distance = pointDistance(as_Spatial(traj_points[-1]),
                                         as_Spatial(traj_points[-length(traj_points)]),
                                         lonlat = T)/1000
    traveled_distance = cumsum(consecutive_distance)
    traj = traj %>% mutate(consecutive_distance_km = c(NA, consecutive_distance),
                           traveled_distance_km = c(0, traveled_distance),
                           .before = geometry)
  } else if (nrow(traj) == 1) {
    traj = traj %>% mutate(consecutive_distance_km = NA,
                           traveled_distance_km = 0,
                           .before = geometry)
  }
  
  #### Re-weight sample to reflect original distribution of HYSPLIT points
  # i.e. duplicate trajectory given sample weight
  crosswalk_t = crosswalk %>%
    filter(datetime %in% unique(traj$traj_dt_i),
           lat %in% unique(traj$lat_i),
           lon %in% unique(traj$lon_i),
           height %in% unique(traj$height_i))
  traj = traj %>%
    left_join(crosswalk_t, by = c("traj_dt_i" = "datetime", "lat_i" = "lat",
                                  "lon_i" = "lon", "height_i" = "height"))
  
  #### Append points by operational day
  # Convert to UTC-06:00 and get operational date of points
  traj = traj %>%
    mutate(traj_dt_UTCm6 = with_tz(traj_dt, "Etc/GMT+6"),
           date = format(traj_dt_UTCm6, "%Y%m%d")) %>%
    select(date, traj_dt_UTCm6, lon, lat,
           height, pressure, hour_along,
           consecutive_distance_km, traveled_distance_km, 
           id_hysplit, id_height)
  
  # For each operational day, read trajectory points file for that day,
  # append additional from this trajectory, and save/overwrite
  temp_dir_i = temp_dir
  if (use_group) if (i > l_traj_files_2) temp_dir_i = group_temp_dir
  
  initialization_date = initialization_dates[i]
  traj_dates = seq.Date(initialization_date - days(1), initialization_date + days(duration_days), by = "day")
  traj_dates = format(traj_dates, "%Y%m%d")
  return_codes = c()
  for (traj_date in traj_dates) {
    # Skip dates outside time period (e.g. dates with trajectory points but did not initialize trajectories from)
    if (!(traj_date %in% all_dates_str)) {
      return_codes = c(return_codes, 11)
      next
    }
    traj_points = traj %>% filter(date == traj_date)
    # Skip dates without any trajectory points
    if (nrow(traj_points) == 0) {
      return_codes = c(return_codes, 12)
      next
    }
    traj_points_file = paste0(temp_dir_i, "trajectory_points", traj_date, "_", str_pad(i, 7, "left", 0), ".rds")
    saveRDS(traj_points, traj_points_file)
    return_codes = c(return_codes, 0)
  }
  return(return_codes)
}
print_time(start_time)

start_time = get_start_time()
saveRDS(traj_out, paste0(misc_dir, "traj_out.rds"))
print_time(start_time)

#-------------------------------------------------------------------------------
#### Combine qualifying trajectory points by operational day ####
print("STARTED PART 2-------------------------------------------------------------------------------")
traj_points_files = list.files(temp_dir, pattern = "^trajectory_points.*\\.rds$")
if (use_group) traj_points_files_group = list.files(group_temp_dir, pattern = "^trajectory_points.*\\.rds$")
start_time = get_start_time()
consecutive_distances = foreach(d = all_dates_str, .combine = c) %dopar% {
  print(paste("Started working on date", d, "at:", Sys.time()))
  traj_points_files_d = grep(d, traj_points_files, value = T)
  if (length(traj_points_files_d) > 0) traj_points_files_d = paste0(temp_dir, traj_points_files_d)
  if (use_group) {
    traj_points_files_d_group = grep(d, traj_points_files_group, value = T)
    if (length(traj_points_files_d_group) > 0) {
      traj_points_files_d_group = paste0(group_temp_dir, traj_points_files_d_group)
      traj_points_files_d = c(traj_points_files_d, traj_points_files_d_group)
    }
  }
  if (length(traj_points_files_d) == 0) return(numeric(0))

  traj_points = traj_points_files_d %>% map_dfr(readRDS)
  traj_date_file = paste0(date_dir, "trajectory_points", d, ".rds")
  saveRDS(traj_points, traj_date_file)
  if (remove_temp) file.remove(traj_points_files_d)
  return(traj_points$consecutive_distance_km)
}
print_time(start_time)
if (remove_temp) unlink(temp_dir, recursive = T)
if (use_group) if(remove_temp) unlink(group_temp_dir, recursive = T)
stopImplicitCluster()

start_time = get_start_time()
saveRDS(consecutive_distances, paste0(misc_dir, "consecutive_distances.rds"))
print_time(start_time)
