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

# Paths
path_scratch = Sys.getenv("SCRATCH")
run_dir = Sys.getenv("SLURM_JOB_NAME")
path_run = paste0(path_scratch, "/", run_dir, "/")
path_data = paste0(path_run, "data/")
traj_dir = paste0(path_data, "traj/")
temp_dir = paste0(path_data, "temp/")
date_dir = paste0(path_data, "date/")
misc_dir = paste0(path_data, "misc/")
if (!dir.exists(temp_dir)) dir.create(temp_dir)
if (!dir.exists(date_dir)) dir.create(date_dir)
if (!dir.exists(misc_dir)) dir.create(misc_dir)
remove_temp = F

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
# Last edited November 2021
# 
# Trajectories can tell us about smoke plume height and/or smoke intensity/density.
#-------------------------------------------------------------------------------
# Set duration cutoff
duration_days = 6
duration_cutoff = 24*duration_days

# NEED TO REDO FOR BEGINNING OF PERIOD 20100601 THRU 6 DAYS FROM 20100531 ONCE GET FROM START OF 2006
# Set time period
start_date = "20100607" # "20100601" # "20060101
end_date = "20201230" # "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")

# Get trajectory file paths
# Takes a few minutes to list files on local but many hours on Sherlock
# traj_files = list.files(traj_dir, recursive = T, full.names = T)
# saveRDS(traj_files, paste0(path_run, "traj_files_2010_2020.rds"))
traj_files = readRDS(paste0(path_run, "traj_files_2010_2020.rds"))
traj_files = paste0(traj_dir, traj_files)
initialization_dates = file_path_sans_ext(basename(traj_files))
initialization_dates = gsub("traj-traj-fwd-", "", initialization_dates)
initialization_dates = paste0(20, substr(initialization_dates, 1, 8))
initialization_dates = ymd(initialization_dates)
all_dates_ext = (ymd(start_date) - days(duration_days)) %--% (ymd(end_date) + days(1))
w = which(initialization_dates %within% all_dates_ext)
traj_files = traj_files[w]
initialization_dates = initialization_dates[w]

# Load smoke plumes
smoke = readRDS(paste0(path_run, "smoke_plumes_sfdf.RDS")) %>% select(date)

# Drop plumes with invalid geometry (i.e. 161 plumes across 124 dates)
# Takes ~1-2 hours
# valid_plume = st_is_valid(smoke)
# saveRDS(valid_plume, paste0(path_run, "valid_plume.rds"))
# valid_plume = readRDS(paste0(path_run, "valid_plume.rds"))
# smoke = smoke %>% filter(valid_plume)
crs_hms = st_crs(smoke)

# No need to copy this to each CPU if we do not filter by plume-point overlap
rm(smoke)

# Read in crosswalk
crosswalk = readRDS(paste0(path_run, "hms_hysplit_initialization_semiduplicates.rds")) %>% 
  mutate(datetime = ymd_hm(paste(ymd, hour, minute), tz = "UTC")) %>% 
  select(id, lon, lat, height, datetime) %>% 
  rename(id_hysplit = id)

#-------------------------------------------------------------------------------
#### Get qualifying trajectory points and save by operational day ####
print("STARTED PART 1-------------------------------------------------------------------------------")
registerDoParallel(num_cores)
start_time = get_start_time()
traj_out = foreach(i = 1:length(traj_files)) %dopar% {
  print(paste("Started file", i, "at:", Sys.time()))
  traj_file = traj_files[i]
  traj = readRDS(traj_file) %>% 
    # Cut off trajectory duration
    filter(hour_along <= duration_cutoff) %>% 
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
    select(date, traj_dt_UTCm6, lon, lat, height, pressure, hour_along, id_hysplit)
  
  # For each operational day, read trajectory points file for that day,
  # append additional from this trajectory, and save/overwrite
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
    traj_points_file = paste0(temp_dir, "trajectory_points", traj_date, "_", str_pad(i, 7, "left", 0), ".rds")
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
start_time = get_start_time()
date_out = foreach(d = all_dates_str) %dopar% {
  print(paste("Started working on date", d, "at:", Sys.time()))
  traj_points_files_d = grep(d, traj_points_files, value = T)
  if (length(traj_points_files_d) == 0) return(1)
  traj_points_files_d = paste0(temp_dir, traj_points_files_d)
  traj_points = traj_points_files_d %>% map_dfr(readRDS)
  traj_date_file = paste0(date_dir, "trajectory_points", d, ".rds")
  saveRDS(traj_points, traj_date_file)
  if (remove_temp) file.remove(traj_points_files_d)
  return(0)
}
print_time(start_time)
if (remove_temp) unlink(temp_dir, recursive = T)
stopImplicitCluster()

start_time = get_start_time()
saveRDS(date_out, paste0(misc_dir, "date_out.rds"))
print_time(start_time)
