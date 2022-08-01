scratch <- Sys.getenv("SCRATCH")

split_chunks <- function(v, chunks) {
  return(split(v, cut(seq_along(v), chunks, labels = FALSE)))
}

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

library(stringr)
library(raster)
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(lutz)
library(lubridate)

num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

#-------------------------------------------------------------------------------
# Match HYSPLIT Trajectory Points to EPA Station-Days
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Distance (meters) within which to match
within_m <- 1000*100

# Read in trajectory points
dat_traj <- readRDS(paste0(scratch, "/matching/trajectories_CA_2020_overlap48.rds")) %>% 
  select(id_traj, traj_dt, id_hysplit, lon, lat, height, pressure, hour_along)

# Read in EPA station-days
dat_epa <- readRDS(paste0(scratch, "/matching/epa_station_level_pm25_data.rds")) %>% 
  select(id, lon, lat, date, pm25, county, county_code, cbsa_code, cbsa_name) %>% 
  mutate(date = str_pad(date, 8, "left", 0),
         date = paste(substr(date, 5, 8), substr(date, 1, 2), substr(date, 3, 4), sep = "-")) %>% 
  rename(id_epa = id, date_epa = date) %>% 
  as.data.frame() %>% 
  # Discard duplicate rows
  distinct()

# EPA sometimes records multiple PM2.5 values for the same station-day
# Average station-day PM2.5 values
dat_epa <- dat_epa %>% 
  group_by(across(c(-pm25))) %>% 
  summarize(pm25 = mean(pm25, na.rm = TRUE)) %>% 
  ungroup()

# Keep only data in overlapping time period
min_traj <- (min(dat_traj$traj_dt) - days(1)) %>% strftime(format = "%Y-%m-%d", tz = "UTC")
max_traj <- max(dat_traj$traj_dt) %>% strftime(format = "%Y-%m-%d", tz = "UTC")
min_epa <- min(dat_epa$date_epa)
max_epa <- max(dat_epa$date_epa)
min_dt <- max(min_traj, min_epa)
max_dt <- min(max_traj, max_epa)
dat_traj <- dat_traj %>% filter(ymd(min_dt, tz = "UTC") <= traj_dt, 
                                traj_dt <= ymd(max_dt, tz = "UTC"))
dat_epa <- dat_epa %>% filter(min_dt <= date_epa, date_epa <= max_dt)

#-------------------------------------------------------------------------------
#### Spatial matching ####
# Get unique trajectory point locations
loc_traj <- dat_traj %>% 
  select(lon, lat) %>% 
  distinct()

# Get unique EPA station locations
loc_epa <- dat_epa %>% 
  select(lon, lat) %>% 
  distinct()

# Match each EPA station to trajectory points within a specific distance
nr <- nrow(loc_epa)
radius_lat <- within_m/111111
registerDoParallel(num_cores)
start_time <- get_start_time()
dat_matched <- foreach(i = 1:nr, .combine = bind_rows, .multicombine = TRUE) %dopar% {
  pt_epa <- loc_epa[i,]
  
  # Find trajectory points within a square around EPA station
  radius_lon <- radius_lat/cos(pt_epa$lat * pi/180)
  within_square <- loc_traj$lat >= pt_epa$lat - radius_lat & 
    loc_traj$lat <= pt_epa$lat + radius_lat & 
    loc_traj$lon >= pt_epa$lon - radius_lon & 
    loc_traj$lon <= pt_epa$lon + radius_lon
  
  # Default to NA for EPA stations w/ no matches
  matches <- data.frame(lon = NA, lat = NA, dist = NA)
  
  if (any(within_square)) {
    # Find trajectory points within a circle around EPA station
    dists <- pointDistance(pt_epa, 
                           loc_traj[within_square,], 
                           lonlat = TRUE)
    within_circle <- dists <= within_m
    
    if(any(within_circle)) {
      matches <- loc_traj[within_square,][within_circle,] %>% 
        mutate(dist = dists[within_circle])
    }
  }
  # Bind EPA station location and matched trajectory point locations
  matches <- pt_epa %>% 
    rename(lon_epa = lon, lat_epa = lat) %>% 
    bind_cols(matches) %>% 
    rename(lon_traj = lon, lat_traj = lat)
}
print_time(start_time)
stopImplicitCluster()

# Drop station-days with no spatially matching trajectory points
dat_matched <- dat_matched %>% drop_na()

#-------------------------------------------------------------------------------
#### Temporal matching ####
# Get local time zone of EPA station
dat_matched <- dat_matched %>% 
  mutate(tz_epa = tz_lookup_coords(lat_epa, lon_epa),
         date_traj = NA)

nr <- nrow(dat_matched)
# Set number of chunks: 2000 rows per chunk when using 6 cores, decrease if more cores
nc <- ceiling(nr/2000)
chunks <- split_chunks(1:nr, nc)
registerDoParallel(num_cores)
start_time <- get_start_time()
dat_matched <- foreach(i = 1:nc, .combine = bind_rows, .multicombine = TRUE) %dopar% {
  # Get location pair and its local time zone
  loc_pairs <- dat_matched[chunks[[i]],] %>% 
    # Join on EPA and trajectory locations to date-times and variables of interest
    left_join(dat_epa, by = c("lon_epa" = "lon", "lat_epa" = "lat")) %>% 
    left_join(dat_traj, by = c("lon_traj" = "lon", "lat_traj" = "lat"))
  
  # Adjust date-time to local time zone and convert date to character
  tzs <- unique(loc_pairs$tz_epa)
  for (t in tzs) {
    same_tz <- loc_pairs$tz_epa == t
    date_traj <- loc_pairs$traj_dt[same_tz]
    loc_pairs$date_traj[same_tz] <- date_traj %>% strftime(format = "%Y-%m-%d", tz = t)
  }
  
  # Keep only where dates match
  loc_pairs <- loc_pairs %>% filter(date_epa == date_traj)
}
print_time(start_time)
stopImplicitCluster()

# Discard unnecessary columns
dat_matched <- dat_matched %>% 
  rename(date = date_epa) %>% 
  select(-tz_epa, -date_traj, -traj_dt)

# Save matched data
saveRDS(dat_matched, paste0(scratch, "/matching/results/trajectories_EPA_CA_2020_matched48.rds"))
