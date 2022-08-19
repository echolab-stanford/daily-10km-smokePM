source("work/get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(sp)
library(sf)
library(lubridate)
library(stringr)
library(foreach)
library(doParallel)

num_cores <- 6

#-------------------------------------------------------------------------------
# Subset to Trajectory Points that Overlap Smoke Plumes
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Read in trajectory points
dat_traj <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_CA_2020_overlap48.rds"))

# Read in smoke plumes
dat_smoke <- readRDS(paste0(path_dropbox, "smoke/smoke_plumes_spdf.RDS"))

# Get trajectories and plumes into WGS84 geographic coordinate system
dat_traj <- st_as_sf(dat_traj, coords = c("lon", "lat"))
st_crs(dat_traj) <- 4326
dat_smoke <- st_as_sf(dat_smoke)
st_crs(dat_smoke) <- st_crs(dat_traj)

# Format plume start and end times
# ARE SMOKE PLUMES IN UTC?
dat_smoke <- dat_smoke %>% 
  mutate(Start = paste(date, str_sub(Start, -4, -1)) %>% ymd_hm(), 
         End = paste(date, str_sub(End, -4, -1)) %>% ymd_hm())

# Discard observations where definitely no temporal overlap
start <- max(min(dat_traj$traj_dt), min(dat_smoke$Start))
end <- min(max(dat_traj$traj_dt), max(dat_smoke$End))
dat_traj <- dat_traj %>% filter(start <= traj_dt, traj_dt <= end)
dat_smoke <- dat_smoke %>% filter(start <= Start, End <= end)

# Format plume intervals
dat_smoke <- dat_smoke %>% 
  mutate(interval = interval(Start, End))

nc <- 200
chunks <- split_chunks(1:nrow(dat_traj), nc)
registerDoParallel(num_cores)
start_time <- get_start_time()
dat_overlap <- foreach(i = 1:nc, .combine = bind_rows, .multicombine = TRUE) %dopar% {
  # Find trajectory points that overlap smoke plumes
  dat_overlap_i <- st_join(dat_traj[chunks[[i]],], dat_smoke, left = FALSE)
  
  # Discard where no temporal overlap
  dat_overlap_i <- dat_overlap_i %>% filter(traj_dt %within% interval)
  
  # Return overlapped data chunk
  dat_overlap_i
}
print_time(start_time)
beep_alert("Done overlapping trajectory points and plumes")
stopImplicitCluster()

# Save trajectories
saveRDS(dat_overlap, paste0(path_dropbox, "hms_hysplit/trajectories_CA_2020_overlapbreystep2.rds"))
