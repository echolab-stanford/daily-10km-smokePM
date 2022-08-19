source("work/get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(zoo)

#-------------------------------------------------------------------------------
# Link Trajectory Points to HYSPLIT Points
# Written by Jessica
# Last edited June 2021
# 
# A trajectory point (lon + lat + height + date) can be recorded multiple times 
# to represent multiple HYSPLIT points that rounded to the same initialization
# point.
#-------------------------------------------------------------------------------
# Read in crosswalk
crosswalk <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_semiduplicates.rds")) %>% 
  mutate(datetime = sprintf("%s %s:%s:00", ymd, hour, minute) %>% as.POSIXct(tz = "UTC")) %>% 
  select(id, lon, lat, height, datetime) %>% 
  rename(id_hysplit = id)

# Read in trajectory points
dat_traj <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_CA_2020.rds")) %>% 
  bind_rows() %>% 
  select(-receptor) %>% 
  mutate(year = year + 2000L)

# Assign trajectory IDs
inits <- dat_traj$hour_along == 0
dat_traj[inits, "id_traj"] <- 1:sum(inits)
dat_traj$id_traj <- na.locf(dat_traj$id_traj)

# Get initialization location in each row
loc_inits <- dat_traj[inits, c("lat", "lon", "height")]
dat_traj[inits, "lat_i"] <- loc_inits$lat
dat_traj[inits, "lon_i"] <- loc_inits$lon
dat_traj[inits, "height_i"] <- loc_inits$height
dat_traj$lat_i <- na.locf(dat_traj$lat_i)
dat_traj$lon_i <- na.locf(dat_traj$lon_i)
dat_traj$height_i <- na.locf(dat_traj$height_i)

# Link trajectory points to HYSPLIT points
dat_linked <- dat_traj %>% 
  left_join(crosswalk, by = c("traj_dt_i" = "datetime", "lat_i" = "lat", 
                              "lon_i" = "lon", "height_i" = "height"))

# Save HYSPLIT-linked trajectory points
saveRDS(dat_linked, paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_CA_2020_linked.rds"))
