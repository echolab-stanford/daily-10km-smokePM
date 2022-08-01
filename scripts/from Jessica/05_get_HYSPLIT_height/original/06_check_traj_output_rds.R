library(dplyr)
library(purrr)
library(splitr)
library(zoo)

#-------------------------------------------------------------------------------
# Subset to Trajectories Run w/ Duration < 6 Weeks
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
scratch <- Sys.getenv("SCRATCH")

# Duration that each trajectory should have run
dur <- 24*7*6

# Get trajectories
trajs <- list.files(paste0(scratch, "/trajectories/rds"), full.names = TRUE) %>% 
  map_dfr(readRDS) %>% 
  mutate(year = year + 2000)

# Assign trajectory IDs
trajs[trajs$hour_along == 0, "id"] <- 1:sum(trajs$hour_along == 0)
trajs$id <- na.locf(trajs$id)

# Get duration run for each trajectory
trajs <- trajs %>% 
  group_by(id) %>% 
  mutate(duration = n()) %>% 
  ungroup()
print("before filtering")
print(nrow(trajs))
print(count(trajs, duration))

# Subset to initialization points of trajectories that ran 6 weeks
trajs <- trajs %>% filter(hour_along == 0, duration == dur + 1)
print("after filtering")
print(nrow(trajs))
print(count(trajs, duration))

# Read in initialization points
dat_hms_hysplit <- readRDS(paste0(scratch, "/hms_hysplit/hms_hysplit_initialization_distinct.rds"))

# Subset to initialization points that did not run 6 weeks
dat_hms_hysplit <- anti_join(dat_hms_hysplit, trajs)
saveRDS(dat_hms_hysplit, paste0(scratch, "/hms_hysplit/hms_hysplit_initialization_rerun.rds"))
