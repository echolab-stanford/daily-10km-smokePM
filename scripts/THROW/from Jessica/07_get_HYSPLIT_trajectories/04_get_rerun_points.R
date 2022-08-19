source("work/05_get_HYSPLIT_height/00_utils.R")
path_seagate <- "/Volumes/Seagate PD JBL/"
path_rds <- paste0(path_seagate, "HYSPLIT/6-week trajectories/rds/")

library(dplyr)
library(lubridate)
library(foreach)

#-------------------------------------------------------------------------------
# Get HYSPLIT Initialization Points to Re-run
# Written by Jessica
# Last edited September 2021
# 
# segfault errors occur when generating trajectories
#-------------------------------------------------------------------------------
# Read in initialization points
dat_hysplit_points <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct.rds"))

# Get initialization points that don't need to be re-run
# Took 18.5 minutes
print("Getting list of files")
# start_time <- get_start_time()
# files <- list.files(paste0(path_rds), full.names = TRUE, recursive = TRUE)
# print_time(start_time)
# saveRDS(files, "~/Desktop/HYSPLIT_files.rds")
files <- readRDS("~/Desktop/HYSPLIT_files.rds")

print("Building data frame of initialization points not needing re-run")
start_time <- get_start_time()
dat_done <- foreach(file = files, .combine = bind_rows, .multicombine = TRUE) %do% {
  traj <- readRDS(file) %>% 
    select(lon_i, lat_i, height_i, traj_dt_i) %>% 
    distinct()
  if (nrow(traj) != 1) print(file, nrow(traj))
  return(data.frame(lon = traj$lon_i,
                    lat = traj$lat_i,
                    height = traj$height_i,
                    year = year(traj$traj_dt_i),
                    month = month(traj$traj_dt_i),
                    day = day(traj$traj_dt_i),
                    hour = hour(traj$traj_dt_i)))
}
print_time(start_time)

# Using file name doesn't work
# library(data.table)
# x <- basename(head(files))
# x2 <- gsub("_-", "_m", x)
# x3 <- strsplit(x2, "_|-") %>% 
#   lapply(as.data.frame.list) %>% 
#   rbindlist() %>% 
#   select(4, 5, 6, 7, 9, 11, 13) %>% 
#   setNames(c("year", "month", "day", "hour", "lat", "lon", "height")) %>% 
#   mutate(across(c(lat, lon), function(x) gsub("m", "-", x)),
#          across(c(lat, lon), function(x) gsub("p", "\\.", x)),
#          across(everything(), as.numeric))
# df <- dat_hysplit_points %>% anti_join(x3)
# df1 <- setdiff(dat_hysplit_points %>% select(-minute, -ymd, -state), x3)

# Get initialization points that need to be re-run
dat_redo <- dat_hysplit_points %>% anti_join(dat_done)

# Save
saveRDS(dat_redo, paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_rerun.rds"))
