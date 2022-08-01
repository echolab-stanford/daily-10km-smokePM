source("work/05_get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(lubridate)
library(foreach)
library(progressr)
library(stringr)

#-------------------------------------------------------------------------------
# Get HYSPLIT Initialization Points to Re-run
# Written by Jessica
# Last edited October 2021
#-------------------------------------------------------------------------------
# Read in rerun01
rerun1 <- readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_initialization_rerun.rds")

# Load successfully run trajectories from rerun01
files <- list.files("/Volumes/Seagate PD JBL/HYSPLIT/temp/rerun01/consolidated/rds/", full.names = TRUE, recursive = TRUE)
start_time <- get_start_time()
# progress <- txtProgressBar(min = 0, max = length(files), initial = 0, style = 3)
success1 <- function(x) {
  p <- progressor(along = x)
  res <- foreach(i = 1:length(x), .combine = bind_rows, .multicombine = TRUE) %do% {
    p()
    if (i %% 1000 == 0) print(paste(i, Sys.time()))
    file <- x[i]
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
  # setTxtProgressBar(progress, i)
  }
  return(res)
}
success1 <- with_progress(success1(files))
print_time(start_time)

# Get rerun02
rerun2 <- rerun1 %>% anti_join(success1)

# Save rerun02
saveRDS(rerun2, "~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_initialization_rerun02.rds")

#-------------------------------------------------------------------------------
# Copy and integrate success1 into previous runs
make_files_to_names <- function(x) return(file.path("/Volumes", x[3], x[4], "6-week trajectories", x[8], x[10], x[11], x[12], x[13], x[14]))

rds_files_from <- files
rds_files_to <- str_split(rds_files_from, "/") %>% lapply(make_files_to_names) %>% unlist()

raw_files_from <- list.files("/Volumes/Seagate PD JBL/HYSPLIT/temp/rerun01/consolidated/raw/", full.names = TRUE, recursive = TRUE)
raw_files_to <- str_split(raw_files_from, "/") %>% lapply(make_files_to_names) %>% unlist()

file.copy(rds_files_from, rds_files_to)
res <- file.copy(raw_files_from, raw_files_to)
