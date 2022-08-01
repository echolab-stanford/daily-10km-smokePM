source("work/get_HYSPLIT_height/00_utils.R")

library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(foreach)
library(doParallel)
library(splitr)
library(ff)

num_cores <- 2

#-------------------------------------------------------------------------------
# Parallelize splitr
# Written by Jessica
# Last edited May 2020
# 
# Define function for running splitr in parallel to generate HYSPLIT trajectories
# more efficiently:
#    Create meteorology directory if does not exist.
#    Create working directory for each core. Prevents mix-up of intermediate
#       files that would render output inaccurate.
#    Feed HYSPLIT starting points as irregular parameters.
#    Generate HYSPLIT trajectories using multiple cores simultaneously.
#    Gather generated trajectories in one output directory.
#    Delete working directories when done generating all trajectories.
# 
# Issues remaining:
#    Parallelize across months as opposed to days to minimize overhead.
#    Delete meteorology files that are no longer needed to prevent memory overload.
#    Specify minutes as a parameter.
#       Most likely need to fork splitr and modify source code.
# 
# Questions:
#    Are HMS HYSPLIT starting points specified in UTC?
#    Are HMS HYSPLIT durations in units of hours?
#    Why are there duplicate HMS HYSPLIT starting points?
#       Error in data recording.
#    What injection heights will we use?
#-------------------------------------------------------------------------------
# Set path for directories
# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Documents/04 Sub-Annual Model of Wildfire PM/test10/consolidated"
# Meteorology directory
met_dir <- "/Users/jessssli/Documents/04 Sub-Annual Model of Wildfire PM/test10/met"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Documents/04 Sub-Annual Model of Wildfire PM/test10/out"

# Read in HYSPLIT points in NOAA HMS data
dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points.rds"))

# Restructure HMS data
dat_hms_hysplit <- dat_hms_hysplit %>% 
  map_dfr(st_drop_geometry) %>% 
  mutate(lon = ifelse(is.na(Lon), X......LON, Lon),
         lat = ifelse(is.na(Lat), X....LAT, Lat),
         YYYYmmdd = ifelse(is.na(Date), X.YearMmDd, Date),
         HHMM = ifelse(is.na(Time), X.HhMm, Time),
         duration = ifelse(is.na(Duration), X.Dura, Duration)) %>% 
  separate(YYYYmmdd, c("year", "month", "day"), c(4, 6)) %>% 
  separate(HHMM, c("hour", "minute"), 2) %>% 
  mutate(hour = as.numeric(hour), 
         minute = as.numeric(minute),
         height = 500) %>% ### NEED INJECTION HEIGHTS
  select(lon, lat, height, year, month, day, hour, minute, duration) %>% 
  distinct()

# Remove later
dat_hms_hysplit <- dat_hms_hysplit %>% head(70)

#-------------------------------------------------------------------------------
# Define function for running splitr in parallel
hysplit_trajectory_parallel <- 
  function(num_cores = 1, out_dir = NULL, exec_rm = TRUE,
           lat = 49.263, lon = -123.25, height = 50, duration = 24, days = NULL, 
           daily_hours = 0, direction = "forward", met_type = "reanalysis", 
           vert_motion= 0, model_height = 20000, extended_met = FALSE, 
           config = NULL, ascdata = NULL, traj_name = NULL, binary_path = NULL,
           met_dir = NULL, exec_dir = NULL, clean_up = FALSE) {
    
    # Create meteorology directory
    if (!dir.exists(met_dir)) dir.create(met_dir)
    
    # Create a working directory for each core
    if (!dir.exists(exec_dir)) dir.create(exec_dir)
    nced <- nchar(exec_dir)
    if (substr(exec_dir, nced, nced) != "/") exec_dir <- paste0(exec_dir, "/")
    exec_dir <- paste0(exec_dir, seq(num_cores))
    for (d in exec_dir) if (!dir.exists(d)) dir.create(d)
    
    # Create output directory
    if (!dir.exists(out_dir)) dir.create(out_dir)
    
    # Each core handles one day at a time
    unique_days <- unique(days)
    foreach(i = seq_along(unique_days)) %dopar% {
      
      # Discard meteorology data no longer needed
      ###
      
      # Determine which working directory to use
      k <- (i %% num_cores) %>% mapvalues(0, num_cores)
      
      # Subset for the day
      days_i <- unique_days[i]
      x <- (days == days_i)
      lat_i <- lat[x]
      lon_i <- lon[x]
      height_i <- height[x]
      duration_i <- duration[x]
      daily_hours_i <- daily_hours[x]
      
      for (j in seq(sum(x))) {
        # Generate HYSPLIT trajectory for each point that day
        hysplit_trajectory(
          lat = lat_i[j], 
          lon = lon_i[j], 
          height = height_i[j], 
          duration = duration_i[j], 
          day = days_i,
          daily_hours = daily_hours_i[j], 
          direction = direction, 
          met_type = met_type, 
          vert_motion = vert_motion, 
          model_height = model_height, 
          extended_met = extended_met, 
          config = config, 
          ascdata = ascdata, 
          traj_name = paste(traj_name, i, sep = "_"),
          binary_path = binary_path, 
          met_dir = met_dir, 
          exec_dir = exec_dir[k], 
          clean_up = clean_up
        )
      }
    }
    # Consolidate output to one folder
    paste0(exec_dir, "/1") %>% 
      lapply(list.files, full.names = TRUE, recursive = TRUE) %>% 
      unlist() %>% 
      lapply(file.move, out_dir)
    
    # Delete working directories
    if (exec_rm) unlink(exec_dir, recursive = TRUE)
  }

#-------------------------------------------------------------------------------
# Run in splitr in parallel
registerDoParallel(num_cores)
start_time <- get_start_time()
hysplit_trajectory_parallel(
  num_cores = num_cores,
  out_dir = out_dir,
  lat = dat_hms_hysplit$lat,
  lon = dat_hms_hysplit$lon,
  height = dat_hms_hysplit$height,
  duration = dat_hms_hysplit$duration,
  days = paste(dat_hms_hysplit$year, dat_hms_hysplit$month, dat_hms_hysplit$day, 
               sep = "-"),
  daily_hours = dat_hms_hysplit$hour,
  met_type = "gdas1",
  traj_name = "my_traj",
  met_dir = met_dir, 
  exec_dir = exec_dir, 
  clean_up = FALSE
)
print_time(start_time)
stopImplicitCluster()
beep_alert("Finished generating HYSPLIT trajectories")


















# Why doesn't the number of files match the number of rows?
# What is the rho error? Is it the same as segfault error?
x <- paste0(exec_dir, "/1") %>%
  lapply(list.files, full.names = TRUE, recursive = TRUE) %>%
  unlist() %>% 
  lapply(file.copy, out_dir)

x[20000]
list.files("~/Downloads/my_dir/my_out_dir/", full.names = TRUE) %>% 
  file.copy("/Users/jessssli/Downloads/my_dir2/")

trajs <- trajectory_read(out_dir)
traj <- trajectory_read("~/Documents/HYSPLIT_CA_2020/exec/1/1/_1/")

unique_days <- unique(dat_hms_hysplit$ymd)
fff <- vector("numeric", length(unique_days))
rrr <- vector("numeric", length(unique_days))
for (i in seq_along(unique_days)) {
  print(paste("Day:", unique_days[i]))
  rows <- nrow(dat_hms_hysplit %>% filter(ymd == unique_days[i]))
  print(paste("Trajectories:", rows))
  files <- length(list.files(paste0(
    "/Users/jessssli/Documents/HYSPLIT_CA_2020/consolidated_folders/_", i)))
  print(paste("Files:", files))
  fff[i] <- files
  rrr[i] <- rows
}
dfeee <- data.frame(ymd = unique_days, rrr, fff, eee = rrr - fff)
View(dfeee %>% arrange(desc(eee)))
saveRDS(dfeee, "~/Documents/04 Sub-Annual Model of Wildfire PM/rows_not_run.rds")
dat_hms_hysplit %>% filter(month == 9, day == 10) %>% View()
dat_hms_hysplit %>% filter(month == 9, day == 10) %>% count(hour) %>% arrange(desc(n))
dat_hms_hysplit %>% filter(month == 9, day == 10, hour == 0) %>% View()









