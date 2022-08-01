library(splitr)
x <- hysplit_trajectory(
  lat = 37.05, 
  lon = -119.25, 
  height = 500,
  duration = 96,
  days = "2020-01-11",
  daily_hours = 23,
  met_type = "gdas1", 
  met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
  exec_dir = "/Users/jessssli/Downloads/mydir1/",
  clean_up = TRUE
)
#71-95
max(x$hour_along)

library(dplyr)

df <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_initialization_rerun_run07.rds")
dat_hms_hysplit <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_initialization_distinct.rds")
dat_hms_hysplit <- dat_hms_hysplit %>% filter(state == "California", year == 2020)

success <- anti_join(dat_hms_hysplit, df)
any(success$day == 4 & success$hour %in% 22:23)
