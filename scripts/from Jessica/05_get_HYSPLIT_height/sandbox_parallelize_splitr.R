# Does Daylight Savings matter? NO
# Does duration matter? YES, you start of first day next week when doing 72 hours
# but only last 2 hours of the week if doing 24 hours
# So what do we lose in 6 week duration case? Works same as 72 hours
# Is it only around end of week or also end of month? End of month too. So likely 
# based on gdas1 filing
# How to check? 
# Naive solution: forward date input date-times and compare with problem intervals
# Perhaps faster: back date problem intervals and compare with input date-times
# How to correct? Add 24 hours to each problem date-time, then check df output,
# and redo with 48 hours if df output still short for some reason
# Need to add 48 hours for all due to leap year issue


library(splitr)
library(lubridate)

hour <- 21
day <- "2020-08-15"
dur <- 23

x <- hysplit_trajectory(
  lat = 37.05, 
  lon = -119.25, 
  height = 500,
  duration = dur,
  days = day,
  daily_hours = hour,
  met_type = "gdas1", 
  met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
  exec_dir = "/Users/jessssli/Documents/splitr_testing/",
  clean_up = FALSE
)
range(x$hour_along)

hour <- 21
day <- "2020-08-15"
for (dur in 140:170) {
  x <- hysplit_trajectory(
    lat = 37.05, 
    lon = -119.25, 
    height = 500,
    duration = dur,
    days = day,
    daily_hours = hour,
    met_type = "gdas1", 
    met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
    exec_dir = "/Users/jessssli/Documents/splitr_testing/",
    clean_up = TRUE
  )
  
  m <- max(x$hour_along)
  print(paste(dur, m, dur == m, ymd_h(paste(day, hour)) + hours(dur)))
}

# 23F, +24


hour <- 0
day <- "2020-03-07"
for (dur in 20:50) {
  x <- hysplit_trajectory(
    lat = 37.05, 
    lon = -119.25, 
    height = 500,
    duration = dur,
    days = day,
    daily_hours = hour,
    met_type = "gdas1", 
    met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
    exec_dir = "/Users/jessssli/Documents/splitr_testing/",
    clean_up = TRUE
  )
  
  m <- max(x$hour_along)
  print(paste(dur, m, dur == m, ymd_h(paste(day, hour)) + hours(dur)))
}


hour <- 21
day <- "2020-03-04"
for (dur in 68:100) {
  x <- hysplit_trajectory(
    lat = 37.05, 
    lon = -119.25, 
    height = 500,
    duration = dur,
    days = day,
    daily_hours = hour,
    met_type = "gdas1", 
    met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
    exec_dir = "/Users/jessssli/Documents/splitr_testing/",
    clean_up = TRUE
  )
  
  m <- max(x$hour_along)
  print(paste(dur, m, dur == m, ymd_h(paste(day, hour)) + hours(dur)))
}

# 22 and 20

# cases, check, correct

hour <- 0
day <- "2020-08-31"
for (dur in 20:50) {
  x <- hysplit_trajectory(
    lat = 37.05, 
    lon = -119.25, 
    height = 500,
    duration = dur,
    days = day,
    daily_hours = hour,
    met_type = "gdas1", 
    met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
    exec_dir = "/Users/jessssli/Documents/splitr_testing/",
    clean_up = TRUE
  )
  
  m <- max(x$hour_along)
  print(paste(dur, m, dur == m, ymd_h(paste(day, hour)) + hours(dur)))
}


hour <- 21
day <- "2020-08-28"
for (dur in 70:100) {
  x <- hysplit_trajectory(
    lat = 37.05, 
    lon = -119.25, 
    height = 500,
    duration = dur,
    days = day,
    daily_hours = hour,
    met_type = "gdas1", 
    met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
    exec_dir = "/Users/jessssli/Documents/splitr_testing/",
    clean_up = TRUE
  )
  
  m <- max(x$hour_along)
  print(paste(dur, m, dur == m, ymd_h(paste(day, hour)) + hours(dur)))
}


hour <- 0
day <- "2020-08-28"
for (dur in 20:120) {
  x <- hysplit_trajectory(
    lat = 37.05, 
    lon = -119.25, 
    height = 500,
    duration = dur,
    days = day,
    daily_hours = hour,
    met_type = "gdas1", 
    met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
    exec_dir = "/Users/jessssli/Documents/splitr_testing/",
    clean_up = TRUE
  )
  
  m <- max(x$hour_along)
  print(paste(dur, m, dur == m, ymd_h(paste(day, hour)) + hours(dur)))
}

library(foreach)
library(doParallel)
registerDoParallel(8)
hour <- 21
day <- "2020-07-15"
foreach(dur = 1131:1160) %dopar% {
  exec_dir_i <- paste0("/Users/jessssli/Documents/splitr_testing/", dur)
  if (!dir.exists(exec_dir_i)) dir.create(exec_dir_i)
  x <- hysplit_trajectory(
    lat = 37.05, 
    lon = -119.25, 
    height = 500,
    duration = dur,
    days = day,
    daily_hours = hour,
    met_type = "gdas1", 
    met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
    exec_dir = exec_dir_i,
    clean_up = TRUE
  )
  
  m <- max(x$hour_along)
  print(paste(dur, m, dur == m, ymd_h(paste(day, hour)) + hours(dur)))
}
stopImplicitCluster()


hour <- 21
day <- "2020-02-25"
for (dur in 70:170) {
  x <- hysplit_trajectory(
    lat = 37.05, 
    lon = -119.25, 
    height = 500,
    duration = dur,
    days = day,
    daily_hours = hour,
    met_type = "gdas1", 
    met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
    exec_dir = "/Users/jessssli/Documents/splitr_testing/",
    clean_up = TRUE
  )
  
  m <- max(x$hour_along)
  print(paste(dur, m, dur == m, ymd_h(paste(day, hour)) + hours(dur)))
}
