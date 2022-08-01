source("work/05_get_HYSPLIT_height/00_utils.R")

library(sf)
library(dplyr)
library(tidyr)
library(purrr)

#-------------------------------------------------------------------------------
# Prepare NOAA HMS HYSPLIT Points
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
# Read in HYSPLIT points in NOAA HMS data
dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_20060419-20201231.rds"))

#-------------------------------------------------------------------------------
#### Restructure HMS data ####
dat_hms_hysplit <- dat_hms_hysplit %>% 
  map_dfr(st_drop_geometry) %>% 
  rename(id_hysplit = ID,
         lon = Lon,
         lat = Lat,
         date = Date.YYYYmmdd,
         time = Time.HHMM,
         duration = Duration.HHMM) %>% 
  # There are 9 rows w/ Duration = "0000"
  filter(duration > "0000")

#-------------------------------------------------------------------------------
#### Redefine HYSPLIT points based on duration following Brey et al. (2018) ####
dat_hms_hysplit <- dat_hms_hysplit %>% 
  mutate(
    # Get date-time
    datetime_0 = paste(date, time) %>% as.POSIXct(tz = "UTC", format = "%Y%m%d %H%M"),
    # Convert duration from HHMM to minutes
    duration_hours = as.integer(substr(duration, 1, 2)),
    duration_minutes = as.integer(substr(duration, 3, 4)),
    duration = duration_hours*60 + duration_minutes,
    # Unlike Brey et al., we have durations beyond 24 hours, so slightly modify
    # initialization percentiles to be evenly distanced
    seconds = duration*60,
    # Convert duration from minutes to hours
    duration = duration/60,
    denominator = pmax(0, duration - 1) %/% 6 + 2,
    numerator_1 = 1,
    numerator_2 = ifelse(0 <= duration & duration <= 6,  NA, 2),
    numerator_3 = ifelse(0 <= duration & duration <= 12, NA, 3),
    numerator_4 = ifelse(0 <= duration & duration <= 18, NA, 4),
    numerator_5 = ifelse(0 <= duration & duration <= 24, NA, 5),
    numerator_6 = ifelse(0 <= duration & duration <= 30, NA, 6),
    datetime_1 = datetime_0 + seconds * numerator_1 / denominator,
    datetime_2 = datetime_0 + seconds * numerator_2 / denominator,
    datetime_3 = datetime_0 + seconds * numerator_3 / denominator,
    datetime_4 = datetime_0 + seconds * numerator_4 / denominator,
    datetime_5 = datetime_0 + seconds * numerator_5 / denominator,
    datetime_6 = datetime_0 + seconds * numerator_6 / denominator
  ) %>% 
  pivot_longer(cols = starts_with("datetime"), 
               names_to = "initialization_along",
               names_prefix = "datetime_",
               values_to = "datetime") %>% 
  mutate(initialization_along = as.numeric(initialization_along)) %>% 
  filter(initialization_along != 0) %>% 
  drop_na(datetime)

#-------------------------------------------------------------------------------
#### splitr does not take minutes as input, so round to nearest hour ####
dat_hms_hysplit <- dat_hms_hysplit %>% 
  mutate(datetime = datetime %>% 
           round(units = "hours") %>% 
           strftime(format = "%Y-%m-%d-%H-%M")) %>% 
  # Reformat date and time columns
  separate(datetime, c("year", "month", "day", "hour", "minute"), sep = "-") %>% 
  mutate(ymd = paste(year, month, day, sep = "-"),
         across(c(year, month, day, hour, minute), as.numeric))

#-------------------------------------------------------------------------------
#### Specify injection heights following Brey et al. (2018) ####
dat_hms_hysplit <- dat_hms_hysplit %>% 
  mutate(height_1 = 500,
         height_2 = 1500,
         height_3 = 2500) %>% 
  pivot_longer(cols = starts_with("height"),
               names_to = "id_height",
               names_prefix = "height_",
               values_to = "height") %>% 
  mutate(id_height = as.numeric(id_height),
         date = as.character(date)) %>% 
  select(id_hysplit, lon, lat, date, time, duration, 
         initialization_along, year, month, day, hour, minute, ymd,
         id_height, height)

# Save potentially semi-duplicate HYSPLIT points
saveRDS(dat_hms_hysplit, paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_duplicates_20060419-20201231.rds"))

#-------------------------------------------------------------------------------
#### Discard points that rounded to the same date-time ####
dat_hms_hysplit <- dat_hms_hysplit %>% 
  select(lon, lat, height, year, month, day, hour, minute, ymd) %>%   
  distinct()

# Save data
saveRDS(dat_hms_hysplit, paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct_20060419-20201231.rds"))

#-------------------------------------------------------------------------------
#### Save distinct initialization points with weights column for easy use ####
dat_hms_hysplit = readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_duplicates_20060419-20201231.rds"))
dat_hms_hysplit = dat_hms_hysplit %>% 
  count(lon, lat, height, year, month, day, hour, minute, ymd) %>% 
  rename(weight = n)

saveRDS(dat_hms_hysplit, paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct_20060419-20201231_weights.rds"))
