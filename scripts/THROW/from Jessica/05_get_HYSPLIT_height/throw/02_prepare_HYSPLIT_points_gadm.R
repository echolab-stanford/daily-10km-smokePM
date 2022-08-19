source("work/get_HYSPLIT_height/00_utils.R")

library(raster)
library(sp)
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
dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points.rds"))

#-------------------------------------------------------------------------------
#### Restructure HMS data ####
dat_hms_hysplit <- dat_hms_hysplit %>% 
  # Discard geometry
  map_dfr(st_drop_geometry) %>% 
  rename(id = ID, lon = Lon, lat = Lat, date = Date, time = Time, duration = Duration)

dat_hms_hysplit <- dat_hms_hysplit %>% 
  # There are 6 rows w/ Date = 200000 and Duration = 0
  # Throw away for now
  filter(nchar(date) == 8 & duration > 0) %>% 
  # Discard duplicates that have same data (even if different ID)
  # Duplicates don't seem to convey anything about fire intensity and seem
  # an artifact of data recording
  select(-id) %>% 
  distinct() %>% 
  # Assign new IDs to potentially semi-duplicate HYSPLIT points
  mutate(id = row_number())

#-------------------------------------------------------------------------------
#### Redefine HYSPLIT points based on duration following Brey et al. (2018) ####
dat_hms_hysplit <- dat_hms_hysplit %>% 
  mutate(
    # Get date-time
    time = case_when(0 <= time & time < 10 ~ paste0("000", time),
                     10 <= time & time < 100 ~ paste0("00", time),
                     100 <= time & time < 1000 ~ paste0("0", time),
                     TRUE ~ as.character(time)),
    datetime_0 = paste(date, time) %>% as.POSIXct(tz = "UTC", format = "%Y%m%d %H%M"),
    # Convert duration from HHMM to hours
    duration = duration/100,
    # Unlike Brey et al., we have durations beyond 24 hours, so slightly modify
    # initialization percentiles to be evenly distanced
    seconds = duration*60*60,
    denominator = (function(x) ifelse(x < 0, 0, x))(duration - 1) %/% 6 + 2,
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
               names_to = "height_id",
               names_prefix = "height_",
               values_to = "height") %>% 
  mutate(height_id = as.numeric(height_id),
         date = as.character(date)) %>% 
  select(id, lon, lat, date, time, duration, 
         initialization_along, year, month, day, hour, minute, ymd,
         height_id, height)

# Save potentially semi-duplicate HYSPLIT points
# saveRDS(dat_hms_hysplit, paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_semiduplicates.rds"))

#-------------------------------------------------------------------------------
#### Discard points that rounded to the same date-time ####
dat_hms_hysplit <- dat_hms_hysplit %>% 
  select(lon, lat, height, year, month, day, hour, minute, ymd) %>%   
  distinct()

#-------------------------------------------------------------------------------
#### Make columns for country and state ####
hysplit_points <- dat_hms_hysplit[c("lon", "lat")] %>% distinct()
gadm <- list.files(paste0(path_dropbox, "boundaries/gadm_rds/"), full.names = TRUE) %>% 
  lapply(readRDS) %>% 
  bind()
gadm <- SpatialPolygonsDataFrame(SpatialPolygons(gadm@polygons),
                                 gadm@data %>% select(NAME_0, NAME_1))
o <- over(SpatialPoints(hysplit_points), gadm)
dat_hms_hysplit <- dat_hms_hysplit %>% 
  left_join(bind_cols(hysplit_points, o)) %>% 
  rename(country = NAME_0,
         state = NAME_1)

#-------------------------------------------------------------------------------
# Save data
saveRDS(dat_hms_hysplit, paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct_gadm.rds"))
