source("work/05_get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(tigris)
library(lubridate)
library(sf)
library(foreach)
library(doParallel)

num_cores <- 6

#-------------------------------------------------------------------------------
# Overlap HYSPLIT Points and CAL FIRE Perimeters
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Get California county shapes
# us_counties <- counties()
# ca <- us_counties %>% 
#   filter(STATEFP == "06") %>% 
#   select(COUNTYFP, GEOID, NAME) %>% 
#   rename(county = NAME)

us_states <- states()
ca <- us_states %>%
  filter(NAME == "California") %>%
  select(geometry)

# Read in CAL FIRE prescribed burn perimeters
dat_rx0 <- st_read(dsn = paste0(path_dropbox, "CAL FIRE FRAP/fire20_1.gdb"), layer = "rxburn20_1") %>% 
  st_transform(crs = st_crs(ca)) %>% 
  # Sometimes we are provided a different geometry that won't work for overlapping
  mutate(Shape = st_cast(Shape, "MULTIPOLYGON"),
         # Assign an ID to each perimeter
         id_perimeter = row_number())
dat_rx <- dat_rx0 %>% 
  # Lose 2038 obs (32%)
  filter(!is.na(START_DATE), !is.na(END_DATE)) %>% 
  mutate(
    start_year = year(START_DATE),
    start_month = month(START_DATE),
    start_day = day(START_DATE),
    start_hour = hour(START_DATE),
    start_minute = minute(START_DATE),
    end_year = year(END_DATE),
    end_month = month(END_DATE),
    end_day = day(END_DATE),
    end_hour = hour(END_DATE),
    end_minute = minute(END_DATE),
    # Clean up year
    end_year = ifelse(end_year == 1008, 1908, end_year),
    start_dt = sprintf("%s-%s-%s %s:%s", start_year, start_month, start_day, start_hour, start_minute) %>% 
      # Can't find time zone specification in data documentation, so I assume 
      # Pacific Time since these are fires in California
      ymd_hm(tz = "America/Los_Angeles"),
    end_dt = sprintf("%s-%s-%s %s:%s", end_year, end_month, end_day, end_hour, end_minute) %>% 
      ymd_hm(tz = "America/Los_Angeles"),
    # Get interval of fire perimeter
    interval = interval(start_dt, end_dt)
  ) %>% 
  # Discard fire perimeters just outside California
  # Lose 0 obs
  st_join(ca, left = FALSE)

# Read in CAL FIRE wildfire perimeters
dat_wf0 <- st_read(dsn = paste0(path_dropbox, "CAL FIRE FRAP/fire20_1.gdb"), layer = "firep20_1") %>% 
  st_transform(crs = st_crs(ca)) %>% 
  mutate(Shape = st_cast(Shape, "MULTIPOLYGON"),
         id_perimeter = row_number())
dat_wf <- dat_wf0 %>% 
  # Lose 12688 obs (60%)
  filter(!is.na(ALARM_DATE), !is.na(CONT_DATE)) %>% 
  mutate(
    start_year = year(ALARM_DATE),
    start_month = month(ALARM_DATE),
    start_day = day(ALARM_DATE),
    start_hour = hour(ALARM_DATE),
    start_minute = minute(ALARM_DATE),
    end_year = year(CONT_DATE),
    end_month = month(CONT_DATE),
    end_day = day(CONT_DATE),
    end_hour = hour(CONT_DATE),
    end_minute = minute(CONT_DATE),
    # Clean up year
    start_year = case_when(start_year == 219 ~ 2019,
                           start_year == 2106 ~ 2016, 
                           TRUE ~ start_year),
    end_year = case_when(end_year == 202 ~ 2002,
                         end_year == 209 ~ 2009,
                         end_year == 219 ~ 2019,
                         end_year == 1089 ~ 1989,
                         TRUE ~ end_year),
    start_dt = sprintf("%s-%s-%s %s:%s", start_year, start_month, start_day, start_month, start_hour) %>% 
      ymd_hm(tz = "America/Los_Angeles"),
    end_dt = sprintf("%s-%s-%s %s:%s", end_year, end_month, end_day, end_month, end_hour) %>% 
      ymd_hm(tz = "America/Los_Angeles"),
    interval = interval(start_dt, end_dt)
  ) %>% 
  # Lost 0.2% of obs (17 obs) that are just outside of California
  st_join(ca, left = FALSE)

# Read in HYSPLIT points
dat_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_distinct.rds")) %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  rename(id_hysplit = id)
# Start with geographic coordinate system
st_crs(dat_hysplit) <- 4326
dat_hysplit <- dat_hysplit %>% 
  # Project to coordinate reference system NAD83
  st_transform(crs = st_crs(ca)) %>% 
  # Get date-times
  mutate(date_time = paste(date, time) %>% ymd_hm(tz = "UTC")) %>% 
  select(id_hysplit, date_time, geometry) %>% 
  # Limit to HYSPLIT points in California
  st_join(ca, left = FALSE)

# Discard observations where definitely no temporal overlap
start <- max(min(dat_hysplit$date_time), min(dat_rx$start_dt))
end <- min(max(dat_hysplit$date_time), max(dat_rx$end_dt))
dat_hysplit_rx <- dat_hysplit %>% filter(start <= date_time, date_time <= end)
dat_rx <- dat_rx %>% filter(start <= start_dt, end_dt <= end)

start <- max(min(dat_hysplit$date_time), min(dat_wf$start_dt))
end <- min(max(dat_hysplit$date_time), max(dat_wf$end_dt))
dat_hysplit_wf <- dat_hysplit %>% filter(start <= date_time, date_time <= end)
dat_wf <- dat_wf %>% filter(start <= start_dt, end_dt <= end)

#-------------------------------------------------------------------------------
#### Overlap HYSPLIT points and CAL FIRE perimeters ####
registerDoParallel(num_cores)

# Find overlapping HYSPLIT points and prescribed burn perimeters
nc <- 200
chunks <- split_chunks(1:nrow(dat_hysplit_rx), nc)
start_time <- get_start_time()
dat_overlap_rx <- foreach(i = 1:nc, .combine = bind_rows, .multicombine = TRUE) %dopar% {
  # Find HYSPLIT points that overlap CAL FIRE prescribed burn perimeters
  dat_overlap_i <- st_join(dat_hysplit_rx[chunks[[i]],], dat_rx, left = FALSE)
  
  # Discard where no temporal overlap
  dat_overlap_i <- dat_overlap_i %>% filter(date_time %within% interval)
  
  # Return overlapped data chunk
  dat_overlap_i
}
print_time(start_time)
beep_alert("Done overlapping HYSPLIT points and CAL FIRE prescribed burn perimeters")

# Save overlapping data
saveRDS(dat_overlap_rx, paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_CALFIRE_rx.rds"))

# Find overlapping HYSPLIT points and wildfire perimeters
chunks <- split_chunks(1:nrow(dat_hysplit_wf), nc)
start_time <- get_start_time()
dat_overlap_wf <- foreach(i = 1:nc, .combine = bind_rows, .multicombine = TRUE) %dopar% {
  # Find HYSPLIT points that overlap CAL FIRE wildfire perimeters
  dat_overlap_i <- st_join(dat_hysplit_wf[chunks[[i]],], dat_wf, left = FALSE)
  
  # Discard where no temporal overlap
  dat_overlap_i <- dat_overlap_i %>% filter(date_time %within% interval)
  
  # Return overlapped data chunk
  dat_overlap_i
}
print_time(start_time)
beep_alert("Done overlapping HYSPLIT points and CAL FIRE wildfire perimeters")

# Save overlapping data
saveRDS(dat_overlap_wf, paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_CALFIRE_wf.rds"))

stopImplicitCluster()

#-------------------------------------------------------------------------------
ohrx <- length(unique(dat_overlap_rx$id_hysplit))
ohwf <- length(unique(dat_overlap_wf$id_hysplit))
ofrx <- length(unique(dat_overlap_rx$id_perimeter))
ofwf <- length(unique(dat_overlap_wf$id_perimeter))
nhrx <- nrow(dat_hysplit_rx)
nhwf <- nrow(dat_hysplit_wf)
nrx <- nrow(dat_rx)
nwf <- nrow(dat_wf)

# What percent of HYSPLIT points overlap a CAL FIRE perimeter?
print(paste("Percent of HYSPLIT points that overlap a CAL FIRE prescribed burn perimeter:", ohrx/nhrx))
print(paste("Percent of HYSPLIT points that overlap a CAL FIRE wildfire perimeter:", ohwf/nhwf))

# What percent of CAL FIRE perimeters overlap a HYSPLIT point?
print(paste("Percent of CAL FIRE prescribed burn perimeters that overlap a HYSPLIT point:", ofrx/nrx))
print(paste("Percent of CAL FIRE wildfire perimeters that overlap a HYSPLIT point:", ofwf/nwf))

# Approx. what percent of obs we threw away due to missing start or end date 
# would not have possibly overlapped with HYSPLIT data anyways?
# Most obs have a fire year value
narx <- dat_rx0 %>% filter(is.na(START_DATE) | is.na(END_DATE), YEAR_ < "2010", YEAR_ != "", !is.na(YEAR_)) %>% nrow()
nawf <- dat_wf0 %>% filter(is.na(ALARM_DATE) | is.na(CONT_DATE), YEAR_ < "2010", YEAR_ != "", !is.na(YEAR_)) %>% nrow()
print(paste("Percent of RX fires missing start or end date that couldn't have overlapped with HYSPLIT anyway:", 
            narx/(nrow(dat_rx0) - nrx)))
print(paste("Percent of wildfires missing start or end date that couldn't have overlapped with HYSPLIT anyway:",
            nawf/(nrow(dat_wf0) - nwf)))

# What if we count as overlapping fires that spatially overlap and possibly 
# temporally overlap but lack sufficient interval info to determine fully?
dat_rxna <- dat_rx0 %>% filter(is.na(START_DATE) | is.na(END_DATE), YEAR_ >= "2010")
dat_wfna <- dat_wf0 %>% filter(is.na(ALARM_DATE) | is.na(CONT_DATE), YEAR_ >= "2010")
dat_overlap_rxna <- st_join(dat_hysplit_rx, dat_rxna, left = FALSE)
dat_overlap_wfna <- st_join(dat_hysplit_wf, dat_wfna, left = FALSE)
narx <- nrow(dat_rxna)
nawf <- nrow(dat_wfna)
ohnarx <- length(unique(c(dat_overlap_rxna$id_hysplit, dat_overlap_rx$id_hysplit)))
ohnawf <- length(unique(c(dat_overlap_wfna$id_hysplit, dat_overlap_wf$id_hysplit)))
ofnarx <- length(unique(c(dat_overlap_rxna$id_perimeter, dat_overlap_rx$id_perimeter)))
ofnawf <- length(unique(c(dat_overlap_wfna$id_perimeter, dat_overlap_wf$id_perimeter)))

print(paste("Percent of HYSPLIT points that overlap RX fire or spatially overlap RX fire that's missing start or end date", ohnarx/nhrx))
print(paste("Percent of HYSPLIT points that overlap wildfire or spatially overlap wildfire that's missing start or end date", ohnawf/nhwf))
print(paste("Percent of RX fires that overlap HYSPLIT point or spatially overlap but missing start or end date", ofnarx/(nrx + narx)))
print(paste("Percent of wildfires that overlap HYSPLIT point or spatially overlap but missing start or end date", ofnawf/(nwf + nawf)))

#-------------------------------------------------------------------------------
# Plot timelapse map of fire perimeters and HYSPLIT points in CA 2010-2020
# Color by RX vs wildfire, overlap or not
