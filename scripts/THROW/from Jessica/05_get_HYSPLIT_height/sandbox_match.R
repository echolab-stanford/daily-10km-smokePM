df1 <- data.frame(
  lon = c(143,143,149),
  lat = c(-23,-23,-27),
  height = c(792,863,1250),
  date_traj = c("2020-01-02 2:00", "2020-01-02 12:00", "2020-01-01 8:00")
) %>% 
  mutate(date_traj = as.POSIXct(date_traj, format = "%Y-%m-%d %H:%M", tz = "CET"))

df2 <- data.frame(
  lon = c(146,146,149),
  lat = c(-25,-25,-21),
  pm = c(8,4,13),
  date_epa = c("2020-01-01", "2020-01-02", "2020-01-01")
) %>% 
  mutate(date_epa = as.POSIXct(date_epa, tz = "EST"))

df3 <- data.frame(
  lon_epa = c(146,146),
  lat_epa = c(-25,-25),
  lon_traj = c(143,149),
  lat_traj = c(-23,-27)
)

df4 <- left_join(df3, df2, by = c("lon_epa" = "lon", "lat_epa" = "lat"))

df5 <- df4 %>% left_join(df1, by = c("lon_traj" = "lon", "lat_traj" = "lat"))

# df6 <- df5 %>% mutate(same_date = (as.Date(with_tz(date_traj, "EST"), format = "%Y-%m-%d") == date_epa))
df5 <- df5 %>% mutate(time_zone = c("CET", "CET", "CET", "CET", "PST", "PST"))
df6 <- df5 %>% mutate(adj_date = with_tz(date_traj, tzone = time_zone))
df6 <- df5 %>% mutate(same_date = (with_tz(date_traj, tz = time_zone) == date_epa))

df7 <- df6 %>% filter(same_date)


tz_lookup_coords(34.061353, -84.294204)









dat_epa <- dat_epa %>% filter(year == "2020", county == "Ventura")

df11 <- dat_matched %>% 
  left_join(dat_epa[1:3,], by = c("lon_epa" = "lon", "lat_epa" = "lat")) %>% 
  mutate(month = ifelse(month < 10, "0", "") %>% paste0(month),
         day = ifelse(day < 10, "0", "") %>% paste0(day),
         date_epa = paste(year, month, day, sep = "-")) %>% 
  select(-date, -year, -month, -day) %>% 
  left_join(dat_traj, by = c("lon_traj" = "lon", "lat_traj" = "lat")) %>% 
  select(-year, -month, -day, -hour)

df12 <- df11 %>% 
  mutate(tz_epa = tz_lookup_coords(lat_epa, lon_epa))

df12[1:3, "tz_epa"] <- df4[nrow(df4), "tz_epa"]

# does group by actually work for time zone? yes if used w/ unique
# no wait it doesn't seem like it works...fails to differentiate local tz
# does with_tz deal with DST? yes
df13 <- df12 %>% 
  group_by(tz_epa) %>% 
  mutate(date_traj = traj_dt %>% 
           with_tz(unique(tz_epa))# %>% 
         # date() %>%
         # as.character()
  ) %>% 
  ungroup()

# failed to differentiate local tz
df10 <- df12 %>% 
  rowwise() %>% 
  do(date_traj = with_tz(.$traj_dt, tzone = .$tz_epa)) %>% 
  mutate(date_traj = as.POSIXct(date_traj))

# this works
get_local_time <- function(timestamp_utc, local_tz, format = "%Y-%m-%d %H:%M") {
  l <- lapply(seq(length(timestamp_utc)), 
              function(x) {format(with_tz(timestamp_utc[x], local_tz[x]), format)})
  unlist(l)
}

x <- mutate(df12, date_traj = get_local_time(traj_dt, local_tz = tz_epa, format = "%Y-%m-%d"))

df13 <- df12 %>% 
  mutate(date_traj = traj_dt %>% 
           get_local_time(tz_epa, "%Y-%m-%d %H:%M") %>% 
           date() %>%
           as.character())

df13[1:3, "date_epa"] <- paste0("2020-02-0", 7:9)

df14 <- df13 %>% 
  filter(date_epa == date_traj)















dat_smoke <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/smoke/smoke_plumes_2006_2020.rds")
dat_smoke[[1]]$geometry[[1]] %>% area()
area(dat_smoke[[1]])
avg_area <- 
  avg_radius <- sqrt(avg_area/pi)

plume1 <- shapefile("/Users/jessssli/BurkeLab Dropbox/Data/smoke/hms_smoke20060101.shp")
















get_local_time <- function(timestamp_utc, local_tz, format = "%Y-%m-%d %H:%M") {
  l <- lapply(seq(length(timestamp_utc)),
              function(x) {format(with_tz(timestamp_utc[x], local_tz[x]), format)})
  unlist(l)
}

start_time <- get_start_time()
dat_matched <- dat_matched %>%
  mutate(date_traj = traj_dt %>%
           get_local_time(tz_epa) %>%
           date() %>%
           as.character())
print_time(start_time)






















lats <- inits$lat
lons <- inits$lon
heights <- inits$height

inits <- dat_traj %>% lapply(filter, hour_along == 0)
lats <- inits %>% sapply(pull, lat)
lons <- inits %>% sapply(pull, lon)
heights <- inits %>% sapply(pull, height)

df <- map2(list(dat_traj[[1]], dat_traj[[2]]), 
           inits[1:2, "lat"], 
           ~.x %>% mutate(lat_i = .y))

df <- mapply(mutate, 
             list(dat_traj[[1]], dat_traj[[2]]), 
             lat_i = lats[1:2], 
             lon_i = lons[1:2],
             height_i = heights[1:2],
             SIMPLIFY = FALSE)

dat_linked <- mapply(cbind, dat_traj, lat_i = lats, lon_i = lons, height_i = heights) %>% 
  lapply(left_join, crosswalk, by = c("traj_dt_i" = "datetime", "lat_i" = "lat", 
                                      "lon_i" = "lon", "height_i" = "height"))
dat_linked <- dat_traj %>% 
  mapply(mutate, lat_i = lats, lon_i = lons, height_i = heights) %>% 
  lapply(left_join, crosswalk, by = c("traj_dt_i" = "datetime", "lat_i" = "lat", 
                                      "lon_i" = "lon", "height_i" = "height"))



for (t in tzs) {
  same_tz <- dat_matched$tz_epa == t
  date_traj <- dat_matched[same_tz,"traj_dt"]
  dat_matched[same_tz, "date_traj"] <- with_tz(date_traj, t) %>%
    strftime(format = "%Y-%m-%d", tz = t)
}
























# Get initialization location in each row
inits <- dat_traj %>% map_dfr(filter, hour_along == 0)
dat_traj <- mapply(mutate, 
                   dat_traj, 
                   lat_i = inits$lat, 
                   lon_i = inits$lon, 
                   height_i = inits$height, 
                   SIMPLIFY = FALSE)

# Link trajectory points to HYSPLIT points
dat_linked <- dat_traj %>% 
  lapply(left_join, crosswalk, by = c("traj_dt_i" = "datetime", "lat_i" = "lat", 
                                      "lon_i" = "lon", "height_i" = "height"))

system.time(
  df1 <- dat_traj[1:100] %>% 
    lapply(left_join, crosswalk, by = c("traj_dt_i" = "datetime", "lat_i" = "lat", 
                                        "lon_i" = "lon", "height_i" = "height"))
)
system.time(
  df2 <- dat_traj[1:100] %>% 
    map2(rep(list(crosswalk), 100), 
         left_join, 
         by = c("traj_dt_i" = "datetime", "lat_i" = "lat", 
                "lon_i" = "lon", "height_i" = "height"))
)
newlist <- vector("list", 100)
system.time(
  for (r in 1:100) {
    newlist[[r]] <- left_join(dat_traj[[r]], crosswalk, 
                              by = c("traj_dt_i" = "datetime", "lat_i" = "lat", 
                                     "lon_i" = "lon", "height_i" = "height"))
  }
)
dt <- dat_traj[1:100] %>% bind_rows()
system.time(
  df3 <- left_join(dt, crosswalk, 
                   by = c("traj_dt_i" = "datetime", "lat_i" = "lat", 
                          "lon_i" = "lon", "height_i" = "height"))
)
system.time(
  df3 <- df3 %>% mutate(row = row_number())
)
system.time(df3 <- df3 %>% split(row %/% 73))



dflist1 <- list(
  data.frame(x  = )
)
















# version 1 of matching
# problem: cannot fit in memory when cutoff too large
# even if only include date columns first
source("work/get_HYSPLIT_height/00_utils.R")

library(raster)
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(lutz)
library(lubridate)

num_cores <- 6

#-------------------------------------------------------------------------------
# Match HYSPLIT Trajectory Points to EPA Station-Days
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Distance (meters) within which to match
within_m <- 1000*2

# Read in trajectory points
dat_traj <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_CA_2020_linked.rds")) %>% 
  select(id_traj, traj_dt, id_hysplit, lat, lon, height, pressure, hour_along)

# Read in EPA station-days
dat_epa <- readRDS(paste0(path_dropbox, "PM25/epa_station_level_pm25_data_updated.rds")) %>% 
  select(lat, lon, date, pm25) %>% 
  mutate(id_epa = as.integer(as.factor(paste(lon, lat))),
         date = paste(substr(date, 5, 8), substr(date, 1, 2), substr(date, 3, 4), sep = "-")) %>% 
  rename(date_epa = date) %>% 
  as.data.frame() %>% 
  # Discard duplicate rows
  distinct()

#-------------------------------------------------------------------------------
# Get unique trajectory point locations
loc_traj <- dat_traj %>% 
  select(lon, lat) %>% 
  distinct()

# Get unique EPA station locations
loc_epa <- dat_epa %>% 
  select(lon, lat) %>% 
  distinct()

# Match each EPA station to trajectory points within a specific distance
n_epa <- nrow(loc_epa)
radius_lat <- within_m/111111
registerDoParallel(num_cores)
start_time <- get_start_time()
dat_matched <- foreach(i = 1:n_epa, .combine = bind_rows) %dopar% {
  pt_epa <- loc_epa[i,]
  
  # Find trajectory points within a square around EPA station
  radius_lon <- radius_lat/cos(pt_epa$lat * pi/180)
  within_square <- loc_traj$lat >= pt_epa$lat - radius_lat & 
    loc_traj$lat <= pt_epa$lat + radius_lat & 
    loc_traj$lon >= pt_epa$lon - radius_lon & 
    loc_traj$lon <= pt_epa$lon + radius_lon
  
  # Default to NA for EPA stations w/ no matches
  matches <- data.frame(lon = NA, lat = NA, dist = NA)
  
  if (any(within_square)) {
    # Find trajectory points within a circle around EPA station
    dists <- pointDistance(pt_epa, 
                           loc_traj[within_square,], 
                           lonlat = TRUE)
    within_circle <- dists <= within_m
    
    if(any(within_circle)) {
      matches <- loc_traj[within_square,][within_circle,] %>% 
        mutate(dist = dists[within_circle])
    }
  }
  # Bind EPA station location and matched trajectory point locations
  matches <- pt_epa %>% 
    rename(lon_epa = lon, lat_epa = lat) %>% 
    bind_cols(matches) %>% 
    rename(lon_traj = lon, lat_traj = lat)
}
print_time(start_time)
stopImplicitCluster()

# Drop station-days with no spatially matching trajectory points
dat_matched <- dat_matched %>% drop_na()

# TO DO: VECTOR MEMORY EXHAUSTED WHEN CUTOFF TOO LARGE
# Join variables of interest to location pairs
dat_matched <- dat_matched %>% 
  left_join(dat_epa, by = c("lon_epa" = "lon", "lat_epa" = "lat")) %>% 
  left_join(dat_traj, by = c("lon_traj" = "lon", "lat_traj" = "lat"))

# Get local time zone of EPA station
dat_matched <- dat_matched %>% 
  mutate(tz_epa = tz_lookup_coords(lat_epa, lon_epa))

# Get trajectory points into local time zone of EPA station as dates
dat_matched$date_traj <- NA
tzs <- unique(dat_matched$tz_epa)
for (t in tzs) {
  same_tz <- dat_matched$tz_epa == t
  date_traj <- dat_matched[same_tz, "traj_dt"]
  dat_matched[same_tz, "date_traj"] <- date_traj %>% strftime(format = "%Y-%m-%d", tz = t)
}

# Filter to matches on the same date
dat_matched <- dat_matched %>% filter(date_epa == date_traj)

# Discard unnecessary columns
dat_matched <- dat_matched %>% 
  rename(date = date_epa) %>% 
  select(-tz_epa, -date_traj, -traj_dt)

# Save matched data
saveRDS(dat_matched, paste0(path_results, "trajectories_EPA_CA_2020.rds"))


# version 2 of matching
# problem: left_join is slow
source("work/get_HYSPLIT_height/00_utils.R")

library(raster)
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(lutz)
library(lubridate)

num_cores <- 6

#-------------------------------------------------------------------------------
# Match HYSPLIT Trajectory Points to EPA Station-Days
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Distance (meters) within which to match
within_m <- 1000*2

# Read in trajectory points
dat_traj <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_CA_2020_linked.rds")) %>% 
  select(id_traj, traj_dt, id_hysplit, lat, lon, height, pressure, hour_along)

# Read in EPA station-days
dat_epa <- readRDS(paste0(path_dropbox, "PM25/epa_station_level_pm25_data_updated.rds")) %>% 
  select(lat, lon, date, pm25) %>% 
  mutate(id_epa = as.integer(as.factor(paste(lon, lat))),
         date = paste(substr(date, 5, 8), substr(date, 1, 2), substr(date, 3, 4), sep = "-")) %>% 
  rename(date_epa = date) %>% 
  as.data.frame() %>% 
  # Discard duplicate rows
  distinct()

#-------------------------------------------------------------------------------
#### Spatial matching ####
# Get unique trajectory point locations
loc_traj <- dat_traj %>% 
  select(lon, lat) %>% 
  distinct()

# Get unique EPA station locations
loc_epa <- dat_epa %>% 
  select(lon, lat) %>% 
  distinct()

# Match each EPA station to trajectory points within a specific distance
nr <- nrow(loc_epa)
radius_lat <- within_m/111111
registerDoParallel(num_cores)
start_time <- get_start_time()
dat_matched <- foreach(i = 1:nr, .combine = bind_rows) %dopar% {
  pt_epa <- loc_epa[i,]
  
  # Find trajectory points within a square around EPA station
  radius_lon <- radius_lat/cos(pt_epa$lat * pi/180)
  within_square <- loc_traj$lat >= pt_epa$lat - radius_lat & 
    loc_traj$lat <= pt_epa$lat + radius_lat & 
    loc_traj$lon >= pt_epa$lon - radius_lon & 
    loc_traj$lon <= pt_epa$lon + radius_lon
  
  # Default to NA for EPA stations w/ no matches
  matches <- data.frame(lon = NA, lat = NA, dist = NA)
  
  if (any(within_square)) {
    # Find trajectory points within a circle around EPA station
    dists <- pointDistance(pt_epa, 
                           loc_traj[within_square,], 
                           lonlat = TRUE)
    within_circle <- dists <= within_m
    
    if(any(within_circle)) {
      matches <- loc_traj[within_square,][within_circle,] %>% 
        mutate(dist = dists[within_circle])
    }
  }
  # Bind EPA station location and matched trajectory point locations
  matches <- pt_epa %>% 
    rename(lon_epa = lon, lat_epa = lat) %>% 
    bind_cols(matches) %>% 
    rename(lon_traj = lon, lat_traj = lat)
}
print_time(start_time)
stopImplicitCluster()

# Drop station-days with no spatially matching trajectory points
dat_matched <- dat_matched %>% drop_na()

#-------------------------------------------------------------------------------
#### Temporal matching ####
# Get local time zone of EPA station
dat_matched <- dat_matched %>% 
  mutate(tz_epa = tz_lookup_coords(lat_epa, lon_epa),
         date_traj = NA)

nr <- nrow(dat_matched)
registerDoParallel(num_cores)
start_time <- get_start_time()
dat_matched <- foreach(i = 1:nr, .combine = bind_rows) %dopar% {
  # Get location pair and its local time zone
  loc_pair <- dat_matched[i,]
  t <- loc_pair$tz_epa
  
  # Join on EPA and trajectory locations to date-times and variables of interest
  loc_pair <- loc_pair %>% 
    left_join(dat_epa, by = c("lon_epa" = "lon", "lat_epa" = "lat")) %>% 
    left_join(dat_traj, by = c("lon_traj" = "lon", "lat_traj" = "lat"))
  
  # Adjust date-time to local time zone and convert date to character
  date_traj <- loc_pair$traj_dt
  loc_pair$date_traj <- date_traj %>% strftime(format = "%Y-%m-%d", tz = t)
  
  # Keep only if dates match
  loc_pair <- loc_pair %>% filter(date_epa == date_traj)
}
print_time(start_time)
stopImplicitCluster()

# Discard unnecessary columns
dat_matched <- dat_matched %>% 
  rename(date = date_epa) %>% 
  select(-tz_epa, -date_traj, -traj_dt)

# Save matched data
saveRDS(dat_matched, paste0(path_results, "trajectories_EPA_CA_2020.rds"))

































# GET AVERAGE OR MEDIAN SMOKE PLUME RADIUS FOR BUFFER
library(raster)
library(dplyr)
library(sp)
library(sf)

smoke <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/smoke/smoke_plumes_spdf.RDS")
smoke@polygons[[1]]
st_area(smoke@polygons[[1]])
area(smoke@polygons[[1]])
area(smoke[1])
"+proj=longlat"
SpatialPolygons(smoke@polygons[1]) %>% area() #### m2
smoke@polygons[[1]]@area
x <- smoke@polygons[1]
SpatialPolygons(smoke@polygons[1]) %>% st_as_sf() %>% st_area()
crs(SpatialPolygons(x)) <- "+proj=longlat"
st_plot(SpatialPolygons(x) %>% st_as_sf)
ggplot(data = x, mapping = aes(long, lat))
plot(SpatialPolygons(smoke@polygons[1:1000]))
plot()

library(usmap)
plot_usmap()
plot(SpatialPolygons(smoke@polygons[1:1000]))

usa <- map_data("usa")
ggplot() + 
  geom_polygon(data = usa, mapping = aes(long, lat)) + 
  geom_sf(data = SpatialPolygons(smoke@polygons[9200], proj = CRS("+proj=longlat")) %>% st_as_sf(), color = "red", fill = "red") +
  coord_sf(crs = "+proj=longlat") + 
  theme_light()
# geom_polygon(data = smoke@polygons[1], mapping = aes(long, lat))

smoke@polygons[9200] %>% SpatialPolygons() %>% area()
?area
smoke@polygons[[9200]]@area




















# CONFIRM THESE FILES ARE THE SAME AT LEAST FOR 2020
# THE MAIN DIFFERENCE SEEMS TO BE THAT THE ONE IN THE EPA FOLDER HAS EARLIER DATES
library(stringr)
str_pad(9:11, 3, "left", 0)

epa1 <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/PM25/epa_station_level_pm25_data_updated.rds") %>% 
  mutate(across(c(year, county_code, cbsa_code), as.integer))
epa2 <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/PM25/EPA/epa_station_level_pm25_data_updated.rds") %>% 
  mutate(date = str_pad(date, 8, "left", 0))

x <- min(epa1$date)
epa3 <- epa2 %>% filter(date >= x)
epa4 <- epa1 %>% 
  filter("12312019" <= date, date <= "12312020") %>% 
  arrange(date, lat, lon, pm25)
epa5 <- epa2 %>% 
  filter("12312019" <= date, date <= "12312020") %>% 
  arrange(date, lat, lon, pm25)
all(epa4 == epa5, na.rm = TRUE)

 

















# WHICH DF TO USE?
# dropbox history
df1 <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/boundaries/10km_grid_EPA_crosswalk.RDS")
df2 <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/MERRA-2/us_grid_final/grid_aot_2020.RDS")

# github 04_dataprep_plume_pm_model - NOT AVAILABLE
"data/clean/epa_smoke_clean.rds"
# github 05_process_pbl_epa_monitors -  NOT AVAILABLE
"data/clean/epa_smoke_clean_california.rds"
# github 13_model_pm_from_smoke
df3 <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/smoke/smoke-aot-gridded-data/smoke_aot_grid_point_data_sparse.rds")

# github data folder and 13_model_pm_from_smoke. comes before df3
df4 <- readRDS("/Users/jessssli/Documents/GitHub/purple-air-infiltration/data/epa_monitor_gridded_smoke_aot_data.rds")

epa <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/PM25/EPA/epa_station_level_pm25_data_updated.rds")

# df3 doesn't have AOT
# df4 doesn't have background AOT
























path_results <- "~/Documents/GitHub/purple-air-infiltration/work/get_HYSPLIT_height/results/"
path_dropbox <- "~/BurkeLab Dropbox/Data/"

# Read in EPA station-days
dat_epa <- readRDS(paste0(path_dropbox, "PM25/EPA/epa_station_level_pm25_data.rds")) %>% 
  dplyr::select(id, lat, lon, date, pm25, county, county_code, cbsa_code, cbsa_name) %>% 
  mutate(date = str_pad(date, 8, "left", 0),
         date = paste(substr(date, 5, 8), substr(date, 1, 2), substr(date, 3, 4), sep = "-")) %>% 
  rename(id_epa = id, date_epa = date) %>% 
  as.data.frame() %>% 
  # Discard duplicate rows
  distinct()

# Read in merged data that needs county and CBSA info
dat_merged <- readRDS(paste0(path_results, "trajectories_EPA_smoke_AOT_CA_2020.rds"))

# Join the data frames
df <- dat_merged %>% 
  left_join(dat_epa, by = c("id_epa", "date" = "date_epa", "lon_epa" = "lon", 
                            "lat_epa" = "lat", "pm25")) %>% 
  dplyr::select(date, id_epa, lon_epa, lat_epa, county, county_code, cbsa_code, cbsa_name, pm25,
          grid_id, dist, aot, mean_aot, aot_anom, 
          smoke_day, light, medium, dense, total,
          agg_method, cutoff,
          height, var_height, pressure, var_pressure,
          hour_along, var_hour_along, n_traj_points) %>% 
  arrange(date, id_epa, grid_id, agg_method, cutoff)

# Save the file
saveRDS(df, paste0(path_results, "trajectories_EPA_smoke_AOT_CA_2020_withcounty.rds"))



