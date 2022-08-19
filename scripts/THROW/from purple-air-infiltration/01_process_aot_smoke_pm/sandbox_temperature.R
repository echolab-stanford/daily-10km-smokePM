# source("work/get_HYSPLIT_height/00_utils.R")
source("~/Documents/GitHub/smoke_PM_prediction/work/05_get_HYSPLIT_height/00_utils.R")
path_github <- "~/Documents/GitHub/purple-air-infiltration/"

library(ncdf4)
library(sp)
library(raster)
library(foreach)
library(doParallel)
library(dplyr)

num_cores <- 8

#-------------------------------------------------------------------------------
# Get Temperature (ERA5)
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Merge separately downloaded files and save
stack1 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2006-2010_2m_daily_temp.nc"))
stack2 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2011-2015_2m_daily_temp.nc"))
stack3 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2016-2020_2m_daily_temp.nc"))
dat_temp0 <- stack(stack1, stack2, stack3)
# writeRaster(dat_temp0, paste0(path_dropbox, "ERA5/2m_temp/era5_2006-2020_2m_daily_temp.nc"))

#-------------------------------------------------------------------------------
# Load temperature data
# dat_temp0 <- brick(paste0(path_dropbox, "ERA5/2m_temp/era5_2006-2020_2m_daily_temp.nc"))

# Read in project grid as shape in same CRS
project_grid <- readRDS(paste0(path_github, "data/grid.RDS"))
project_grid <- spTransform(project_grid, crs(dat_temp0))

# Match project grid cell to overlapping ERA5 grid cell
project_grid$cell_temp <- cellFromXY(dat_temp0[[1]], coordinates(project_grid))

ccc <- ncell(dat_temp0)
w <- which(is.na(dat_temp0[[1]][1:ccc]))

plot(project_grid[project_grid$cell_temp %in% w,])



# nc <- ncell(dat_temp0)
# cell_temp_na <- which(is.na(dat_temp0[[1]][1:nc]))
# cell_temp_nna <- setdiff(1:nc, cell_temp_na)
# cell_temp_nna <- coordinates(dat_temp0)[cell_temp_nna,]
# 
# cell_proj_na <- project_grid$cell_temp %in% cell_temp_na
# nn <- get.knnx(cell_temp_nna,
#                coordinates(project_grid[cell_proj_na,]),
#                k = 1)
# nn$nn.index[nn$nn.dist > 1] <- NA
# project_grid$cell_temp[cell_proj_na] <- nn$nn.index



































path_dropbox <- "~/BurkeLab Dropbox/Data/"
path_github <- "~/Documents/GitHub/purple-air-infiltration/"

library(ncdf4)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(rasterVis)
library(tmap)
library(lubridate)

# Load temperature data
stack1 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2006-2010_2m_daily_temp.nc"))
stack2 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2011-2015_2m_daily_temp.nc"))
stack3 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2016-2020_2m_daily_temp.nc"))
dat_temp0 <- stack(stack1, stack2, stack3)
dat_temp <- dat_temp0[[1]]

# Read in project grid as shape in same CRS
project_grid <- readRDS(paste0(path_github, "data/grid.RDS"))
pg2 <- gBuffer(project_grid, byid = TRUE, width = 5000, capStyle = "SQUARE")
project_grid <- spTransform(project_grid, crs(dat_temp))

# Match project grid cell to overlapping ERA5 grid cell
project_grid$cell_temp <- cellFromXY(dat_temp, coordinates(project_grid))

# Convert grids to sf
dat_temp <- as.data.frame(dat_temp, xy = TRUE) %>% st_as_sf(coords = c("x", "y"))
st_crs(dat_temp) <- st_crs(dat_temp0)
project_grid <- st_as_sf(project_grid)

# Load EPA station locations
loc_epa <- readOGR(paste0(path_dropbox, "PM25/epa_station_locations"), "epa_station_locations")
loc_epa <- spTransform(loc_epa, crs(project_grid))
loc_epa <- as.data.frame(loc_epa, xy = TRUE)

# Read in EPA panel with gridded covariates
dat_epa0 <- readRDS(paste0(path_dropbox, "PM25/panel_station_pm_smoke_day_w_county.RDS"))
dat_epa <- dat_epa0 %>% 
  drop_na(pm25, pbl, state) %>% 
  filter(is.na(temperature)) %>% 
  inner_join(loc_epa) %>% 
  st_as_sf(coords = c("lon", "lat"))
st_crs(dat_epa) <- st_crs(project_grid)

# Read in gridded temperature
grid_temp <- readRDS(paste0(path_dropbox, "ERA5/2m_temp/grid_temperature/grid_temperature_2006.rds"))
date1 <- unique(grid_temp$date)[1]
project_grid <- project_grid %>% 
  left_join(grid_temp %>% filter(date == date1), by = c("ID" = "id_grid"))

# Plot ERA5 grid cells, project 10 km grid cells, and EPA stations
# ggplot() + 
#   geom_sf(data = dat_temp, mapping = aes(color = is.na(X2006.01.01))) +
#   # geom_sf(data = project_grid %>% filter(is.na(temperature)), color = "black") +
#   geom_sf(data = dat_epa, color = "yellow", size = 0.5) + 
#   theme_light() + 
#   theme(legend.position = "none")

# pg_lonlat <- st_coordinates(project_grid)
# project_grid <- project_grid %>% mutate(lon = pg_lonlat[, 1], lat = pg_lonlat[, 2])
pg2 <- spTransform(pg2, crs(dat_temp))
pg2 <- pg2 %>% 
  st_as_sf() %>% 
  st_join(project_grid)
# gplot(dat_temp0[[1]]) + 
#   geom_tile(aes(fill = is.na(value))) + 
#   # geom_sf(data = pg2 %>% filter(is.na(temperature)), mapping = aes(x = lon, y = lat), color = "black") +
#   geom_sf(data = dat_epa, mapping = aes(x= coords.x1, y = coords.x2), color = "yellow", size = 0.5)

# Get county boundaries
counties = readRDS(paste0(path_dropbox, "boundaries/all_national_counties.rds"))
counties = counties[!counties$STATEFP %in% 
                      c("02", "15", "60", "66", "69", "72", "78"), ]
counties = spTransform(counties, crs(dat_temp))
counties <- counties %>% st_as_sf()

# Plot ERA5 grid cell boundaries, 10 km grid cell boundaries, county boundaries,
# and EPA station points
# ggplot() + 
#   geom_sf(data = counties) + 
#   geom_sf(data = pg2 %>% filter(is.na(temperature)), color = "black", fill = NA) + 
#   geom_sf(data = dat_epa, color = "yellow", size = 0.5)
# 
# gplot(dat_temp0[[1]]) + 
#   geom_tile(aes(fill = is.na(value))) +
#   geom_sf(data = counties) + 
#   geom_sf(data = pg2 %>% filter(is.na(temperature)), color = "black", fill = NA) + 
#   geom_sf(data = dat_epa, color = "yellow", size = 0.5)

plot(dat_temp0[[1]], col = rgb(1, 0.55, 0.41, alpha = 0.5))
plot(counties$geometry, add = TRUE)
plot(pg2 %>% filter(is.na(temperature)) %>% pull(geometry), add = TRUE, col = rgb(0, 0, 1, alpha = 0.5))
plot(dat_epa$geometry, add = TRUE, type = "p", pch = 19, cex = 1, col = "yellow")

# tm_shape(dat_temp0[[1]]) + 
#   tm_raster()
# tm_shape(counties) + 
#   tm_borders() + 
#   tm_shape(pg2 %>% filter(is.na(temperature))) + 
#   tm_borders(col = "green") + 
#   tm_shape(dat_epa) + 
#   tm_dots(col = "yellow")

# How many EPA stations are affected?
length(unique(dat_epa$id))
# How many obs are affected?
nrow(dat_epa)
# What time period is affected?
hist(dat_epa$date, breaks = 14)
range(dat_epa$date)
for (i in 1:length(unique(dat_epa$id))) {
  id_i <- unique(dat_epa$id)[i]
  print(paste(i, year(min(dat_epa %>% filter(id == id_i) %>% pull(date)))))
}
# How many obs are smoke days?
count(dat_epa, smoke_day)
# What percent of our West Coast sample is missing as a result?
x1 <- count(dat_epa %>% filter(state %in% c("06", "53", "41")), smoke_day) %>% filter(smoke_day == 1) %>% pull(n)
x2 <- nrow(readRDS("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/complex_MERRA_model/cleaned_station_covariates_smokedays_WestCoast_2006_mid2020.rds"))
x1/(x1 + x2)

# How does TIGER compare with other boundaries data?
# Natural Earth
library(rnaturalearth)
usa <- ne_states(country = "United States of America", returnclass = "sf")
conus <- usa %>% 
  st_transform(st_crs(project_grid)) %>% 
  filter(!(postal %in% c("AK", "HI")))

plot(dat_temp0[[1]], col = rgb(1, 0.55, 0.41, alpha = 0.5))
plot(counties$geometry, add = TRUE, col = rgb(0, 1, 0, alpha = 0.5))
plot(conus$geometry, add = TRUE, col = rgb(1, 0, 0, alpha = 0.5))
plot(dat_epa$geometry, add = TRUE, type = "p", pch = 19, cex = 1, col = "yellow")

# GADM - TOO SLOW
# gadm <- list.files(paste0(path_dropbox, "boundaries/gadm_rds/"), full.names = TRUE) %>% 
#   lapply(readRDS) %>% 
#   bind() %>% 
#   st_as_sf() %>% 
#   st_transform(st_crs(project_grid))

# plot(counties$geometry, col = rgb(0, 1, 0, alpha = 0.5))
# plot(gadm$geometry, add = TRUE, col = rgb(1, 0, 0, alpha = 0.5))

# NN outside 1 degree?
# run the get_temperature_2 script for 2020
# merra = readRDS(paste0(paste0(path_dropbox, "MERRA-2/us_grid_final/grid_aot_", 2020, ".RDS")))
crosswalk <- readRDS(paste0(path_dropbox, "boundaries/10km_grid_EPA_crosswalk.RDS"))

temperature <- temperature %>% rename(grid_id = id_grid)

# merra <- merra %>% 
#   select(-temperature) %>% 
#   mutate(date = ymd(date))

dat_epa1 <- dat_epa %>% 
  rename(epa_id = id) %>% 
  select(-temperature) %>% 
  left_join(crosswalk) %>% 
  left_join(temperature) %>% 
  filter(is.na(temperature))

# Plot ERA5 land, project grid cells at NA overlap, EPA before and after NN-ing
# but this is not showing up right for some reason so most EPA stations will
# look like they get NA-ed, this is not true, see map below.
plot(dat_temp0[[1]], col = rgb(1, 0.55, 0.41, alpha = 0.5))
# plot(counties$geometry, add = TRUE, col = rgb(0, 1, 0, alpha = 0.5))
# plot(conus$geometry, add = TRUE, col = rgb(1, 0, 0, alpha = 0.5))
plot(pg2 %>% filter(is.na(temperature)) %>% pull(geometry), add = TRUE, col = rgb(0, 0, 1, alpha = 0.5))
plot(dat_epa$geometry, add = TRUE, type = "p", pch = 19, cex = 1, col = rgb(1, 1, 0, alpha = 0.5))
plot(dat_epa1$geometry, add = TRUE, type = "p", pch = 19, cex = 1, col = rgb(1, 0, 1, alpha = 0.5))

# Plot ERA5 land, project grid cells at NA overlap, and project grid centroids
# that don't have non-NA NN ERA5 cell in 1 degree
plot(dat_temp0[[1]], col = rgb(1, 0.55, 0.41, alpha = 0.5))
plot(pg2 %>% filter(is.na(temperature)) %>% pull(geometry), add = TRUE, col = rgb(0, 0, 1, alpha = 0.5))
plot(project_grid %>% 
       filter(ID %in% c(1395, 1396, 1908, 1909, 1910, 1911, 2930, 2931)) %>% 
       pull(geometry),
     add = TRUE, type = "p", pch = 19, cex = 1, col = rgb(1, 1, 0, alpha = 0.5))









# library(dplyr)
# library(tidyr)
# library(lubridate)
# library(stringr)
# 
# path_dropbox <- "~/BurkeLab Dropbox/Data/"
# epa = readRDS(paste0(path_dropbox, "PM25/EPA/epa_station_level_pm25_data.rds"))
# merra = readRDS(paste0(paste0(path_dropbox, "MERRA-2/us_grid_final/grid_aot_", 2020, ".RDS")))
# crosswalk <- readRDS(paste0(path_dropbox, "boundaries/10km_grid_EPA_crosswalk.RDS"))
# 
# temperature <- temperature %>% rename(grid_id = id_grid)
# 
# merra <- merra %>% 
#   select(-temperature) %>% 
#   mutate(date = ymd(date))
# 
# epa <- epa %>% 
#   mutate(date = mdy(str_pad(date, 8, "left", 0))) %>% 
#   rename(epa_id = id)
# 
# df <- epa %>% 
#   left_join(crosswalk) %>% 
#   left_join(merra) %>% 
#   left_join(temperature)
# 
# dat_epa0 <- readRDS(paste0(path_dropbox, "PM25/panel_station_pm_smoke_day_w_county.RDS"))
# dat_epa <- dat_epa0 %>% 
#   drop_na(pm25, pbl, state) %>% 
#   filter(is.na(temperature)) %>% 
#   select(-temperature) %>% 
#   left_join() %>% 
#   inner_join(loc_epa) %>% 
#   st_as_sf(coords = c("lon", "lat"))
# st_crs(dat_epa) <- st_crs(project_grid)































library(raster)
library(FNN)

elev = raster(nrows = 6, ncols = 6, res = 0.5,
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
              vals = c(1:35, NA))
ncell(elev)
getValues(elev)
values(elev)
elev[1:36]
elev[1:ncell(elev)] == values(elev)

nna <- which(!is.na(values(elev)))
coordinates(elev)[which(!is.na(values(elev))),]
coordinates(elev)[which(is.na(values(elev))),]
cell_temp_na <- which(is.na(values(elev)))
cell_proj_na <- o %in% cell_temp_na


pg <- raster(nrows = 4, ncols = 4, res = 0.64, 
             xmn = -1.6, xmx = 1.6, ymn = -1.6, ymx = 1.6,
             vals = 1)

o <- cellFromXY(elev, coordinates(pg))
nn <- get.knnx(coordinates(elev)[nna,],
               coordinates(pg),
               k = 1)

ifelse(cell_proj_na,# & nn$nn.dist <= 1, 
       nn$nn.index, 
       o)












# Load temperature data
dat_temp0 <- stack(
  raster(nrows = 6, ncols = 6, res = 0.5,
         xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
         vals = c(1:28, NA, 30:36)),
  raster(nrows = 6, ncols = 6, res = 0.5,
         xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
         vals = c(1:28*3, NA, 30:36*3))
)
dat_temp <- dat_temp0[[1]]

# Read in project grid as shape in same CRS
project_grid <- raster(nrows = 4, ncols = 4, res = 0.8, 
                       xmn = -1.6, xmx = 1.6, ymn = -1.6, ymx = 1.6,
                       vals = 1)
project_grid <- rasterToPoints(project_grid, spatial = TRUE)
project_grid <- spTransform(project_grid, crs(dat_temp0))

# Match project grid cell to overlapping ERA5 grid cell
# project_grid$cell_temp <- cellFromXY(dat_temp0[[1]], coordinates(project_grid))

# Change ERA5 grid cell to nearest ERA5 grid cell with non-NA
cell_temp_nna <- which(!is.na(values(dat_temp0[[1]])))
nn <- get.knnx(coordinates(dat_temp0)[cell_temp_nna,],
               coordinates(project_grid),
               k = 1)
# cell_temp_na <- which(is.na(values(dat_temp0[[1]])))
# cell_proj_na <- project_grid$cell_temp %in% cell_temp_na
# Use nearest neighbor only if within 1 degree
# project_grid$cell_temp <- ifelse(cell_proj_na,# & nn$nn.dist <= 1, 
#                                  nn$nn.index, 
#                                  project_grid$cell_temp)
project_grid$cell_temp <- nn$nn.index

dat_temp0[[2]][project_grid$cell_temp]
dat_temp0[project_grid$cell_temp]

nearestLand(coordinates(project_grid), dat_temp0[[1]], max_distance = 150000)

