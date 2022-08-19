source("work/08_figures/00_utils.R")

library(reticulate)
# use_condaenv("r-reticulate")
if (!py_module_available("cdsapi")) py_install("cdsapi", pip = TRUE)
if (!py_module_available("tenacity")) py_install("tenacity", pip = TRUE)
source_python(paste0(path_github, "work/08_figures/05_define_CDS_API_request_hourly.py"))

library(ncdf4)
library(raster)
library(dplyr)
library(lubridate)
library(sp)
library(rgeos)
library(sf)
library(fields)
library(ggplot2)

#-------------------------------------------------------------------------------
# Plot ERA5 rasters
# Written by Jessica
# Last edited January 2022
#-------------------------------------------------------------------------------
# Contiguous US
north = 49.92
south = 24.43
east = -66.62
west = -125.5

# Download hourly raster data
retrieve_era5_hourly(
  dataset = "reanalysis-era5-land",
  variable = "2m_temperature",
  years = chosen_year,
  months = chosen_month,
  days = as.integer(chosen_day):(as.integer(chosen_day) + 1),
  hours = 0:23,
  product_type = "reanalysis",
  area = list(lat = c(south, north), lon = c(west, east)),
  folder = paste0(path_github, "data/")
)

#-------------------------------------------------------------------------------
# Get hourly rasters
hourly = stack(paste0(path_github, "data/reanalysis-era5-land_2m_temperature_",
                      chosen_year, "_", chosen_month, "_",
                      chosen_day, "-", as.integer(chosen_day) + 1, "_00-23.nc"))
dates = names(hourly)
dates = as.POSIXct(dates, format = "X%Y.%m.%d.%H.%M", tz = "UTC")
dates = with_tz(dates, "Etc/GMT+6")
chosen_date_interval = ymd_h(paste(chosen_date, "00"), tz = "Etc/GMT+6") %--% 
  ymd_hm(paste(chosen_date, "2359"), tz = "Etc/GMT+6")
dates = dates[which(dates %within% chosen_date_interval)]
dates_UTCm6 = format(dates, "X%Y.%m.%d.%H")
dates = with_tz(dates, "UTC")
dates = format(dates, "X%Y.%m.%d.%H.%M.%OS")
hourly = subset(hourly, dates)
names(hourly) = dates_UTCm6

# Get daily raster
daily = stack(paste0(path_dropbox, "ERA5/Land/2m_temperature/USA/raw/UTC-0600/daily_mean_of_1-hourly/reanalysis-era5-land_2m_temperature_daily_mean_",
                     chosen_year, "_", chosen_month, ".nc"))
daily = subset(daily, sprintf("X%s.%s.%s", chosen_year, chosen_month, chosen_day))

# Plot hourly and daily rasters
pdf(paste0(path_figures, "era5_", chosen_date, ".pdf"), width = 24, height = 8)
zlim = range(values(hourly), na.rm = T)
par0 = par()
layout(cbind(matrix(1:24, nrow = 4, byrow = T), matrix(rep(25, 20), nrow = 4)))
par(mar = par0$mar - 2)
for (i in 1:24) image(subset(hourly, names(hourly)[i]), col = bpy.colors(), zlim = zlim, 
                      axes = F, xlab = NA, ylab = NA, main = gsub("X", "", names(hourly)[i]))
par(mar = par0$mar + 2)
image(daily, col = bpy.colors(), zlim = zlim, 
      axes = F, xlab = NA, ylab = NA)
title(main = gsub("X", "", names(daily)), line = 1)
image.plot(legend.only = T, horizontal = T, legend.width = 0.5, 
           legend.lab = "2m Temperature", col = bpy.colors(), zlim = zlim)
dev.off()

#-------------------------------------------------------------------------------
# Load 10 km grid
project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS")

# Get 10 km gridded ERA5
gridded = readRDS(paste0(path_dropbox, "ERA5/Land/2m_temperature/USA/10km_grid/UTC-0600/daily_mean_of_1-hourly/grid_2m_temperature_daily_mean_",
                         chosen_year, "_", chosen_month, ".rds"))
gridded = gridded %>% filter(date == ymd(chosen_date))

# Prepare for plotting
gridded = st_as_sf(project_grid) %>% 
  rename(id_grid = ID) %>% 
  left_join(gridded, by = "id_grid")
g = rasterFromXYZ(st_drop_geometry(gridded) %>% select(COORDX, COORDY, `2m_temperature`),
                  crs = crs(project_grid))
g = projectRaster(g, daily)

# Plot
pdf(paste0(path_figures, "era5_gridded_", chosen_date, ".pdf"), width = 12, height = 8)
image(g, col = bpy.colors(), zlim = zlim, axes = F, xlab = NA, ylab = NA)
image.plot(legend.only = T, horizontal = T, legend.width = 0.5, 
           legend.lab = "2m Temperature", col = bpy.colors(), zlim = zlim)
dev.off()

#-------------------------------------------------------------------------------
# Zoom in using a callout?
