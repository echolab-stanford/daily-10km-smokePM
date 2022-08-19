source("work/05_get_HYSPLIT_height/00_utils.R")
path_github <- "~/Documents/GitHub/purple-air-infiltration/" # delete once move repos

library(FNN)
library(ncdf4)
library(sp)
library(raster)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# Get Temperature (ERA5)
# Written by Jessica
# Last edited August 2021
#-------------------------------------------------------------------------------
# Load temperature data
stack1 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2006-2010_2m_daily_temp.nc"))
stack2 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2011-2015_2m_daily_temp.nc"))
stack3 <- stack(paste0(path_dropbox, "ERA5/2m_temp/era5_2016-2020_2m_daily_temp.nc"))
dat_temp0 <- stack(stack1, stack2, stack3)

# Read in project grid in same CRS
project_grid <- readRDS(paste0(path_github, "data/grid.RDS"))
project_grid <- spTransform(project_grid, crs(dat_temp0))

# Match project grid cell to overlapping ERA5 grid cell
project_grid$cell_temp <- cellFromXY(dat_temp0[[1]], coordinates(project_grid))

# Find nearest neighbor non-NA ERA5 grid cell
cell_temp_nna <- which(!is.na(values(dat_temp0[[1]])))
nn <- get.knnx(coordinates(dat_temp0)[cell_temp_nna,],
               coordinates(project_grid),
               k = 1)
cell_temp_na <- which(is.na(values(dat_temp0[[1]])))
cell_proj_na <- project_grid$cell_temp %in% cell_temp_na

# Match to NN non-NA ERA5 grid cell in 1 degree if overlap temperature is NA
project_grid$cell_temp <- ifelse(cell_proj_na & nn$nn.dist <= 1,
                                 cell_temp_nna[nn$nn.index],
                                 project_grid$cell_temp)

# Theoretically NN-ing all project grid cells to non-NA ERA5 grid cell should be 
# equivalent if not for cutoff distance
# project_grid$cell_temp <- cell_temp_nna[nn$nn.index]

#-------------------------------------------------------------------------------
grid_years <- 2006:2020
grid_dates <- names(dat_temp0)

for (grid_year in grid_years) {
  # Work on one year at a time
  print(paste("Working on:", grid_year, Sys.time()))
  dat_temp <- dat_temp0 %>% subset(grep(grid_year, grid_dates, value = TRUE))
  
  # Get daily temperature in each project grid cell
  start_time <- get_start_time()
  temperature <- dat_temp[project_grid$cell_temp]
  print_time(start_time)
  
  temperature <- data.frame(id_grid = project_grid$ID,
                            temperature) %>% 
    pivot_longer(cols = -id_grid,
                 names_to = "date",
                 values_to = "temperature") %>% 
    mutate(date = as.Date(date, format = "X%Y.%m.%d", tz = "UTC"))
  
  # Save daily grid temperature values
  saveRDS(temperature, sprintf("%sERA5/2m_temp/grid_temperature/grid_temperature_%s.rds", path_dropbox, grid_year))
}
