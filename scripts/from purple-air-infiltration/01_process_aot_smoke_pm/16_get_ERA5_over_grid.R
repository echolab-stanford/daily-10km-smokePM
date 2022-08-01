source("work/05_get_HYSPLIT_height/00_utils.R")
path_github <- "~/Documents/GitHub/purple-air-infiltration/" # delete once move repos

library(FNN)
library(ncdf4)
library(sp)
library(raster)
library(dplyr)
library(tidyr)
library(stringr)

#-------------------------------------------------------------------------------
# Get ERA5 Variables Over 10 km Grid
# Written by Jessica
# Last edited September 2021
#-------------------------------------------------------------------------------
# Read in project grid
project_grid0 <- readRDS(paste0(path_github, "data/grid.RDS"))

# Define function for getting an ERA5 variable over 10 km grid
get_over_grid <- function(dataset, variable, statistic, time_zone, path_era5 = file.path(path_dropbox, "ERA5")) {
  # Confirm dataset
  stopifnot(dataset %in% c("global", "land"))
  dataset_dir <- str_to_title(dataset)
  time_zone_dir <- gsub(":", "", time_zone)
  
  # Load ERA5 data
  path_in <- file.path(path_era5, dataset_dir, variable, "USA", "raw", time_zone_dir,
                       paste0(statistic, "_of_1-hourly"))
  file_names <- list.files(path_in, full.names = TRUE, pattern = "\\.nc$")
  dat_variable0 <- file_names %>% lapply(stack) %>% stack()
  
  # Get project grid in same CRS
  project_grid <- spTransform(project_grid0, crs(dat_variable0))
  
  # Match project grid cell to overlapping ERA5 grid cell
  project_grid$cell_era5 <- cellFromXY(dat_variable0[[1]], coordinates(project_grid))
  
  # Find nearest neighbor non-NA ERA5 grid cell
  cell_era5_nna <- which(!is.na(values(dat_variable0[[1]])))
  nn <- get.knnx(coordinates(dat_variable0)[cell_era5_nna,],
                 coordinates(project_grid),
                 k = 1)
  cell_era5_na <- which(is.na(values(dat_variable0[[1]])))
  cell_proj_na <- project_grid$cell_era5 %in% cell_era5_na
  
  # Match to NN non-NA ERA5 grid cell in 1 degree if overlap value is NA
  project_grid$cell_era5 <- ifelse(cell_proj_na & nn$nn.dist <= 1,
                                   cell_era5_nna[nn$nn.index],
                                   project_grid$cell_era5)
  
  # Theoretically NN-ing all project grid cells to non-NA ERA5 grid cell should be 
  # equivalent if not for cutoff distance
  # project_grid$cell_era5 <- cell_era5_nna[nn$nn.index]
  
  grid_year_months <- file_names %>% str_sub(-10, -4) %>% unique() %>% sort()
  wind_speed <- variable == "wind_speed"
  if (wind_speed) grid_year_months <- file_names %>% str_sub(-7, -4) %>% unique() %>% sort()
  grid_dates <- names(dat_variable0)
  
  for (grid_year_month in grid_year_months) {
    # Work on one month at a time
    print(paste("Working on:", variable, statistic, grid_year_month, Sys.time()))
    dat_variable <- dat_variable0 %>% subset(grep(gsub("_", "\\.", grid_year_month), 
                                                  grid_dates, value = TRUE))

    # Get daily value in each project grid cell
    start_time <- get_start_time()
    era5_values <- dat_variable[project_grid$cell_era5]
    print_time(start_time)
    
    # Reshape for merging later
    gridded_values <- data.frame(id_grid = project_grid$ID,
                                 era5_values) %>% 
      pivot_longer(cols = -id_grid,
                   names_to = "date",
                   values_to = variable) %>% 
      mutate(date = as.Date(date, format = "X%Y.%m.%d", tz = "UTC"))
    
    # Save daily gridded values
    saveRDS(gridded_values, 
            file.path(
              path_era5, dataset_dir, variable, "USA", "10km_grid", time_zone_dir, paste0(statistic, "_of_1-hourly"),
              paste0(paste("grid", variable, statistic, grid_year_month, sep = "_"), ".rds")
            ))
  }
}

# Get variables over project grid
get_over_grid("global", "boundary_layer_height", "daily_mean", "UTC-06:00")
get_over_grid("global", "boundary_layer_height", "daily_minimum", "UTC-06:00")
get_over_grid("global", "boundary_layer_height", "daily_maximum", "UTC-06:00")
get_over_grid("global", "mean_sea_level_pressure", "daily_mean", "UTC-06:00")
get_over_grid("land", "2m_temperature", "daily_mean", "UTC-06:00")
get_over_grid("land", "2m_dewpoint_temperature", "daily_mean", "UTC-06:00")
get_over_grid("land", "10m_u_component_of_wind", "daily_mean", "UTC-06:00")
get_over_grid("land", "10m_v_component_of_wind", "daily_mean", "UTC-06:00")
get_over_grid("land", "surface_pressure", "daily_mean", "UTC-06:00")
get_over_grid("land", "total_precipitation", "daily_maximum", "UTC+00:00")
get_over_grid("land", "wind_speed", "daily_mean", "UTC+00:00")
