source("work/get_HYSPLIT_height/00_utils.R")

library(ncdf4)
library(raster)
library(sp)
library(lubridate)
library(foreach)
library(doParallel)
library(dplyr)
library(tibble)

num_cores <- 8

#-------------------------------------------------------------------------------
# Get Planetary Boundary Layer
# Written by Jessica
# Last edited June 2021
# 
# For each grid cell, get daily average, minimum, and maximum PBL.
#-------------------------------------------------------------------------------
# Load PBL data
dat_pbl0 <- brick(paste0(path_dropbox, "ERA5/pbl/era5_pbl_usa_2006_2020_6x_daily.nc"))

# Read in project grid and transform to same CRS
project_grid <- readRDS(paste0(path_github, "data/grid.RDS"))
project_grid <- spTransform(project_grid, crs(dat_pbl0))

# Match project grid cell to overlapping ERA5 grid cell
project_grid$cell_pbl <- cellFromXY(dat_pbl0[[1]], coordinates(project_grid))

#-------------------------------------------------------------------------------
grid_years <- 2006:2020
grid_date_times <- names(dat_pbl0)
nc <- ncell(project_grid)/nlayers(project_grid)

registerDoParallel(num_cores)
for (grid_year in grid_years) {
  # Work on one year at a time
  print(paste("Working on:", grid_year, Sys.time()))
  dat_pbl <- dat_pbl0 %>% subset(grep(grid_year, grid_date_times, value = TRUE))
  
  # Get three-hourly PBL in each daily project grid cell
  # This takes 2-3 minutes
  nl <- nlayers(dat_pbl)
  start_time <- get_start_time()
  pbl <- foreach(i = 1:nl) %dopar% dat_pbl[[i]][project_grid$cell_pbl]
  print_time(start_time)
  
  # Get dates
  pbl <- matrix(unlist(pbl), nc, nl)
  date_times <- names(dat_pbl)
  months <- substr(date_times, 7, 8)
  days <- substr(date_times, 10, 11)
  dates <- paste(grid_year, months, days, sep = "-")
  
  # Get average, minimum, and maximum PBL each day per grid cell
  # This takes ~10 minutes
  start_time <- get_start_time()
  pbl <- foreach(i = 1:nc, .combine = bind_rows, .multicombine = TRUE) %dopar% {
    return(
      data.frame(date = dates,
                 pbl = pbl[i,]) %>%
        group_by(date) %>%
        summarize(pbl_mean = mean(pbl), pbl_min = min(pbl), pbl_max = max(pbl)) %>%
        ungroup() %>% 
        add_column(id_grid = project_grid$ID[i])
    )
  }
  print_time(start_time)

  # Clean up data frame
  pbl <- pbl %>%
    mutate(date = ymd(date, tz = "UTC")) %>%
    rename(pbl = pbl_mean) %>% 
    select(id_grid, date, pbl, pbl_min, pbl_max)
  
  # Save daily grid PBL values
  saveRDS(pbl, sprintf("%sERA5/pbl/grid_PBL_%s.rds", path_dropbox, grid_year))
  rm(pbl)
}
stopImplicitCluster()
