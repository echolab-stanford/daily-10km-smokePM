print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}
  
library(ncdf4)
library(raster)
library(rgeos)
library(sp)
library(exactextractr)
library(Hmisc)
library(lubridate)
library(foreach)
library(doParallel)
library(dplyr)
library(tibble)

num_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
scratch <- Sys.getenv("SCRATCH")

#-------------------------------------------------------------------------------
# Get Planetary Boundary Layer
# Written by Jessica
# Last edited June 2021
# 
# For each grid cell, get daily average, minimum, and maximum PBL.
#-------------------------------------------------------------------------------
# Load PBL data
dat_pbl0 <- brick(paste0(scratch, "/gridding/era5_pbl_usa_2006_2020_6x_daily.nc"))

# Read in project grid as shape in same CRS
project_grid <- readRDS(paste0(scratch, "/gridding/grid.RDS"))
project_grid <- gBuffer(project_grid, byid = TRUE, width = 5000, capStyle = "SQUARE")
project_grid <- spTransform(project_grid, crs(dat_pbl0))

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
  nl <- nlayers(dat_pbl)
  start_time <- get_start_time()
  pbl <- foreach(i = 1:nl) %dopar% {
    return(exact_extract(dat_pbl[[i]], project_grid, 
                         fun = function(val, wgt) wtd.mean(val, weights = wgt)))
  }
  print_time(start_time)

  # Get dates
  pbl <- matrix(unlist(pbl), nc, nl)
  date_times <- names(dat_pbl)
  months <- substr(date_times, 7, 8)
  days <- substr(date_times, 10, 11)
  dates <- paste(grid_year, months, days, sep = "-")
  
  # Get average, minimum, and maximum PBL each day per grid cell
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
  saveRDS(pbl, sprintf("%s/gridding/grid_PBL_%s.rds", scratch, grid_year))
}
stopImplicitCluster()
