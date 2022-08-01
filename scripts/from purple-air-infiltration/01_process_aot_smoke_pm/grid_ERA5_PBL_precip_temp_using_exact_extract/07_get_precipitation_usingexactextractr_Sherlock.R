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
library(sp)
library(raster)
library(rgeos)
library(exactextractr)
library(Hmisc)
library(foreach)
library(doParallel)
library(dplyr)

num_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
scratch <- Sys.getenv("SCRATCH")

#-------------------------------------------------------------------------------
# Get Precipitation (ERA5)
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Load precipitation data
dat_precip0 <- brick(paste0(scratch, "/gridding/era5_2006-2020_daily_precip.nc"))

# Read in project grid as shape in same CRS
project_grid <- readRDS(paste0(scratch, "/gridding/grid.RDS"))
project_grid <- gBuffer(project_grid, byid = TRUE, width = 5000, capStyle = "SQUARE")
project_grid <- spTransform(project_grid, crs(dat_precip0))

#-------------------------------------------------------------------------------
grid_years <- 2006:2020
grid_dates <- names(dat_precip0)
nc <- ncell(project_grid)/nlayers(project_grid)

registerDoParallel(num_cores)
for (grid_year in grid_years) {
  # Work on one year at a time
  print(paste("Working on:", grid_year, Sys.time()))
  dat_precip <- dat_precip0 %>% subset(grep(grid_year, grid_dates, value = TRUE))
  
  # Get daily precipitation in each project grid cell
  nl <- nlayers(dat_precip)
  start_time <- get_start_time()
  precipitation <- foreach(i = 1:nl, .combine = c) %dopar% {
    return(
      exact_extract(dat_precip[[i]], project_grid, 
                    fun = function(val, wgt) wtd.mean(val, weights = wgt))
    )
  }
  print_time(start_time)

  precipitation <- data.frame(id_grid = project_grid$ID, 
                              date = rep(names(dat_precip), each = nc),
                              precipitation = precipitation) %>% 
    mutate(date = as.Date(date, format = "X%Y.%m.%d", tz = "UTC"))
  
  # Save daily grid precipitation values
  saveRDS(precipitation, sprintf("%s/gridding/grid_precipitation_%s.rds", scratch, grid_year))
}
stopImplicitCluster()
