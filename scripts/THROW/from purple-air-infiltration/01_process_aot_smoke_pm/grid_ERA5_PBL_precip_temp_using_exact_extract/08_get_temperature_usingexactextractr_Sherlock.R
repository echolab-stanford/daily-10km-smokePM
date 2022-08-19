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
# Get Temperature (ERA5)
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Merge separately downloaded files and save
stack1 <- stack(paste0(scratch, "/gridding/era5_2006-2010_2m_daily_temp.nc"))
stack2 <- stack(paste0(scratch, "/gridding/era5_2011-2015_2m_daily_temp.nc"))
stack3 <- stack(paste0(scratch, "/gridding/era5_2016-2020_2m_daily_temp.nc"))
dat_temp0 <- stack(stack1, stack2, stack3)
# writeRaster(dat_temp0, paste0(path_dropbox, "ERA5/2m_temp/era5_2006-2020_2m_daily_temp.nc"))

#-------------------------------------------------------------------------------
# Load temperature data
# dat_temp0 <- brick(paste0(path_dropbox, "ERA5/2m_temp/era5_2006-2020_2m_daily_temp.nc"))

# Read in project grid as shape in same CRS
project_grid <- readRDS(paste0(scratch, "/gridding/grid.RDS"))
project_grid <- gBuffer(project_grid, byid = TRUE, width = 5000, capStyle = "SQUARE")
project_grid <- spTransform(project_grid, crs(dat_temp0))

#-------------------------------------------------------------------------------
grid_years <- 2006:2020
grid_dates <- names(dat_temp0)
nc <- ncell(project_grid)/nlayers(project_grid)

registerDoParallel(num_cores)
for (grid_year in grid_years) {
  # Work on one year at a time
  print(paste("Working on:", grid_year, Sys.time()))
  dat_temp <- dat_temp0 %>% subset(grep(grid_year, grid_dates, value = TRUE))
  
  # Get daily temperature in each project grid cell
  nl <- nlayers(dat_temp)
  start_time <- get_start_time()
  temperature <- foreach(i = 1:nl, .combine = c) %dopar% {
    return(
      exact_extract(dat_temp[[i]], project_grid, 
                    fun = function(val, wgt) wtd.mean(val, weights = wgt))
    )
  }
  print_time(start_time)
  
  temperature <- data.frame(id_grid = project_grid$ID, 
                            date = rep(names(dat_temp), each = nc),
                            temperature = temperature) %>% 
    mutate(date = as.Date(date, format = "X%Y.%m.%d", tz = "UTC"))
  
  # Save daily grid temperature values
  saveRDS(temperature, sprintf("%s/gridding/grid_temperature_%s.rds", scratch, grid_year))
}
stopImplicitCluster()
