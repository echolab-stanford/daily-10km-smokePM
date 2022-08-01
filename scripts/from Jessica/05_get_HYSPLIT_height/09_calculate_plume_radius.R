source("work/05_get_HYSPLIT_height/00_utils.R")

library(sp)
library(raster)
library(dplyr)

#-------------------------------------------------------------------------------
# Estimate Average Smoke Plume Radius
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Read in smoke plumes
dat_smoke <- readRDS(paste0(path_dropbox, "smoke/smoke_plumes_spdf.RDS"))
plumes <- dat_smoke@polygons

# Convert plume polygons to Spatial
plumes <- SpatialPolygons(plumes, proj = CRS("+proj=longlat"))

# Get plumes' areas in meters squared
plume_area <- plumes %>% area()

# Calculate average radius if average plume area coerced to circle
avg_area <- mean(plume_area)
avg_radius <- sqrt(avg_area/pi)
print(paste("Average plume radius:", avg_radius, "meters"))

# Calculate median area if median plume area coerced to circle
med_area <- median(plume_area)
med_radius <- sqrt(med_area/pi)
print(paste("Median plume radius:", med_radius, "meters"))

# Calculate radius for each plume and get summary statistics
plume_radius <- sqrt(plume_area/pi)
summary(plume_radius)
hist(plume_radius)
hist(plume_radius[plume_radius < 500000])
quantile(plume_radius, 0.95)
