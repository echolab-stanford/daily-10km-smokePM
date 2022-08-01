source("work/08_figures/00_utils.R")

library(dplyr)
library(lubridate)
library(ggplot2)
library(sp)
library(rgeos)
library(sf)
library(tigris)
library(magick)

#-------------------------------------------------------------------------------
# Plot fire points
# Written by Jessica
# Last edited January 2022
#-------------------------------------------------------------------------------
# Get fire points
fire_points = read_sf(paste0(path_dropbox, "fire/hms_fire", chosen_date, ".shp"))
fire_points_m1 = read_sf(paste0(path_dropbox, "fire/hms_fire", format(ymd(chosen_date) - days(1), "%Y%m%d"), ".shp"))
fire_points_m2 = read_sf(paste0(path_dropbox, "fire/hms_fire", format(ymd(chosen_date) - days(2), "%Y%m%d"), ".shp"))
fire_points_m3 = read_sf(paste0(path_dropbox, "fire/hms_fire", format(ymd(chosen_date) - days(3), "%Y%m%d"), ".shp"))
fire_points_previous = bind_rows(fire_points_m1, fire_points_m2, fire_points_m3)

# Get CONUS background
usa = states()
conus = usa %>% filter(!(STUSPS %in% c("AK", "AS", "GU", "HI", "MP", "PR", "VI")))

# Get fire clusters
fire_clusters = readRDS(paste0(path_dropbox, "fire/clusters_", chosen_year, ".rds"))
fire_clusters = fire_clusters[which(fire_clusters$date == chosen_date),]
fire_clusters = st_as_sf(fire_clusters)

st_crs(fire_points) = st_crs(fire_clusters)
st_crs(fire_points_previous) = st_crs(fire_clusters)

# Get gridded distance to fire
fire_grid = readRDS(paste0(path_dropbox, "10km_grid_data/distance_to_fire_cluster/grid_distance_to_fire_cluster_", 
                           chosen_year, "_", chosen_month, ".rds"))
fire_grid = fire_grid %>% filter(date == ymd(chosen_date))

# Get into same CRS
project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS")
project_grid = gBuffer(project_grid, byid = T, width = 5000, capStyle = "SQUARE")
project_grid = st_as_sf(project_grid)
fire_grid = project_grid %>% left_join(fire_grid, by = c("ID" = "id_grid"))
fire_grid = fire_grid %>% st_transform(st_crs(fire_clusters))

# Plot gridded distance to fire
p = ggplot(fire_grid, aes(fill = km_dist)) + 
  geom_sf(color = NA) + 
  scale_fill_gradient(low = "red", high = "black") + 
  theme_void() + 
  labs(fill = "Distance\nto Fire (km)")

# Save
ggsave(paste0(path_figures, "hms_fire", chosen_date, "_gridded.png"),
       plot = p, width = 12, height = 8)

#-------------------------------------------------------------------------------
# Get points and clusters in spot chosen to zoom in on
# https://en.wikipedia.org/wiki/Camp_Fire_(2018)
chosen_fire_clusters = fire_clusters %>% 
  st_crop(xmin = west, xmax = east, ymin = south, ymax = north)
chosen_fire_points = fire_points %>% 
  st_crop(xmin = west, xmax = east, ymin = south, ymax = north)
chosen_fire_points_previous = fire_points_previous %>% 
  st_crop(xmin = west, xmax = east, ymin = south, ymax = north)

# Prepare legend
leg = ggplot() + 
  geom_point(data = data.frame(x = 1, y = 1.25), mapping = aes(x, y), color = "black") + 
  geom_text(data = data.frame(x = 1.1, y = 1.25, z = "Fire~Point~(Day~italic(d))"), 
            mapping = aes(x, y, label = z), hjust = 0, parse = T) +
  geom_point(data = data.frame(x = 1, y = 0), mapping = aes(x, y), shape = 1) + 
  geom_text(data = data.frame(x = 1.1, y = 0, z = "Fire~Point~(Days~italic(d-3)~to~italic(d-1))"), 
            mapping = aes(x, y, label = z), hjust = 0, parse = T) +
  geom_point(data = data.frame(x = 1, y = -1.25), mapping = aes(x, y), 
             color = "red", alpha = 0.3, shape = 18, size = 7) + 
  geom_text(data = data.frame(x = 1.1, y = -1.25, z = "Buffer"), 
            mapping = aes(x, y, label = z), hjust = 0) +
  theme_void() +
  coord_cartesian(xlim = c(-1, 3), ylim = c(-10, 10))
leg_file = paste0(path_figures, "hms_fire", chosen_date, "_points_clusters_legend.png")
ggsave(leg_file, plot = leg, width = 3000, height = 1500, units = "px")
leg = image_read(leg_file)
leg = image_trim(leg)
leg = image_resize(leg, "x150")
image_write(leg, path = leg_file)

# Plot fire clusters and points
p = ggplot() + 
  geom_sf(data = chosen_fire_clusters, fill = "red", color = NA, alpha = 0.3) + 
  geom_sf(data = chosen_fire_points_previous, shape = 1) + 
  geom_sf(data = chosen_fire_points, color = "black") + 
  theme_void()
p_file = paste0(path_figures, "hms_fire", chosen_date, "_points_clusters.png")
ggsave(p_file, plot = p, width = 2000, height = 2000, units = "px")
p = image_read(p_file)

# Append legend
p = image_append(c(p, leg))
image_write(p, p_file)
