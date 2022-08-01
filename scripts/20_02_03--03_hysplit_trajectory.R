source("work/08_figures/00_utils.R")

library(lubridate)
library(dplyr)
library(tidyr)
library(sp)
library(rgeos)
library(sf)
library(ggplot2)

#-------------------------------------------------------------------------------
# Plot HYSPLIT trajectory
# Written by Jessica
# Last edited January 2022
#-------------------------------------------------------------------------------
# Narrow down to one HYSPLIT initialization point
hysplit = readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct_20060419-20201231.rds")) %>% 
  filter(ymd(ymd) == ymd(chosen_date),
         lon > west, lon < east,
         lat > south, lat < north,
         height == 1500)

# Input these parameters to HYSPLIT GUI to generate and save hms_hysplit_trajplot.pdf
View(hysplit[30,])

#-------------------------------------------------------------------------------
# Get same CRS as fire
fire_clusters = readRDS(paste0(path_dropbox, "fire/clusters_", chosen_year, ".rds"))
fire_clusters = st_as_sf(fire_clusters)
crs_use = st_crs(fire_clusters)
rm(fire_clusters)

# Load 10 km grid
project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS")
project_grid = gBuffer(project_grid, byid = T, width = 5000, capStyle = "SQUARE")
project_grid = st_as_sf(project_grid) %>% st_transform(crs_use) %>% select(id_grid = ID)

# Read in trajectory point counts
traj_grid = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_2006-2020/grid_trajectory_points_", chosen_year, "_", chosen_month, ".rds"))
traj_grid = traj_grid %>% 
  filter(date == chosen_date) %>% 
  select(id_grid, date, starts_with("num_traj_points_height")) %>% 
  pivot_longer(cols = starts_with("num_traj_points_height"),
               names_prefix = "num_traj_points_height_",
               names_to = "height_bin",
               values_to = "num_traj_points") %>% 
  mutate(height_bin = factor(height_bin, levels = 1:5))
traj_grid = project_grid %>% left_join(traj_grid, by = "id_grid")

# Plot
p = ggplot(traj_grid %>% mutate(num_traj_points = na_if(num_traj_points, 0))) + 
  geom_sf(aes(fill = num_traj_points), color = NA) + 
  facet_wrap(vars(height_bin), ncol = 5) + 
  scale_fill_gradient(low = "darkorange4", high = "orange1", 
                      limits = c(NA, 100), oob = scales::squish, 
                      breaks = c(1, 25, 50, 75, 100), labels = c("1", "25", "50", "75", ">100")) + 
  theme_void() +
  labs(fill = "Trajectory\nPoint Count")

# Save
ggsave(paste0(path_figures, "hms_hysplit_trajectory_point_counts_", chosen_date, ".png"),
       plot = p, width = 15, height = 3)
