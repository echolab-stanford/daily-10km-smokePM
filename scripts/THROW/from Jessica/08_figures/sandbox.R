source("work/08_figures/00_utils.R")

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(data.table)
library(sp)
library(rgeos)
library(sf)
library(tigris)
library(layer)
library(splitr)

# ------------------------------------------------------------------------------
# Play around with layer
# Last edited January 2022 by Jessica
# 
# Issues:
#     - Cannot plot polygons?
#     - Cannot plot points or linestrings with grid?
#     - Cannot plot factors?
# ------------------------------------------------------------------------------
# Load 10 km grid
project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS")
project_grid = gBuffer(project_grid, byid = T, width = 5000, capStyle = "SQUARE")
project_grid = st_as_sf(project_grid) %>% select(id_grid = ID)

# ------------------------------------------------------------------------------
# Get fire points
fire_points = read_sf(paste0(path_dropbox, "fire/hms_fire", chosen_date, ".shp"))
fire_points_m1 = read_sf(paste0(path_dropbox, "fire/hms_fire", format(ymd(chosen_date) - days(1), "%Y%m%d"), ".shp"))
fire_points_m2 = read_sf(paste0(path_dropbox, "fire/hms_fire", format(ymd(chosen_date) - days(2), "%Y%m%d"), ".shp"))
fire_points_m3 = read_sf(paste0(path_dropbox, "fire/hms_fire", format(ymd(chosen_date) - days(3), "%Y%m%d"), ".shp"))
fire_points_previous = bind_rows(fire_points_m1, fire_points_m2, fire_points_m3)

# Get fire clusters
fire_clusters = readRDS(paste0(path_dropbox, "fire/clusters_", chosen_year, ".rds"))
fire_clusters = fire_clusters[which(fire_clusters$date == chosen_date),]
fire_clusters = st_as_sf(fire_clusters)

st_crs(fire_points) = st_crs(fire_clusters)
st_crs(fire_points_previous) = st_crs(fire_clusters)

# Get points and clusters in spot chosen to zoom in on
# https://en.wikipedia.org/wiki/Camp_Fire_(2018)
# chosen_fire_clusters = fire_clusters %>% 
#   st_crop(xmin = west, xmax = east, ymin = south, ymax = north)
# chosen_fire_points = fire_points %>% 
#   st_crop(xmin = west, xmax = east, ymin = south, ymax = north)
# chosen_fire_points_previous = fire_points_previous %>% 
#   st_crop(xmin = west, xmax = east, ymin = south, ymax = north)

# ------------------------------------------------------------------------------
# Get distance to fire cluster
fire_distance = readRDS(paste0(path_dropbox, "10km_grid_data/distance_to_fire_cluster/grid_distance_to_fire_cluster_", chosen_year, "_", chosen_month, ".rds"))
fire_distance = fire_distance %>% 
  filter(date == ymd(chosen_date)) %>% 
  select(id_grid, date, km_dist)
fire_distance = project_grid %>% left_join(fire_distance, by = "id_grid")

# ------------------------------------------------------------------------------
# Narrow down to one HYSPLIT initialization point
hysplit = readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct_20060419-20201231.rds")) %>% 
  filter(ymd(ymd) == ymd(chosen_date),
         lon > west, lon < east,
         lat > south, lat < north,
         height == 1500)
initialization = hysplit[30,]

dir.create(paste0(path_github, "work/08_figures/tmp/"))
trajectory = hysplit_trajectory(
  lat = initialization$lat,
  lon = initialization$lon,
  height = initialization$height,
  duration = 6*24,
  days = initialization$ymd,
  daily_hours = initialization$hour,
  met_type = "gdas1",
  met_dir = normalizePath(paste0(path_dropbox, "meteorology/gdas1/")),
  exec_dir = normalizePath(paste0(path_github, "work/08_figures/tmp/"))
)
unlink(paste0(path_github, "work/08_figures/tmp/"), recursive = T)

trajectory = st_as_sf(trajectory, coords = c("lon", "lat"), crs = st_crs(fire_clusters)) # not sure what should the CRS really be

# ------------------------------------------------------------------------------
# Get trajectory point counts
traj_point_counts = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_2006-2020/grid_trajectory_points_", chosen_year, "_", chosen_month, ".rds"))
traj_point_counts = traj_point_counts %>% 
  filter(date == chosen_date) %>% 
  select(id_grid, date, starts_with("num_traj_points_height"))
traj_point_counts = project_grid %>% left_join(traj_point_counts, by = "id_grid")

# ------------------------------------------------------------------------------
# Read in smoke plumes
smoke_plumes = readRDS(paste0(path_dropbox, "smoke/smoke_plumes_sfdf.rds"))
smoke_plumes = smoke_plumes %>% filter(date == chosen_date)

# ------------------------------------------------------------------------------
# Get smoke day grid
smoke_day = readRDS(paste0(path_dropbox, "smoke/10km_grid/grid_smoke_day_", chosen_year, "_", chosen_month, ".rds"))
smoke_day = smoke_day %>% 
  filter(date == ymd(chosen_date)) %>% 
  select(id_grid, date, smoke_day)
smoke_day = project_grid %>% 
  left_join(smoke_day, by = "id_grid")

# ------------------------------------------------------------------------------
# AOD Missingness
aod_na = list.files(paste0(path_project, "data/2_from_EE/maiac_AODmissings/"),
                    pattern = sprintf("^aod_pctMissing_10km_subgrid_.*_%s.*\\.csv$", chosen_year),
                    full.names = T) %>% 
  map_dfr(read.csv) %>% 
  rename(id_grid = ID, date = start_date, perc_aod_missing = mean) %>% 
  filter(date == chosen_date)
aod_na = project_grid %>% left_join(aod_na, by = "id_grid")

# ------------------------------------------------------------------------------
# Get filled smoke day grid
filled_smoke_day = readRDS(paste0(path_project, "data/3_intermediate/all_smoke_days_incl_cloudy.rds"))
filled_smoke_day = filled_smoke_day %>% 
  filter(date == ymd(chosen_date)) %>% 
  select(id_grid = grid_id_10km, date, smoke_day)
filled_smoke_day = project_grid %>% 
  left_join(filled_smoke_day, by = "id_grid") %>% 
  replace_na(list(date = ymd(chosen_date), smoke_day = 0))

# ------------------------------------------------------------------------------
# Plot layers
deg = 30
layers = list(fire_points, fire_points_previous, st_cast(fire_clusters, "LINESTRING"), fire_distance,
              trajectory, traj_point_counts, traj_point_counts, traj_point_counts, traj_point_counts, traj_point_counts,
              st_cast(smoke_plumes, "LINESTRING"), smoke_day, aod_na, filled_smoke_day)
i = c(1, 2, 5)
i = c(3, 11)
i = c(4, 6, 7, 8, 9, 10, 12, 13, 14)
p_layers = lapply(layers[i], tilt_map, 
                  x_shift = -((seq_along(layers) - 1)*100000*deg)[seq_along(i)],
                  y_shift = ((seq_along(layers) - 1)*100000*deg)[seq_along(i)])
plot_tiltedmaps(p_layers,
                layer = c(NA, NA, NA, "km_dist", NA, "num_traj_points_height_1", 
                          "num_traj_points_height_2", "num_traj_points_height_3", "num_traj_points_height_4", "num_traj_points_height_5",
                          NA, "smoke_day", "perc_aod_missing", "smoke_day")[i])
