source("work/06_complex_MERRA_model/00_utils.R")
path_project = paste0(path_dropbox, "../Projects/smoke_PM_prediction/")
path_results = paste0(path_github, "work/09_test_HYSPLIT/results/")
path_seagate = "/Volumes/Seagate PD JBL/"

library(sp)
library(rgeos)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------
# Expand Buffer for Trajectory Point Counts
# Written by Jessica
# Last edited November 2021
# 
# Given our daily 10 km grid with counts of trajectory points in each height bin,
# modify counts of trajectory points in each height bin to draw from an enlarged
# buffer centered at each grid cell. The aim is to count trajectory points overlapping
# adjacent bins due to discrete hourly resolution of trajectory data and the 
# fact that trajectories follow average wind vector and not particle dispersion.
#-------------------------------------------------------------------------------
# Set window (number of project grid cells as diameter of square buffer)
win = 5

# Set output path
path_out = paste0(path_dropbox, "hms_hysplit/10km_grid_buffered/")

# Read in project grid and set up raster with IDs
project_grid = readRDS(paste0("~/Documents/GitHub/purple-air-infiltration/", "data/grid.RDS")) %>% 
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  rename(id_grid = ID)
project_grid_rast = rast(project_grid %>% 
                           select(COORDX, COORDY, id_grid) %>% 
                           as.matrix(),
                         crs = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs",
                         type = "xyz")

# Get grid file names
grid_files = list.files(paste0(path_dropbox, "hms_hysplit/10km_grid/"), full.names = T)
traj_points_grid = readRDS(grid_files[1])

# Takes ~30 seconds per month
pb = txtProgressBar(max = length(grid_files), style = 3)
start_time = get_start_time()
for (f in seq_along(grid_files)) {
  # For each month file
  grid_file = grid_files[f]
  traj_points_grid = readRDS(grid_file)
  dates_m = unique(traj_points_grid$date)
  out = vector("list", length(dates_m))
  for (j in seq_along(dates_m)) {
    # For each day
    d = dates_m[j]
    traj_points_grid_d = traj_points_grid %>% filter(date == d)
    traj_points_grid_d = project_grid %>% full_join(traj_points_grid_d, by = "id_grid")
    
    # Rasterize counts in each height bin
    val_rast = rast(traj_points_grid_d %>% 
                      select(COORDX, COORDY, starts_with("num_traj_points")) %>% 
                      as.matrix(),
                    crs = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs",
                    type = "xyz")
    
    # Focalize counts based on window size
    foc_rast = terraMultiFocal(val_rast, w = win, fun = sum, na.rm = T)
    
    # Convert from raster to data frame
    foc_rast = c(project_grid_rast, foc_rast)
    out_d = as.data.frame(values(foc_rast)) %>% 
      filter(id_grid %in% project_grid$id_grid) %>% 
      mutate(date = d, .after = id_grid)
    out[[j]] = out_d
  }
  out = bind_rows(out) %>% arrange(id_grid, date)
  out_file = paste0(path_out, basename(grid_file))
  
  # Save month file
  saveRDS(out, out_file)
  
  setTxtProgressBar(pb, f)
}
print_time(start_time)

#-------------------------------------------------------------------------------
#### Quantify how many grid cell-days with smoke PM have no overlapping trajectories ####
# Get smoke PM at EPA station-days over project grid
# Read in project grid
project_grid = readRDS(paste0("~/Documents/GitHub/purple-air-infiltration/", "data/grid.RDS"))
project_grid = gBuffer(project_grid, 
                       byid = T, 
                       width = 5000, 
                       capStyle = "SQUARE")
traj_points = readRDS(paste0(path_seagate, "HYSPLIT/trajectory points/trajectory_points20100607.rds"))
project_grid = project_grid %>% 
  st_as_sf() %>% 
  st_transform(st_crs(traj_points)) %>% 
  rename(id_grid = ID)

# WHY ARE THERE 2 EPA STATION IDS AT THE SAME LOCATION? ID# 171630010 AND ID# 171639010
epa_locations = read_sf(paste0(path_dropbox, "PM25/epa_station_locations/"))
epa_project_grid_crosswalk = epa_locations %>% 
  st_join(project_grid) %>% 
  st_drop_geometry() %>% 
  select(epa_id = id, lon, lat, id_grid)

# THIS IS NO LONGER ACCURATE
epa = readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates.rds"))
smoke_pm_grid = epa %>% 
  select(epa_id, lon, lat, date, smokePM) %>% 
  mutate(date = format(date, "%Y%m%d")) %>% 
  left_join(epa_project_grid_crosswalk) %>% 
  select(id_grid, date, smokePM)
smoke_pm_grid = smoke_pm_grid %>% 
  group_by(id_grid, date) %>% 
  summarize(smokePM = mean(smokePM, na.rm = T)) %>% 
  ungroup()

year_months = gsub("grid_trajectory_points_", "", tools::file_path_sans_ext(basename(grid_files)))
counts = vector("list", length(year_months))

# Takes ~18.5 seconds per month
start_time = get_start_time()
pb = txtProgressBar(max = length(year_months), style = 3)
for (m in 1:length(year_months)) {
  year_month = year_months[m]
  
  # Get smoke grids
  smoke_day_grid = readRDS(paste0(path_dropbox, "smoke/10km_grid/grid_smoke_day_", year_month, ".rds"))
  smoke_day_grid = smoke_day_grid %>% mutate(date = format(date, "%Y%m%d"))
  
  # Get anomalous AOT grids
  aot_anom_smoke_days = readRDS(paste0(path_project, "data/3_intermediate/aot_anom_smoke_days_", substr(year_month, 6, 7), ".rds"))
  aot_anom_smoke_days = aot_anom_smoke_days %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    rename(id_grid = grid_id_10km)
  aot_anom_grid = smoke_day_grid %>% 
    select(id_grid, date, smoke_day) %>% 
    left_join(aot_anom_smoke_days, by = c("id_grid", "date")) %>% 
    replace_na(list(aot_anom = 0)) %>% 
    select(-smoke_day)
  
  # Get trajectory points grids
  traj_points_grid = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_buffered/grid_trajectory_points_", year_month, ".rds"))
  
  # Combine grids
  grid_m = reduce(list(smoke_pm_grid, smoke_day_grid, aot_anom_grid, traj_points_grid), right_join, by = c("id_grid", "date"))
  
  # Get count of grid cells by condition and date
  counts[[m]] = grid_m %>% 
    group_by(date) %>% 
    count(smoke_day, 
          smokePM > 50, 
          aot_anom > 0.5, 
          num_traj_points_total > 0) %>% 
    ungroup()
  setTxtProgressBar(pb, m)
}
print_time(start_time)
saveRDS(counts, paste0(path_results, "counts_buffered.rds"))
counts = readRDS(paste0(path_results, "counts_buffered.rds"))
counts = bind_rows(counts)

# Calculate quantities of interest
# Smoke day grid cell-days
counts %>% 
  filter(smoke_day == 1) %>% 
  group_by(smoke_day, `num_traj_points_total > 0`) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n))

# High smoke PM grid cell-days where we have EPA data
counts %>% 
  filter(`smokePM > 50`) %>% 
  group_by(`smokePM > 50`, `num_traj_points_total > 0`) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n))

# Likely high smoke PM grid cell-days
counts %>% 
  filter(`aot_anom > 0.5`) %>% 
  group_by(`aot_anom > 0.5`, `num_traj_points_total > 0`) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n))

# High (or likely high) smoke PM grid cell-days
counts %>% 
  filter(`smokePM > 50` | (is.na(`smokePM > 50`) & `aot_anom > 0.5`)) %>% 
  group_by(`smokePM > 50`| (is.na(`smokePM > 50`) & `aot_anom > 0.5`), `num_traj_points_total > 0`) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n))

#-------------------------------------------------------------------------------
# Map on day with biggest/smallest gap in count of cells that should have and do have trajectory points
counts_df = counts %>% 
  mutate(high_likely_high = isTRUE(`smokePM > 50`) | (is.na(`smokePM > 50`) & `aot_anom > 0.5`)) %>% 
  filter(high_likely_high) %>% 
  group_by(date, high_likely_high, `num_traj_points_total > 0`) %>% 
  summarize(n = sum(n)) %>% 
  ungroup()
smoke_dates_not_online = readRDS(paste0(path_dropbox, "smoke/smoke_dates_not_online.rds"))
counts_panel = expand.grid(date = unique(counts_df$date), `num_traj_points_total > 0` = c(T, F)) %>% 
  mutate(high_likely_high = T, .before = `num_traj_points_total > 0`) %>% 
  left_join(counts_df) %>% replace_na(list(n = 0)) %>% 
  group_by(date, high_likely_high) %>% 
  mutate(prop = n/sum(n), 
         gap = (cur_data() %>% filter(!`num_traj_points_total > 0`) %>% pull(n)) - 
           (cur_data() %>% filter(`num_traj_points_total > 0`) %>% pull(n))) %>% 
  ungroup() %>% 
  filter(!(date %in% smoke_dates_not_online))
# gap_date = counts_panel %>% 
#   # slice_max(gap) %>%
#   slice_min(gap) %>% 
#   pull(date) %>% 
#   unique()
gap_date = counts_panel %>% 
  filter(`num_traj_points_total > 0` == F) %>% 
  slice_max(n) %>% 
  pull(date)

year_month = paste0(substr(gap_date, 1, 4), "_", substr(gap_date, 5, 6))

# Get smoke grids
smoke_day_grid = readRDS(paste0(path_dropbox, "smoke/10km_grid/grid_smoke_day_", year_month, ".rds"))
smoke_day_grid = smoke_day_grid %>% mutate(date = format(date, "%Y%m%d"))

# Get anomalous AOT grids
aot_anom_smoke_days = readRDS(paste0(path_project, "data/3_intermediate/aot_anom_smoke_days_", substr(year_month, 6, 7), ".rds"))
aot_anom_smoke_days = aot_anom_smoke_days %>% 
  mutate(date = format(date, "%Y%m%d")) %>% 
  rename(id_grid = grid_id_10km)
aot_anom_grid = smoke_day_grid %>% 
  select(id_grid, date, smoke_day) %>% 
  left_join(aot_anom_smoke_days) %>% 
  replace_na(list(aot_anom = 0)) %>% 
  select(-smoke_day)

# Get trajectory points grids
traj_points_grid = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_buffered/grid_trajectory_points_", year_month, ".rds"))

# Combine grids
grid_m = reduce(list(smoke_pm_grid, smoke_day_grid, aot_anom_grid, traj_points_grid), right_join)
grid_d = grid_m %>% filter(date == gap_date)
grid_d = project_grid %>% full_join(grid_d)
df = grid_d %>% 
  mutate(high_likely_high = ifelse(is.na(smokePM), aot_anom > 0.5, smokePM > 50),
         traj_over = num_traj_points_total > 0,
         attribute = ifelse(smoke_day, "smoke day", NA),
         attribute = ifelse(high_likely_high, "high/likely high smoke PM", attribute),
         attribute = ifelse(traj_over, "trajectory overlaps", attribute),
         attribute = factor(attribute, levels = c("smoke day", "high/likely high smoke PM", "trajectory overlaps")))

# Plot
ggplot(df, aes(color = attribute, fill = attribute)) + 
  geom_sf() + 
  scale_color_manual(values = c("orange", "red", "purple", "lightgray")) + 
  scale_fill_manual(values = c("orange", "red", "purple", "lightgray")) + 
  labs(subtitle = gap_date)
# ggsave(paste0(path_results, "map_trajectory_overlap_max_gap_buffered.png"), width = 12, height = 8)
# ggsave(paste0(path_results, "map_trajectory_overlap_min_gap_buffered.png"), width = 12, height = 8)
ggsave(paste0(path_results, "map_trajectory_overlap_max_n_buffered.png"), width = 12, height = 8)

#-------------------------------------------------------------------------------
# Map smoke dates not online or chosen dates
chosen_dates = smoke_dates_not_online[which(substr(smoke_dates_not_online, 1, 6) >= start_date &
                                              substr(smoke_dates_not_online, 1, 6) <= end_date)]
# chosen_dates = c("20200913", "20150701")
chosen_dates = c("20150701", "20200913", "20170429", "20170905", "20200916")
for (i in seq_along(chosen_dates)) {
  gap_date = chosen_dates[i]
  year_month = paste0(substr(gap_date, 1, 4), "_", substr(gap_date, 5, 6))
  
  # Get smoke grids
  smoke_day_grid = readRDS(paste0(path_dropbox, "smoke/10km_grid/grid_smoke_day_", year_month, ".rds"))
  smoke_day_grid = smoke_day_grid %>% mutate(date = format(date, "%Y%m%d"))
  
  # Get anomalous AOT grids
  aot_anom_smoke_days = readRDS(paste0(path_project, "data/3_intermediate/aot_anom_smoke_days_", substr(year_month, 6, 7), ".rds"))
  aot_anom_smoke_days = aot_anom_smoke_days %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    rename(id_grid = grid_id_10km)
  aot_anom_grid = smoke_day_grid %>% 
    select(id_grid, date, smoke_day) %>% 
    left_join(aot_anom_smoke_days) %>% 
    replace_na(list(aot_anom = 0)) %>% 
    select(-smoke_day)
  
  # Get trajectory points grids
  traj_points_grid = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_buffered/grid_trajectory_points_", year_month, ".rds"))
  
  # Combine grids
  grid_m = reduce(list(smoke_pm_grid, smoke_day_grid, aot_anom_grid, traj_points_grid), right_join)
  grid_d = grid_m %>% filter(date == gap_date)
  grid_d = project_grid %>% full_join(grid_d)
  df = grid_d %>% 
    mutate(high_likely_high = ifelse(is.na(smokePM), aot_anom > 0.5, smokePM > 50),
           traj_over = num_traj_points_total > 0,
           attribute = ifelse(smoke_day, "smoke day", NA),
           attribute = ifelse(high_likely_high, "high/likely high smoke PM", attribute),
           attribute = ifelse(traj_over, "trajectory overlaps", attribute),
           attribute = factor(attribute, levels = c("smoke day", "high/likely high smoke PM", "trajectory overlaps")))
  
  # Plot
  ggplot(df, aes(color = attribute, fill = attribute)) + 
    geom_sf() + 
    scale_color_manual(values = c("orange", "red", "purple", "lightgray")) + 
    scale_fill_manual(values = c("orange", "red", "purple", "lightgray")) + 
    labs(subtitle = gap_date)
  if (gap_date == "20150701") {
    ggplot(df, aes(color = attribute, fill = attribute)) + 
      geom_sf() + 
      scale_color_manual(values = c("orange", "purple", "lightgray")) + 
      scale_fill_manual(values = c("orange", "purple", "lightgray")) + 
      labs(subtitle = gap_date)
  }
  ggsave(paste0(path_results, "map_trajectory_overlap_", gap_date, "_buffered.png"), width = 12, height = 8)
}

#-------------------------------------------------------------------------------
# Map with height bin counts
# Get data for chosen dates
df = chosen_dates %>% 
  map_dfr(function(x) {
    year_month = paste0(substr(x, 1, 4), "_", substr(x, 5, 6))
    
    smoke_day_grid = readRDS(paste0(path_dropbox, "smoke/10km_grid/grid_smoke_day_", year_month, ".rds")) %>% 
      mutate(date = format(date, "%Y%m%d")) %>% 
      filter(date == x)
    
    aot_anom_smoke_days = readRDS(paste0(path_project, "data/3_intermediate/aot_anom_smoke_days_", substr(year_month, 6, 7), ".rds")) %>% 
      mutate(date = format(date, "%Y%m%d")) %>% 
      rename(id_grid = grid_id_10km)
    aot_anom_grid = smoke_day_grid %>% 
      select(id_grid, date, smoke_day) %>% 
      left_join(aot_anom_smoke_days) %>% 
      replace_na(list(aot_anom = 0)) %>% 
      select(-smoke_day) %>% 
      filter(date == x)
    
    traj_points_grid = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_buffered/grid_trajectory_points_", year_month, ".rds")) %>% 
      filter(date == x)
    
    grid_d = reduce(list(smoke_pm_grid, smoke_day_grid, aot_anom_grid, traj_points_grid), right_join)
    return(grid_d)
  })
df = project_grid %>% full_join(df)

# Plot maps
purple = c(r = 138/255, g = 43/255, b = 226/255)
# purple = c(r = 186/255, g = 85/255, b = 219/255)
red = rgb(1,0,0)
orange = rgb(1,0.65,0)
white = rgb(1,1,1)
df_color = df %>% 
  gather(key = "height_bin",
         value = "num_traj_points",
         starts_with("num_traj_points_height")) %>% 
  mutate(height_bin = as.integer(gsub("num_traj_points_height_", "", height_bin)))
hist(df_color$num_traj_points)
top_code = 500
df_color = df_color %>% 
  mutate(color = ifelse(smoke_day == 1, orange, white),
         color = ifelse(ifelse(is.na(smokePM), aot_anom > 0.5, smokePM > 50), red, color),
         color = ifelse(num_traj_points > 0, 
                        # purple to black as increasing in count
                        rgb(purple["r"] - (purple["r"]*pmin(num_traj_points/top_code, 1)),
                            purple["g"] - (purple["g"]*pmin(num_traj_points/top_code, 1)),
                            purple["b"] - (purple["b"]*pmin(num_traj_points/top_code, 1))),
                        color))

# Takes ~1.1 hours
# start_time = get_start_time()
# p = ggplot(df_color) + 
#   geom_sf(aes(fill = color), color = NA) + 
#   facet_wrap(vars(date, height_bin), 
#              nrow = length(chosen_dates), 
#              ncol = length(unique(df_color$height_bin))) + 
#   scale_fill_identity()
# ggsave(paste0(path_results, "map_trajectory_overlap_bins_buffered.png"), 
#        plot = p, width = 24, height = 16)
# print_time(start_time)

# Takes ~1 hour to plot and ~10 minutes to save
plots = vector("list", length(chosen_dates)*length(unique(df_color$height_bin)))
start_time = get_start_time()
i = 1
for (d in sort(chosen_dates)) {
  for (b in sort(unique(df_color$height_bin))) {
    plots[[i]] = ggplot(df_color %>% filter(date == d, height_bin == b)) + 
      geom_sf(aes(fill = color), color = NA) + 
      scale_fill_identity() + 
      labs(title = d, subtitle = paste("Bin", b))
    i = i + 1
  }
}
print_time(start_time)
ggarrange(plotlist = plots)
ggsave(paste0(path_results, "map_trajectory_overlap_bins_buffered.png"), width = 24, height = 16)
