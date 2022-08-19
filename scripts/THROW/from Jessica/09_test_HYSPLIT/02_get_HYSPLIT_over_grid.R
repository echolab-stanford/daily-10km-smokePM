source("work/06_complex_MERRA_model/00_utils.R")
path_project = paste0(path_dropbox, "../Projects/smoke_PM_prediction/")
path_results = paste0(path_github, "work/09_test_HYSPLIT/results/")
path_seagate = "/Volumes/Seagate PD JBL/"

library(rgeos)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(data.table)
library(ggplot2)

#-------------------------------------------------------------------------------
# Get HYSPLIT Trajectories Over Daily 10 km Grid
# Written by Jessica
# Last edited November 2021
#-------------------------------------------------------------------------------
# Set time period
start_date = "20100607" # "20100601" # "20060101"
end_date = "20201230" # "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(substr(all_dates, 1, 7))
year_months = gsub("-", "_", year_months)

# Get trajectory points files
traj_points_files = list.files(paste0(path_seagate, "HYSPLIT/trajectory points/"), pattern = "^trajectory_points.*\\.rds$")
traj_points_dates = gsub("trajectory_points", "", traj_points_files)
traj_points_dates = gsub("\\.rds", "", traj_points_dates)
traj_points_files = traj_points_files[which(traj_points_dates %in% all_dates_str)]
traj_points_files = paste0(path_seagate, "HYSPLIT/trajectory points/", traj_points_files)

# RERUN AND MAYBE CHANGE BINS BASED ON 2006-2020 ONCE FULL DATA AVAILABLE
# Plot distribution of heights at trajectory points
heights = vector("list", length(traj_points_files))
pb = txtProgressBar(max = length(traj_points_files), style = 3)
for (i in 1:length(traj_points_files)) {
  traj_points = readRDS(traj_points_files[i])
  heights[[i]] = traj_points %>% pull(height)
  setTxtProgressBar(pb, i)
}
heights = unlist(heights)
saveRDS(heights, paste0(path_seagate, "HYSPLIT/miscellaneous/grid_trajectories/heights.rds"))
heights = readRDS(paste0(path_seagate, "HYSPLIT/miscellaneous/grid_trajectories/heights.rds"))
hist(heights, xlab = "mAGL")
quantile(heights, seq(0, 1, 0.01))

# Choose height bins
height_bins = quantile(heights, seq(0, 1, 0.2))
height_bins = height_bins[-c(1, length(height_bins))]
empty_height_bins_df = 1:(length(height_bins) + 1) %>% 
  lapply(function(x) data.frame(0) %>% setNames(paste0("num_traj_points_height_", x))) %>% 
  bind_cols()

# Set up empty aggregates
vbls = c("height", "pressure", "hour_along")
msrs = c("mean", "median", "sd")
empty_aggregates_df = expand.grid(msr = msrs, vbl = vbls) %>% 
  unite(vbl_msr, vbl, msr) %>% 
  bind_cols(value = NA) %>% 
  pivot_wider(names_from = vbl_msr)
msrs_list = sprintf("%s = as.numeric(%s(x))", msrs, msrs)
msrs_list = paste(msrs_list, collapse = ", ")
msrs_list = sprintf("function(x) list(%s)", msrs_list)
msrs_list = parse(text = msrs_list)
msrs_list = eval(msrs_list)

# Choose distance buffer (meters)
distance_buffer = 50*1000

# Read in project grid
project_grid = readRDS(paste0("~/Documents/GitHub/purple-air-infiltration/", "data/grid.RDS"))
project_grid = gBuffer(project_grid, 
                       byid = T, 
                       width = distance_buffer, 
                       capStyle = "ROUND", 
                       quadsegs = 30)
project_grid = project_grid %>% 
  st_as_sf() %>% 
  st_transform(st_crs(traj_points)) %>% 
  rename(id_grid = ID)

# Takes ~7 minutes per month
start_time = get_start_time()
pb = txtProgressBar(max = length(year_months), style = 3)
for (m in 1:length(year_months)) {
  # if (m < which(year_months == "2014_02")) next
  year_month = year_months[m]
  dates_m = grep(paste0("^", gsub("_", "", year_month)), all_dates_str, value = T)
  traj_points_grid = vector("list", length(dates_m))
  for (d in 1:length(dates_m)) {
    # Empty set-up when no trajectory points file for the day
    traj_points_grid_d = data.frame(id_grid = project_grid$id_grid,
                                    date = dates_m[d],
                                    num_traj_points_total = 0) %>% 
      bind_cols(empty_height_bins_df) %>% 
      bind_cols(empty_aggregates_df)
    
    # Get trajectory points for the day
    traj_points_file = grep(dates_m[d], traj_points_files, value = T)
    if (length(traj_points_file) > 0) {
      traj_points = readRDS(traj_points_file)
      
      # Get trajectory points over grid cells
      traj_points_grid_d = project_grid %>% 
        st_join(traj_points) %>% 
        st_drop_geometry() %>% 
        mutate(date = dates_m[d])
      
      # Get count of trajectory points by height bin (cumulative)
      height_bins_df = 1:length(height_bins) %>% 
        lapply(function(x) data.frame(traj_points_grid_d$height < height_bins[x]) %>% setNames(paste0("num_traj_points_height_", x))) %>% 
        bind_cols()
      last_height_bin = ifelse(is.na(height_bins_df[length(height_bins_df)]), NA, T) %>% 
        data.frame() %>% 
        setNames(paste0("num_traj_points_height_", length(height_bins) + 1))
      height_bins_df = height_bins_df %>% bind_cols(last_height_bin)
      
      # Aggregate to the grid cell-day
      traj_points_grid_d = traj_points_grid_d %>% bind_cols(height_bins_df)
      traj_points_grid_d = as.data.table(traj_points_grid_d)
      num_traj_points_total = traj_points_grid_d[
        , .(num_traj_points_total  = sum(!is.na(height))), 
        by = .(id_grid, date)
      ] %>% as.data.frame()
      num_traj_points_height_bins = traj_points_grid_d[
        , lapply(.SD, sum, na.rm = T), 
        by = .(id_grid, date), 
        .SDcols = grep("^num_traj_points_height", names(traj_points_grid_d), value = T)
      ] %>% as.data.frame()
      vbl_msr = traj_points_grid_d[
        , unlist(lapply(.SD, msrs_list), recursive = F), 
        by = .(id_grid, date), 
        .SDcols = grep(paste0("^", paste(vbls, collapse = "|")), names(traj_points_grid_d), value = T)
      ] %>% as.data.frame()
      traj_points_grid_d = reduce(list(num_traj_points_total, num_traj_points_height_bins, vbl_msr), full_join)
      names(traj_points_grid_d) = gsub("\\.", "_", names(traj_points_grid_d))
      
      # De-cumulate trajectory point counts in height bins
      height_bins_cols = traj_points_grid_d %>% select(starts_with("num_traj_points_height"))
      for (j in length(height_bins_cols):1) {
        if (j == 1) next
        height_bins_cols[j] = height_bins_cols[j] - height_bins_cols[j - 1]
      }
      traj_points_grid_d = traj_points_grid_d %>% 
        select(-starts_with("num_traj_points_height")) %>% 
        bind_cols(height_bins_cols) %>% 
        select(id_grid, date, num_traj_points_total, starts_with(c("num_traj_points_height", vbls)))
    }
    traj_points_grid[[d]] = traj_points_grid_d
  }
  traj_points_grid = traj_points_grid %>% bind_rows() %>% arrange(id_grid, date)
  traj_points_grid_file = paste0(path_dropbox, "hms_hysplit/10km_grid/grid_trajectory_points_", year_month, ".rds")
  saveRDS(traj_points_grid, traj_points_grid_file)
  setTxtProgressBar(pb, m)
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
    left_join(aot_anom_smoke_days) %>% 
    replace_na(list(aot_anom = 0)) %>% 
    select(-smoke_day)
  
  # Get trajectory points grids
  traj_points_grid = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid/grid_trajectory_points_", year_month, ".rds"))
  
  # Combine grids
  grid_m = reduce(list(smoke_pm_grid, smoke_day_grid, aot_anom_grid, traj_points_grid), right_join)
  
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
saveRDS(counts, paste0(path_results, "counts.rds"))
counts = readRDS(paste0(path_results, "counts.rds"))
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
traj_points_grid = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid/grid_trajectory_points_", year_month, ".rds"))

# Combine grids
grid_m = reduce(list(smoke_pm_grid, smoke_day_grid, aot_anom_grid, traj_points_grid), right_join)
grid_d = grid_m %>% filter(date == gap_date)
grid_d = project_grid %>% full_join(grid_d)
df = grid_d %>% 
  mutate(high_likely_high = ifelse(is.na(smokePM), F, smokePM > 50) | (is.na(smokePM) & aot_anom > 0.5),
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
# ggsave(paste0(path_results, "map_trajectory_overlap_max_gap.png"), width = 12, height = 8)
# ggsave(paste0(path_results, "map_trajectory_overlap_min_gap.png"), width = 12, height = 8)
ggsave(paste0(path_results, "map_trajectory_overlap_max_n.png"), width = 12, height = 8)

#-------------------------------------------------------------------------------
# Map smoke dates not online
smoke_dates_not_online = smoke_dates_not_online[which(substr(smoke_dates_not_online, 1, 6) >= start_date &
                                                        substr(smoke_dates_not_online, 1, 6) <= end_date)]
# smoke_dates_not_online = c("20200913", "20150701")
# smoke_dates_not_online = c("20150701", "20200913", "20170429", "20170905", "20200916")
for (i in seq_along(smoke_dates_not_online)) {
  gap_date = smoke_dates_not_online[i]
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
  traj_points_grid = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid/grid_trajectory_points_", year_month, ".rds"))
  
  # Combine grids
  grid_m = reduce(list(smoke_pm_grid, smoke_day_grid, aot_anom_grid, traj_points_grid), right_join)
  grid_d = grid_m %>% filter(date == gap_date)
  grid_d = project_grid %>% full_join(grid_d)
  df = grid_d %>% 
    mutate(high_likely_high = ifelse(is.na(smokePM), F, smokePM > 50) | (is.na(smokePM) & aot_anom > 0.5),
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
  ggsave(paste0(path_results, "map_trajectory_overlap_", gap_date, ".png"), width = 12, height = 8)
}
