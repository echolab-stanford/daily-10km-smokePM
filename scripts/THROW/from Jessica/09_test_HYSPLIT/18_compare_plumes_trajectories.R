source("work/06_complex_MERRA_model/00_utils.R")
path_project = paste0(path_dropbox, "../Projects/smoke_PM_prediction/")
path_results = "~/Documents/GitHub/smoke_PM_prediction/work/09_test_HYSPLIT/results/"

library(rgeos)
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(scales)

#-------------------------------------------------------------------------------
# Compare Smoke Day According to Plumes and Trajectories
# Written by Jessica
# Last edited December 2021
# 
# What % of high anomalous PM station-days/high anomalous AOT grid cell-days that
# are smoke day according to plumes are also smoke day according to trajectories?
# Vice-versa?
# Subset to dates where HYSPLIT trajectory point counts are not missing.
# Subset to dates not missing smoke data.
# 
# For specific dates, plot high anom AOT or high anom PM cells and color by 
# smoke day according to plume and/or trajectories.
#-------------------------------------------------------------------------------
# Set thresholds
min_num_traj_points = 3 # minimum traj point count if HYSPLIT points
min_pm25_anom = 50 # from previous evaluations
min_aot_anom = 0.1 # 5th percentile value of anom AOT among station-days w/ anom PM > 50 and plume overhead

# Get time period and trajectory duration
duration_days = 6
start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(format(all_dates, "%Y_%m"))

# Get dates where plume data and trajectory point counts not missing
na_dates_plume = readRDS(paste0(path_dropbox, "smoke/smoke_dates_not_online.rds"))
na_dates_traj = c(format(seq.Date(ymd(start_date), ymd("20060418"), by = "day"), "%Y%m%d"),
                  readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_not_online.rds")),
                  readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_gis_corrupt.rds")),
                  readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_oddly_empty.rds"))) %>% 
  lapply(function(x) format(seq.Date(ymd(x), ymd(x) + days(duration_days), by = "day"), "%Y%m%d")) %>% 
  unlist() %>% 
  unique() %>% 
  sort()
na_dates = sort(unique(c(na_dates_plume, na_dates_traj)))
nna_dates = setdiff(all_dates_str, na_dates)

# Read in anomalous PM
anom_pm = readRDS(paste0(path_project, "data/epa_pm25_anom_all_days.rds")) %>% 
  mutate(date = format(date, "%Y%m%d")) %>% 
  filter(date %in% nna_dates)

counts_stations = vector("list", length(year_months))
counts_grid = vector("list", length(year_months))
start_time = get_start_time()
for (m in 1:length(year_months)) {
  year_month = year_months[m]
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  nna_dates_m = grep(paste0("^", gsub("_", "", year_month)), nna_dates, value = T)
  if (length(nna_dates_m) == 0) next
  
  # Subset anomalous PM for the month
  anom_pm_m = anom_pm %>% 
    filter(date %in% nna_dates_m) %>% 
    select(epa_id = id, grid_id_10km, date, pm25_anom)
  
  # Read in anomalous AOT
  anom_aot_m = readRDS(paste0(path_project, "data/3_intermediate/aot_anom_all_days_", m_str, ".rds")) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    filter(date %in% nna_dates_m)
  
  # Read in smoke plumes
  plume_m = readRDS(paste0(path_project, "data/smoke_days/grid_smoke_day_", year_month, ".rds")) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    filter(date %in% nna_dates_m) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_plume = smoke_day)
  
  # Read in trajectory point counts
  traj_m = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_2006-2020/grid_trajectory_points_", year_month, ".rds")) %>% 
    filter(date %in% nna_dates_m) %>% 
    mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_traj)
  
  # Combine grids
  stations_m = reduce(list(anom_pm_m, plume_m, traj_m), left_join, by = c("grid_id_10km", "date"))
  grid_m = reduce(list(anom_aot_m, plume_m, traj_m), left_join, by = c("grid_id_10km", "date"))
  
  # Get counts
  counts_stations[[m]] = stations_m %>% 
    filter(pm25_anom > min_pm25_anom) %>% 
    count(date, smoke_day_plume, smoke_day_traj)
  counts_grid[[m]] = grid_m %>% 
    filter(aot_anom > min_aot_anom) %>% 
    count(date, smoke_day_plume, smoke_day_traj)
}
print_time(start_time)
# saveRDS(counts_stations, "~/Desktop/tmp/counts_stations_day.rds")
# saveRDS(counts_grid, "~/Desktop/tmp/counts_grid_day.rds")
counts_stations = readRDS("~/Desktop/tmp/counts_stations_day.rds")
counts_grid = readRDS("~/Desktop/tmp/counts_grid_day.rds")

# Aggregate counts
counts_stations_day = bind_rows(counts_stations)
counts_stations = counts_stations_day %>% 
  filter((smoke_day_plume == 1) | (smoke_day_traj == 1)) %>% 
  group_by(smoke_day_plume, smoke_day_traj) %>% 
  summarize(n = sum(n)) %>% 
  ungroup()
counts_grid_day = bind_rows(counts_grid)
counts_grid = counts_grid_day %>% 
  bind_rows() %>% 
  filter((smoke_day_plume == 1) | (smoke_day_traj == 1)) %>% 
  group_by(smoke_day_plume, smoke_day_traj) %>% 
  summarize(n = sum(n)) %>% 
  ungroup()

# Calculate percents
count_stations_smoke_day_plume = counts_stations %>% filter(smoke_day_plume == 1) %>% pull(n) %>% sum()
count_stations_smoke_day_traj = counts_stations %>% filter(smoke_day_traj == 1) %>% pull(n) %>% sum()
count_stations_smoke_day_plume_traj = counts_stations %>% filter(smoke_day_plume == 1, smoke_day_traj == 1) %>% pull(n)
perc_station_smoke_day_traj_of_plume = count_stations_smoke_day_plume_traj/count_stations_smoke_day_plume
perc_station_smoke_day_plume_of_traj = count_stations_smoke_day_plume_traj/count_stations_smoke_day_traj

count_grid_smoke_day_plume = counts_grid %>% filter(smoke_day_plume == 1) %>% pull(n) %>% sum()
count_grid_smoke_day_traj = counts_grid %>% filter(smoke_day_traj == 1) %>% pull(n) %>% sum()
count_grid_smoke_day_plume_traj = counts_grid %>% filter(smoke_day_plume == 1, smoke_day_traj == 1) %>% pull(n)
perc_grid_smoke_day_traj_of_plume = count_grid_smoke_day_plume_traj/count_grid_smoke_day_plume
perc_grid_smoke_day_plume_of_traj = count_grid_smoke_day_plume_traj/count_grid_smoke_day_traj

print(paste("% high anom. PM station-days SD by plumes also SD by trajectories:", round(perc_station_smoke_day_traj_of_plume*100, 2), "%"))
print(paste("% high anom. PM station-days SD by trajectories also SD by plumes:", round(perc_station_smoke_day_plume_of_traj*100, 2), "%"))
print(paste("% high anom. AOT grid cell-days SD by plumes also SD by trajectories:", round(perc_grid_smoke_day_traj_of_plume*100, 2), "%"))
print(paste("% high anom. AOT grid cell-days SD by trajectories also SD by plumes:", round(perc_grid_smoke_day_plume_of_traj*100, 2), "%"))

#-------------------------------------------------------------------------------
# Set dates of interest
# chosen_dates = c("20150701", "20200913", "20170429", "20170905", "20200916")
chosen_dates = c("20150701", "20200913", "20170905", "20200916")
stopifnot(all(chosen_dates %in% nna_dates))

date_max_agree_stations = counts_stations_day %>% 
  filter(smoke_day_plume == 1, smoke_day_traj == 1) %>% 
  slice_max(n) %>% 
  pull(date)
date_max_disagree_stations = counts_stations_day %>% 
  filter(((smoke_day_plume == 1) | (smoke_day_traj == 1)) & !((smoke_day_plume == 1) & smoke_day_traj == 1)) %>% 
  slice_max(n) %>% 
  pull(date)
# proportions = data.frame(expand.grid(unique(counts_stations_day$date), 
#                                      unique(counts_stations_day$smoke_day_plume), 
#                                      unique(counts_stations_day$smoke_day_traj))) %>% 
#   rename(date = Var1, smoke_day_plume = Var2, smoke_day_traj = Var3) %>% 
#   mutate(date = as.character(date))
# proportions = counts_stations_day %>% 
#   full_join(proportions) %>% 
#   replace_na(list(n = 0)) %>% 
#   group_by(date) %>% 
#   mutate(prop = n/sum(n)) %>% 
#   ungroup()
# date_max_agree_proportion_stations = proportions %>% 
#   filter(smoke_day_plume == 1, smoke_day_traj == 1) %>% 
#   slice_max(prop) %>% 
#   pull(date)
# date_max_disagree_proportion_stations = proportions %>% 
#   filter(((smoke_day_plume == 1) | (smoke_day_traj == 1)) & !((smoke_day_plume == 1) & smoke_day_traj == 1)) %>% 
#   slice_max(prop) %>% 
#   pull(date)

date_max_agree_grid = counts_grid_day %>% 
  filter(smoke_day_plume == 1, smoke_day_traj == 1) %>% 
  slice_max(n) %>% 
  pull(date)
date_max_disagree_grid = counts_grid_day %>% 
  filter(((smoke_day_plume == 1) | (smoke_day_traj == 1)) & !((smoke_day_plume == 1) & smoke_day_traj == 1)) %>% 
  slice_max(n) %>% 
  pull(date)
# proportions = data.frame(expand.grid(unique(counts_grid_day$date), 
#                                      unique(counts_grid_day$smoke_day_plume), 
#                                      unique(counts_grid_day$smoke_day_traj))) %>% 
#   rename(date = Var1, smoke_day_plume = Var2, smoke_day_traj = Var3) %>% 
#   mutate(date = as.character(date))
# proportions = counts_grid_day %>% 
#   full_join(proportions) %>% 
#   replace_na(list(n = 0)) %>% 
#   group_by(date) %>% 
#   mutate(prop = n/sum(n)) %>% 
#   ungroup()
# date_max_agree_proportion_grid = proportions %>% 
#   filter(smoke_day_plume == 1, smoke_day_traj == 1) %>% 
#   slice_max(prop) %>% 
#   pull(date)
# date_max_disagree_proportion_grid = proportions %>% 
#   filter(((smoke_day_plume == 1) | (smoke_day_traj == 1)) & !((smoke_day_plume == 1) & smoke_day_traj == 1)) %>% 
#   slice_max(prop) %>% 
#   pull(date)

other_chosen_dates = c("20070518", "20181111", "20110930", "20080709", "20170828", "20071022", "20120710", "20160325", "20130817", "20140718", "20150818")
chosen_dates = sort(unique(c(date_max_agree_stations, date_max_disagree_stations, 
                             date_max_agree_grid, date_max_disagree_grid,
                             chosen_dates, other_chosen_dates)))

# Read in project grid
project_grid = readRDS(paste0("~/Documents/GitHub/purple-air-infiltration/", "data/grid.RDS")) %>% 
  gBuffer(byid = T, width = 5000, capStyle = "SQUARE") %>% 
  st_as_sf() %>% 
  st_transform(st_crs(readRDS(paste0(path_dropbox, "smoke/smoke_plumes_sfdf.rds")))) %>% 
  select(grid_id_10km = ID)

# Get daily grids
start_time = get_start_time()
df = chosen_dates %>% 
  map_dfr(function(x) {
    y_str = substr(x, 1, 4)
    m_str = substr(x, 5, 6)
    year_month = paste0(y_str, "_", m_str)
    
    # Average across stations within a grid cell
    anom_pm_d = anom_pm %>% 
      filter(date == x) %>% 
      group_by(grid_id_10km, date) %>% 
      summarize(pm25_anom = mean(pm25_anom, na.rm = T)) %>% 
      ungroup()
    
    anom_aot_d = readRDS(paste0(path_project, "data/3_intermediate/aot_anom_all_days_", m_str, ".rds")) %>% 
      mutate(date = format(date, "%Y%m%d")) %>%  
      filter(date == x)
    
    plume_d = readRDS(paste0(path_project, "data/smoke_days/grid_smoke_day_", year_month, ".rds")) %>% 
      mutate(date = format(date, "%Y%m%d")) %>% 
      filter(date == x) %>% 
      select(grid_id_10km = id_grid, date, smoke_day_plume = smoke_day)
    
    traj_d = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_2006-2020/grid_trajectory_points_", year_month, ".rds")) %>% 
      filter(date == x) %>% 
      select(grid_id_10km = id_grid, date, num_traj_points_height_1)
    
    grid_d = reduce(list(anom_pm_d, anom_aot_d, plume_d, traj_d), full_join, by = c("grid_id_10km", "date"))
    return(grid_d)
  })
print_time(start_time)
df = project_grid %>% full_join(df, by = "grid_id_10km")

# Plot
white = rgb(1,1,1)
red = rgb(1,0,0)
yellow = rgb(1,1,0)
blue = rgb(0,0,1)
purple = rgb(138/255,43/255,226/255)
df_color = df %>% 
  mutate(high_likely_high = ifelse(is.na(pm25_anom), aot_anom > min_aot_anom, pm25_anom > min_pm25_anom),
         smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0),
         color = ifelse(high_likely_high, yellow, white),
         color = ifelse(high_likely_high & (smoke_day_plume == 1), red, color),
         color = ifelse(high_likely_high & (smoke_day_traj == 1), blue, color),
         color = ifelse(high_likely_high & (smoke_day_plume == 1) & (smoke_day_traj == 1), purple, color))
p = ggplot(df_color) + 
  geom_sf(aes(fill = color), color = NA) + 
  facet_wrap(vars(date), nrow = 3, ncol = 6) +
  scale_fill_identity()

ggsave(paste0(path_results, "map_smoke_day_plume_trajectory.png"), plot = p, width = 24, height = 8)

#-------------------------------------------------------------------------------
# Do trajectories tell us the presence of smoke when plumes are undetected due 
# to interfering cloud cover?
min_perc_aod_missing = 0.75
aod_missing_files = list.files(paste0(path_project, "data/2_from_EE/maiac_AODmissings/"))
aod_missing_years = sapply(aod_missing_files, function(x) substr(strsplit(x, "_")[[1]][6], 1, 4))

# Takes ~1 minute per month
anom_pm_SDplume = vector("list", length(year_months))
anom_pm_notSDplume_SDtraj_cloudy = vector("list", length(year_months))
start_time = get_start_time()
for (m in 1:length(year_months)) {
  year_month = year_months[m]
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  nna_dates_m = grep(paste0("^", gsub("_", "", year_month)), nna_dates, value = T)
  if (length(nna_dates_m) == 0) next
  
  # Subset anomalous PM for the month
  anom_pm_m = anom_pm %>% 
    filter(date %in% nna_dates_m) %>% 
    select(epa_id = id, grid_id_10km, date, pm25_anom)

  # Read in smoke plumes
  plume_m = readRDS(paste0(path_project, "data/smoke_days/grid_smoke_day_", year_month, ".rds")) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    filter(date %in% nna_dates_m) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_plume = smoke_day)
  
  # Read in trajectory point counts
  traj_m = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_2006-2020/grid_trajectory_points_", year_month, ".rds")) %>% 
    filter(date %in% nna_dates_m) %>% 
    mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_traj)

  # Read in percent missing AOD
  aod_missing_m = aod_missing_files[which(aod_missing_years == y_str)]
  aod_missing_m = paste0(path_project, "data/2_from_EE/maiac_AODmissings/", aod_missing_m)
  aod_missing_m = aod_missing_m %>% 
    map_dfr(read.csv, colClasses = c(start_date = "character")) %>% 
    select(grid_id_10km = ID, date = start_date, perc_aod_missing = mean) %>% 
    filter(date %in% nna_dates_m)
    
  # Combine grids
  stations_m = reduce(list(anom_pm_m, plume_m, traj_m, aod_missing_m), left_join, by = c("grid_id_10km", "date"))

  # Get counts
  anom_pm_SDplume[[m]] = stations_m %>% 
    filter(smoke_day_plume == 1) %>% 
    pull(pm25_anom)
  anom_pm_notSDplume_SDtraj_cloudy[[m]] = stations_m %>% 
    filter(smoke_day_plume == 0,
           smoke_day_traj == 1, 
           perc_aod_missing > min_perc_aod_missing) %>% 
    pull(pm25_anom)
}
print_time(start_time)
saveRDS(anom_pm_SDplume, paste0(path_results, "anom_pm_SDplume.rds"))
saveRDS(anom_pm_notSDplume_SDtraj_cloudy, paste0(path_results, "anom_pm_notSDplume_SDtraj_cloudy.rds"))

anom_pm_SDplume = unlist(anom_pm_SDplume)
anom_pm_notSDplume_SDtraj_cloudy = unlist(anom_pm_notSDplume_SDtraj_cloudy)

# Plot distribution of anomalous PM on station-days that are smoke day according
# to trajectory point count, do not have plume overhead, and are cloudy
# ax = pretty(range(c(anom_pm_SDplume, anom_pm_notSDplume_SDtraj_cloudy)), n = 100)
pdf(paste0(path_results, "anom_pm_SD.pdf"), width = 13, height = 6)
hist(anom_pm_SDplume, col = alpha("red", 0.5))#, breaks = ax)
hist(anom_pm_notSDplume_SDtraj_cloudy, add = T, col = alpha("blue", 0.5))#, breaks = ax)
dev.off()

summary(anom_pm_SDplume)
summary(anom_pm_notSDplume_SDtraj_cloudy)

quantile(anom_pm_SDplume, seq(0, 1, 0.01))
quantile(anom_pm_notSDplume_SDtraj_cloudy, seq(0, 1, 0.01))
