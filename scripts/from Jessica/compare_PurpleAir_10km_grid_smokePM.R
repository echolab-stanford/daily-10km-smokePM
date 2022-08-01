path_dropbox = "~/BurkeLab Dropbox/Data/"
path_project = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction/"
path_github = "~/Documents/GitHub/smoke_PM_prediction/"

library(stringr)
library(lubridate)
library(sf)
library(tigris)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)

#-------------------------------------------------------------------------------
# Compare Smoke PM2.5 at PurpleAir Monitors and 10 km Grid
# Written by Jessica
# Last edited March 2022
#-------------------------------------------------------------------------------
# Choose minimum number of hourly observations
min_hours = 1

#-------------------------------------------------------------------------------
#### Process PurpleAir PM2.5 to monitor-day ####
# Original hourly data file too large to load, so we run this step on cluster 
# and load from intermediate saved daily data file

# # Load clean PurpleAir outdoor monitor data
# # Have 79837229 observations
# purpleair = readRDS(file.path(path_project, "../pollution_infiltration/data/outdoor_monitor_data_clean_part1.rds")) %>%
#   mutate(
#     # bottom code at 0
#     pm25_out = pmax(pm25_out, 0),
#     # top code any values 500-1000 at 500
#     pm25_out = ifelse(pm25_out >= 500 & pm25_out <= 1000, 500, pm25_out),
#     # set any values > 1000 to NA
#     pm25_out = ifelse(pm25_out > 1000, NA, pm25_out)
#   ) %>%
#   # Drop missing hourly observations
#   # Have 58312646 observations (73.0%)
#   drop_na(pm25_out) %>%
#   # Filter to observations over time period for which we have smoke days
#   # Have 58279714 observations (99.9%)
#   # Have 6618 monitors
#   filter(year >= 2006, year <= 2020) %>%
#   select(purpleair_id = ID_out, lon = Lon_out, lat = Lat_out, year, month, day, hour, pm25 = pm25_out)
# 
# # Aggregate to daily level
# purpleair = purpleair %>%
#   # Have 2516222 observations
#   group_by(purpleair_id, lon, lat, year, month, day) %>%
#   summarize(pm25 = mean(pm25),
#             num_hourly_obs = n()) %>%
#   ungroup() %>%
#   # Filter to days with minimum number of hourly observations
#   # Have 2516222 observations (100%)
#   # Have 6618 monitors (100%)
#   filter(num_hourly_obs >= min_hours) %>%
#   group_by(purpleair_id) %>%
#   mutate(num_daily_obs = n()) %>%
#   ungroup()
# 
# saveRDS(purpleair, file.path(path_project, "outdoor_monitor_data_clean_part1_daily.rds"))

purpleair = readRDS(file.path(path_project, "outdoor_monitor_data_clean_part1_daily.rds"))

#-------------------------------------------------------------------------------
#### Match PurpleAir monitors and 10 km grid cells ####
# Load PurpleAir monitor locations
purpleair_loc = readRDS(file.path(path_project, "../pollution_infiltration/data/outdoor_monitor_locs.rds")) %>% 
  select(purpleair_id = ID_out, lon = Lon_out, lat = Lat_out) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Load 10 km grid
grid_10km = read_sf(file.path(path_project, "data/1_grids/10km_grid/")) %>% 
  select(grid_id_10km = ID)

# Match each PurpleAir monitor to the overlapping 10 km grid cell
o = purpleair_loc %>% 
  st_transform(st_crs(grid_10km)) %>% 
  st_join(grid_10km) %>% 
  st_drop_geometry()

#-------------------------------------------------------------------------------
#### Classify monitor-days as smoke day or non-smoke day ####
# Load smoke days
smoke_days = readRDS(file.path(path_project, "data/3_intermediate/all_smoke_days_incl_cloudy.rds")) %>% 
  select(grid_id_10km, date, smoke_day)

# Join smoke day classification to PM2.5 observations by grid cell ID and date
purpleair = purpleair %>% 
  left_join(o, by = "purpleair_id") %>% 
  mutate(date = ymd(paste(year, month, day))) %>% 
  left_join(smoke_days, by = c("grid_id_10km", "date")) %>% 
  replace_na(list(smoke_day = 0))

#-------------------------------------------------------------------------------
#### Calculate smoke PM observations ####
# For each monitor-month, limit to non-smoke day observations at the monitor in 
# that calendar month in the three-year window
background_pm25 = purpleair %>% 
  filter(smoke_day == 0) %>% 
  group_by(purpleair_id, month, year) %>% 
  summarize(pm25 = list(pm25)) %>% 
  rowwise() %>% 
  mutate(nobs = length(pm25)) %>% 
  ungroup() %>% 
  arrange(purpleair_id, month, year) %>% 
  group_by(purpleair_id, month) %>% 
  mutate(pm25_lead = lead(pm25, n = 1, default = list(NA)),
         pm25_lag = lag(pm25, n = 1, default = list(NA)),
         nobs_lead = lead(nobs, n = 1, default = 0),
         nobs_lag = lag(nobs, n = 1, default = 0)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(pm25_3yr = list(c(pm25, pm25_lag, pm25_lead)),
         nobs_3yr = nobs + nobs_lead + nobs_lag) %>% 
  rowwise() %>% 
  # Obtain background PM2.5 by aggregating (taking the median)
  mutate(pm25_med_3yr = median(unlist(pm25_3yr), na.rm = T)) %>% 
  select(purpleair_id, year, month, pm25_med_3yr, nobs_3yr, nobs, nobs_lead, nobs_lag) %>% 
  filter(nobs > 0, (nobs_lead > 0 | nobs_lag > 0))

# For each monitor-smoke day, match to background PM2.5 for the monitor-month
purpleair = purpleair %>% 
  # Have 325490 observations (12.9%)
  # Have 5644 monitors (85.3%)
  filter(smoke_day == 1) %>% 
  # Drop observations with insufficient data for calculating background PM2.5
  # Have 175312 observations (53.9%)
  # Have 2550 monitors (45.2%)
  inner_join(background_pm25, by = c("purpleair_id", "year", "month")) %>% 
  # Subtract background PM2.5 from total PM2.5
  mutate(smokePM = pmax(ifelse(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0))

#-------------------------------------------------------------------------------
#### Match to smoke PM predictions ####
# Load 10 km grid smoke PM predictions
preds = readRDS(file.path(path_project, "output/smokePM_predictions_2006_2020.rds")) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

# Join smoke PM predictions to observations by 10 km grid cell ID and date
df = purpleair %>% 
  left_join(preds, by = c("grid_id_10km", "date"))

#-------------------------------------------------------------------------------
#### Compare smoke PM predictions and observations ####
# Plot distribution of PurpleAir monitor-smoke days over time
p_dates = ggplot(df, aes(x = date)) + 
  # Each bin is approximately 2 weeks large
  geom_histogram(bins = ((as.integer(max(df$date) - min(df$date)) + 1) %/% 14) + 1, 
                 closed = "left") + 
  # Enforce limits on date range
  scale_x_date(limits = c(min(df$date) - days(1), max(df$date) + days(1)), 
               oob = squish) + 
  theme_classic() + 
  labs(title = "b) temporal distribution of PurpleAir data",
       y = "count of monitor-smoke days") + 
  theme(plot.title = element_text(face = "bold", hjust = -0.22))

# Count smoke days at each monitor
df_sf = df %>% 
  group_by(lon, lat) %>% 
  summarize(num_smoke_days = n()) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
conus = states() %>% 
  filter(!(NAME %in% c("Alaska", 
                       "American Samoa",
                       "Guam",
                       "Hawaii",
                       "Commonwealth of the Northern Mariana Islands",
                       "Puerto Rico",
                       "United States Virgin Islands")))

# Plot map showing spatial distribution of PurpleAir monitors with smoke days
p_map = ggplot() + 
  geom_sf(data = conus) +
  geom_sf(data = df_sf, mapping = aes(color = num_smoke_days), alpha = 0.5) +
  theme_void() +
  labs(title = "c) spatial distribution of PurpleAir data",
       color = "count of smoke days") + 
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.023)) + 
  guides(color = guide_colorbar(title.vjust = 0.8))

# Plot scatter and simple linear regression of predictions on observations
p_comparison = ggplot(df, aes(x = smokePM, y = smokePM_pred)) + 
  geom_bin2d(bins = 70) + 
  geom_abline(intercept = 0, slope = 1, color = "grey30") +
  theme_classic() +
  scale_fill_continuous(type = "viridis",
                        trans = "log", 
                        breaks = c(1, 10, 100, 1000, 10000)) + 
  scale_x_continuous(trans = "pseudo_log", 
                     breaks = c(0, 1, 5, 10, 50, 100, 500),
                     expand = c(0, 0)) + 
  scale_y_continuous(trans = "pseudo_log", 
                     breaks = c(0, 1, 5, 10, 50, 100, 500),
                     expand = c(0, 0)) + 
  labs(title = "a) predicted and observed smoke pollution",
       x = expression(observed~smokePM[2.5]~(PurpleAir)),
       y = expression(predicted~smokePM[2.5]),
       caption = paste("count of monitors =", 
                       prettyNum(length(unique(df$purpleair_id)), 
                                 big.mark = ",", scientific = F))) + 
  theme(plot.title = element_text(face = "bold", hjust = -0.3))

# Compose into one figure
p = grid.arrange(p_comparison, p_dates, p_map,
                 layout_matrix = matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 
                                          1, 1, 1, 1, 3, 3, 3, 3,  
                                          1, 1, 1, 1, 3, 3, 3, 3),
                                        nrow = 3, byrow = T))
ggsave(file.path(path_github, "figures/PurpleAir_comparison.png"), 
       plot = p, width = 14, height = 8)
