path_dropbox = "~/BurkeLab Dropbox/Data"
path_project = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction"
path_github = "~/Documents/GitHub/smoke_PM_prediction"
path_figures = file.path(path_github, "figures")

library(lubridate)
library(stringr)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(caret)
library(ggplot2)
library(ggrepel)
library(geomtextpath)
library(ggpubr)
library(ggsn)
library(foreach)
library(doParallel)

num_cores = 4

# ------------------------------------------------------------------------------
# How to update 2021 and potentially further years' smokePM predictions?
# Written by: Jessica Li
# Last edited: August 2022
# 
# HYSPLIT points are a "subset" of fire points that are smoke-producing, so 
# let's first examine how many fire points we have per HYSPLIT point and how far 
# off their coordinates are (+ why if possible), next impute HYSPLIT points from 
# fire points by subsetting to fire points underneath smoke plumes or with 
# FRP above some threshold when underneath clouds, and then evaluate this 
# method of imputation.
# ------------------------------------------------------------------------------
# Set dates
start_date = "20060419"
end_date = "20220709"

################################################################################
# Load and clean HMS data
################################################################################
#### Load satellite dates ####
satellites  = read.csv(file.path(path_project, "impute_HYSPLIT_points", "satellite_dates.csv"))
d = file.info(file.path(path_project, "impute_HYSPLIT_points", "satellite_dates.csv"))$mtime %>% 
  format("%d%m%Y")
satellites = satellites %>% 
  mutate(in_use = (End.Date == "In use"), 
         End.Date = ifelse(in_use, d, End.Date), 
         across(c(Start.Date, End.Date), dmy), 
         # Helpful proportions for time series plots
         y = 1/nrow(.)*row_number())

#### Load fire points ####
fire = readRDS(file.path(path_dropbox, "fire/txt/hms_fires_20030401-20220711.RDS"))
crs_use = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Set bounding box
# Adapted from https://www.ospo.noaa.gov/Products/land/hms.html#about with generous margins
north = 72 + 20
south = 14.6 - 20
east = -50 + 40
west = -170 - 40

# Clean, combine, and crop
clean_fire = function(x) {
  x = x %>% 
    filter(Lon >= west, Lon <= east, Lat >= south, Lat <= north) %>% 
    mutate(ID = row_number(),
           Time = str_pad(Time, 4, "left", 0),
           across(c(Satellite, Method.of.Detect), trimws))
  return(x)
}
if (!file.exists(file.path(path_project, "impute_HYSPLIT_points", "fire_points.rds"))) {
  fire_points = fire %>%
    map_dfr(clean_fire) %>%
    st_as_sf(coords = c("Lon", "Lat"), remove = F, crs = crs_use) %>%
    filter(Time <= "2400", substr(Time, 3, 4) <= "59")
  # Note: Time is formatted HHMM
  
  saveRDS(fire_points, file.path(path_project, "impute_HYSPLIT_points", "fire_points.rds"))
} else {
  fire_points = readRDS(file.path(path_project, "impute_HYSPLIT_points", "fire_points.rds"))
}
rm(fire)

#### Load HYSPLIT points ####
# First HYSPLIT points with duration are from Apr 19, 2006
hysplit1 = readRDS(file.path(path_dropbox, "hms_hysplit/hms_hysplit_points_20060419-20201231.rds"))
# Most of 2021 was not in the archive, so starts at Oct 21, 2021
hysplit2 = readRDS(file.path(path_dropbox, "hms_hysplit/hms_hysplit_points_20211021-20220709.rds"))

# # Combine into sf data frame
if (!file.exists(file.path(path_project, "impute_HYSPLIT_points", "hysplit_points.rds"))) {
  hysplit_points = bind_rows(hysplit1 %>% map_dfr(st_drop_geometry),
                             hysplit2 %>% map_dfr(st_drop_geometry)) %>%
    filter(Lon >= west, Lon <= east, Lat >= south, Lat <= north) %>%
    # There are 9 rows w/ Duration = "0000"
    filter(Duration.HHMM > "0000") %>%
    st_as_sf(coords = c("Lon", "Lat"), remove = F, crs = crs_use) %>%
    filter(Time.HHMM <= "2400", substr(Time.HHMM, 3, 4) <= "59")
  # Note: ID is only unique within each date
  
  saveRDS(hysplit_points, file.path(path_project, "impute_HYSPLIT_points", "hysplit_points.rds"))
} else {
  hysplit_points = readRDS(file.path(path_project, "impute_HYSPLIT_points", "hysplit_points.rds"))
}
rm(hysplit1, hysplit2)

#### Load smoke plumes ####
smoke_plumes = readRDS(file.path(path_dropbox, "smoke/smoke_plumes_sfdf_20050805_20220711.RDS"))

#### Limit to fire, HYSPLIT, and smoke data on the same dates
common_dates = gsub("-", "", seq.Date(ymd(start_date), ymd(end_date), by = "day"))
common_dates = common_dates[(common_dates %in% as.character(fire_points$date)) & 
                              (common_dates %in% hysplit_points$Date.YYYYmmdd) & 
                              (common_dates %in% smoke_plumes$date)]
fire_points = fire_points %>% mutate(date = as.character(date)) %>% filter(date %in% common_dates)
hysplit_points = hysplit_points %>% filter(Date.YYYYmmdd %in% common_dates)
smoke_plumes = smoke_plumes %>% filter(date %in% common_dates)

# consider what's happening when you don't have at least one of fire, hysplit, 
# or smoke?

################################################################################
# How many fire points, HYSPLIT points, and smoke plumes are there each day?
################################################################################
#### Fire points ####
# Total number of fire points
# 13802410
nrow(fire_points)

# Approximate number of fire points each day
# 2517.31
nrow(fire_points)/length(unique(fire_points$date))

# Distribution of number of fire points each day
fire_points %>% 
  st_drop_geometry() %>% 
  count(date) %>% 
  {ggplot(.) + 
      geom_histogram(aes(n)) + 
      geom_vline(aes(xintercept = mean(n)), color = "green4") + 
      geom_vline(aes(xintercept = median(n)), color = "blue") + 
      geom_text(aes(mean(n) + max(n)/10, 2600, label = sprintf("mean = %s", round(mean(n)))), color = "green4") + 
      geom_text(aes(median(n) + max(n)/10, 2400, label = sprintf("median = %s", round(median(n)))), color = "blue") + 
      theme_light()}
ggsave(file.path(path_figures, "fire_points_distribution.png"), 
       width = 8, height = 4)

# Number of fire points each day as time series
fire_points %>% 
  st_drop_geometry() %>% 
  count(date) %>% 
  {ggplot(.) + 
      geom_line(aes(ymd(date), n), size = 0.2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$n)/-2, 
                           xend = End.Date, yend = y*max(.$n)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(!in_use),
                       size = 2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$n)/-2, 
                           xend = End.Date, yend = y*max(.$n)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(in_use),
                       size = 2,
                       arrow = arrow(length = unit(0.05, "inches"))) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      coord_cartesian(xlim = c(ymd(start_date), NA)) + 
      theme_light() + 
      theme(legend.position = "none")}
ggsave(file.path(path_figures, "fire_points_time_series.png"), 
       width = 8, height = 4)

#### HYSPLIT points ####
# Total number of HYSPLIT points
# 1085337
nrow(hysplit_points)

# Approximate number of HYSPLIT points each day
# 197.9458
nrow(hysplit_points)/length(unique(hysplit_points$Date.YYYYmmdd))

# Distribution of number of HYSPLIT points each day
hysplit_points %>% 
  st_drop_geometry() %>% 
  count(Date.YYYYmmdd) %>% 
  {ggplot(.) + 
      geom_histogram(aes(n)) + 
      geom_vline(aes(xintercept = mean(n)), color = "green4") + 
      geom_vline(aes(xintercept = median(n)), color = "blue") + 
      geom_text(aes(mean(n) + max(n)/10, 2600, label = sprintf("mean = %s", round(mean(n)))), color = "green4") + 
      geom_text(aes(median(n) + max(n)/10, 2400, label = sprintf("median = %s", round(median(n)))), color = "blue") + 
      theme_light()}
ggsave(file.path(path_figures, "hysplit_points_distribution.png"), 
       width = 8, height = 4)

# Number of HYSPLIT points each day as time series
hysplit_points %>% 
  st_drop_geometry() %>% 
  count(Date.YYYYmmdd) %>% 
  {ggplot(.) + 
      geom_line(aes(ymd(Date.YYYYmmdd), n), size = 0.2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$n)/-2, 
                           xend = End.Date, yend = y*max(.$n)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(!in_use),
                       size = 2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$n)/-2, 
                           xend = End.Date, yend = y*max(.$n)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(in_use),
                       size = 2,
                       arrow = arrow(length = unit(0.05, "inches"))) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      coord_cartesian(xlim = c(ymd(start_date), NA)) + 
      theme_light() + 
      theme(legend.position = "none")}
ggsave(file.path(path_figures, "hysplit_points_time_series.png"), 
       width = 8, height = 4)

#### Smoke plumes ####
# Total number of smoke plumes
# 412046
nrow(smoke_plumes)

# Approximate number of smoke plumes each day
# 75.14974
nrow(smoke_plumes)/length(unique(smoke_plumes$date))

# Distribution of number of smoke plumes each day
smoke_plumes %>% 
  st_drop_geometry() %>% 
  count(date) %>% 
  {ggplot(.) + 
      geom_histogram(aes(n)) + 
      geom_vline(aes(xintercept = mean(n)), color = "green4") + 
      geom_vline(aes(xintercept = median(n)), color = "blue") + 
      geom_text(aes(mean(n) + max(n)/10, 1400, label = sprintf("mean = %s", round(mean(n)))), color = "green4") + 
      geom_text(aes(median(n) + max(n)/10, 1200, label = sprintf("median = %s", round(median(n)))), color = "blue") + 
      theme_light()}
ggsave(file.path(path_figures, "smoke_plumes_distribution.png"), 
       width = 8, height = 4)

# Number of smoke plumes each day as time series
smoke_plumes %>% 
  st_drop_geometry() %>% 
  count(date) %>% 
  {ggplot(.) + 
      geom_line(aes(ymd(date), n), size = 0.2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$n)/-2, 
                           xend = End.Date, yend = y*max(.$n)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(!in_use),
                       size = 2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$n)/-2, 
                           xend = End.Date, yend = y*max(.$n)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(in_use),
                       size = 2,
                       arrow = arrow(length = unit(0.05, "inches"))) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      coord_cartesian(xlim = c(ymd(start_date), NA)) + 
      theme_light() + 
      theme(legend.position = "none")}
ggsave(file.path(path_figures, "smoke_plumes_time_series.png"), 
       width = 8, height = 4)

################################################################################
# Approximately how many fire points are there for each HYSPLIT point?
################################################################################
# Approximate number of fire points per HYSPLIT point
# 12.71717
nrow(fire_points)/nrow(hysplit_points)

# Distribution of approximate number of fire points per HYSPLIT point each day
full_join(
  fire_points %>% 
    st_drop_geometry() %>% 
    count(date) %>% 
    rename(n_fire_points = n),
  hysplit_points %>% 
    st_drop_geometry() %>% 
    count(Date.YYYYmmdd) %>% 
    rename(n_hysplit_points = n), 
  by = c("date" = "Date.YYYYmmdd")
) %>% 
  mutate(fp_per_hp = n_fire_points/n_hysplit_points) %>% 
  {ggplot(.) + 
      geom_histogram(aes(fp_per_hp)) + 
      geom_vline(aes(xintercept = mean(fp_per_hp)), color = "green4") + 
      geom_vline(aes(xintercept = median(fp_per_hp)), color = "blue") + 
      geom_text(aes(mean(fp_per_hp) + max(fp_per_hp)/10, 1400, label = sprintf("mean = %s", round(mean(fp_per_hp)))), color = "green4") + 
      geom_text(aes(median(fp_per_hp) + max(fp_per_hp)/10, 1200, label = sprintf("median = %s", round(median(fp_per_hp)))), color = "blue") + 
      theme_light()}
ggsave(file.path(path_figures, "fire_per_hysplit_distribution.png"), 
       width = 8, height = 4)

# Time series of approximate number of fire points per HYSPLIT point each day
full_join(
  fire_points %>% 
    st_drop_geometry() %>% 
    count(date) %>% 
    rename(n_fire_points = n),
  hysplit_points %>% 
    st_drop_geometry() %>% 
    count(Date.YYYYmmdd) %>% 
    rename(n_hysplit_points = n), 
  by = c("date" = "Date.YYYYmmdd")
) %>% 
  mutate(fp_per_hp = n_fire_points/n_hysplit_points) %>% 
  {ggplot(.) + 
      geom_line(aes(ymd(date), fp_per_hp), size = 0.2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$fp_per_hp)/-2, 
                           xend = End.Date, yend = y*max(.$fp_per_hp)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(!in_use),
                       size = 2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$fp_per_hp)/-2, 
                           xend = End.Date, yend = y*max(.$fp_per_hp)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(in_use),
                       size = 2,
                       arrow = arrow(length = unit(0.05, "inches"))) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      coord_cartesian(xlim = c(ymd(start_date), NA)) + 
      theme_light() + 
      theme(legend.position = "none")}
ggsave(file.path(path_figures, "fire_per_hysplit_time_series.png"), 
       width = 8, height = 4)

# Time series of approximate number of fire points per HYSPLIT point each day as points
full_join(
  fire_points %>% 
    st_drop_geometry() %>% 
    count(date) %>% 
    rename(n_fire_points = n),
  hysplit_points %>% 
    st_drop_geometry() %>% 
    count(Date.YYYYmmdd) %>% 
    rename(n_hysplit_points = n), 
  by = c("date" = "Date.YYYYmmdd")
) %>% 
  mutate(fp_per_hp = n_fire_points/n_hysplit_points) %>% 
  {ggplot(., aes(ymd(date), fp_per_hp)) + 
      geom_point(alpha = 0.5) + 
      geom_label_repel(aes(label = round(fp_per_hp), y = 1100), 
                       filter(., fp_per_hp > 1000), 
                       label.padding = 0.05, 
                       label.size = 0.05, 
                       size = 2, 
                       nudge_y = 100) + 
      geom_textsegment(aes(x = Start.Date, y = y*1000/-2, 
                           xend = End.Date, yend = y*1000/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(!in_use),
                       size = 2) + 
      geom_textsegment(aes(x = Start.Date, y = y*1000/-2, 
                           xend = End.Date, yend = y*1000/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(in_use),
                       size = 2,
                       arrow = arrow(length = unit(0.05, "inches"))) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      coord_cartesian(xlim = c(ymd(start_date), NA), ylim = c(NA, 1000)) + 
      theme_light() + 
      theme(legend.position = "none") + 
      labs(caption = sprintf("median = %s\nmean = %s", round(median(.$fp_per_hp)), round(mean(.$fp_per_hp))))}
ggsave(file.path(path_figures, "fire_per_hysplit_time_series_points.png"), 
       width = 8, height = 4)

# I guess the outliers are just on very cloudy days with lots of fires?
# No it's weird that they all occur in a relatively short span of time. I wonder
# if they had an issue with the system?

# Distribution of number of fire points by approximate number of fire points per 
# HYSPLIT point each day
full_join(
  fire_points %>% 
    st_drop_geometry() %>% 
    count(date) %>% 
    rename(n_fire_points = n),
  hysplit_points %>% 
    st_drop_geometry() %>% 
    count(Date.YYYYmmdd) %>% 
    rename(n_hysplit_points = n), 
  by = c("date" = "Date.YYYYmmdd")
) %>% 
  mutate(fp_per_hp = n_fire_points/n_hysplit_points) %>% 
  {ggplot(.) + 
      geom_density(aes(n_fire_points, 
                       fill = fp_per_hp > 200, 
                       group = fp_per_hp > 200), 
                   alpha = 0.5) + 
      theme_light()}
ggsave(file.path(path_figures, "fire_points_distribution_by_fire_per_hysplit.png"), 
       width = 8, height = 5)

# Distribution of number of HYSPLIT points by approximate number of fire points per 
# HYSPLIT point each day
full_join(
  fire_points %>% 
    st_drop_geometry() %>% 
    count(date) %>% 
    rename(n_fire_points = n),
  hysplit_points %>% 
    st_drop_geometry() %>% 
    count(Date.YYYYmmdd) %>% 
    rename(n_hysplit_points = n), 
  by = c("date" = "Date.YYYYmmdd")
) %>% 
  mutate(fp_per_hp = n_fire_points/n_hysplit_points) %>% 
  {ggplot(.) + 
      geom_density(aes(n_hysplit_points, 
                       fill = fp_per_hp > 200, 
                       group = fp_per_hp > 200), 
                   alpha = 0.5) + 
      theme_light()}
ggsave(file.path(path_figures, "hysplit_points_distribution_by_fire_per_hysplit.png"), 
       width = 8, height = 5)

# Maps for dates with high approximate number of fire points per HYSPLIT point
dates = full_join(
  fire_points %>% 
    st_drop_geometry() %>% 
    count(date) %>% 
    rename(n_fire_points = n),
  hysplit_points %>% 
    st_drop_geometry() %>% 
    count(Date.YYYYmmdd) %>% 
    rename(n_hysplit_points = n), 
  by = c("date" = "Date.YYYYmmdd")
) %>% 
  mutate(fp_per_hp = n_fire_points/n_hysplit_points) %>% 
  filter(fp_per_hp > 1000) %>% 
  pull(date)
for (d in dates) {
  f = fire_points %>% filter(date == d)
  h = hysplit_points %>% filter(Date.YYYYmmdd == d)
  
  # All dates have 0 duplicate fire points
  print(paste(d, "Number of duplicates =", sum(duplicated(f))))
  
  bind_rows(
    f %>% mutate(type = "fire point"), 
    h %>% mutate(type = "hysplit point") %>% rename(date = Date.YYYYmmdd)
  ) %>% 
    {ggplot(.) + 
        geom_sf(aes(color = type)) + 
        labs(title = ymd(d)) + 
        theme(legend.title = element_blank())}
  ggsave(file.path(path_figures, sprintf("map_fire_hysplit_high_ratio_%s.png", d)), 
         width = 8, height = 5)
}

################################################################################
# Are HYSPLIT points really a perfect subset? If not, how far off are HYSPLIT point 
# coordinates from fire point coordinates, and what seems to be the underlying 
# reason for the apparent mismatch/noise?
# 
# They are an intersecting set. Looks like fire points and HYSPLIT points are 
# geolocated on/snapped to the same grid. Some HYSPLIT points are fire points, 
# but some are also not fire points. However, I can only confidently confirm 
# this for prior to ~Nov 2016, when GOES-16 comes online. For 2017 onward, it's 
# very hard to tell if they're on the same grid because HYSPLIT and fire points 
# are so much sparser when you zoom in than they were in previous years.
################################################################################
# Map for date with highest number of HYSPLIT points, zoomed in
d = full_join(
  fire_points %>% 
    st_drop_geometry() %>% 
    count(date) %>% 
    rename(n_fire_points = n),
  hysplit_points %>% 
    st_drop_geometry() %>% 
    count(Date.YYYYmmdd) %>% 
    rename(n_hysplit_points = n), 
  by = c("date" = "Date.YYYYmmdd")
) %>% 
  mutate(fp_per_hp = n_fire_points/n_hysplit_points) %>% 
  slice_max(n_hysplit_points) %>% 
  pull(date)
f = fire_points %>% filter(date == d)
h = hysplit_points %>% filter(Date.YYYYmmdd == d)
ggplot() + 
  geom_sf(aes(color = "fire point"), f, size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h, size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(-119.7, -119.5), ylim = c(44.1, 44.4)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank())
ggsave(file.path(path_figures, sprintf("map_fire_hysplit_zoomed_max_hysplit_%s.png", d)), 
       width = 4, height = 8)

# Maps for dates with highest number of HYSPLIT points each year, zoomed in
dates = full_join(
  fire_points %>% 
    st_drop_geometry() %>% 
    count(date) %>% 
    rename(n_fire_points = n),
  hysplit_points %>% 
    st_drop_geometry() %>% 
    count(Date.YYYYmmdd) %>% 
    rename(n_hysplit_points = n), 
  by = c("date" = "Date.YYYYmmdd")
) %>% 
  mutate(fp_per_hp = n_fire_points/n_hysplit_points,
         year = substr(date, 1, 4)) %>% 
  group_by(year) %>% 
  slice_max(n_hysplit_points) %>% 
  ungroup() %>% 
  pull(date)
dates = c(dates[1:15], "20200915", dates[16:17])
f = fire_points %>% filter(date %in% dates)
h = hysplit_points %>% filter(Date.YYYYmmdd %in% dates)

p = vector("list", length(dates))

i = 1
d = dates[i]
x.min = -120.8
x.max = -119.8
y.min = 48.7
y.max = 49.15
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 2
d = dates[i]
x.min = -115.7
x.max = -115.2
y.min = 44.5
y.max = 44.7
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 3
d = dates[i]
x.min = -114.7
x.max = -114.3
y.min = 60.5
y.max = 60.72
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 4
d = dates[i]
x.min = -103
x.max = -102
y.min = 51.5
y.max = 52
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 5
d = dates[i]
x.min = -123.75
x.max = -123.4
y.min = 53.15
y.max = 53.4
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 6
d = dates[i]
x.min = -96.46
x.max = -96
y.min = 38.8
y.max = 39.1
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 7
d = dates[i]
x.min = -122.75
x.max = -122.52
y.min = 39.26
y.max = 39.35
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 8
d = dates[i]
x.min = -78
x.max = -77
y.min = 52.5
y.max = 52.8
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 9
d = dates[i]
x.min = -123.1
x.max = -122.9
y.min = 41.2
y.max = 41.4
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 10
d = dates[i]
x.min = -116.4
x.max = -116
y.min = 57.7
y.max = 57.95
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 11
d = dates[i]
x.min = -154
x.max = -153.4
y.min = 66.55
y.max = 66.85
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 12
d = dates[i]
x.min = -124.2
x.max = -124.15
y.min = 42.3
y.max = 42.34
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 13
d = dates[i]
x.min = -124.6
x.max = -124.15
y.min = 53.3
y.max = 53.5
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 14
d = dates[i]
x.min = -73.865
x.max = -73.84
y.min = 10.245
y.max = 10.255
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 15
d = dates[i]
x.min = -95
x.max = -85
y.min = 14.5
y.max = 20
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 16
d = dates[i]
x.min = -123.16
x.max = -123.14
y.min = 39.54
y.max = 39.56
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 17
d = dates[i]
x.min = -91.9
x.max = -91.5
y.min = 30.5
y.max = 30.95
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

i = 18
d = dates[i]
x.min = -102.5
x.max = -102.35
y.min = 18.43
y.max = 18.56
p[[i]] = ggplot() + 
  geom_sf(aes(color = "fire point"), f %>% filter(date == d), size = 1.5) + 
  geom_sf(aes(color = "hysplit point"), h %>% filter(Date.YYYYmmdd == d), size = 0.5) + 
  scale_color_manual(values = c("orange", "blue")) + 
  coord_sf(xlim = c(x.min, x.max), ylim = c(y.min, y.max)) + 
  labs(title = ymd(d)) + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) + 
  scale_x_continuous(breaks = seq(x.min, x.max, length.out = 5)) + 
  scale_y_continuous(breaks = seq(y.min, y.max, length.out = 5)) + 
  scalebar(dist = round(sqrt((x.max-x.min)^2 + (y.max-y.min)^2)/10*111), 
           dist_unit = "km", transform = T, 
           x.min = x.min, x.max = x.max, y.min = y.min, y.max = y.max,
           st.size = 3, border.size = 0.1)

for (i in 1:length(p)) {
  ggsave(file.path(path_figures, sprintf("map_fire_hysplit_zoomed_max_hysplit_%s.png", dates[i])),
         plot = p[[i]], width = 6, height = 6)
}
p = ggarrange(plotlist = p, nrow = 3, ncol = 6, common.legend = T)
ggsave(file.path(path_figures, "maps_fire_hysplit_zoomed_max_hysplit_annual.png"), 
       p, width = 24, height = 16)

################################################################################
# How much of FRP data native to fire points data are missing?
################################################################################
# Proportion of fire points missing FRP value
# 0.5088653
fire_points %>% 
  st_drop_geometry() %>% 
  mutate(Fire.RadPower = na_if(Fire.RadPower, -999)) %>% 
  count(is.na(Fire.RadPower)) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(`is.na(Fire.RadPower)`) %>% 
  pull(prop)

# Distribution of proportion of fire points missing FRP value each day
fire_points %>% 
  st_drop_geometry() %>% 
  mutate(Fire.RadPower = na_if(Fire.RadPower, -999)) %>% 
  count(date, is.na(Fire.RadPower)) %>% 
  group_by(date) %>% 
  full_join(data.frame(`is.na(Fire.RadPower)` = c(T, F), check.names = F)) %>% 
  mutate(date = replace_na(date, date[1]),
         n = replace_na(n, 0),
         prop = n/sum(n)) %>% 
  filter(`is.na(Fire.RadPower)`) %>% 
  ungroup() %>% 
  {ggplot(.) + 
      geom_histogram(aes(prop)) + 
      geom_vline(aes(xintercept = mean(prop)), color = "green4") + 
      geom_vline(aes(xintercept = median(prop)), color = "blue") + 
      geom_text(aes(mean(prop) + max(prop)/10, 2800, label = sprintf("mean = %s", round(mean(prop), 2))), color = "green4") + 
      geom_text(aes(median(prop) + max(prop)/10, 3000, label = sprintf("median = %s", round(median(prop), 2))), color = "blue") + 
      theme_light()}
ggsave(file.path(path_figures, "proportion_fire_missing_FRP_distribution.png"), 
       width = 8, height = 4)

# Time series of proportion of fire points missing FRP value each day
fire_points %>% 
  st_drop_geometry() %>% 
  mutate(Fire.RadPower = na_if(Fire.RadPower, -999)) %>% 
  count(date, is.na(Fire.RadPower)) %>% 
  group_by(date) %>% 
  full_join(data.frame(`is.na(Fire.RadPower)` = c(T, F), check.names = F)) %>% 
  mutate(date = replace_na(date, date[1]),
         n = replace_na(n, 0),
         prop = n/sum(n)) %>% 
  filter(`is.na(Fire.RadPower)`) %>% 
  ungroup() %>% 
  {ggplot(.) + 
      geom_line(aes(ymd(date), prop), size = 0.2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$prop)/-2, 
                           xend = End.Date, yend = y*max(.$prop)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(!in_use),
                       size = 2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$prop)/-2, 
                           xend = End.Date, yend = y*max(.$prop)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(in_use),
                       size = 2,
                       arrow = arrow(length = unit(0.05, "inches"))) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      coord_cartesian(xlim = c(ymd(start_date), NA)) + 
      theme_light() + 
      theme(legend.position = "none")}
ggsave(file.path(path_figures, "proportion_fire_missing_FRP_time_series.png"), 
       width = 8, height = 4)

################################################################################
# Try to impute HYSPLIT points from fire points and smoke plumes
################################################################################
# Get fire points that are also HYSPLIT points and HYSPLIT points that are also 
# fire points
# Checking not only for spatial intersection but also the potential differences 
# in counts of fire or HYSPLIT points at a given lonlat if there are multiple 
# points at the same lonlat on the same day. Do not read into potential 
# differences in timing of the fire or HYSPLIT points at the same lonlat, i.e. 
# earlier fire points would be more likely to get matched with a HYSPLIT 
# point simply if the data frame of fire points is ordered chronologically
path_fire_matched = file.path(path_project, "impute_HYSPLIT_points", "fire_points_matched")
path_hysplit_matched = file.path(path_project, "impute_HYSPLIT_points", "hysplit_points_matched")
path_fire_hysplit_matched = file.path(path_project, "impute_HYSPLIT_points", "fire_hysplit_matched")
if (!dir.exists(path_fire_matched)) dir.create(path_fire_matched)
if (!dir.exists(path_hysplit_matched)) dir.create(path_hysplit_matched)
if (!dir.exists(path_fire_hysplit_matched)) dir.create(path_fire_hysplit_matched)
if (length(list.file(path_fire_hysplit_matched)) == 0) {
  start_time = Sys.time()
  for (d in common_dates) {
    print(d)
    
    f = fire_points %>% filter(date == d)
    h = hysplit_points %>% filter(Date.YYYYmmdd == d)
    s = smoke_plumes %>% filter(date == d)
    
    fh = f %>% 
      mutate(num_hysplit = rowSums(st_intersects(., h, sparse = F))) %>% 
      group_by(Lon, Lat) %>% 
      mutate(hysplit = 1*(row_number() <= num_hysplit)) %>% 
      ungroup()
    fhs = fh %>% 
      mutate(smoke = 1*any(rowSums(st_intersects(., s, sparse = F)) > 0),
             pred_hysplit_s = smoke)
    
    hf = h %>% 
      mutate(num_fire = rowSums(st_intersects(., f, sparse = F))) %>% 
      group_by(Lon, Lat) %>% 
      mutate(fire = 1*(row_number() <= num_fire)) %>% 
      ungroup()
    hfs = hf %>% 
      left_join(fhs %>% 
                  st_drop_geometry() %>% 
                  select(Lon, Lat, smoke, starts_with("pred")) %>% 
                  distinct(), 
                by = c("Lon", "Lat")) %>% 
      mutate(across(starts_with("pred"), ~ifelse(fire == 0, 0, .)))
    
    m = bind_rows(
      hfs %>% 
        st_drop_geometry() %>% 
        mutate(ref = 1, hysplit = 1) %>% 
        select(Lon, Lat, date = Date.YYYYmmdd, fire, hysplit, ref, starts_with("pred")), 
      fhs %>% 
        st_drop_geometry() %>% 
        filter(hysplit == 0) %>% 
        mutate(ref = 0, fire = 1) %>% 
        select(Lon, Lat, date, fire, hysplit, ref, starts_with("pred"))
    )
    
    saveRDS(fhs, file.path(path_fire_matched, sprintf("fhs_%s.rds", d)))
    saveRDS(hfs, file.path(path_hysplit_matched, sprintf("hfs_%s.rds", d)))
    saveRDS(m, file.path(path_fire_hysplit_matched, sprintf("m_%s.rds", d)))
  }
  end_time = Sys.time()
  end_time - start_time
}
matches = list.files(file.path(path_fire_hysplit_matched), full.names = T) %>% 
  map_dfr(readRDS)


################################################################################
# Try to impute HYSPLIT points from fire points, smoke plumes, AOD missingness, 
# and FRP
################################################################################
# Load AOD missingness
aod_missingness_y = substr(start_date, 1, 4)
aod_missingness = list.files(
  file.path(path_project, "data", "2_from_EE", "maiac_AODmissings"),
  pattern = paste0("^aod_pctMissing_10km_subgrid_1?[0-9]_", aod_missingness_y, 
                   "[0-1][0-9][0-3][0-9]_", aod_missingness_y, "[0-1][0-9][0-3][0-9]\\.csv$"),
  full.names = T
) %>% 
  map_dfr(read.csv) %>% 
  mutate(start_date = as.character(start_date))

# Set threhsold above which is considered cloudy (as from the paper)
min_perc_aod_missing = 0.75

# Load 10km grid
grid_10km = read_sf(file.path(path_project, "data", "1_grids", "10km_grid"))

# Load FRP data

# Distribution of FRP data within HYSPLIT points etc etc

# Set threshold(s) above which is considered likely smoke-producing


# Get fire points that are also HYSPLIT points and HYSPLIT points that are also 
# fire points
# Checking not only for spatial intersection but also the potential differences 
# in counts of fire or HYSPLIT points at a given lonlat if there are multiple 
# points at the same lonlat on the same day. Do not read into potential 
# differences in timing of the fire or HYSPLIT points at the same lonlat, i.e. 
# earlier fire points would be more likely to get matched with a HYSPLIT 
# point simply if the data frame of fire points is ordered chronologically
path_fire_matched = file.path(path_project, "impute_HYSPLIT_points", "fire_points_matched")
path_hysplit_matched = file.path(path_project, "impute_HYSPLIT_points", "hysplit_points_matched")
path_fire_hysplit_matched = file.path(path_project, "impute_HYSPLIT_points", "fire_hysplit_matched")
if (!dir.exists(path_fire_matched)) dir.create(path_fire_matched)
if (!dir.exists(path_hysplit_matched)) dir.create(path_hysplit_matched)
if (!dir.exists(path_fire_hysplit_matched)) dir.create(path_fire_hysplit_matched)
if (length(list.file(path_fire_hysplit_matched)) == 0) {
  start_time = Sys.time()
  for (d in common_dates) {
    print(d)
    
    y = substr(d, 1, 4)
    f = fire_points %>% filter(date == d)
    h = hysplit_points %>% filter(Date.YYYYmmdd == d)
    s = smoke_plumes %>% filter(date == d)
    if (y != aod_missingness_y) {
      aod_missingness_y = y
      aod_missingness = list.files(
        file.path(path_project, "data", "2_from_EE", "maiac_AODmissings"),
        pattern = paste0("^aod_pctMissing_10km_subgrid_1?[0-9]_", aod_missingness_y, 
                         "[0-1][0-9][0-3][0-9]_", aod_missingness_y, "[0-1][0-9][0-3][0-9]\\.csv$")
      ) %>% 
        map_dfr(read.csv) %>% 
        mutate(start_date = as.character(start_date))
    }
    cl = aod_missingness %>% filter(start_date == d)
    cl = grid_10km %>% right_join(cl, by = "ID")
    
    fh = f %>% 
      mutate(num_hysplit = rowSums(st_intersects(., h, sparse = F))) %>% 
      group_by(Lon, Lat) %>% 
      mutate(hysplit = 1*(row_number() <= num_hysplit)) %>% 
      ungroup()
    fhs = fh %>% 
      mutate(smoke = 1*any(rowSums(st_intersects(., s, sparse = F)) > 0),
             pred_hysplit_s = smoke)
    fhsc = fhs %>% 
      st_join(cl %>% select(perc_aod_missing = mean), left = T) %>% 
      mutate(cloud = 1*(perc_aod_missing > min_perc_aod_missing))
    fhscr = fhsc %>% 
      do_something() # get FRP and then binary for exceeding threshold(s) and get preds
    
    hf = h %>% 
      mutate(num_fire = rowSums(st_intersects(., f, sparse = F))) %>% 
      group_by(Lon, Lat) %>% 
      mutate(fire = 1*(row_number() <= num_fire)) %>% 
      ungroup()
    hfscr = hf %>% 
      left_join(fhscr %>% 
                  st_drop_geometry() %>% 
                  select(Lon, Lat, smoke, perc_aod_missing, cloud, frp, starts_with("pred")) %>% 
                  distinct(), 
                by = c("Lon", "Lat")) %>% 
      mutate(across(starts_with("pred"), ~ifelse(fire == 0, 0, .)))
    
    m = bind_rows(
      hfscr %>% 
        st_drop_geometry() %>% 
        mutate(ref = 1, hysplit = 1) %>% 
        select(Lon, Lat, date = Date.YYYYmmdd, fire, hysplit, ref, starts_with("pred")), 
      fhscr %>% 
        st_drop_geometry() %>% 
        filter(hysplit == 0) %>% 
        mutate(ref = 0, fire = 1) %>% 
        select(Lon, Lat, date, fire, hysplit, ref, starts_with("pred"))
    )
    
    saveRDS(fhscr, file.path(path_fire_matched, sprintf("fhscr_%s.rds", d)))
    saveRDS(hfscr, file.path(path_hysplit_matched, sprintf("hfscr_%s.rds", d)))
    saveRDS(m, file.path(path_fire_hysplit_matched, sprintf("m_%s.rds", d)))
  }
  end_time = Sys.time()
  end_time - start_time
}
matches = list.files(file.path(path_fire_hysplit_matched), full.names = T) %>% 
  map_dfr(readRDS)

################################################################################
# What proportion of fire points are HYSPLIT points? What proportion of HYSPLIT 
# points are fire points? How might these proportions change over time?
################################################################################
# Approximate proportion of fire points that are HYSPLIT points
# 0.02558488
nrow(matches %>% filter(fire == 1, hysplit == 1))/nrow(matches %>% filter(fire == 1))

# Distribution of proportion of fire points that are HYSPLIT points each day
matches %>% 
  group_by(date) %>% 
  summarize(num_fire = sum(fire),
            num_hysplit = sum(hysplit),
            num_fire_hysplit = sum((fire == 1) & (hysplit == 1))) %>% 
  mutate(prop = num_fire_hysplit/num_fire) %>% 
  ungroup() %>% 
  {ggplot(.) + 
      geom_histogram(aes(prop)) + 
      geom_vline(aes(xintercept = mean(prop)), color = "green4") + 
      geom_vline(aes(xintercept = median(prop)), color = "blue") + 
      geom_text(aes(mean(prop) + max(prop)/10, 2400, label = sprintf("mean = %s", round(mean(prop), 2))), color = "green4") + 
      geom_text(aes(median(prop) + max(prop)/10, 2300, label = sprintf("median = %s", round(median(prop), 2))), color = "blue") + 
      theme_light()}
ggsave(file.path(path_figures, "proportion_hysplit_of_fire_distribution.png"), width = 8, height = 5)

# Time series of proportion of fire points that are HYSPLIT points each day
matches %>% 
  group_by(date) %>% 
  summarize(num_fire = sum(fire),
            num_hysplit = sum(hysplit),
            num_fire_hysplit = sum((fire == 1) & (hysplit == 1))) %>% 
  mutate(prop = num_fire_hysplit/num_fire) %>% 
  ungroup() %>% 
  {ggplot(.) + 
      geom_line(aes(ymd(date), prop), size = 0.2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$prop)/-2, 
                           xend = End.Date, yend = y*max(.$prop)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(!in_use),
                       size = 2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$prop)/-2, 
                           xend = End.Date, yend = y*max(.$prop)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(in_use),
                       size = 2,
                       arrow = arrow(length = unit(0.05, "inches"))) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      coord_cartesian(xlim = c(ymd(start_date), NA)) + 
      theme_light() + 
      theme(legend.position = "none")}
ggsave(file.path(path_figures, "proportion_hysplit_of_fire_time_series.png"), width = 8, height = 5)

# Approximate proportion of HYSPLIT points that are fire points
# 0.3253671
nrow(matches %>% filter(hysplit == 1, fire == 1))/nrow(matches %>% filter(hysplit == 1))

# Distribution of proportion of HYSPLIT points that are fire points each day
matches %>% 
  group_by(date) %>% 
  summarize(num_fire = sum(fire),
            num_hysplit = sum(hysplit),
            num_fire_hysplit = sum((fire == 1) & (hysplit == 1))) %>% 
  mutate(prop = num_fire_hysplit/num_hysplit) %>% 
  ungroup() %>% 
  {ggplot(.) + 
      geom_histogram(aes(prop)) + 
      geom_vline(aes(xintercept = mean(prop)), color = "green4") + 
      geom_vline(aes(xintercept = median(prop)), color = "blue") + 
      geom_text(aes(mean(prop) + max(prop)/10, 2400, label = sprintf("mean = %s", round(mean(prop), 2))), color = "green4") + 
      geom_text(aes(median(prop) + max(prop)/10, 2300, label = sprintf("median = %s", round(median(prop), 2))), color = "blue") + 
      theme_light()}
ggsave(file.path(path_figures, "proportion_fire_of_hysplit_distribution.png"), width = 8, height = 5)

# Time series of proportion of HYSPLIT points that are fire points each day
matches %>% 
  group_by(date) %>% 
  summarize(num_fire = sum(fire),
            num_hysplit = sum(hysplit),
            num_fire_hysplit = sum((fire == 1) & (hysplit == 1))) %>% 
  mutate(prop = num_fire_hysplit/num_hysplit) %>% 
  ungroup() %>% 
  {ggplot(.) + 
      geom_line(aes(ymd(date), prop), size = 0.2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$prop)/-2, 
                           xend = End.Date, yend = y*max(.$prop)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(!in_use),
                       size = 2) + 
      geom_textsegment(aes(x = Start.Date, y = y*max(.$prop)/-2, 
                           xend = End.Date, yend = y*max(.$prop)/-2, 
                           label = Data.Set, color = Data.Set), 
                       data = satellites %>% filter(in_use),
                       size = 2,
                       arrow = arrow(length = unit(0.05, "inches"))) + 
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
      coord_cartesian(xlim = c(ymd(start_date), NA)) + 
      theme_light() + 
      theme(legend.position = "none")}
ggsave(file.path(path_figures, "proportion_fire_of_hysplit_time_series.png"), width = 8, height = 5)

################################################################################
# How well does subsetting to fire points that overlap a smoke plume on a given 
# day replicate the set of HYSPLIT points for which we do have data?
################################################################################
# Set positive class = 1
matches = matches %>% mutate(across(c(ref, starts_with("pred")), factor, levels = 1:0))
conf = confusionMatrix(data = matches$pred_hysplit_s, reference = matches$ref, positive = "1")

# True positive rate
# 0.325322
nrow(matches %>% filter(pred_hysplit_s == 1, ref == 1))/nrow(matches %>% filter(ref == 1))

# False positive rate
# 0.999191
nrow(matches %>% filter(pred_hysplit_s == 1, ref == 0))/nrow(matches %>% filter(ref == 0))

# True negative rate
# 0.0008089654
nrow(matches %>% filter(pred_hysplit_s == 0, ref == 0))/nrow(matches %>% filter(ref == 0))

# False negative rate
# 0.674678
nrow(matches %>% filter(pred_hysplit_s == 0, ref == 1))/nrow(matches %>% filter(ref == 1))

################################################################################
# How well does subsetting to fire points above an FRP threshold replicate the 
# HYSPLIT points under cloud or at least not under smoke plume?
################################################################################
# Plot ROC curve
# Varying sensitivity to FRP threshold

################################################################################
# How to assign smoke-producing duration and start time?
################################################################################
