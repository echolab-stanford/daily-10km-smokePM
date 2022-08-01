library(tidyverse)
library(magrittr)
library(layer)
library(sf)
# library(raster)

source("scripts/0_config.R")

epa_ll <- read_sf(paste0(data_path, "/epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)

sample_start <- as.Date("2006-01-01")
sample_end <- as.Date("2020-12-31")

# pm2.5 from EPA stations
epa_pm <- readRDS(paste0(data_path, "/epa_station_level_pm25_data.rds")) %>% 
  select(id, year, month, day, pm25) %>%
  unite(date, year, month, day, sep = "-", remove = FALSE) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(date >= sample_start & date <= sample_end)

# smoke
smoke_missing_dates <- list.files(paste0(data_path, "/smoke_days"), 
                                  full.names = TRUE) %>% 
  map(function(x) readRDS(x) %>% filter(note_smoke_date_not_online) %>% pull(date) %>% unique) 

smoke_missing_dates %<>% Reduce(c, .)

smoke_days <- readRDS(paste0(data_path, "/3_intermediate/all_smoke_days_incl_cloudy.rds")) %>% 
  filter(grid_id_10km %in% (epa_ll$grid_id_10km %>% unique))

# calculate smoke PM
smokePM <- readRDS(paste0(data_path, "/epa_station_level_pm25_data.rds")) %>% 
  select(id, year, month, day, pm25) %>%
  group_by(id, year, month, day) %>% 
  summarise(pm25 = mean(pm25)) %>% 
  unite(date, year, month, day, sep = "-", remove = FALSE) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(date >= sample_start & date <= sample_end) %>%
  left_join(epa_ll %>% st_drop_geometry() %>% select(id, grid_id_10km), by = "id") %>%
  filter(!is.na(grid_id_10km)) %>%
  left_join(smoke_days %>% select(-note_smoke_date_not_online), 
            by = c("date", "grid_id_10km")) %>%
  replace_na(list(smoke_day = 0)) %>% 
  {left_join(., 
             nonsmoke_medians(filter(., !(date %in% smoke_missing_dates)), 
                              pm25, smoke_day, id, month, year), 
             by = c("id", "month", "year"))} %>% 
  mutate(pm25_anom = pm25 - pm25_med_3yr, 
         smokePM = pmax(0, pm25_anom)*smoke_day,
         smoke_missing_date = (date %in% smoke_missing_dates))

saveRDS(smokePM, paste0(data_path, "/3_intermediate/station_smokePM.rds"))
