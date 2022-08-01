library(rlang)
library(sf)
library(raster)

source("./scripts/0_config.R")

# sample range
sample_start <- as.Date("2006-01-01")
sample_end <- as.Date("2020-12-31")

# pm2.5 from EPA stations
epa_pm <- readRDS(paste0(data_path, "/epa_station_level_pm25_data.rds")) %>% 
  select(id, year, month, day, pm25) %>%
  unite(date, year, month, day, sep = "-", remove = FALSE) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(date >= sample_start & date <= sample_end)

# station locations and crosswalk to 10km grid
epa_ll <- read_sf(paste0(data_path, "/epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)

epa_grid_cells <- epa_ll$grid_id_10km %>% unique

# gridded smoke data
smoke_missing_dates <- list.files(paste0(data_path, "/smoke_days"), 
                                  full.names = TRUE) %>% 
  map(function(x) readRDS(x) %>% filter(note_smoke_date_not_online) %>% pull(date) %>% unique) 

smoke_missing_dates %<>% Reduce(c, .)

smoke_days <- readRDS(paste0(data_path, "/3_intermediate/all_smoke_days.rds")) %>% 
  filter(grid_id_10km %in% epa_grid_cells)
# everything not in the missing list and not in smoke_days is a zero

# calculate smoke PM----
smokePM <- epa_pm %>%
  left_join(epa_ll %>% st_drop_geometry() %>% select(id, grid_id_10km), by = "id") %>%
  filter(!is.na(grid_id_10km)) %>%
  left_join(smoke_days %>% select(-note_smoke_date_not_online), 
            by = c("date", "grid_id_10km")) %>%
  replace_na(list(smoke_day = 0)) %>% # fill other grid-days with zeros
  {left_join(., 
             nonsmoke_medians(filter(., !(date %in% smoke_missing_dates)), 
                              pm25, smoke_day, id, month, year), 
             by = c("id", "month", "year"))} %>% 
  mutate(pm25_anom = pm25 - pm25_med_3yr, 
         smokePM = pmax(0, pm25_anom)*smoke_day) %>% 
  filter(smoke_day == 1 & !is.na(pm25_anom)) %>% 
  filter(!(date %in% smoke_missing_dates))