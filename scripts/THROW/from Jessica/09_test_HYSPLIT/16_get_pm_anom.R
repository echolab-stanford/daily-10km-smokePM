path_project = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction/"

nonsmoke_medians <- function(data, 
                             main_var, 
                             smoke_day, 
                             spatial_unit, 
                             temporal_unit, 
                             temporal_trend){
  main_var <- enquo(main_var)
  smoke_day <- enquo(smoke_day)
  spatial_unit <- enquo(spatial_unit)
  temporal_unit <- enquo(temporal_unit)
  temporal_trend <- enquo(temporal_trend)
  
  new_name <- paste0(rlang::as_name(main_var), "_med_3yr")
  
  full_panel <- expand.grid(id = data %>% pull(!!spatial_unit) %>% unique, 
                            month = data %>% pull(!!temporal_unit) %>% unique, 
                            year = data %>% pull(!!temporal_trend) %>% unique) %>% 
    rename(!!spatial_unit := id, 
           !!temporal_unit := month, 
           !!temporal_trend := year) %>% 
    ungroup
  
  data %>% 
    filter(!is.na(!!main_var) & !!smoke_day == 0) %>%
    full_join(full_panel) %>%
    group_by(!!spatial_unit, !!temporal_unit, !!temporal_trend) %>%
    summarise(main_var = list(!!main_var),
              nobs = n(),
              .groups = "drop") %>%
    arrange(!!spatial_unit, !!temporal_unit, !!temporal_trend) %>%
    group_by(!!spatial_unit, !!temporal_unit) %>%
    mutate(main_var_lag = lag(main_var, n = 1, default = list(NA)),
           main_var_lead = lead(main_var, n = 1, default = list(NA)),
           nobs_lag = lag(nobs, n = 1, default = 0),
           nobs_lead = lead(nobs, n = 1, default = 0)) %>%
    ungroup %>%
    rowwise %>%
    mutate(main_var_3yr = list(c(main_var, main_var_lag, main_var_lead)),
           main_var_med_3yr = median(unlist(main_var_3yr), na.rm = T),
           nobs_3yr = nobs + nobs_lead + nobs_lag) %>%
    ungroup %>%
    transmute(!!spatial_unit, !!temporal_unit, !!temporal_trend,
              nobs_3yr,
              !!new_name := main_var_med_3yr)
}

library(sf)
library(dplyr)
library(tidyr)
library(purrr)

#-------------------------------------------------------------------------------
# Get Anomalous PM2.5 (All Days)
# Written by Marissa with modifications by Jessica
# Last edited December 2021
# 
# Get anomalous PM2.5 at EPA stations on all days, not bottom-coded to 0, not
# recoded to 0 on non-smoke days, not including days where smoke data was missing.
#-------------------------------------------------------------------------------
# sample range
sample_start <- as.Date("2006-01-01")
sample_end <- as.Date("2020-12-31")

# pm2.5 from EPA stations
epa_pm = readRDS(paste0(path_project, "data/epa_station_level_pm25_data.rds")) %>% 
  select(id, year, month, day, pm25) %>%
  unite(date, year, month, day, sep = "-", remove = FALSE) %>% 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
  filter(date >= sample_start & date <= sample_end)

# station locations and crosswalk to 10km grid
epa_ll <- read_sf(paste0(path_project, "data/epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)
epa_grid_cells <- epa_ll$grid_id_10km %>% unique

# gridded smoke data
smoke_missing_dates <- list.files(paste0(path_project, "data/smoke_days"), 
                                  full.names = TRUE) %>% 
  map(function(x) readRDS(x) %>% filter(note_smoke_date_not_online) %>% pull(date) %>% unique) 
smoke_missing_dates <- smoke_missing_dates %>% Reduce(c, .)

# everything not in the missing list and not in smoke_days is a zero
smoke_days <- readRDS(paste0(path_project, "data/3_intermediate/all_smoke_days.rds")) %>% 
  filter(grid_id_10km %in% epa_grid_cells)

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
  mutate(pm25_anom = pm25 - pm25_med_3yr) %>% 
  filter(!is.na(pm25_anom)) %>% 
  filter(!(date %in% smoke_missing_dates))

# save
saveRDS(smokePM, paste0(path_project, "data/epa_pm25_anom_all_days.rds"))
