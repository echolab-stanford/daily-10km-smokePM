path_dropbox = "~/BurkeLab Dropbox/Data/"
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

library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(sf)

#-------------------------------------------------------------------------------
# Get Anomalous AOT (All Days)
# Written by Marissa with modifications by Jessica
# Last edited January 2022
#-------------------------------------------------------------------------------
years = 2006:2020
months = 1:12
start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
project_grid = read_sf(paste0(path_project, "data/1_grids/10km_grid/10km_grid.shp"))
project_grid_ids = project_grid$ID

smoke_all <- readRDS(paste0(path_project, "data/3_intermediate/all_smoke_days_incl_cloudy.rds"))
smoke_dates_not_online = ymd(readRDS(paste0(path_dropbox, "smoke/smoke_dates_not_online.rds")))

for (m in months) {
  m_str = str_pad(m, 2, "left", 0)
  dates_m = all_dates[which(month(all_dates) == m)]
  project_grid_m = expand.grid(grid_id_10km = project_grid_ids, date = dates_m)
  
  # Smoke
  smoke = smoke_all %>% filter(month(date) == m)
  smoke = left_join(project_grid_m, smoke, by = c("grid_id_10km", "date"))
  smoke = smoke %>% 
    mutate(note_smoke_date_not_online = date %in% smoke_dates_not_online,
           smoke_day = ifelse(is.na(smoke_day) & !note_smoke_date_not_online, 0, smoke_day))
  
  aot = paste0(path_project, "data/MERRA2_AOT/daily_grid_aot_", years, "_", m, ".RDS") %>% 
    map_dfr(readRDS) %>% 
    mutate(date = ymd(date)) %>% 
    rename(grid_id_10km = grid_id) %>% 
    left_join(smoke, by = c("date", "grid_id_10km")) %>% 
    mutate(month = month(date), 
           year = year(date))
  
  aot_medians = nonsmoke_medians(filter(aot, !note_smoke_date_not_online),
                                 aot, smoke_day, grid_id_10km, month, year)
  
  saveRDS(aot_medians, paste0(path_project, "data/3_intermediate/aot_nonsmoke_medians_", m_str, ".rds"))
  
  aot_anom = aot %>% 
    left_join(aot_medians, by = c("grid_id_10km", "month", "year")) %>%
    transmute(grid_id_10km, 
              date,
              aot_anom = aot - aot_med_3yr)
  
  saveRDS(aot_anom, paste0(path_project, "data/3_intermediate/aot_anom_all_days_", m_str, ".rds"))
}
