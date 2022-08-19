source("./scripts/0_config.R")

smoke <- list.files(paste0(data_path, "/smoke_days"), 
                    full.names = TRUE) %>% 
  map_dfr(function(x) readRDS(x) %>% select(-total, -light, -medium, -dense, -note_smoke_date_repaired_geometry, -note_smoke_date_empty_data)) %>% 
  rename(grid_id_10km = id_grid)

# read RDS of all smoke days
smoke_all <- readRDS(paste0(data_path, "/3_intermediate/all_smoke_days.rds"))

# go one month at a time, load in the AOT data, and calculate non-smoke medians, saving anomalies on smoke days
map(1:12, function(month){
  print(month)
  m_string = ifelse(month < 10, paste0("0", month), as.character(month))
  # calculate medians, using only the dates with smoke online
  month_data <- list.files(paste0(data_path, "/MERRA2_AOT"),
                    pattern = paste0("_", month, ".RDS"),
                    full.names = TRUE) %>% 
    map_dfr(function(x) readRDS(x)) %>% 
    mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>% 
    rename(grid_id_10km = grid_id) %>% 
    left_join(smoke, by = c("date", "grid_id_10km")) %>%
    mutate(month = lubridate::month(date), 
           year = lubridate::year(date))
  month_medians <- nonsmoke_medians(filter(month_data, !note_smoke_date_not_online), 
                                    aot, smoke_day, grid_id_10km, 
                                    month, year)
  saveRDS(month_medians, paste0(paste0(data_path, "/3_intermediate/aot_nonsmoke_medians_", m_string, ".rds")))
  print("medians calculated")
  
  
  # save the anomalies on smoke days (including the filled)
  month_data %>% 
    left_join(smoke_all %>% select(grid_id_10km, date, smoke_day_fill = smoke_day),
              by = c("grid_id_10km", "date")) %>%
    # left_join(smoke_fill, by = c("grid_id_10km", "date")) %>%  # previously just joined in data on filled days
    mutate(smoke_day = ifelse(is.na(smoke_day), smoke_day_fill, smoke_day)) %>%
    filter(smoke_day == 1) %>%
    left_join(month_medians, by = c("grid_id_10km", "month", "year")) %>%
    transmute(grid_id_10km, 
              date,
              aot_anom = aot - aot_med_3yr) %>% 
    saveRDS(paste0(data_path, "/3_intermediate/aot_anom_smoke_days_", m_string, ".rds"))
  print("month file saved")
  return(NA)
})
