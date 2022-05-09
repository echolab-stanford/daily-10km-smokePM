source("./scripts/0_config.R")

epa_data <- readRDS(paste0(data_path, "/3_intermediate/station_smokePM.rds"))

# how many stations do we have data at 
epa_data$id %>% unique  %>% length

# how many stations have observations for at least 5 years 
epa_data %>% 
  group_by(id) %>% 
  summarise(start_date = min(date), 
            end_date = max(date),
            .groups = "drop") %>% 
  mutate(time_range = difftime(end_date, start_date, units = "days")) %>% 
  filter(time_range > (365*5)) %>% 
  nrow

# how many obs for aod model 
test <- readRDS("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/data/4_clean/aod_training.rds")
nrow(test)

# stats on extreme smoke days 
smokePM_preds <- readRDS(paste0(output_path, "/smokePM_predictions_2006_2020.rds")) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

pop <- list.files(paste0(data_path, "/2_from_EE/populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv)

grid_10km <- st_read(paste0(data_path, "/1_grids/grid_10km_wgs84")) %>% 
  mutate(area = st_area(grid_10km) %>% unclass) # area in m^2

extremes_by_year <- smokePM_preds %>%
  mutate(year = lubridate::year(date)) %>% 
  {full_join(group_by(., year) %>% 
               summarise(days_over50 = sum(smokePM_pred > 50),
                         days_over100 = sum(smokePM_pred > 100), 
                         days_over200 = sum(smokePM_pred > 200), 
                         .groups = "drop"),
             group_by(., year, grid_id_10km) %>%  
               summarise(pop_over50 = any(smokePM_pred >50),
                         pop_over100 = any(smokePM_pred > 100), 
                         pop_over200 = any(smokePM_pred > 200),
                         .groups = "drop") %>% 
               left_join(pop %>% left_join(grid_10km) %>%
                           transmute(grid_id_10km = ID, pop = mean*area)) %>% 
               group_by(year) %>% 
               summarise(across(starts_with("pop_over"), ~sum(.x*pop)), 
                         .groups = "drop"))}