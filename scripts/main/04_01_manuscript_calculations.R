# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Calculates statistics in manuscript.
# ------------------------------------------------------------------------------
epa_data <- readRDS(file.path(path_data, "3_intermediate", "station_smokePM.rds"))

# how many stations do we have data at 
epa_data$id %>% unique %>% length

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
test <- readRDS(file.path(path_data, "4_clean", "aod_training.rds"))
nrow(test)

# stats on extreme smoke days 
smokePM_preds <- readRDS(file.path(path_output, "smokePM", "predictions", "combined", "smokePM_predictions_20060101_20201231.rds"))

pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv)

grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84")) %>% 
  mutate(area = st_area(geometry) %>% unclass) # area in m^2

# This may require a large amount of memory
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
saveRDS(extremes_by_year, file.path(path_data, "extremes_by_year.rds"))
# extremes_by_year = readRDS(file.path(path_data, "extremes_by_year.rds"))

# how far off are we on the really low smoke days PM
smokePM_preds <- list.files(file.path(path_output, "smokePM", "model"), 
                            pattern = "smokePM_pred", 
                            full.names = TRUE) %>% 
  grep("drop\\.", ., value = TRUE) %>% 
  map_dfr(function(x){
    readRDS(x) %>% 
      mutate(test_fold = as.numeric(gsub("^smokePM_pred_fold|_drop-?.*\\.rds$", "", basename(x))))
  }) %>%
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

smokePM_data <- readRDS(file.path(path_data, "4_clean", "smokePM_training.rds"))

comp <- smokePM_preds %>% 
  mutate(test = ifelse(fold == test_fold, "test", "train")) %>%
  filter(test == "test") %>%
  left_join(smokePM_data %>% select(id, date, smokePM)) 

comp %>% 
  filter(smokePM < 1) %>% 
  mutate(pred_bin = cut(smokePM_pred, c(0, 5, 10, Inf), 
                        right = FALSE,
                        include.lowest = T)) %>% 
  group_by(pred_bin) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n))
