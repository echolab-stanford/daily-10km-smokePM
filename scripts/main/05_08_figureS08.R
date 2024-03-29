source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 8.
# ------------------------------------------------------------------------------
crosswalk <- readRDS(file.path(path_data, "1_grids", "grid_crosswalk_1km_10km.rds"))

aod_train <- readRDS(file.path(path_data, "4_clean", "aod_training.rds"))

aod_train %<>% select(starts_with("grid"), date, month, aod_anom, lat, lon, fold)

aod_train_cells <- aod_train$grid_id_1km %>% unique

# use the extracted aod that we didn't train on to test with
smoke_missing_dates = readRDS(file.path(path_data, "smoke", "smoke_dates_not_online.rds"))
smoke_missing_dates = ymd(smoke_missing_dates)

smoke_days <- readRDS(file.path(path_data, "3_intermediate", "all_smoke_days_incl_cloudy.rds"))

aod_null_value <- -999999

aod_test <- map_dfr(list.files(file.path(path_data, "2_from_EE", "maiac_AOD_training"), full.names = TRUE), 
                    function(x) read_csv(x) %>% filter(!(grid_id %in% aod_train_cells))) %>% 
  transmute(grid_id_1km = grid_id, 
            date = as.Date(as.character(start_date), format = "%Y%m%d"),
            aod = median) %>% 
  filter(date >= as.Date("2006-01-01")) %>%
  mutate(month = lubridate::month(date), 
         year = lubridate::year(date)) %>% 
  left_join(crosswalk, by = "grid_id_1km") %>%
  left_join(smoke_days %>% select(-note_smoke_date_not_online), 
            by = c("date", "grid_id_10km")) %>%
  replace_na(list(smoke_day = 0)) %>% 
  filter(aod != aod_null_value) %>%
  {left_join(., 
             nonsmoke_medians(filter(., !(date %in% smoke_missing_dates)), 
                              aod, smoke_day, grid_id_1km, month, year), 
             by = c("grid_id_1km", "month", "year"))} %>% 
  mutate(aod_anom = aod - aod_med_3yr) 

aod_test_cells <- aod_test$grid_id_1km %>% unique()

all_aod_cells <- c(aod_test_cells, aod_train_cells)

aod_preds <- readRDS(file.path(path_output, "smokePM", "model", "test_aod_preds.rds"))
# predicted vs observed, in and out of sample 
aod_comp<- rbind(aod_train %>% select(grid_id_1km, date, aod_anom) %>% mutate(test_train = "train"), 
                 aod_test %>% filter(smoke_day == 1) %>% select(grid_id_1km, date, aod_anom) %>% mutate(test_train = "test")) %>% 
  left_join(aod_preds)

aod_comp %>% 
  arrange(test_train) %>%
  {ggplot(data = ., aes(x= aod_anom, y = aod_anom_pred, color = test_train)) + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  geom_point(alpha = 0.3) + 
  ggpubr::stat_cor(aes(label = ..rr.label..), digits = 4,
                   show_guide = FALSE) +
  scale_color_manual(values = c("black", "blue"), name = "") +
  xlab("observed AOD anomalies") + ylab("predicted AOD anomalies") + 
  theme_classic()} %>% 
  ggsave(file.path(path_figures, "figureS08a.png"), 
         ., width = 6, height = 6)

# variable importance 
var_import <- readRDS(file.path(path_output, "smokePM", "model", "AOD_var_importance.rds"))
var_import$variable_importance %>% 
  left_join(data.frame(feat_name = var_import$feature_names) %>% 
              mutate(Feature = paste0("f", (1:n()) - 1)))  %>% 
  mutate(group = case_when(grepl("aod_anom|aot_anom", feat_name) ~ "aerosols",
                           grepl("fire", feat_name) ~ "fire", 
                           grepl("traj", feat_name) ~ "hysplit",
                           feat_name %in% c("lat", "lon") ~ "cross sectional",
                           feat_name %in% c("dewpoint_temp_2m", "temp_2m", "wind_u", "wind_v", "precip", "pbl_mean",
                                          "surface_pressure", "sea_level_pressure") ~ "meteorology",
                           T ~ as.character(NA))) %>% 
  mutate(feat_name = factor(feat_name, levels = rev(.$feat_name), ordered = T)) %>% 
  mutate(feat_name = recode(feat_name, 
                            "lon" = "Longitude", 
                            "lat" = "Latitude",
                            "closest_fire_num_points" = "Points in closest fire cluster",
                            "dewpoint_temp_2m" = "Dewpoint temperature", 
                            "fire_dist_km" = "Distance to closest fire cluster", 
                            "closest_fire_area" = "Area of closest fire cluster",
                            "sea_level_pressure" = "Sea level pressure",
                            "wind_v" = "Wind speed (eastward)", 
                            "wind_u" = "Wind speed (westward)", 
                            "precip" = "Total precipitation", 
                            "pbl_mean" = "Planetary Boundary Layer (mean)",
                            "temp_2m" = "Temperature", 
                            "aot_anom" = "AOT anomaly (current)",
                            "aot_anom_lag1" = "AOT anomaly (1-day lag)",
                            "aot_anom_lag2" = "AOT anomaly (2-day lag)")) %>%
  magrittr::extract(1:15,) %>%
  {ggplot(data = ., 
          aes(x = Gain, y = feat_name)) + 
      geom_segment(aes(yend = feat_name, color = group), xend = 0, lwd = 6) + 
      xlim(0, NA) + 
      scale_color_manual(values = MetBrewer::met.brewer("VanGogh2", 15)[c(15, 3, 7, 9)],
                         name = "") +
      theme_classic() + 
      ylab("") + 
      theme(legend.position = c(0.7, 0.3))} %>% 
  ggsave(file.path(path_figures, "figureS08b.png"), ., 
         width = 4, height = 4)
