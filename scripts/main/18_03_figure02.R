# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots model performance.
# ------------------------------------------------------------------------------
# Load smokePM predictions from held-out folds
smokePM_preds <- list.files(file.path(path_output, "smokePM", "model"),
                            pattern = "smokePM_pred",
                            full.names = TRUE) %>%
  grep("drop\\.", ., value = TRUE) %>%
  map_dfr(function(x){
    readRDS(x) %>% mutate(test_fold = as.numeric(
      substr(gsub("smokePM_pred_fold", "", basename(x)), 1, 1)))
  }) %>%
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

smokePM_data <- readRDS(file.path(path_data, "4_clean", "smokePM_training.rds")) %>% 
  select(id, date, smokePM, water:wetlands)

epa_data <- readRDS(file.path(path_data, "3_intermediate", "station_smokePM.rds")) 

states <- tigris::states(cb = TRUE)

epa_ll <- st_read(file.path(path_data, "epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)

# panel A, observed smoke PM vs predicted smoke PM 
smokePM_preds %>% 
  mutate(test = ifelse(fold == test_fold, "test", "train")) %>%
  filter(test == "test") %>%
  left_join(smokePM_data %>% select(id, date, smokePM)) %>% 
  {ggplot(data = ., 
          aes(x = smokePM, y = smokePM_pred)) + 
      geom_bin2d(bins = 70) + 
      geom_abline(intercept = 0, slope = 1, color = "grey30") +
      scale_fill_continuous(type = "viridis", 
                            trans = "pseudo_log", 
                            breaks = c(0, 1, 10, 100, 1000, 10000)) +
      scale_x_continuous(trans = "pseudo_log", 
                         breaks = c(0, 1, 5, 10, 50, 100, 500, 1000),
                         expand = c(0, 0)) +
      scale_y_continuous(trans = "pseudo_log", 
                         breaks = c(0, 1, 5, 10, 50, 100, 500, 1000), 
                         expand = c(0, 0)) +
      ylab(expression(predicted~smoke~PM[2.5])) + xlab(expression(observed~smoke~PM[2.5])) +
      theme_classic() + 
      theme(text = element_text(size = 18))} %>% 
  ggsave(filename = file.path(path_figures, "figure02a.png"), 
         ., width = 7, height = 5)

# panel B, variable importance 
list.files(file.path(path_output, "smokePM", "model"), 
           pattern = "var_importance", 
           full.names = T) %>% 
  grep("fold99", ., value = T) %>% 
  readRDS() %>% 
  extract2("variable_importance") %>% 
  mutate(Feature = factor(Feature, levels = rev(.$Feature), ordered = T)) %>% 
  mutate(group = case_when(grepl("aod_anom|aot_anom", Feature) ~ "aerosols",
                           grepl("fire", Feature) ~ "fire", 
                           grepl("traj", Feature) ~ "hysplit",
                           Feature %in% c("lat", "lon") ~ "cross sectional",
                           Feature %in% c("dewpoint_temp_2m", "temp_2m", 
                                          "surface_pressure", "sea_level_pressure") ~ "meteorology",
                           T ~ as.character(NA))) %>% 
  mutate(plot_feature = recode(Feature, 
                               "aod_anom_pred_1.00" = "Predicted AOD anomaly (max)",
                               "aod_anom_pred_0.75" = "Predicted AOD anomaly (75th)",
                               "aod_anom_pred_0.50" = "Predicted AOD anomaly (50th)",
                               "aod_anom_pred_0.25" = "Predicted AOD anomaly (25th)",
                               "aod_anom_pred_0.00" = "Predicted AOD anomaly (min)",
                               "aod_anom_pred_mean" = "Predicted AOD anomaly (mean)",
                               "lon" = "Longitude", 
                               "lat" = "Latitude",
                               "closest_fire_num_points" = "Points in closest fire cluster",
                               "dewpoint_temp_2m" = "Dewpoint Temperature", 
                               "fire_dist_km" = "Distance to closest fire cluster", 
                               "closest_fire_area" = "Area of closest fire cluster",
                               "num_traj_points_height_1" = "HYSPLIT trajectories below 1km", 
                               "temp_2m" = "Temperature", 
                               "aot_anom_lag2" = "AOT anomaly (2-day lag)")) %>%
  magrittr::extract(1:15,) %>%
  {ggplot(data = ., 
          aes(x = Gain, y = plot_feature)) + 
      geom_segment(aes(yend = plot_feature, color = group), xend = 0, lwd = 6) + 
      xlim(0, NA) + 
      scale_color_manual(values = MetBrewer::met.brewer("VanGogh2", 15)[c(15, 3, 7, 1, 9)], 
                         name = "") +
      theme_classic() + 
      ylab("") + 
      theme(legend.position = c(0.7, 0.3),
            plot.margin = margin(5.5, 9, 5.5, 5.5, "pt"))} %>% 
  ggsave(filename = file.path(path_figures, "figure02b.png"), 
         ., width = 5.5, height = 3.5)

# should we do joint feature importance given the correlation of all the aod variables? 

# panel C, map r2 by station
station_performance <- smokePM_preds %>% 
  filter(fold == test_fold) %>% 
  left_join(smokePM_data %>% select(id, date, smokePM)) %>% 
  select(-test_fold, -date) %>%
  nest_by(id) %>% 
  mutate(n = nrow(data), 
         fold = unique(data$fold),
         n_unique_smokePM = length(unique(data$smokePM))) %>% 
  filter(n > 1 & n_unique_smokePM > 1) %>% 
  mutate(r2 = fixest::r2(fixest::feols(smokePM ~ smokePM_pred, 
                                       data = data), 
                         "r2") %>% unname,
         max_smokePM = max(data$smokePM),
         avg_smokePM = mean(data$smokePM),
         skew_smokePM = e1071::skewness(data$smokePM), 
         kurt_smokePM = e1071::kurtosis(data$smokePM),
         range_smokePM = range(data$smokePM) %>% diff,
         var_smokePM = var(data$smokePM)) %>% 
  {right_join(epa_ll ,.)} %>% 
  cbind(.,
        stations_50km = st_distance(.) %>% 
          as.matrix %>% 
          units::set_units(NULL) %>%  
          magrittr::is_less_than(50000) %>% 
          rowSums(),
        stations_100km = st_distance(.) %>% 
          as.matrix %>% 
          units::set_units(NULL) %>%  
          magrittr::is_less_than(100000) %>% 
          rowSums()) %>% 
  left_join(epa_data %>% 
              group_by(id, smoke_day) %>% 
              summarise(avg_totalPM = mean(pm25), 
                        var_totalPM = var(pm25),
                        skew_totalPM = e1071::skewness(pm25),
                        kurt_totalPM = e1071::kurtosis(pm25),
                        .groups = "drop") %>% 
              mutate(smoke_day = recode(smoke_day, 
                                        "1" = "smoke", 
                                        "0" = "non_smoke")) %>% 
              pivot_wider(values_from = ends_with("_totalPM"), 
                          names_sep = "_",
                          names_from = smoke_day)) %>% 
  left_join(smokePM_data %>% select(id, water:wetlands) %>% unique) %>%
  mutate(class = names(select(st_drop_geometry(.), water:wetlands))[max.col(select(st_drop_geometry(.), water:wetlands))])

station_performance %<>% select(-data)

station_performance$r2 %>% quantile(probs = c(0.05, 0.5, 0.95))

station_performance %>% 
  filter(n > 50) %>%
  {{ggplot(data = .) +
      geom_sf(data = states  %>% filter(!(STATEFP %in% nonContig_stateFIPS)),
              color = "grey10", fill = "grey80") +
      geom_sf(mapping = aes(color = r2)) +
      scale_colour_gradientn(colors = brewer.pal(11, "RdYlBu"),
                             name = expression(R^2),
                             guide = "none") +
      theme_void()} %>%
      ggsave(filename = file.path(path_figures, "figure02c_map.png"),
             plot = .,
             width =7, height = 5)
    {ggplot(data = mutate(., hist_col = cut(r2, breaks = seq(0, 1, length.out = 12))), 
            aes(x= r2)) + 
        geom_histogram(aes(fill = hist_col), breaks = seq(0, 1, length.out = 12)) + 
        scale_colour_manual(values= brewer.pal(11, "RdYlBu"),
                            name = expression(R^2), aesthetics = c("color", "fill"), 
                            guide = "none") + 
        theme_classic() +
        xlab(expression(R^2)) + 
        theme(text = element_text(size = 18))} %>% 
      ggsave(filename = file.path(path_figures, "figure02c_histogram.png"), 
             plot = ., 
             width = 2.5*1.5, height = 1.5*1.5)}

# Panel D, predictors of station performance
ee_Initialize(email = gee_email)

lc = ee$ImageCollection("MODIS/006/MCD12Q1") %>% 
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>% 
  ee$ImageCollection$first() %>% 
  ee$Image$select("LC_Type5")

if (!file.exists(file.path(path_data, "station_MODIS_landcover.rds"))) {
  # Takes ~12 minutes
  start_time = get_start_time()
  station_lc = map_dfr(1:nrow(station_performance), function(i) {
    print(i)
    ee_extract(lc, station_performance[i,], 
               scale = lc$projection()$nominalScale()$getInfo())
  }) %>% 
    select(id, Type5 = LC_Type5)
  print_time(start_time)
  saveRDS(station_lc, file.path(path_data, "station_MODIS_landcover.rds"))
} else {
  station_lc = readRDS(file.path(path_data, "station_MODIS_landcover.rds"))
}

# station_lc <- ee_extract(lc, station_performance, scale = lc$projection()$nominalScale()$getInfo()) %>% 
#   select(id, Type5 = LC_Type5)

# 0	1c0dff	Water Bodies: at least 60% of area is covered by permanent water bodies.
# 1	05450a	Evergreen Needleleaf Trees: dominated by evergreen conifer trees (>2m). Tree cover >10%.
# 2	086a10	Evergreen Broadleaf Trees: dominated by evergreen broadleaf and palmate trees (>2m). Tree cover >10%.
# 3	54a708	Deciduous Needleleaf Trees: dominated by deciduous needleleaf (larch) trees (>2m). Tree cover >10%.
# 4	78d203	Deciduous Broadleaf Trees: dominated by deciduous broadleaf trees (>2m). Tree cover >10%.
# 5	dcd159	Shrub: Shrub (1-2m) cover >10%.
# 6	b6ff05	Grass: dominated by herbaceous annuals (<2m) that are not cultivated.
# 7	dade48	Cereal Croplands: dominated by herbaceous annuals (<2m). At least 60% cultivated cereal crops.
# 8	c24f44	Broadleaf Croplands: dominated by herbaceous annuals (<2m). At least 60% cultivated broadleaf crops.
# 9	a5a5a5	Urban and Built-up Lands: at least 30% impervious surface area including building materials, asphalt, and vehicles.
# 11	f9ffa4	Non-Vegetated Lands: at least 60% of area is non-vegetated barren (sand, rock, soil) with less than 10% vegetation.

station_difftime <- epa_data %>% 
  ungroup %>% 
  select(id, date) %>% 
  arrange(id, date) %>% 
  group_by(id) %>% 
  mutate(seq_obs = date - lag(date)) %>% 
  summarise(med_seq_obs = median(seq_obs, na.rm = T)) %>% 
  ungroup 

mod_data <- station_performance %>% 
  filter(n > 50) %>%
  left_join(station_lc) %>% 
  left_join(station_difftime) %>%
  left_join(epa_data %>% 
              group_by(id) %>% 
              summarise(start_date = min(date), 
                        end_date = max(date), 
                        date_range = as.numeric(difftime(end_date, start_date, units = "days")))) %>%
  mutate(var_smokePM_log = log(var_smokePM),
         avg_smokePM_log = log(avg_smokePM), 
         avg_totalPM_non_smoke_log = log(avg_totalPM_non_smoke), 
         var_totalPM_non_smoke_log = log(var_totalPM_non_smoke), 
         n_log = log(n), 
         date_range_log = log(date_range), 
         lc_shrub = (Type5 == 5)*1, 
         lc_barren = (Type5 == 11)*1, 
         med_seq_obs = as.numeric(med_seq_obs)) 

mod <- fixest::feols(r2 ~  var_smokePM_log + 
                       avg_smokePM_log +
                       avg_totalPM_non_smoke_log +
                       var_totalPM_non_smoke_log +
                       n +
                       lat + lon +
                       lc_shrub +
                       lc_barren +
                       stations_100km +
                       stations_50km +
                       med_seq_obs, 
                     data = mod_data)
summary(mod)

# change in r2 (per ols model) from going 5th to 95th percentile
mod_data %>% st_drop_geometry() %>% 
  select(-id, -grid_id_10km) %>%
  summarise(across(where(is.numeric), 
                   list(q5 =~quantile(.x, probs = 0.05), 
                        q95 =~quantile(.x, probs = 0.95)),
                   .names = "{.col}.{.fn}")) %>% 
  pivot_longer(everything()) %>% 
  separate(name, c("name", "quantile"), sep = "\\.") %>% 
  pivot_wider(names_from = quantile) %>% 
  full_join(summary(mod)$coeftable %>% 
              as.data.frame() %>% 
              rownames_to_column(var = "name")) %>% 
  rename(se = `Std. Error`) %>% 
  mutate(q95 = ifelse(grepl("lc_", name), 1, q95)) %>% 
  mutate(est_5_to_95 = (q95 - q5)*Estimate, 
         se_5_to_95 = abs(q95 - q5)*se, 
         lwr = est_5_to_95 + qnorm(0.05)*se_5_to_95, 
         upr = est_5_to_95 + qnorm(0.95)*se_5_to_95) %>%
  filter(!is.na(est_5_to_95)) %>% 
  arrange(est_5_to_95) %>% 
  mutate(fig_name = recode(name, 
                           "lat" = "latitude", 
                           "lon" = "longitude",
                           "avg_smokePM_log" = "log(average smoke pollution)",
                           "var_smokePM_log" = "log(variance in smoke pollution)",
                           "avg_totalPM_non_smoke_log" = "log(average non-smoke pollution)", 
                           "var_totalPM_non_smoke_log" = "log(variance in non-smoke pollution)", 
                           "n" = "number of station observations", 
                           "lc_shrub" = "shrubland landcover", 
                           "lc_barren" = "barren landcover",
                           "stations_100km" = "number of stations\nwithin 100km",
                           "stations_50km" = "number of stations\nwithin 50km",
                           "date_range_log" = " ", 
                           "med_seq_obs" = "frequency of station observations")) %>%
  mutate(fig_name = factor(fig_name, levels = unique(fig_name), ordered = T),
         group = case_when(grepl("smokePM", name) ~ "smokePM variables",
                           grepl("totalPM_non_smoke", name) ~ "totalPM variables",
                           name %in% c("n_log", "date_range_log", "med_seq_obs", 
                                       "stations_100km", "stations_50km") ~ "station data amount", 
                           grepl("lc_", name) ~ "landcover", 
                           T ~ "other")) %>%  
  {ggplot(data = ., 
          mapping = aes(y = fig_name, x = est_5_to_95)) + 
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_point() + 
      geom_linerange(aes(xmin = lwr, xmax = upr)) + 
      theme_classic() + 
      xlab(expression(paste("change in ", R^2, " from ", 5^th, " to ", 95^th, " percentile"))) + 
      ylab("") + 
      theme(text = element_text(size = 16))} %>% 
  ggsave(file.path(path_figures, "figure02d.png"), ., width = 7, height = 5)
