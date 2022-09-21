source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1

#-------------------------------------------------------------------------------
# Written by: Marissa Childs and Jessica Li
# Gets anomalous AOD 1 km predictions and 10 km aggregations
#-------------------------------------------------------------------------------
# Set time period
start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(substr(all_dates, 1, 7))
year_months = gsub("-", "_", year_months)

# Load 1st stage model for predicting AOD at 1 km
xgb_mod <- xgb.load(file.path(path_output, "anomAOD", "model", "aod_mod.xgb"))

# Load crosswalk between 1 km grid and 10 km grid
crosswalk <- readRDS(file.path(path_data, "1_grids", "grid_crosswalk_1km_10km.rds"))

#-------------------------------------------------------------------------------
#### Cross-sectional variables ####
# Latitude and longitude
print(paste("Longitude and latitude", "--------------------------------------------------"))
start_time = get_start_time()
cell_cent <- st_read(file.path(path_data, "1_grids", "1km_aod_grid_wgs84")) %>%
  st_centroid %>%
  {cbind(.,
         st_coordinates(.))} %>%
  st_drop_geometry() %>%
  rename(grid_id_1km = grid_id,
         lat = Y, 
         lon = X)
print_time(start_time)

# Elevation
print(paste("Elevation", "--------------------------------------------------"))
start_time = get_start_time()
files = list.files(file.path(path_data, "2_from_EE", "elevation_1km_subgrid"), 
                   full.names = TRUE)
files_filled = list.files(file.path(path_data,"2_from_EE", "elevation_1km_subgrid_filled"), 
                          full.names = TRUE)
subgrids = gsub("^elevation_avg_sd_1km_subgrid_|\\.csv$", "", basename(files))
subgrids_filled = gsub("^elevation_avg_sd_1km_subgrid_|\\.csv$|_filled", "", basename(files_filled))
subgrids = setdiff(subgrids, subgrids_filled)
files = c(files_filled, 
          file.path(path_data, "2_from_EE", "elevation_1km_subgrid", 
                    paste0("elevation_avg_sd_1km_subgrid_", subgrids, ".csv")))

elev <- files %>% 
  map_dfr(function(x) read.csv(x)) %>% 
  rename_with(function(x) {paste0("elevation_", x)}, .cols = !grid_id) %>% 
  rename(elevation_stdDev = elevation_stdDev_stdDev,
         grid_id_1km = grid_id)
print_time(start_time)

# NLCD
# Takes a few hours
print(paste("NLCD", "--------------------------------------------------"))
start_time = get_start_time()
files = list.files(file.path(path_data, "2_from_EE", "NLCD_1km_subgrid"), 
                   full.names = TRUE)
files_filled = list.files(file.path(path_data, "2_from_EE", "NLCD_1km_subgrid_filled"), 
                          full.names = TRUE)
subgrids = gsub("^NLCD_areas_1km_subgrid_|\\.csv$", "", basename(files))
subgrids_filled = gsub("^NLCD_areas_1km_subgrid_|\\.csv$|_filled", "", basename(files_filled))
subgrids = setdiff(subgrids, subgrids_filled)
files = file.path(path_data, "2_from_EE", "NLCD_1km_subgrid", 
                  paste0("NLCD_areas_1km_subgrid_", subgrids, ".csv"))

nlcd <- files %>% 
  map_dfr(function(x) read.csv(x)) %>%  #  
  mutate(groups = gsub("\\[|\\]", "", groups), # get rid of outer brackets
         groups = strsplit(groups, "\\}, \\{")) %>% # split on commas between brackets
  unnest(groups, keep_empty = TRUE) %>% # groups is now a list that we want to unnest (i.e. lengthen)
  mutate(groups = gsub("\\{|\\}", "", groups)) %>%  # drop the extra brackets left behind
  separate(groups, into = c("landcover", "area"), sep = ",") %>% # split in commas to get land cover class and area
  mutate(landcover = trimws(gsub("landcover=", "", landcover, fixed = TRUE)), # drop "landcover"
         area = trimws(gsub("sum=", "", area, fixed = TRUE)) %>% as.numeric, # drop "sum"
         landcover = recode(landcover, # recode the landcover variables to their classes
                            "1.0" = "water",
                            "2.0" = "developed",
                            "3.0" = "barren",
                            "4.0" = "forest",
                            "5.0" = "shrubland",
                            "7.0" = "herbaceous",
                            "8.0" = "cultivated",
                            "9.0" = "wetlands")) %>%
  pivot_wider(names_from = landcover, values_from = area, # make it wider, one row for each grid cell, filling missings with 0s because that land class wasn't in the grid cell
              values_fill = 0) %>%
  mutate(total = water + developed + barren + forest + shrubland + herbaceous + cultivated + wetlands) %>% # calculate total area for the grid cell
  mutate(across(!total & !grid_id, ~.x/total)) %>%   # calculate percentages in each landcover class
  rename(grid_id_1km = grid_id) %>% 
  select(-total) %>%
  bind_rows(map_dfr(files_filled, read.csv))
print_time(start_time)

#-------------------------------------------------------------------------------
#### Time-varying variables ####
filled_fire = readRDS(file.path(path_data, "3_intermediate", "filled_fire.rds"))
filled_smoke_days = readRDS(file.path(path_data, "3_intermediate", "all_smoke_days_incl_cloudy.rds"))

out = vector("list", length(year_months))

# Takes ~5-10 minutes per low-fire month and ~20-60 minutes per high-fire month
registerDoParallel(num_cores)
for (m in 1:length(year_months)) {
  year_month = year_months[m]
  print(paste(year_month, "--------------------------------------------------"))
  start_time = get_start_time()
  
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  dates_m = ymd(grep(paste0("^", y_str, m_str), all_dates_str, value = T))
  prev_m_str = as.integer(m_str) - 1
  prev_m_str = ifelse(prev_m_str == 0, "12", str_pad(prev_m_str, 2, "left", 0))
  prev_y_str = ifelse(prev_m_str == "12", as.character(as.integer(y_str) - 1), y_str)
  prev_ym_str = paste0(prev_y_str, "_", prev_m_str)
  
  # Anomalous AOT
  aot = file.path(path_data, "3_intermediate", paste0("aot_anom_smoke_days_", m_str, ".rds"))
  aot = readRDS(aot) %>% filter(year(date) == as.integer(y_str))
  if (format(min(dates_m), "%Y%m%d") != start_date) {
    prev_aot = file.path(path_data, "3_intermediate", paste0("aot_anom_smoke_days_", prev_m_str, ".rds"))
    prev_aot = readRDS(prev_aot) %>% filter(year(date) == as.integer(prev_y_str))
    aot = bind_rows(aot, prev_aot)
  }
  aot = aot %>% 
    mutate(orig = 1) %>%
    # add 3 lags of aot, filling with zeros on missing days
    {full_join(., 
               expand.grid(grid_id_10km = pull(., grid_id_10km) %>% unique,
                           date = seq.Date(pull(., date) %>% min, 
                                           pull(., date) %>% max, 
                                           by = "day")), 
               by = c("grid_id_10km", "date"))} %>% 
    replace_na(list(aot_anom = 0)) %>% 
    arrange(grid_id_10km, date) %>% 
    group_by(grid_id_10km) %>% 
    mutate(aot_anom_lag1 = lag(aot_anom, 1),
           aot_anom_lag2 = lag(aot_anom, 2),
           aot_anom_lag3 = lag(aot_anom, 3),
           aot_anom_lag1 = ifelse(is.na(aot_anom_lag1), aot_anom, aot_anom_lag1),
           aot_anom_lag2 = ifelse(is.na(aot_anom_lag2), aot_anom_lag1, aot_anom_lag2),
           aot_anom_lag3 = ifelse(is.na(aot_anom_lag3), aot_anom_lag2, aot_anom_lag3)) %>% 
    ungroup %>% 
    filter(orig == 1, # only keep the original set of days
           year(date) == as.integer(y_str),
           month(date) == as.integer(m_str)) %>% 
    select(-orig)
  
  # ERA5
  era5_global = list.files(file.path(path_data, "ERA5_variables", "Global"),
                           full.names = T) %>% 
    paste0("/USA/10km_grid/UTC-0600") %>% 
    list.files(full.names = T) %>% 
    map(function(x) {
      x_name = str_split(x, pattern = "\\/")[[1]]
      l_x_name = length(x_name)
      old_name = x_name[l_x_name - 4]
      new_name = x_name[c(l_x_name - 4, l_x_name)] %>% paste0(collapse = "_")
      list.files(x, full.names = TRUE, pattern = year_month) %>%
        map_dfr(function(x) readRDS(x)) %>%
        rename(!!new_name := !!old_name)
    }) %>%
    reduce(.f = full_join, by = c("id_grid", "date"))
  
  era5_land = list.files(file.path(path_data, "ERA5_variables", "Land"),
                         full.names = T) %>% 
    {paste0(., "/USA/10km_grid/", ifelse(grepl("precipitation", .), "UTC+0000", "UTC-0600"))} %>%
    list.files(full.names = TRUE) %>%
    map(function(x) {
      x_name = str_split(x, pattern = "\\/")[[1]]
      l_x_name = length(x_name)
      old_name = x_name[l_x_name - 4]
      new_name = x_name[c(l_x_name - 4, l_x_name)] %>% paste0(collapse = "_")
      list.files(x, full.names = TRUE, pattern = year_month) %>%
        map_dfr(function(x) readRDS(x)) %>%
        rename(!!new_name := !!old_name)
    }) %>%
    reduce(.f = full_join, by = c("id_grid", "date"))
  
  # Fire
  fire_dist = file.path(path_data, "distance_to_fire_cluster", paste0("grid_distance_to_fire_cluster_", year_month, ".rds"))
  fire_dist = readRDS(fire_dist) %>% 
    left_join(filled_fire, by = c("id_grid", "date")) %>% 
    mutate(km_dist = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, km_dist.y, km_dist.x),
           area = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, area.y, area.x),
           num_points = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, num_points.y, num_points.x)) %>% 
    select(id_grid, date, km_dist, area, num_points)
  
  out_m = foreach(d = 1:length(dates_m), .combine = c) %dopar% {
    ymd_str = format(dates_m[d], "%Y%m%d")
    
    #### Predict AOD #####------------------------------------------------------
    smoke_days_d = filled_smoke_days %>% filter(date == dates_m[d], smoke_day == 1)
    if (nrow(smoke_days_d) == 0) return(1)
    aot_d = aot %>% filter(date == dates_m[d])
    era5_global_d = era5_global %>% filter(date == dates_m[d])
    era5_land_d = era5_land %>% filter(date == dates_m[d])
    fire_dist_d = fire_dist %>% filter(date == dates_m[d])
    
    # Combine 10 km predictors
    pred_data <- smoke_days_d %>%
      select(grid_id_10km, date) %>% 
      left_join(aot_d, 
                by = c("grid_id_10km", "date")) %>% 
      left_join(fire_dist_d %>% select(grid_id_10km = id_grid, 
                                       date, 
                                       fire_dist_km = km_dist, 
                                       closest_fire_area = area, 
                                       closest_fire_num_points = num_points), 
                by = c("grid_id_10km", "date")) %>% 
      left_join(era5_global_d %>% select(grid_id_10km = id_grid, 
                                         date, 
                                         pbl_max = `boundary_layer_height_daily_maximum_of_1-hourly`,
                                         pbl_mean = `boundary_layer_height_daily_mean_of_1-hourly`,
                                         pbl_min = `boundary_layer_height_daily_minimum_of_1-hourly`,
                                         sea_level_pressure = `mean_sea_level_pressure_daily_mean_of_1-hourly`), 
                by = c("grid_id_10km", "date")) %>% 
      inner_join(era5_land_d %>% select(grid_id_10km = id_grid, 
                                        date, 
                                        wind_u = `10m_u_component_of_wind_daily_mean_of_1-hourly`,
                                        wind_v = `10m_v_component_of_wind_daily_mean_of_1-hourly`,
                                        dewpoint_temp_2m = `2m_dewpoint_temperature_daily_mean_of_1-hourly`,
                                        temp_2m = `2m_temperature_daily_mean_of_1-hourly`,
                                        surface_pressure = `surface_pressure_daily_mean_of_1-hourly`,
                                        precip = `total_precipitation_daily_maximum_of_1-hourly`) %>% 
                   drop_na(), 
                 by = c("grid_id_10km", "date"))
    if (nrow(pred_data) == 0) return(2)
    
    # Get predictors at 1 km
    pred_data = pred_data %>% left_join(crosswalk, by = "grid_id_10km")
    pred_data = reduce(list(pred_data, cell_cent, elev), left_join, by = "grid_id_1km")
    pred_data = pred_data %>% inner_join(nlcd, by = "grid_id_1km")
    if (nrow(pred_data) == 0) return(3)
    pred_data = pred_data %>% mutate(month = factor(as.integer(m_str), levels = 1:12))
    
    # Get 1 km anomalous AOD predictions
    pred_data_mat = pred_data %>% 
      select(month, lat, lon, aot_anom, 
             aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
             fire_dist_km, closest_fire_area, closest_fire_num_points,
             pbl_min, pbl_max, pbl_mean,
             wind_u, wind_v, 
             dewpoint_temp_2m, temp_2m,  
             sea_level_pressure, surface_pressure, precip, 
             elevation_mean, elevation_stdDev, 
             developed, barren, forest, shrubland, cultivated, 
             wetlands, herbaceous, water)
    
    pred_data_mat <- model.matrix.lm(~.-1,
                                     data = pred_data_mat,
                                     na.action = "na.pass") %>% 
      xgb.DMatrix()
    new_preds <- pred_data %>% 
      {cbind(select(., grid_id_1km, date), 
             aod_anom_pred = predict(xgb_mod, pred_data_mat))}
    
    # save the 1km predictions 
    saveRDS(new_preds, 
            file.path(path_output, "anomAOD", "predictions", "1km_smoke_days", 
                      paste0("AOD_predictions_1km_", ymd_str, ".rds")))
    print(paste(ymd_str, " - 1km predictions saved"))
    
    # save quantiles + mean aggregated to 10km grid cell
    preds_10km_feats <- new_preds %>% 
      left_join(crosswalk, by = "grid_id_1km") %>% 
      group_by(grid_id_10km, date) %>% 
      summarise(aod_anom_pred_0.00 = quantile(aod_anom_pred, 0),
                aod_anom_pred_0.25 = quantile(aod_anom_pred, 0.25),
                aod_anom_pred_0.50 = quantile(aod_anom_pred, 0.5),
                aod_anom_pred_0.75 = quantile(aod_anom_pred, 0.75),
                aod_anom_pred_1.00 = quantile(aod_anom_pred, 1),
                aod_anom_pred_mean = mean(aod_anom_pred)) %>% 
      ungroup()
    saveRDS(preds_10km_feats, 
            file.path(path_output, "anomAOD", "predictions", "10km_smoke_days", 
                      paste0("AOD_predictions_10km_", ymd_str, ".rds")))
    print(paste(ymd_str, " - 10km aggregates of predictions saved"))
    return(0)
  }
  out[[m]] = out_m
  print_time(start_time)
}
stopImplicitCluster()
