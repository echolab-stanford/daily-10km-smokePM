#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Combines smoke PM2.5 training dataset.
#-------------------------------------------------------------------------------
# NOTES: 
# stations with NA for grid cell are in HI or AK

# nested cv folds 
n_nest_cv_folds <- 5

# sample range
sample_start <- as.Date("2006-01-01")
sample_end <- as.Date("2020-12-31")

# station locations and crosswalk to 10km grid
epa_ll <- read_sf(file.path(path_data, "epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)

epa_grid_cells <- epa_ll$grid_id_10km %>% unique()

# smoke ----
# results in the 10km grid ids and dates that are smoke days
smoke_missing_dates = readRDS(file.path(path_data, "smoke", "smoke_dates_not_online.rds"))
smoke_missing_dates = ymd(smoke_missing_dates)

smoke_days <- readRDS(file.path(path_data, "3_intermediate", "all_smoke_days_incl_cloudy.rds")) %>% 
  filter(grid_id_10km %in% epa_grid_cells)
# everything not in the missing list and not in smoke_days is a zero

# calculate smoke PM----
smokePM <- readRDS(file.path(path_data, "3_intermediate", "station_smokePM.rds")) %>% 
  filter(smoke_day == 1 & !is.na(pm25) & !smoke_missing_date)

# cross sectional variables ----
# elevation
elev <- read.csv(file.path(path_data,"2_from_EE", "elevation_avg_sd_10km_grid.csv")) %>% 
  filter(ID %in% epa_grid_cells) %>%
  transmute(grid_id_10km = ID,
            elevation_stdDev = stdDev_stdDev,
            elevation_mean = mean)

# nlcd
nlcd <- read.csv(file.path(path_data, "2_from_EE", "NLCD_areas_10km_grid.csv")) %>% 
  filter(ID %in% epa_grid_cells) %>%    
  select(ID, groups) %>%
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
  mutate(across(!total & !ID, ~.x/total)) %>%   # calculate percentages in each landcover class
  rename(grid_id_10km = ID)

# add latitude and longitude
cell_cent <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84")) %>% 
  filter(ID %in% epa_grid_cells) %>%
  st_centroid %>%
  {cbind(., 
         st_coordinates(.))} %>% 
  st_drop_geometry() %>% 
  rename(lat = Y, 
         lon = X)

# add cross sectional info to aod_cells
epa_grid_cells <- cell_cent %>% select(grid_id_10km = ID, lat, lon) %>% 
  full_join(elev, by = "grid_id_10km") %>% 
  full_join(nlcd %>% select(-total), by = "grid_id_10km") 

# time varying ----

# AOT ----
aot_anom <- list.files(file.path(path_data, "3_intermediate"), 
                       pattern = "aot_anom_all_days", full.names = T) %>% 
  purrr::map_dfr(function(x) readRDS(x) %>% filter(grid_id_10km %in% epa_grid_cells$grid_id_10km)) %>%
  arrange(grid_id_10km, date) %>% 
  group_by(grid_id_10km) %>% 
  mutate(aot_anom_lag1 = lag(aot_anom, 1),
         aot_anom_lag2 = lag(aot_anom, 2),
         aot_anom_lag3 = lag(aot_anom, 3)) %>% 
  ungroup()

# ERA5 ----
era5_global <- list.files(file.path(path_data, "ERA5_variables", "Global"),
                          full.names = TRUE) %>% 
  paste0(., "/USA/10km_grid/UTC-0600") %>% 
  list.files(full.names = TRUE) %>% 
  map(function(x) {
    print(x)
    x_name = str_split(x, pattern = "\\/")[[1]]
    l_x_name = length(x_name)
    old_name = x_name[l_x_name - 4]
    new_name = x_name[c(l_x_name - 4, l_x_name)] %>% paste0(collapse = "_")
    list.files(x, full.names = TRUE) %>%
      map_dfr(function(y) readRDS(y) %>% filter(id_grid %in% epa_grid_cells$grid_id_10km)) %>% 
      rename(!!new_name := !!old_name)
  }) %>% 
  reduce(.f = full_join, by = c("id_grid", "date"))

era5_land <- list.files(file.path(path_data, "ERA5_variables", "Land"),
                        full.names = TRUE) %>% 
  {paste0(., "/USA/10km_grid/", ifelse(grepl("precipitation", .), "UTC+0000", "UTC-0600"))} %>% 
  list.files(full.names = TRUE) %>% 
  map(function(x) {
    print(x)
    x_name = str_split(x, pattern = "\\/")[[1]]
    l_x_name = length(x_name)
    old_name = x_name[l_x_name - 4]
    new_name = x_name[c(l_x_name - 4, l_x_name)] %>% paste0(collapse = "_")
    list.files(x, full.names = TRUE) %>% 
      map_dfr(function(y) readRDS(y) %>% filter(id_grid %in% epa_grid_cells$grid_id_10km)) %>% 
      rename(!!new_name := !!old_name)
  }) %>% 
  reduce(.f = full_join, by = c("id_grid", "date"))

# fire variables ----
filled_fire = readRDS(file.path(path_data, "3_intermediate", "filled_fire.rds"))
fire_dist = file.path(path_data, "distance_to_fire_cluster") %>% 
  list.files(full.names = TRUE) %>% 
  purrr::map_dfr(function(x) readRDS(x) %>% filter(id_grid %in% epa_grid_cells$grid_id_10km)) %>% 
  left_join(filled_fire, by = c("id_grid", "date")) %>% 
  mutate(km_dist = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, km_dist.y, km_dist.x),
         area = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, area.y, area.x),
         num_points = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, num_points.y, num_points.x)) %>% 
  select(id_grid, date, km_dist, area, num_points)
rm(filled_fire)

# AOD predictions ----
aod_pred <- list.files(file.path(path_output, "anomAOD", "predictions", "10km_smoke_days"), 
                       full.names = TRUE,
                       pattern = "rds") %>% 
  purrr::map_dfr(function(x){readRDS(x) %>% filter(grid_id_10km %in% epa_grid_cells$grid_id_10km)})

# AOD percent missing ----
aod_missing <- list.files(file.path(path_data, "2_from_EE", "maiac_AODmissings"),
                          full.names = T) %>% 
  purrr::map_dfr(function(x){
    read.csv(x) %>% 
      mutate(date = as.Date(as.character(start_date), format = "%Y%m%d")) %>% 
      rename(AODmissing = mean, 
             grid_id_10km = ID) %>% 
      filter(grid_id_10km %in% epa_grid_cells$grid_id_10km) %>% 
      return
  })

# hysplit ----
hysplit <- purrr::map_dfr(
  list.files(file.path(path_data, "HYSPLIT", "10km_grid_2006-2020"), 
             pattern = "rds", 
             full.names = T), 
  function(x) {readRDS(x) %>% filter(id_grid %in% epa_grid_cells$grid_id_10km)}
) %>% 
  mutate(date = as.Date(date, format = "%Y%m%d"))

# combine data sets ----
pred_data <- smokePM %>% ungroup %>%
  select(id, date, grid_id_10km, month, smokePM) %>%
  left_join(epa_grid_cells, 
            by = c("grid_id_10km")) %>%
  left_join(aot_anom, 
            by = c("grid_id_10km", "date")) %>% 
  left_join(aod_pred %>% mutate(across(.fns = unname)), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(aod_missing %>% select(grid_id_10km, AODmissing, date), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(fire_dist %>% select(grid_id_10km = id_grid, 
                                 date, 
                                 fire_dist_km = km_dist, 
                                 closest_fire_area = area, 
                                 closest_fire_num_points = num_points), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(era5_global %>% select(grid_id_10km = id_grid, 
                                   date, 
                                   pbl_max = `boundary_layer_height_daily_maximum_of_1-hourly`,
                                   pbl_mean = `boundary_layer_height_daily_mean_of_1-hourly`,
                                   pbl_min = `boundary_layer_height_daily_minimum_of_1-hourly`,
                                   sea_level_pressure = `mean_sea_level_pressure_daily_mean_of_1-hourly`), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(era5_land %>% select(grid_id_10km = id_grid, 
                                 date, 
                                 wind_u = `10m_u_component_of_wind_daily_mean_of_1-hourly`,
                                 wind_v = `10m_v_component_of_wind_daily_mean_of_1-hourly`,
                                 dewpoint_temp_2m = `2m_dewpoint_temperature_daily_mean_of_1-hourly`,
                                 temp_2m = `2m_temperature_daily_mean_of_1-hourly`,
                                 surface_pressure = `surface_pressure_daily_mean_of_1-hourly`,
                                 precip = `total_precipitation_daily_maximum_of_1-hourly`), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(hysplit %>% select(grid_id_10km = id_grid, 
                               date, contains("num_traj_points_height")),
            by = c("grid_id_10km", "date"))

# create 5 spatial folds based on merra2 grid cells to avoid information leakage 
grid_merra = raster(list.files(file.path(path_data, "MERRA2_AOT"), pattern = "nc",
                               full.names = TRUE))


grid_10km = st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

epa_grid_cells$cell_merra <- raster::cellFromXY(grid_merra, 
                                                grid_10km %>% filter(ID %in% epa_grid_cells$grid_id_10km) %>% st_centroid() %>% st_coordinates)

set.seed(98765)
epa_grid_cells %<>% left_join(.,
                              dplyr::select(., cell_merra) %>%
                                unique %>%
                                mutate(order = sample.int(n(), n(), replace = FALSE)) %>% # reorder for randomness before assigning to folds
                                arrange(order) %>%
                                mutate(fold = mod(order, n_nest_cv_folds)),
                              by = "cell_merra")

# save final data set, ensuring it doesn't have any missing smoke dates 
pred_data %>% 
  mutate(month = as.factor(month)) %>% 
  left_join(epa_grid_cells %>% select(grid_id_10km, fold), by = "grid_id_10km") %>% 
  filter(!(date %in% smoke_missing_dates)) %>% 
  drop_na(smokePM) %>% 
  saveRDS(file.path(path_data, "4_clean", "smokePM_training.rds"))
