library(rlang)
library(sf)

source("./scripts/0_config.R")

n_fold <- 4

# crosswalk between grids -----
crosswalk <- readRDS(paste0(data_path, "/1_grids/grid_crosswalk_1km_10km.rds"))

# subsample to get 1000 locations instead of 5000, ensuring they are 5km away from each other  ----
pop_1km <- purrr::map_dfr(list.files(paste0(data_path, "/2_from_EE/populationDensity_1km_subgrid/"), full.names = TRUE), 
                          function(x){
                            read.csv(x)
                          })

grid_orig <- st_read(paste0(data_path, "/1_grids/1km_aod_grid_wgs84_training"))
grid_dists <- st_distance(st_centroid(grid_orig))
grid_5km_dist <- (units::drop_units(grid_dists) < 5000)

pop_remain <- pop_1km %>% filter(grid_id %in% grid_orig$grid_id)

sample_ids <- c()
set.seed(10001)
while(length(sample_ids) < 1000 & nrow(pop_remain) > 0){
  print(nrow(pop_remain))
  # sample a point
  new_samp <- sample(pop_remain$grid_id, 1, prob = pop_remain$mean)
  # add it to the sampled list 
  sample_ids %<>% c(new_samp)
  
  # identify cells within 5km 
  nearby_cell_ids <- which(grid_orig$grid_id == new_samp) %>% # identify the index of new_samp
    {magrittr::extract(grid_5km_dist, .,)} %>% # extract that row from the distance matrix 
    which %>% # identify which in indices are < 5km away 
    {magrittr::extract(grid_orig$grid_id, .)}# identify the associated grid_ids
  # nearby_cells <- grid_1km_orig$grid_id[which(grid_25km_dist[which(pop_5000$grid_id == new_samp),])]
  # exclude them from future sampling
  pop_remain %<>% filter(!(grid_id %in% nearby_cell_ids))
}

rm(pop_remain, nearby_cell_ids, new_samp, 
   grid_orig, grid_dists, grid_5km_dist)

# smoke ----
# results in the 10km grid ids and dates that are smoke days
smoke_missing_dates <- list.files(paste0(data_path, "/smoke_days"), 
                                  full.names = TRUE) %>% 
  map(function(x) readRDS(x) %>% filter(note_smoke_date_not_online) %>% pull(date) %>% unique) 

smoke_missing_dates %<>% Reduce(c, .)

smoke_days <- readRDS(paste0(data_path, "/3_intermediate/all_smoke_days_incl_cloudy.rds"))
# everything not in the missing list and not in smoke_days is a zero

# aod data ----
aod_null_value <- -999999

aod <- map_dfr(list.files(paste0(data_path,"/2_from_EE/maiac_AOD_training/"), full.names = TRUE), 
               function(x) read_csv(x) %>% filter(grid_id %in% sample_ids)) %>% 
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

# use aod, smoke, and crosswalk between grids to make a panel of the training 1km grid cell-days with smoke----
aod_cells <- data.frame(grid_id_1km = sample_ids) %>% 
  left_join(crosswalk, by = "grid_id_1km")


# assumption was 5k grid cells X 365 days x 15 years x 5% smoke days x 50% missing AOD ~ 700K obs
# we ended up with ~1.3 M obs, partly because there were ~2M smoke day obs

# elevation
elev <- list.files(paste0(data_path,"/2_from_EE/elevation_1km_subgrid"), full.names = TRUE) %>% 
  map_dfr(function(x) read.csv(x) %>% filter(grid_id %in% aod_cells$grid_id_1km)) %>% 
  rename_with(function(x) {paste0("elevation_", x)}, .cols = !grid_id) %>% 
  rename(elevation_stdDev = elevation_stdDev_stdDev,
         grid_id_1km = grid_id)

# nlcd
nlcd <- list.files(paste0(data_path,"/2_from_EE/NLCD_1km_subgrid"), full.names = TRUE) %>% 
  map_dfr(function(x) read.csv(x) %>% filter(grid_id %in% aod_cells$grid_id_1km)) %>%  #  
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
  rename(grid_id_1km = grid_id)

# add cross sectional info to aod_cells
aod_cells %<>% full_join(elev, by = "grid_id_1km") %>% 
  full_join(nlcd %>% select(-total), by = "grid_id_1km")
rm(elev, nlcd)

# add latitude and longitude
cell_cent <- st_read(paste0(data_path,"/1_grids/1km_aod_grid_wgs84_training")) %>% 
  filter(grid_id %in% aod_cells$grid_id_1km) %>%
  st_centroid %>%
  {cbind(., 
         st_coordinates(.))} %>% 
  st_drop_geometry() %>% 
  rename(lat = Y, 
         lon = X)

aod_cells %<>% left_join(cell_cent, by = c("grid_id_1km" = "grid_id"))

# fire distance and cluster size ----
fire_dist = list.files(paste0(data_path,"/distance_to_fire_cluster/"), 
                       full.names = TRUE) %>% 
  map_dfr(function(x) readRDS(x) %>% filter(id_grid %in% aod_cells$grid_id_10km))

filled_fire = readRDS(paste0(data_path, "3_intermediate/filled_fire.rds"))

fire_dist = readRDS(fire_dist) %>% 
  left_join(filled_fire, by = c("id_grid", "date")) %>% 
  mutate(km_dist = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, km_dist.y, km_dist.x),
         area = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, area.y, area.x),
         num_points = ifelse(note_fire_date_not_online | note_fire_date_clusters_too_small, num_points.y, num_points.x)) %>% 
  select(id_grid, date, km_dist, area, num_points)

# ERA5 weather ----
era5_global <- list.files(paste0(data_path,"/ERA5_variables/Global"),
           full.names = TRUE) %>% 
  paste0(., "/USA/10km_grid/UTC-0600") %>% 
  list.files(full.names = TRUE) %>% 
  map(function(x) {
    print(x)
    old_name = str_split(x, pattern = "\\/")[[1]][10]
    new_name = str_split(x, pattern = "\\/")[[1]][c(10, 14)] %>% paste0(collapse = "_")
    list.files(x, full.names = TRUE) %>% 
      map_dfr(function(y) readRDS(y) %>% filter(id_grid %in% aod_cells$grid_id_10km)) %>% 
      rename(!!new_name := !!old_name)
    }) %>% 
  reduce(.f = full_join, by = c("id_grid", "date"))

era5_land <- list.files(paste0(data_path,"/ERA5_variables/Land"),
                        full.names = TRUE) %>% 
  {paste0(., "/USA/10km_grid/", ifelse(grepl("precipitation", .), "UTC+0000", "UTC-0600"))} %>% 
  list.files(full.names = TRUE) %>% 
  map(function(x) {
    print(x)
    old_name = str_split(x, pattern = "\\/")[[1]][10]
    new_name = str_split(x, pattern = "\\/")[[1]][c(10, 14)] %>% paste0(collapse = "_")
    list.files(x, full.names = TRUE) %>% 
      map_dfr(function(y) readRDS(y) %>% filter(id_grid %in% aod_cells$grid_id_10km)) %>% 
      rename(!!new_name := !!old_name)
  }) %>% 
  reduce(.f = full_join, by = c("id_grid", "date"))

# AOT ----
# these are aot anomalies on smoke days only
aot <- list.files(paste0(data_path,"/3_intermediate"), pattern = "aot_anom_smoke_days", 
                  full.names = TRUE) %>%
  map_dfr(function(x) readRDS(x) %>% filter(grid_id_10km %in% aod_cells$grid_id_10km)) %>% 
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
         aot_anom_lag3 = lag(aot_anom, 3)) %>% 
  ungroup %>% 
  filter(orig == 1) %>% # only keep the original set of days
  select(-orig)

# combine data sets into aod panel ----
aod_smoke_panel <-  aod %>%
  filter(grid_id_1km %in% sample_ids) %>%
  filter(smoke_day == 1) %>% 
  select(grid_id_1km, grid_id_10km, date, month, aod_anom) %>% 
  left_join(aot, 
            by = c("grid_id_10km", "date")) %>% 
  left_join(fire_dist %>% select(grid_id_10km = id_grid, 
                                 date, 
                                 fire_dist_km = km_dist, 
                                 closest_fire_area = area, 
                                 closest_fire_num_points = num_points), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(era5_global %>% select(grid_id_10km = id_grid, date, 
                                   pbl_max = `boundary_layer_height_daily_maximum_of_1-hourly`,
                                   pbl_mean = `boundary_layer_height_daily_mean_of_1-hourly`,
                                   pbl_min = `boundary_layer_height_daily_minimum_of_1-hourly`,
                                   sea_level_pressure = `mean_sea_level_pressure_daily_mean_of_1-hourly`), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(era5_land %>% select(grid_id_10km = id_grid, date, 
                                 wind_u = `10m_u_component_of_wind_daily_mean_of_1-hourly`,
                                 wind_v = `10m_v_component_of_wind_daily_mean_of_1-hourly`,
                                 dewpoint_temp_2m = `2m_dewpoint_temperature_daily_mean_of_1-hourly`,
                                 temp_2m = `2m_temperature_daily_mean_of_1-hourly`,
                                 surface_pressure = `surface_pressure_daily_mean_of_1-hourly`,
                                 precip = `total_precipitation_daily_maximum_of_1-hourly`), 
            by = c("grid_id_10km", "date")) %>% 
  # add station splits for spatial CV on internal model tuning 
  # (this is to optimize for a model that has best out of sample location performance)
  left_join(aod_cells %>% 
              {.[sample.int(nrow(.), nrow(.), replace = FALSE),]} %>% # reorder for randomness before assigning to folds
              mutate(fold = mod(row_number(), n_fold)), 
            by = c("grid_id_1km", "grid_id_10km"))

# we occasionally get NAs for aot_anom because there aren't any obs to make a location-month specific median
aod_smoke_panel %>% 
  filter(!is.na(aod_anom)) %>% 
  filter(!(date %in% smoke_missing_dates)) %>% 
  mutate(month = as.factor(month)) %>%
  saveRDS(paste0(data_path,"/4_clean/aod_training.rds"))
