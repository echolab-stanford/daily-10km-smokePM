source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Aggregates 10 km grid smokePM predictions to zip level.
# ------------------------------------------------------------------------------
unit <- "zcta" # alternatively, "county"
# load shapefile, plus 10km grid transformed to match the crs
if (unit == "county") {
  unit_sf <- counties() %>% 
    filter(STATEFP %in% nonContig_stateFIPS == F)
} else if (unit == "tract") {
  unit_sf <- states() %>% 
    filter(STATEFP %in% nonContig_stateFIPS == F) %>% 
    pull(STATEFP) %>% 
    map_dfr(function(x){
      tracts(x, year = 2019) %>% select(STATEFP, GEOID)
    })
} else if (unit == "zcta") {
  unit_sf <- states() %>% 
    filter(STATEFP %in% nonContig_stateFIPS == F) %>% 
    pull(STATEFP) %>% 
    map_dfr(function(x){
      zctas(x, year = 2019) %>% select(ZCTA5CE10, GEOID10)
    })
} else {
  stop("only allowed units are \"tract\" or \"zcta\" or \"county\"")
}

# read in the grid
grid_10km <- st_read(file.path(path_final, "10km_grid", "10km_grid_wgs84", "10km_grid_wgs84.shp")) %>% 
  st_transform(st_crs(unit_sf))

# make a crosswalk with intersection area with grid cells
if (unit %in% c("county", "tract")) {
  unit_cross = st_intersection(unit_sf,
                               grid_10km) %>% 
    select(GEOID, grid_id_10km = ID) %>% 
    {cbind(st_drop_geometry(.), 
           area = st_area(.))} 
} else if (unit == "zcta") {
  unit_cross = st_intersection(unit_sf,
                               grid_10km) %>% 
    select(GEOID10, grid_id_10km = ID) %>% 
    {cbind(st_drop_geometry(.), 
           area = st_area(.))} 
}
# save the crosswalk since it takes a while to make 
saveRDS(unit_cross, file.path(path_data, paste0(unit, "_grid_area_crosswalk.rds")))
# unit_cross <- readRDS(file.path(path_data, paste0(unit, "_grid_area_crosswalk.rds")))

# smoke PM predictions 
smokePM <- readRDS(file.path(path_output, "smokePM", "predictions", "combined", "smokePM_predictions_20060101_20201231.rds"))

# population by grid cell
pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv)

# loop through work with one date at a time? rbind the data frames from the dates together
if (unit == "zcta") {
  unit_cross = unit_cross %>% rename(GEOID = GEOID10)
}
avg_unit_smokePM <- smokePM %>% 
  pull(date) %>% 
  unique %>% 
  purrr::map_dfr(function(i){
    print(i)
    # filter to the date of interest
    smokePM %>% 
      filter(date == i) %>%
      # add on all the tract/county unit IDs that are among the grid cells with smoke 
      left_join(unit_cross %>% select(grid_id_10km, GEOID),
                by = "grid_id_10km") %>% 
      filter(!is.na(GEOID)) %>% # drop grid cells that don't match to a unit
      # full set of unit-days with smoke 
      select(GEOID, date) %>% 
      unique %>%
      # join in all grid-cells for each unit
      left_join(unit_cross, by = "GEOID") %>% 
      # join in population and smoke PM predictions
      left_join(pop %>% select(grid_id_10km = ID, grid_pop_per_m2 = mean),
                by = "grid_id_10km") %>% 
      left_join(smokePM %>% filter(date == i), 
                by = c("grid_id_10km", "date")) %>%
      # fill missing smoke PM values with zero
      replace_na(list(smokePM_pred = 0)) %>%
      mutate(area = unclass(area),
             pop = grid_pop_per_m2*area) %>%
      # for each unit-date calculate pop-weighted avg 
      group_by(GEOID, date) %>%
      summarise(smokePM_pred = weighted.mean(smokePM_pred, pop),
                .groups = "drop") 
  })
if (unit == "zcta") {
  avg_unit_smokePM = avg_unit_smokePM %>% rename(GEOID10 = GEOID)
}

saveRDS(avg_unit_smokePM, 
        file.path(path_output, "smokePM", "predictions", "combined", 
                  paste0(unit, "_smokePM_predictions_20060101_20201231.rds")))
