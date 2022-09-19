# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Aggregates 10 km grid smokePM predictions to county and census tract levels.
# ------------------------------------------------------------------------------
unit <- "tract" # alternatively, "tract"
# load shapefile, plus 10km grid transformed to match the crs
if(unit == "county"){
  unit_sf <- counties() %>% 
    filter(STATEFP %in% nonContig_stateFIPS == F)
} else if(unit == "tract"){
  unit_sf <- states() %>% 
    filter(STATEFP %in% nonContig_stateFIPS == F) %>% 
    pull(STATEFP) %>% 
    map_dfr(function(x){
      tracts(x, year = 2019) %>% select(STATEFP, GEOID)
    })
} else{
  stop("only allowed units are \"tract\" or \"county\"")
}

# read in the grid
grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84")) %>% 
  st_transform(st_crs(unit_sf))

# make a crosswalk with intersection area with grid cells
unit_cross = st_intersection(unit_sf,
                             grid_10km) %>% 
  select(GEOID, grid_id_10km = ID) %>% 
  {cbind(st_drop_geometry(.), 
         area = st_area(.))} 

# save the crosswalk since it takes a while to make 
saveRDS(unit_cross, file.path(path_data, paste0(unit, "_grid_area_crosswalk.rds")))

# population by grid cell
pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv)

# smoke PM predictions 
smokePM <- readRDS(file.path(path_output, "smokePM_predictions_20060101_20201231.rds"))

# lets only save predictions if there's a smoke day in the unit, start by identifying smoke-days per unit
unit_smoke_days = smokePM %>% # 51434138 rows
  # add unit information, this will duplicate any rows that are in multiple counties
  left_join(unit_cross %>% select(grid_id_10km, GEOID),
            by = "grid_id_10km") %>% # 76009188 rows for county 
  filter(!is.na(GEOID)) %>% # drop grid cells that don't match to a unit
  # full set of unit-days with smoke 
  select(date, GEOID) %>% 
  unique  # 2308941 rows (should actually be less after dropping NAs)

unit_smokePM <- unit_smoke_days %>% 
  # join in all grid-cells for each unit
  left_join(unit_cross, by = "GEOID") %>% # 119622779 rows
  # join in population and smoke PM predictions 
  left_join(pop %>% select(grid_id_10km = ID, grid_pop_per_m2 = mean)) %>%
  left_join(smokePM) # should still be 119622779 rows

# fill missings with 0s
# calculate pop-weighted avg (density * area) over grid cells in each unit
avg_unit_smokePM <- unit_smokePM %>% 
  replace_na(list(smokePM_pred = 0)) %>%
  mutate(area = unclass(area), 
         pop = grid_pop_per_m2*area) %>%
  group_by(GEOID, date) %>% 
  summarise(smokePM_pred = weighted.mean(smokePM_pred, pop)) %>% 
  ungroup

saveRDS(ungroup(avg_unit_smokePM), 
        file.path(path_output, paste0(unit, "_smokePM_predictions_20060101_20201231.rds")))
