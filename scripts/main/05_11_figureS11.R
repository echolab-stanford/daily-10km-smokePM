source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots station MODIS landcover.
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

epa_ll <- st_read(file.path(path_data, "epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)

epa_data <- readRDS(file.path(path_data, "3_intermediate", "station_smokePM.rds")) 

states <- tigris::states(cb = TRUE)

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

# plot station land cover spatially 
epa_ll %>% 
  filter(id %in% station_performance$id) %>% 
  left_join(station_lc) %>% 
  {ggplot(data = .) + 
      geom_sf(data = states  %>% filter(!(STATEFP %in% nonContig_stateFIPS)),
              color = "grey10", fill = "grey95") + 
      geom_sf(aes(color = case_when(Type5 == 11 ~ "barren",
                                    Type5 == 5 ~ "shrubland", 
                                    T ~ "other"),
                  size = I(ifelse(Type5 %in% c(5, 11), 1.3, 0.9)))) + 
      scale_color_manual(name = "landcover", 
                         values = c("#6e948c", "black", "#c38f16")) + 
      theme_void()} %>% 
  ggsave(file.path(path_figures, "figureS11.png"), ., width = 6, height = 4)

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
