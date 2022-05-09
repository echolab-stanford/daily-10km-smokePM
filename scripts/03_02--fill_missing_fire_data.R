years_to_fill <- 2006:2020

source("./scripts/0_config.R")

smoke_missing_dates <- list.files(paste0(data_path, "/smoke_days"), 
                                  full.names = TRUE) %>% 
  map(function(x) readRDS(x) %>% filter(note_smoke_date_not_online) %>% pull(date) %>% unique) %>% 
  Reduce(c, .)

# loop through the years, filling the missing dates
filled <- map_dfr(years_to_fill, function(y){
  fire_dist <- list.files(paste0(data_path,"/distance_to_fire_cluster"), 
             pattern = as.character(y),
             full.names = TRUE) %>% 
    map_dfr(function(x) readRDS(x)) %>% 
    mutate(note_smoke_date_not_online = (date %in% smoke_missing_dates),
           assumed_no_clusters = note_fire_date_not_online & !note_smoke_date_not_online, 
           km_dist = ifelse(note_fire_date_clusters_too_small | assumed_no_clusters, NA, km_dist), 
           area = ifelse(note_fire_date_clusters_too_small | assumed_no_clusters, 0, area), 
           num_points = ifelse(note_fire_date_clusters_too_small | assumed_no_clusters, 0, num_points)) 
  
  fire_fill <- fire_dist %>%
    arrange(id_grid, date) %>% 
    group_by(id_grid) %>%
    mutate(across(.cols = c(km_dist, area, num_points), 
                  .fns = list(lag1 = ~lag(.x, 1), 
                              lag2 = ~lag(.x, 2), 
                              lead1 = ~lead(.x, 1), 
                              lead2 = ~lead(.x, 2)))) %>% 
    filter(is.na(area) | is.na(num_points)) %>% 
    rowwise %>% 
    mutate(km_dist = case_when(!is.na(km_dist_lag1) & !is.na(km_dist_lead1) ~ median(c(km_dist_lag1, km_dist_lead1), na.rm = T), 
                                    T ~ median(c(km_dist_lag2, km_dist_lag1, km_dist_lead1, km_dist_lead2), na.rm = T)),
           area = case_when(!is.na(area_lag1) & !is.na(area_lead1) ~ median(c(area_lag1, area_lead1)), 
                                 T ~ median(c(area_lag2, area_lag1, area_lead1, area_lead2), na.rm = T)), 
           num_points = case_when(!is.na(num_points_lag1) & !is.na(num_points_lead1) ~ median(c(num_points_lag1, num_points_lead1), na.rm = T), 
                                       T ~ median(c(num_points_lag2, num_points_lag1, num_points_lead1, num_points_lead2), na.rm = T))) %>% 
    ungroup() %>% 
    select(id_grid, date, km_dist, area, num_points)
  
  fire_dist %>% 
    filter(note_fire_date_clusters_too_small | assumed_no_clusters) %>% 
    select(id_grid, date, km_dist, area, num_points) %>% 
    rbind(fire_fill) %>% 
    return
})

saveRDS(filled, paste0(data_path, "/3_intermediate/filled_fire.rds"))