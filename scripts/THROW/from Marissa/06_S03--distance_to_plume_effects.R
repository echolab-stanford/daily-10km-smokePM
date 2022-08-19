library(tidyverse)
library(magrittr)
library(layer)
library(sf)
library(splines)
library(fixest)

source("scripts/0_config.R")

epa_data <- readRDS(paste0(data_path, "/3_intermediate/station_smokePM.rds")) %>% 
  ungroup

grid_10km <- st_read(paste0(data_path, "/1_grids/grid_10km_wgs84")) %>% 
  mutate(grid_index = 1:n())
monitors <- st_read(paste0(data_path, "/epa_station_locations")) %>% 
  filter(id %in% unique(epa_data$id)) %>% 
  mutate(monitor_index = 1:n())
states <- tigris::states() %>% st_transform(st_crs(monitors))

monitors$state <- states$STUSPS[st_intersects(monitors, states) %>% unlist]

Sys.time()
cell_monitor_dist <- data.frame(start = c(seq(0, nrow(grid_10km), by = 2000), nrow(grid_10km)) + 1) %>%
  mutate(end = lead(start) - 1) %>%
  filter(!is.na(end)) %>%
  purrr::pmap(function(start, end){
    print(Sys.time())
    st_distance(grid_10km[start:end,], monitors) %>%
      return
  }) %>%
  do.call(what = rbind)
Sys.time()

smoke_days <- readRDS(paste0(data_path, "/3_intermediate/all_smoke_days_incl_cloudy.rds"))  %>%
  left_join(readRDS(paste0(data_path, "/3_intermediate/all_smoke_days.rds")) %>%
              rename(plume_day = smoke_day) %>%
              select(-note_smoke_date_not_online)) %>%
  replace_na(list(plume_day = 0))

smoke_missing_dates <- list.files(paste0(data_path, "/smoke_days"), 
                                  full.names = TRUE) %>% 
  map(function(x) readRDS(x) %>% filter(note_smoke_date_not_online) %>% pull(date) %>% unique) %>% 
  Reduce(c, .)


# add distance to smoke-day to epa data
monitor_smoke_dist <- epa_data %>%
  left_join(monitors %>% st_drop_geometry() %>% select(id, monitor_index)) %>%
  select(id, grid_id_10km, monitor_index, date, pm25) %>%
  nest_by(date) %>%
  rename(monitor_obs = data) %>%
  left_join(smoke_days %>%
              left_join(grid_10km %>% st_drop_geometry %>% select(grid_id_10km = ID, grid_index)) %>%
              nest_by(date) %>%
              rename(smoke_days = data)) %>%
  purrr::pmap_dfr(function(date, monitor_obs, smoke_days){
    if(is.null(smoke_days)){
      plume_dist = Inf
      smoke_dist = Inf
    } else{
      plume_dist <- cell_monitor_dist[smoke_days %>%
                                        filter(plume_day == 1) %>%
                                        pull(grid_index),
                                      monitor_obs %>%
                                        pull(monitor_index)] %>%
        matrix(ncol = nrow(monitor_obs)) %>%
        matrixStats::colMins()
      smoke_dist <- cell_monitor_dist[smoke_days %>%
                                        filter(smoke_day == 1) %>%
                                        pull(grid_index),
                                      monitor_obs %>%
                                        pull(monitor_index)] %>%
        matrix(ncol = nrow(monitor_obs)) %>%
        matrixStats::colMins()
    }
    monitor_obs %>%
      mutate(date = date) %>%
      cbind(plume_dist = plume_dist/1e3) %>%
      cbind(smoke_dist = smoke_dist/1e3) %>%
      return
  })

saveRDS(monitor_smoke_dist, 
        "./output/monitor_smoke_distance.rds")  

# monitor_smoke_dist <- readRDS("./output/monitor_smoke_distance.rds")

reg_df <- monitor_smoke_dist %>% 
  left_join(monitors %>% st_drop_geometry() %>%
              select(id, state)) %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  mutate(plume_dist_factor = case_when(plume_dist == 0 ~ 0,
                                       plume_dist > 0 & plume_dist <= 100 ~ 1,
                                       plume_dist > 100 & plume_dist <= 250 ~ 2,
                                       plume_dist > 250 & plume_dist <= 500 ~ 3,
                                       plume_dist > 500 & plume_dist <= 750 ~ 4,
                                       plume_dist > 750 & plume_dist <= 1000 ~ 5,
                                       plume_dist > 1000 ~ 6),
         smoke_dist_factor = case_when(smoke_dist == 0 ~ 0,
                                       smoke_dist > 0 & smoke_dist <= 100 ~ 1,
                                       smoke_dist > 100 & smoke_dist <= 250 ~ 2,
                                       smoke_dist > 250 & smoke_dist <= 500 ~ 3,
                                       smoke_dist > 500 & smoke_dist <= 750 ~ 4,
                                       smoke_dist > 750 & smoke_dist <= 1000 ~ 5,
                                       smoke_dist > 1000 ~ 6)) %>% 
  mutate(across(ends_with("dist_factor"), function(x){ recode_factor(x, 
                                                                     `0` = "overhead",
                                                                     `1` = "0 - 100",
                                                                     `2` = "100 - 250",
                                                                     `3` = "250 - 500",
                                                                     `4` = "500 - 750",
                                                                     `5` = "750 - 1000",
                                                                     `6` = "1000+",
                                                                     .ordered = TRUE)}))

# add in meteorology (temperature, dewpoint temperature, precipitation)
epa_station_grid_cells <- epa_data$grid_id_10km %>% unique
meteor <- c(grep("temperature|precipitation|wind", 
                 list.files(paste0(data_path, "/ERA5_variables/Land"), full.names = T), 
                 value = T),
            grep("boundary_layer", 
                 list.files(paste0(data_path, "/ERA5_variables/Global"), full.names = T), 
                 value = T)) %>% 
  paste0("/USA/10km_grid/") %>%
  {paste0(., ifelse(grepl("precipitation", .), "UTC+0000", "UTC-0600"))} %>% 
  list.files(full.names = T) %>% 
  map(function(x) {
    print(x)
    type = strsplit(x, "/") %>% 
      extract2(1) %>% 
      extract(14) %>% 
      gsub("daily_|_of_1-hourly", "", .)
    print(type)
    list.files(x, full.names = T) %>% 
      purrr::map_dfr(function(y){
        readRDS(y) %>% 
          filter(id_grid %in% epa_station_grid_cells) %>%
          rename_with(.fn = function(z){paste0(gsub("2m_|10m_", "", z), "_", type)}, 
                      .cols = !c(date, id_grid))
      })
    }) %>% 
  reduce(full_join)

reg_df %<>% left_join(meteor, by = c("grid_id_10km" = "id_grid", "date" = "date"))

# saveRDS(reg_df,
#         "./output/monitor_smoke_distance_w_met.rds")

# reg_df <- readRDS("./output/monitor_smoke_distance_w_met.rds")


etable(feols(pm25 ~ i(plume_dist_factor, ref = "1000+") | 
               id^month + state^year^month, 
             data = reg_df),
       feols(pm25 ~ i(smoke_dist_factor, ref = "1000+") | 
               id^month + state^year^month, 
             data = reg_df),
       feols(pm25 ~ i(plume_dist_factor, ref = "1000+") + 
               ns(temperature_mean, 5) + ns(dewpoint_temperature_mean, 5) + 
               ns(total_precipitation_maximum, 5) + ns(boundary_layer_height_maximum, 5) +
               ns(boundary_layer_height_mean, 5) + ns(boundary_layer_height_minimum, 5) +
               temperature_mean:total_precipitation_maximum | 
               id^month + state^year^month, 
             data = reg_df),
       feols(pm25 ~ i(smoke_dist_factor, ref = "1000+") + 
               ns(temperature_mean, 5) + ns(dewpoint_temperature_mean, 5) + 
               ns(total_precipitation_maximum, 5) + ns(boundary_layer_height_maximum, 5) +
               ns(boundary_layer_height_mean, 5) + ns(boundary_layer_height_minimum, 5) +
               temperature_mean:total_precipitation_maximum | 
               id^month + state^year^month, 
             data = reg_df), 
       keep = "dist_factor",
       subtitles = c("plumes", "plumes + hysplit", "plumes w meteorology", "plumes + hysplit w meteorology"),
       tex = TRUE)

feols(pm25 ~ i(smoke_dist_factor, ref = "1000+") + 
        ns(temperature_mean, 5) + ns(dewpoint_temperature_mean, 5) + 
        ns(total_precipitation_maximum, 5) + ns(boundary_layer_height_maximum, 5) +
        ns(boundary_layer_height_mean, 5) + ns(boundary_layer_height_minimum, 5) +
        temperature_mean:total_precipitation_maximum | 
        id^month + state^year^month, 
      data = reg_df %>% 
        filter(!(date %in% smoke_missing_dates)) %>% 
        filter(is.finite(plume_dist))) 

# mod0 <- feols(pm25 ~ i(smoke_dist_factor, ref = "1500+") | 
#         id^month + state^year^month, 
#       data = temp, 
#       se = "twoway")
# mod_met <- feols(pm25 ~ i(smoke_dist_factor, ref = "1500+") + 
#                    ns(temperature_mean, 5) + ns(dewpoint_temperature_mean, 5) + 
#                    ns(total_precipitation_maximum, 5) + ns(boundary_layer_height_maximum, 5) +
#                    ns(boundary_layer_height_mean, 5) + ns(boundary_layer_height_minimum, 5) +
#                    temperature_mean:total_precipitation_maximum | 
#                 id^month + state^year^month, 
#               data = temp, 
#               se = "twoway")
# 
# rbind(summary(mod0, se = "twoway")$coeftable %>% 
#         rownames_to_column() %>%  
#         mutate(fit = "mod0"),
#       summary(mod_met, se = "twoway")$coeftable %>% 
#         rownames_to_column() %>%  
#         mutate(fit = "mod_met")) %>% 
#   filter(grepl("smoke", rowname)) %>%
#   transmute(rowname = rowname, est = Estimate, 
#             se = `Std. Error`, fit = fit) %>%
#   mutate(panel = "estimates") %>% 
#   mutate(smoke_dist_bin = gsub("smoke_dist_factor::", "", rowname), 
#          smoke_dist_bin = factor(smoke_dist_bin, levels = unique(smoke_dist_bin)),
#          lwr = est + qnorm(0.025)*se, 
#          upr = est + qnorm(0.975)*se) %>% 
#   {ggplot(data = ., 
#           aes(x = smoke_dist_bin, y = est,
#              color = fit)) + 
#   geom_point() +
#   geom_linerange(aes(ymin = lwr, ymax = upr)) +
#   # geom_ribbon(aes(x = as.numeric(smoke_dist_bin), ymax = est, fill = fit),
#   #             ymin = 0,
#   #             alpha = 0.5, inherit.aes = FALSE) +
#   theme_classic() + 
#       theme(legend.position = c(0.8, 0.8)) + 
#   scale_color_manual("", values = c("red", "blue"), labels = c("no controls", "meteorology controls")) + 
#   xlab("Distance to smoke (km)") + ylab("Estimated effect (ug/m^3)")} %>% 
#   ggsave("./figures/distance_to_plume_effect.png", ., 
#           width = 5, height = 3)
# 
# smoke_day_lags <- purrr::map(-3:3, function(x){
#   smoke_days %>% 
#     filter(grid_id_10km %in% unique(temp$grid_id_10km)) %>% 
#     select(-note_smoke_date_not_online) %>%
#     mutate(date = date + as.difftime(x, units = "days")) %>% 
#     rename_with(function(y){paste0(y, "_", ifelse(x < 0, "lead", "lag"), abs(x))}, contains("day"))
# }) %>% reduce(full_join) 
# 
# 
# temp %<>% 
#   left_join(smoke_day_lags)  %>% 
#   mutate(across(c(contains("smoke_day"), contains("plume_day")),
#                 ~ ifelse(is.na(.x), 0, .x)))
# 
# feols(pm25 ~ smoke_day_lag3 + smoke_day_lag2 + smoke_day_lag1 + smoke_day_lag0 + 
#         smoke_day_lead1 + smoke_day_lead2 + smoke_day_lead3 + 
#         ns(temperature_mean, 5) + ns(dewpoint_temperature_mean, 5) + 
#         ns(total_precipitation_maximum, 5) + ns(boundary_layer_height_maximum, 5) +
#         ns(boundary_layer_height_mean, 5) + ns(boundary_layer_height_minimum, 5) +
#         temperature_mean:total_precipitation_maximum | 
#         id^month + state^year^month, 
#       data = temp, 
#       se = "twoway")
# 
# 
# feols(pm25 ~ plume_day_lag3 + plume_day_lag2 + plume_day_lag1 + plume_day_lag0 + 
#         plume_day_lead1 + plume_day_lead2 + plume_day_lead3 + 
#         ns(temperature_mean, 5) + ns(dewpoint_temperature_mean, 5) + 
#         ns(total_precipitation_maximum, 5) + ns(boundary_layer_height_maximum, 5) +
#         ns(boundary_layer_height_mean, 5) + ns(boundary_layer_height_minimum, 5) +
#         temperature_mean:total_precipitation_maximum | 
#         id^month + state^year^month, 
#       data = temp, 
#       se = "twoway")