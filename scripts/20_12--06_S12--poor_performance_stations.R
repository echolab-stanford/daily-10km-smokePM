source("./scripts/0_config.R")
library(sf)

states <- tigris::states(cb = TRUE)

epa_ll <- st_read(paste0(data_path, "/epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)

epa_data <- readRDS(paste0(data_path, "/3_intermediate/station_smokePM.rds"))

smokePM_preds <- list.files("./output/smokePM_model",
                            pattern = "smokePM_pred",
                            full.names = TRUE) %>%
  grep("drop\\.", ., value = TRUE) %>%
  map_dfr(function(x){
    readRDS(x) %>%
      mutate(test_fold = as.numeric(substr(x, 41, 41)))
  }) %>%
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

smokePM_data <- readRDS(paste0(data_path, "/4_clean/smokePM_training.rds")) %>% 
  select(id, date, smokePM)

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
         max_smokePM_pred = max(data$smokePM_pred))

set.seed(909)
station_set <- station_performance %>% 
  select(-data) %>%
  ungroup %>% 
  filter(n > 100) %>%
  filter(r2 < 0.1)
  # slice_sample(n = 4)
  # filter(id %in% c(60379034, 410311034, 482030002, 60250005))
  
smokePM_preds %>% 
  filter(fold == test_fold) %>% 
  filter(id %in% station_set$id) %>% 
  left_join(station_set %>% select(id, r2)) %>% 
  left_join(smokePM_data %>% 
              select(id, date, smokePM)) %>% 
  group_by(id) %>% 
  mutate(max_error = abs(smokePM - smokePM_pred) == max(abs(smokePM - smokePM_pred))) %>%
  mutate(year = lubridate::year(date)) %>% 
  {ggplot(data = ., aes(x = smokePM, y = smokePM_pred)) + 
      geom_point(data = filter(., max_error), 
                 size = 3) +
      geom_point(aes(color = as.factor(id))) +
      geom_abline(intercept = 0, slope = 1, color = "grey") +
      geom_text(data = station_set, 
                aes(x = 0.8*max_smokePM, 
                    y = 0.9*max_smokePM_pred,
                    label = paste("R^2==", signif(r2, 3), sep = "")),
                parse = TRUE,
                color = "black") + 
      geom_text(data = station_set, 
                aes(x = 0.8*max_smokePM, 
                    y = 0.75*max_smokePM_pred,
                    label = paste0("n== ", n)),
                parse = TRUE, 
                color = "black") + 
      facet_wrap(~id, scales = "free", 
                 nrow = 2, ncol = 4) +
      theme_classic() + 
      xlab(expression(observed~smoke~PM[2.5])) + 
      ylab(expression(predicted~smoke~PM[2.5])) + 
      theme(legend.position = "none")} %>% 
  ggsave("./figures/supplement/S3_worst_stations.png",.,
         width = 10, height = 4)


worst_obs <- smokePM_preds %>% 
  filter(fold == test_fold) %>% 
  filter(id %in% station_set$id) %>% 
  left_join(smokePM_data %>% 
              select(id, date, smokePM)) %>% 
  group_by(id) %>% 
  mutate(max_error = abs(smokePM - smokePM_pred) == max(abs(smokePM - smokePM_pred)), 
         max_error_date = date[max_error == T],
         start_date = max_error_date - as.difftime(30, units = "days"), 
         end_date = max_error_date + as.difftime(30, units = "days"), 
         .groups = "drop") %>% 
  filter(max_error)
  
epa_data %>% 
  ungroup %>% 
  filter(id %in% station_set$id) %>% 
  left_join(ungroup(worst_obs) %>% 
              select(id, max_error_date, start_date, end_date)) %>% 
  filter(date >= start_date & date <= end_date) %>%
  group_by(id) %>% 
  mutate(max_pm = max(pm25)) %>% 
  ungroup %>% 
  {ggplot(data = .) + 
      geom_line(aes(x = date, y = pm25)) +
      geom_line(aes(x = date, y = smokePM), color = "red") +
      geom_point(data = filter(., date == max_error_date), 
                 aes(x = date, y = pm25), color = "blue") +
      geom_point(data = filter(., smoke_day == 1),
                 mapping = aes(x = date, y = -0.05*max_pm),
                 color = "grey", inherit.aes = FALSE) +
      facet_wrap(~id, scales = "free", 
                 nrow = 2, ncol = 4) +
      ylab(expression(PM[2.5])) + xlab("") + 
      theme_classic() + 
      theme(legend.position = "none")} %>% 
  ggsave("./figures/supplement/S3_worst_obs_timeseries.png",. , 
         width = 10, height = 4)

{ggplot() +
    geom_sf(data = states %>% filter(!(STATEFP %in% nonContig_stateFIPS))) + 
    geom_sf(data = epa_ll %>% filter(id %in% unique(smokePM_data$id)), 
            size = 1) + 
    geom_sf(data = epa_ll %>% filter(id %in% station_set$id),
            mapping = aes(color = as.factor(id)), 
            size = 3) + 
    theme_void() + 
    theme(legend.position = "none")} %>% 
  ggsave("./figures/supplement/S3_poor_performance_stations_map.png",. ,
         width = 5, height = 3.5)

smokePM_data %>% 
  group_by(id) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  summarise(mean_n = mean(n), 
            med_n = median(n))
