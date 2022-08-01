source("./scripts/0_config.R")
library(sf)

plumes <- readRDS(paste0(data_path, "/3_intermediate/all_smoke_days.rds"))
smoke <- readRDS(paste0(data_path, "/3_intermediate/all_smoke_days_incl_cloudy.rds"))
epa_data <- readRDS(paste0(data_path, "/3_intermediate/station_smokePM.rds")) %>% ungroup
epa_ll <- st_read(paste0(data_path, "/epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)

counties <- tigris::counties(cb = TRUE)
states <- tigris::states(cb = TRUE)

# select stations to show timeseries for
set.seed(20202)
station_set <- epa_data %>%
  filter(date >= as.Date("2020-01-01")) %>% 
  group_by(id) %>% 
  summarise(n = n(), 
            max_PM = max(pm25)) %>% 
  filter(n > 100, max_PM > 50) %>% 
  slice_sample(n = 4) %>% 
  pull(id)

# identify the counties that the stations fall in for labeling 
station_loc <- cbind(id = epa_ll %>% 
                       filter(id %in% station_set) %>% 
                       pull(id), 
                     counties[st_intersects(epa_ll %>% 
                                              filter(id %in% station_set), 
                                            counties %>% st_transform(st_crs(epa_ll))) %>% 
                                unlist, ]) %>% 
  left_join(states %>% select(STATEFP, STUSPS) %>% st_drop_geometry) %>% 
  select(id, NAME, STUSPS)

epa_data %>% 
  filter(id %in% station_set) %>% 
  filter(date >= as.Date("2020-01-01")) %>% 
  left_join(plumes %>% 
              rename(plume = smoke_day) %>% 
              filter(grid_id_10km %in% unique(epa_ll$grid_id_10km))) %>% 
  mutate(smoke_def = case_when(smoke_day == 1 & plume == 1 ~ "plume", 
                               smoke_day == 1 & is.na(plume) ~ "hysplit + aod", 
                               smoke_day == 0 ~ as.character(NA))) %>% 
  left_join(station_loc %>% st_drop_geometry()) %>% 
  mutate(panel = paste0(NAME, " County, ", STUSPS)) %>% 
  group_by(id) %>% 
  mutate(max_pm = max(pm25)) %>%
  {ggplot(data = ., aes(x = date, y = pm25)) + 
      geom_line() +
      geom_point(data = filter(., smoke_day == 1), 
                 mapping = aes(x = date, y = -0.05*max_pm, 
                               color = smoke_def),
                 size = 0.9) + 
      geom_point(data = filter(., panel == "Douglas County, NE" & date == as.Date("2020-07-04")), 
                 color = "blue", size = 0.9) + 
      scale_color_manual(name = "", values = c("grey", "#BD3106")) +
      facet_wrap(~panel, ncol = 1, scales = "free_y") + 
      xlab("") + ylab(expression(PM[2.5])) +
      theme_classic()} %>% 
  ggsave("./figures/supplement/SX_station_timeseries.png", ., 
         width = 5, height = 6)
