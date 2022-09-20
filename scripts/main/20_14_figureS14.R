# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 14.
# ------------------------------------------------------------------------------
states <- tigris::states(cb = T) %>% filter(STATEFP %in% nonContig_stateFIPS == FALSE)

mtbs <- read_sf(file.path(path_data, "mtbs_perimeter_data"))
mtbs %<>% st_make_valid()

state_fires <- st_intersection(mtbs, states)

test <- state_fires

test$state_area = st_area(test$geometry)

state_fire_trend <- test %>% st_drop_geometry() %>%
  mutate(year = lubridate::year(Ig_Date)) %>% 
  group_by(year, STUSPS) %>% 
  summarise(n_fire = n(), 
            area_fire = sum(state_area)) %>% 
  mutate(period = case_when(year >= 2006 & year <= 2010 ~ "2006_2010", 
                            year >= 2016 & year <= 2020 ~ "2016_2020", 
                            T ~ as.character(NA))) %>% 
  filter(!is.na(period)) %>% 
  group_by(period, STUSPS) %>%
  summarise(n_fire = mean(n_fire), 
            area_fire = mean(area_fire)) %>% 
  pivot_wider(names_from = period, 
              values_from = contains("fire")) %>% 
  mutate(pct_change_fire_area = (area_fire_2016_2020 - area_fire_2006_2010)/area_fire_2006_2010)

states %>% 
  left_join(state_fire_trend) %>% 
  mutate(pct_change_fire_area = unclass(pct_change_fire_area)) %>%
  {ggplot(data =., aes(fill = pct_change_fire_area)) + 
      geom_sf() + 
      scale_fill_gradient2() +
      labs(fill = "Percent change\nin burned area") +
      theme_void() + 
      theme(legend.position = c(0.92, 0.32))} %>% 
  ggsave(file.path(path_figures, "figureS14.png"), ., 
         width = 6.75, height = 4)
