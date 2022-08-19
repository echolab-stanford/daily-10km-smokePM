source("./scripts/0_config.R")
states <- tigris::states(cb = T) %>% filter(STATEFP %in% nonContig_stateFIPS == FALSE)

mtbs <- read_sf("~/Downloads/mtbs_perimeter_data") 
mtbs %<>% st_make_valid()
mtbs$area <- unclass(st_area(mtbs))/1e6

mtbs %>% 
  st_simplify(dTolerance = 0.0005) %>%
  mutate(year = lubridate::year(Ig_Date), 
         state = substr(Event_ID, 1, 2)) %>%
  filter(state %in% c("AK", "HI") == FALSE) %>%
  filter(year >= 2006) %>% 
  # filter(year <= 2010) %>%
  group_by(year) %>% 
  slice_max(BurnBndAc, n = 8) %>% 
  # st_drop_geometry() %>% View
  {ggplot(data = .) + 
      geom_sf(data = states, lwd = 0.05) + 
      geom_sf(color = "red", fill = "red") + 
      facet_wrap(~year, nrow = 3, ncol = 5) + 
      theme_void() + 
      theme(text = element_text(size = 20))} %>% 
  ggsave("./figures/supplement/SX_annual_fires.png", 
         ., width = 15, height = 7)

mtbs %>%
  st_simplify(dTolerance = 0.0005) %>%
  mutate(year = lubridate::year(Ig_Date),
         state = substr(Event_ID, 1, 2)) %>%
  filter(state %in% c("AK", "HI") == FALSE) %>%
  filter(year >= 2006) %>%
  group_by(state) %>%
  slice_max(BurnBndAc, n = 3) %>%
  # st_drop_geometry %>% View
{ggplot(data = .) + 
    geom_sf(data = states, lwd = 0.05) + 
    geom_sf(color = "red", fill = "red") + 
    facet_wrap(~year, nrow = 3, ncol = 5) + 
    theme_void() + 
    theme(text = element_text(size = 20))} %>% 
  ggsave("./figures/supplement/SX_state_fires.png", 
         ., width = 15, height = 7)



  