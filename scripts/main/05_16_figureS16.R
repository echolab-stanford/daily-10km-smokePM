source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 16.
# ------------------------------------------------------------------------------
states <- tigris::states(cb = T) %>% filter(STATEFP %in% nonContig_stateFIPS == FALSE)

mtbs <- read_sf(file.path(path_data, "mtbs_perimeter_data"))
mtbs %<>% st_make_valid()
mtbs$area <- unclass(st_area(mtbs))/1e6

mtbs %>%
  st_simplify(dTolerance = 0.0005) %>%
  mutate(year = lubridate::year(Ig_Date),
         state = substr(Event_ID, 1, 2)) %>%
  filter(state %in% c("AK", "HI") == FALSE) %>%
  filter(year >= 2006) %>%
  group_by(state) %>%
  slice_max(BurnBndAc, n = 3) %>%
  {ggplot(data = .) + 
      geom_sf(data = states, lwd = 0.05) + 
      geom_sf(color = "red", fill = "red") + 
      facet_wrap(~year, nrow = 3, ncol = 5) + 
      theme_void() + 
      theme(text = element_text(size = 20))} %>% 
  ggsave(file.path(path_figures, "figureS16.png"), 
         ., width = 15, height = 7)
