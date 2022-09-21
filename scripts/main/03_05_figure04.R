source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots annual smoke PM.
# ------------------------------------------------------------------------------
# Load smoke PM predictions
smokePM_preds <- readRDS(file.path(path_output, "smokePM", "predictions", "combined", "smokePM_predictions_20060101_20201231.rds")) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

annual_smokePM <- smokePM_preds %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(grid_id_10km, year) %>% 
  summarise(smokePM = sum(smokePM_pred), .groups = "drop") %>% 
  # pivot wide, then back to long, to fill any missing years with zeros
  pivot_wider(values_from = smokePM, names_from = year, names_prefix = "year_", values_fill = 0) %>% 
  pivot_longer(starts_with("year"), values_to = "smokePM", names_to = "year", names_prefix = "year_") %>% 
  # identify leap years for dividing by 365
  mutate(year = as.numeric(year)) %>% 
  mutate(y_days = 365 + lubridate::leap_year(year)*1, 
         smokePM = smokePM/y_days)

# small miniatures of annual smoke PM
right_join(grid_10km,
           annual_smokePM %>% 
             mutate(smokePM = pmin(smokePM, 5)),
           by = c("ID" = "grid_id_10km")) %>%
  {ggplot(data = ., aes(color = smokePM, fill = smokePM)) +
      geom_sf() + 
      facet_wrap(~year, nrow = 3, ncol = 5) +
      scale_color_gradientn(colors = viridis::inferno(100, begin = 0, end = 1, direction = 1),
                            name = expression(paste(mu, "g/", m^3)),
                            aesthetics = c("fill", "color"), 
                            values = scales::rescale(sinh(seq(0, 3, length.out = 100))),
                            breaks = seq(0, 5, by = 1),
                            labels = c(seq(0, 4, by = 1), ">5"),
                            guide = guide_colorbar(barheight = 10, 
                                                   title.theme = element_text(size = 16))) +
      theme_void() + 
      theme(text = element_text(size = 20))} %>% 
  ggsave(file.path(path_figures, "figure04.png"), ., width = 15, height = 7)
