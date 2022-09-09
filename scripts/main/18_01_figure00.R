# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots graphical abstract.
# ------------------------------------------------------------------------------
# Load 10 km grid
grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

# Load CONUS states
simple_states <- tigris::states(cb = TRUE) %>% 
  filter(!(STATEFP %in% nonContig_stateFIPS)) %>% 
  st_transform(crs = st_crs(grid_10km))

# Load smokePM predictions
smokePM_preds <- readRDS(file.path(path_output, "smokePM_predictions_20060101_20201231.rds")) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

# Calculate decadal change
decadal_change <- smokePM_preds %>% 
  mutate(year = lubridate::year(date)) %>% 
  # Calculate annual avg contribution of smokePM to daily PM
  group_by(year, grid_id_10km) %>% 
  summarise(annual_total_smokePM = sum(smokePM_pred), 
            annual_days_over50 = sum(smokePM_pred > 50), 
            annual_days_over100 = sum(smokePM_pred > 100), 
            .groups = "drop") %>% 
  mutate(y_days = 365 + leap_year(year)*1, 
         annual_daily_smokePM = annual_total_smokePM/y_days) %>%
  select(-annual_total_smokePM) %>% 
  # Join with full set of years and grid cells to fill in zeros
  {left_join(expand.grid(year = unique(.$year),
                         grid_id_10km = unique(.$grid_id_10km)), 
             .)} %>% 
  replace_na(list(annual_daily_smokePM = 0, 
                  annual_days_over50 = 0, 
                  annual_days_over100 = 0)) %>% 
  # Add the time periods to summarize over 
  mutate(period = case_when(year >= 2006 & year <=2010 ~ "years2006_2010", 
                            year >= 2016 & year <=2020 ~ "years2016_2020",
                            T ~ as.character(NA))) %>% 
  filter(!is.na(period)) %>% 
  # Average the annual daily smokePM to the period
  group_by(period, grid_id_10km) %>% 
  summarise(across(starts_with("annual"), mean),
            .groups = "drop") %>%  
  # Make a row for each metric, then column for each period, and difference the 
  # columns
  pivot_longer(starts_with("annual")) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  mutate(decade_change = years2016_2020 - years2006_2010) %>%
  select(-starts_with("years")) %>% 
  pivot_wider(names_from = name, values_from = decade_change)

# Plot the decadal change in average smokePM
decadal_change %>% 
  left_join(grid_10km %>% select(grid_id_10km = ID)) %>% 
  st_as_sf %>% 
  mutate(annual_daily_smokePM = pmin(5, annual_daily_smokePM)) %>%
  {ggplot(data = ., 
          aes(color = annual_daily_smokePM,
              fill = annual_daily_smokePM)) + 
      geom_sf() + 
      geom_sf(data = simple_states, 
              color = "grey20", lwd = 0.1, fill = NA) + 
      scale_color_gradientn(aesthetics = c("color", "fill"), 
                            name = "", 
                            colors =  cmocean::cmocean("balance",
                                                       start = 0.05, 
                                                       end = 0.95)(101),
                            values = scales::rescale(sinh(seq(-2, 2, length.out = 101))),
                            rescaler = mid_rescaler(0),
                            guide = guide_colorbar(barheight = 5), 
                            breaks = seq(0, 5, by = 1),
                            labels = c(seq(0, 4, by = 1), ">5")) +
      theme_void() + 
      theme(legend.position = c(.87, 0.28),
            legend.justification = "left", 
            legend.title = element_text(size = 10))} %>% 
  ggsave(file.path(path_figures, "figure00a.png"), ., width = 5, height = 3)

# Plot the decadal change in extreme days   
decadal_change %>% 
  left_join(grid_10km %>% select(grid_id_10km = ID)) %>% 
  st_as_sf %>% 
  mutate(annual_days_over50 = pmin(10, annual_days_over50)) %>%
  {ggplot(data = ., 
          aes(color = annual_days_over50,
              fill = annual_days_over50)) + 
      geom_sf() + 
      geom_sf(data = simple_states, 
              color = "grey20", lwd = 0.1, fill = NA) + 
      scale_color_gradientn(aesthetics = c("color", "fill"), 
                            name = "",
                            colors = rev(colorRampPalette(RColorBrewer::brewer.pal(11, "BrBG"))(101)),
                            values = scales::rescale(sinh(seq(-2, 2, length.out = 101))),
                            rescaler = mid_rescaler(0),
                            guide = guide_colorbar(barheight = 5),
                            breaks = seq(-5, 10, by = 5),
                            labels = c(seq(-5, 5, by = 5), ">10")) +
      theme_void() + 
      theme(legend.position = c(.87, 0.28),
            legend.justification = "left", 
            legend.title = element_text(size = 10))} %>% 
  ggsave(file.path(path_figures, "figure00b.png"), ., width = 5, height = 3)
