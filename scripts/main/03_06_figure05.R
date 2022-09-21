source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots state trends.
# ------------------------------------------------------------------------------
# read in grid and load state geometries (a detailed one for calcs and generalized one for plotting)
grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))
states <- tigris::states(cb = FALSE) %>% 
  filter(!(STATEFP %in% nonContig_stateFIPS)) %>% 
  st_transform(crs = st_crs(grid_10km))
simple_states <- tigris::states(cb = TRUE) %>% 
  filter(!(STATEFP %in% nonContig_stateFIPS)) %>% 
  st_transform(crs = st_crs(grid_10km))

# load predictions and population data
smokePM_preds <- readRDS(file.path(path_output, "smokePM", "predictions", "combined", "smokePM_predictions_20060101_20201231.rds")) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv)

# calculate decadal change in average smoke PM 
decadal_change <- smokePM_preds %>% 
  mutate(year = lubridate::year(date)) %>% 
  # calculate annual avg contribution of smokePM to daily PM
  group_by(year, grid_id_10km) %>% 
  summarise(annual_total_smokePM = sum(smokePM_pred), 
            .groups = "drop") %>% 
  mutate(y_days = 365 + leap_year(year)*1, 
         annual_daily_smokePM = annual_total_smokePM/y_days) %>% 
  select(-annual_total_smokePM) %>%
  # join with full set of years and grid cells to fill in zeros
  {left_join(expand.grid(year = unique(.$year),
                         grid_id_10km = unique(.$grid_id_10km)), 
             .)} %>% 
  replace_na(list(annual_daily_smokePM = 0)) %>% 
  # add the time periods to summarise over 
  mutate(period = case_when(year >= 2006 & year <=2010 ~ "years2006_2010", 
                            year >= 2016 & year <=2020 ~ "years2016_2020",
                            T ~ as.character(NA))) %>% 
  filter(!is.na(period)) %>% 
  # average the annual daily smokePM to the period
  group_by(period, grid_id_10km) %>% 
  summarise(annual_daily_smokePM = mean(annual_daily_smokePM),
            .groups = "drop") %>% 
  # make a  column for each period, and difference the columns
  pivot_wider(names_from = period, values_from = annual_daily_smokePM) %>% 
  mutate(decade_change = years2016_2020 - years2006_2010) 

# plot a map of the decadal change
grid_10km %>% 
  left_join(decadal_change, 
            by = c("ID" = "grid_id_10km")) %>% 
  mutate(decade_change = pmin(5, decade_change)) %>%
  {ggplot(data = .) + 
      geom_sf(aes(color = decade_change, fill = decade_change)) + 
      geom_sf(data = simple_states, 
              color = "grey20", lwd = 0.1, fill = NA) + 
      scale_color_gradientn(aesthetics = c("color", "fill"), 
                            name = expression(paste(mu, "g/", m^3)), 
                            colors =  cmocean::cmocean("balance",
                                                       start = 0.05, 
                                                       end = 0.95)(101),
                            values = scales::rescale(sinh(seq(-2, 2, length.out = 101))),
                            rescaler = mid_rescaler(0),
                            guide = guide_colorbar(barheight = 6),
                            breaks = seq(0, 5, by = 1),
                            labels = c(seq(0, 4, by = 1), ">5"),) + 
      theme_void() + 
      theme(legend.position = c(.92, 0.32),
            legend.justification = "center")} %>% 
  ggsave(file.path(path_figures, "figure05a.png"), ., width = 5, height = 4)


state_annual_cells <- states %>% 
  # identify the cells intersecting with each state (the result is the index of the grid cell insecting each state)
  {mutate(., 
          grid_id_10km_ind = st_intersects(., grid_10km))} %>%
  st_drop_geometry() %>% 
  unnest(grid_id_10km_ind) %>%
  # join in the grid cell identifiers based on grid cell index 
  left_join(grid_10km %>% mutate(grid_id_10km_ind = 1:n()) %>% 
              st_drop_geometry()) %>% 
  # join in population density for each 10km grid cell
  left_join(pop %>% 
              mutate(pop_density = mean)) %>% 
  # join in annual smoke averages
  left_join(smokePM_preds %>%
              mutate(year = lubridate::year(date)) %>% 
              group_by(year, grid_id_10km) %>%
              # calculate annual total smoke PM, then divide by days in the year
              summarise(annual_total_smokePM = sum(smokePM_pred), 
                        .groups = "drop") %>% 
              mutate(y_days = 365 + leap_year(year)*1, 
                     annual_daily_smokePM = annual_total_smokePM/y_days) %>% 
              # join with full set of years and grid cells to fill in zeros
              {left_join(expand.grid(year = unique(.$year),
                                     grid_id_10km = unique(.$grid_id_10km)), 
                         .)} %>% 
              replace_na(list(annual_daily_smokePM = 0)), 
            by = c("ID" = "grid_id_10km"))

state_annual_cells %>% 
  group_by(NAME, STUSPS, year) %>% 
  summarise(annual_daily_smokePM = weighted.mean(annual_daily_smokePM, pop_density),
            .groups = "drop") %>% 
  mutate(states = ifelse(NAME %in% c("California", "Florida", 
                                     "New York", "Oregon", "Michigan"), 
                         NAME, 
                         "rest")) %>% 
  {ggplot(mapping = aes(x = year, y = annual_daily_smokePM, 
                        group = NAME)) + 
      geom_line(data = filter(., states == "rest"), 
                color = "grey30", lwd = 0.25, alpha = 0.5) +
      geom_line(data = filter(., states!= "rest"),
                mapping = aes(color = states), 
                lwd = 1.5, alpha = 1) + 
      ylab(expression(paste("smoke ", PM[2.5]," (", mu, "g/", m^3, ")"))) +
      scale_color_manual(values = MetBrewer::met.brewer("VanGogh2", 15)[c(1,3, 7, 9, 15)],
                         guide = "none") +
      theme_classic() } %>% 
  ggsave(file.path(path_figures, "figure05b_time_series.png"),
         ., width = 5, height = 3.5)

simple_states %>%
  mutate(states = ifelse(NAME %in% c("California", "Florida", 
                                     "New York", "Oregon", "Michigan"), 
                         NAME, 
                         "rest")) %>% 
  {ggplot() +
      geom_sf(data = filter(., states == "rest"),
              lwd = 0.1) + 
      geom_sf(data = filter(., states != "rest"),
              mapping = aes(color = states, fill = states),
              lwd = 0.1) + 
      scale_color_manual(values = MetBrewer::met.brewer("VanGogh2", 15)[c(1,3, 7, 9, 15)], #MetBrewer::met.brewer("VanGogh2", 15)[c(1, 4, 9, 14)], 
                         guide = "none", 
                         aesthetics = c("fill", "color")) +
      theme_void()} %>% 
  ggsave(file.path(path_figures, "figure05b_map.png"), 
         ., width = 3, height = 2.5)
