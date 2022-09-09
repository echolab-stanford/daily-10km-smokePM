# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots California fires 2018-2020.

# calfire from https://frap.fire.ca.gov/frap-projects/fire-perimeters/
# ------------------------------------------------------------------------------
# Load smokePM predictions
smokePM_preds <- list.files(
  file.path(path_output, "smokePM_predictions", "10km_smoke_days"), 
  full.names = TRUE
) %>% 
  map_dfr(readRDS) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

# Load 10 km grid
grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

# Load population density
pop <- list.files(
  file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"),
  full.names = T
) %>% 
  purrr::map_dfr(read.csv) %>% 
  rename(pop_density = mean)

# Define fire panels
fire_panels <- data.frame(
  panel_name = c("November 2018", "October - November 2019", "Fall 2020"), 
  start_date = as.Date(c("2018-11-01", "2019-10-20", "2020-08-10")), 
  end_date = as.Date(c("2018-11-30", "2019-11-15", "2020-11-15"))
)

# Load CAL FIRE data
calfire <- st_read(file.path(path_data, "CAL FIRE FRAP", "fire20_1.gdb"), layer = "firep20_1") %>% 
  mutate(Fire = recode(FIRE_NAME, 
                       "CAMP" = "Camp Fire", 
                       "KINCADE" = "Kincade Fire", 
                       "AUGUST COMPLEX FIRES" = "August Complex", 
                       "CZU LIGHTNING COMPLEX" = "CZU Lightning Complex"),
         panel_name = recode(FIRE_NAME, 
                             "CAMP" = "November 2018",  
                             "KINCADE" = "October - November 2019",
                             "AUGUST COMPLEX FIRES" = "Fall 2020", 
                             "CZU LIGHTNING COMPLEX" = "Fall 2020"), 
         panel_fire_no = recode(FIRE_NAME, 
                                "CAMP" = 1,  
                                "KINCADE" = 1,
                                "AUGUST COMPLEX FIRES" = 2, 
                                "CZU LIGHTNING COMPLEX" = 1, 
                                .default = as.numeric(NA)), 
         select_fires = ((FIRE_NAME == "CAMP" & YEAR_ == "2018" & ALARM_DATE > as.Date("2018-11-01")) |
                           (FIRE_NAME == "KINCADE" & YEAR_ == "2019") |
                           FIRE_NAME  %in% c("AUGUST COMPLEX FIRES", "CZU LIGHTNING COMPLEX")))

# Load counties
counties <- tigris::counties()

# Load states
states <- tigris::states()
states %<>% st_transform(crs = st_crs(grid_10km))

# Limit to California
CA_cells <- st_intersects(grid_10km, states %>% filter(NAME == "California"))
CA_cells <- grid_10km$ID[CA_cells %>% 
                           purrr::map_lgl(function(x){length(x) > 0})]

# Match to counties
county_cells <- counties %>% 
  filter(NAME %in% c("Sacramento", "Santa Clara", "Fresno", "Sonoma")) %>% 
  select(NAME, geometry) %>% 
  purrr::pmap_dfr(function(NAME, geometry){
    data.frame(county = NAME, 
               grid_id_10km = grid_10km$ID[st_intersects(grid_10km, geometry) %>% 
                                             purrr::map_lgl(function(x){length(x) > 0})]) %>% 
      return()
  })

# Sampling of fires from 2018 to 2020 in CA
grid_fire_preds <-  fire_panels %>% 
  purrr::pmap_dfr(function(panel_name, start_date, end_date){
    smokePM_preds %>% filter(grid_id_10km %in% county_cells$grid_id_10km) %>% 
      {right_join(., 
                  expand.grid(grid_id_10km = pull(., grid_id_10km) %>% unique,
                              date = seq.Date(from = start_date, 
                                              to = end_date, 
                                              by = "day")))} %>% 
      replace_na(list(smokePM_pred = 0)) %>% 
      left_join(county_cells) %>% 
      mutate(panel_name = panel_name) %>% 
      return
  }) %>% 
  {mutate(., panel_name = factor(panel_name, levels = unique(.$panel_name),
                                 ordered = TRUE))} 

grid_fire_avgs <- group_by(grid_fire_preds, grid_id_10km, panel_name, county) %>% 
  summarise(total_smokePM = mean(smokePM_pred),
            .groups = "drop") %>% 
  group_by(panel_name, county) %>% 
  mutate(nobs = n()) %>% 
  ungroup %>% 
  mutate(type = factor("average", levels = c("time_series","average"), ordered = T),
         county_no = as.numeric(as.factor(county)))

# Plot time series
CA_fires_ts <- ggplot(mapping = aes(x = date, y = smokePM_pred, color = county)) + 
  # Add lines for each grid cell
  geom_line(data = grid_fire_preds %>% 
              mutate(type = factor("time_series", levels = c("time_series", "average"), ordered = T)) %>% 
              group_by(county) %>% 
              mutate(n_cell = length(unique(grid_id_10km))),
            aes(group = grid_id_10km, 
                alpha = I(20/n_cell))) + 
  # Add mean lines for each county
  geom_line(data = left_join(grid_fire_preds, pop, by = c("grid_id_10km" = "ID")) %>%
              group_by(date, county, panel_name) %>%
              summarise(smokePM_pred = weighted.mean(smokePM_pred, pop_density),
                        .groups = "drop") %>%
              mutate(type = factor("time_series", levels = c("time_series", "average"), ordered = T)),
            size = 1.5) +
  # Add dashes for average smoke PM for grid cell
  geom_linerange(data = grid_fire_avgs, 
                 aes(y = total_smokePM, color = county, 
                     xmin = as.Date(county_no*2, origin = "2000-01-01"), 
                     xmax = as.Date(county_no*2+1.5, origin = "2000-01-01"),
                     alpha = I(20/nobs)),
                 inherit.aes = FALSE) + 
  # Add dashes for average smokePM for county
  geom_linerange(data = left_join(grid_fire_avgs, pop, by = c("grid_id_10km" = "ID")) %>%
                   group_by(panel_name, county, county_no, type) %>%
                   summarise(total_smokePM = weighted.mean(total_smokePM, pop_density)),
                 aes(y = total_smokePM, color = county,
                     xmin = as.Date(county_no*2, origin = "2000-01-01"),
                     xmax = as.Date(county_no*2+1.5, origin = "2000-01-01")),
                 size = 1.5,
                 inherit.aes = FALSE) +
  # Add alpha = 0 points to average plots to get the zeros to align
  geom_point(data = left_join(calfire %>% filter(select_fires),
                              grid_fire_avgs %>% group_by(panel_name) %>% summarise(max = max(total_smokePM))),
             aes(y = -panel_fire_no*0.075*max), 
             x = as.Date(4, origin = "2000-01-01"), 
             inherit.aes = FALSE,
             alpha = 0) +
  # Add line ranges on the bottom for the fires' start and end 
  geom_linerange(data = left_join(calfire %>% filter(select_fires),
                                  grid_fire_preds %>% group_by(panel_name) %>% summarise(max = max(smokePM_pred))) %>% 
                   mutate(panel_name = factor(panel_name, levels = levels(grid_fire_preds$panel_name), ordered = T),
                          type = factor("time_series", levels = c("time_series", "average"), ordered = T)), 
                 aes(xmin = as.Date(ALARM_DATE), 
                     xmax = as.Date(CONT_DATE), 
                     y = -panel_fire_no*0.075*max), 
                 inherit.aes = FALSE) + 
  facet_wrap(panel_name~type, scales = "free", nrow =3, ncol = 2,
             labeller = labeller(.multi_line = FALSE)) +
  scale_color_manual(values = MetBrewer::met.brewer("Juarez")[c(1,6,5,3)],
                     guide = "none") +
  scale_y_continuous(sec.axis = sec_axis(~., 
                                         name=expression(paste("Daily average (",mu, "g/", m^3, ")")))) +
  theme_classic() + xlab("") + ylab(expression(paste("smoke ", PM[2.5]," (",mu, "g/", m^3, ")"))) +
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank(),
        panel.spacing.y = unit(1.5, "lines"), 
        panel.spacing.x = unit(-2, "lines"))

# Make it into a gtable so we can edit
CA_fires_ts <- ggplot_gtable(ggplot_build(CA_fires_ts))

# Make the average column narrower 
CA_fires_ts$widths[9] <- CA_fires_ts$widths[9]*0.25 

# Remove right y axis on left panels and left y axis on right panels and x-axis from 2nd column
CA_fires_ts %<>% gtable_remove_grobs(c("axis-l-1-2", "axis-l-2-2", "axis-l-3-2", 
                                       "axis-r-1-1", "axis-r-2-1", "axis-r-3-1",
                                       "axis-b-2-1", "axis-b-2-2", "axis-b-2-3")) 

# Repeat outside y axes on all plots 
CA_fires_ts %>% 
  gtable::gtable_add_grob(gtable::gtable_filter(CA_fires_ts, pattern = "ylab-l"),
                          l = 3, t = 7, b = 8,
                          name = "top-ylab-l") %>% 
  gtable::gtable_add_grob(gtable::gtable_filter(CA_fires_ts, pattern = "ylab-l"),
                          l = 3, t = 17, b = 18,
                          name = "bottom-ylab-l") %>% 
  gtable::gtable_add_grob(gtable::gtable_filter(CA_fires_ts, pattern = "ylab-r"),
                          l = 11, t = 7, b = 8,
                          name = "top-ylab-r") %>% 
  gtable::gtable_add_grob(gtable::gtable_filter(CA_fires_ts, pattern = "ylab-r"),
                          l = 11, t = 17, b = 18,
                          name = "bottom-ylab-r") %>% 
  ggsave(file.path(path_figures, "figure03b-d_time_series.png"), ., width = 6, height = 7)

# Plot map of select CA fires
{ggplot() +
    geom_sf(data = states %>% filter(NAME == "California")) +
    geom_sf(data = counties %>%
              filter(NAME %in% c("Sacramento", "Santa Clara", "Fresno", "Sonoma")),
            aes(color = NAME, fill = NAME)) +
    scale_color_manual(values = MetBrewer::met.brewer("Juarez")[c(1,6,5,3)],
                       aesthetics = c("color", "fill")) +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_color() +
    geom_sf(data = calfire %>% filter(select_fires),
            color = NA,
            fill = "black") +
    theme_void()} %>%
  ggsave(file.path(path_figures, "figure03a.png"), ., width = 4, height = 5)

# Plot all calfires during the time series
fire_panels %>%
  purrr::pmap(function(panel_name, start_date, end_date){
    panel_label = c("b", "c", "d")[which(fire_panels$panel_name == panel_name)]
    fig_name <- file.path(path_figures, paste0("figure03", panel_label, "_map.png"))
    # print(fig_name)
    calfire %>%
      mutate(ALARM_DATE = as.Date(ALARM_DATE),
             CONT_DATE = as.Date(CONT_DATE)) %>%
      rowwise %>%
      mutate(max_start = max(c(ALARM_DATE, start_date)),
             min_end = min(c(CONT_DATE, end_date))) %>%
      ungroup %>%
      filter(max_start < min_end) %>%
      {ggplot(data = .,) +
          geom_sf(data = states %>% filter(NAME == "California")) +
          geom_sf(color = NA,
                  fill = "black") +
          theme_void()} %>%
      ggsave(fig_name,., width = 4, height = 5)
    
  })
