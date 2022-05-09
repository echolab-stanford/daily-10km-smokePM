source("./scripts/0_config.R")
library(sf)
library(grid)
library(cowplot)
smokePM_preds <- list.files("./output/smokePM_predictions/10km_smoke_days", 
                            full.names = TRUE) %>% 
  map_dfr(readRDS) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))
grid_10km <- st_read(paste0(data_path, "/1_grids/grid_10km_wgs84"))

pop <- list.files(paste0(data_path, "/2_from_EE/populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv) %>% 
  rename(pop_density = mean)


fire_panels <- data.frame(panel_name = c("November 2018", "October - November 2019", "Fall 2020"), 
                          start_date = as.Date(c("2018-11-01", "2019-10-20", "2020-08-10")), 
                          end_date = as.Date(c("2018-11-30", "2019-11-15", "2020-11-15")))

# calfire from https://frap.fire.ca.gov/frap-projects/fire-perimeters/
calfire <- st_read("~/Downloads/fire20_1.gdb", layer = "firep20_1") %>% 
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

counties <- tigris::counties()

states <- tigris::states()
states %<>% st_transform(crs = st_crs(grid_10km))
CA_cells <- st_intersects(grid_10km, states %>% filter(NAME == "California"))
CA_cells <- grid_10km$ID[CA_cells %>% 
                           purrr::map_lgl(function(x){length(x) > 0})]

county_cells <- counties %>% 
  filter(NAME %in% c("Sacramento", "Santa Clara", "Fresno", "Sonoma")) %>% 
  select(NAME, geometry) %>% 
  purrr::pmap_dfr(function(NAME, geometry){
    data.frame(county = NAME, 
               grid_id_10km = grid_10km$ID[st_intersects(grid_10km, geometry) %>% 
                                        purrr::map_lgl(function(x){length(x) > 0})]) %>% 
      return
  })

# sampling of fires from 2018 to 2020 in CA
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

CA_fires_ts <- ggplot(mapping = aes(x = date, y = smokePM_pred, color = county)) + 
  # add lines for each grid cell
  geom_line(data = grid_fire_preds %>% 
              mutate(type = factor("time_series", levels = c("time_series", "average"), ordered = T)) %>% 
              group_by(county) %>% 
              mutate(n_cell = length(unique(grid_id_10km))),
            aes(group = grid_id_10km, 
                alpha = I(20/n_cell))) + 
  # add mean lines for each county
  geom_line(data = left_join(grid_fire_preds, pop, by = c("grid_id_10km" = "ID")) %>%
              group_by(date, county, panel_name) %>%
              summarise(smokePM_pred = weighted.mean(smokePM_pred, pop_density),
                        .groups = "drop") %>%
              mutate(type = factor("time_series", levels = c("time_series", "average"), ordered = T)),
            size = 1.5) +
  # add dashes for average smoke PM for grid cell
  geom_linerange(data = grid_fire_avgs, 
                 aes(y = total_smokePM, color = county, 
                     xmin = as.Date(county_no*2, origin = "2000-01-01"), 
                     xmax = as.Date(county_no*2+1.5, origin = "2000-01-01"),
                     alpha = I(20/nobs)),
                 inherit.aes = FALSE) + 
  # dashes for average smokePM for county
  geom_linerange(data = left_join(grid_fire_avgs, pop, by = c("grid_id_10km" = "ID")) %>%
                   group_by(panel_name, county, county_no, type) %>%
                   summarise(total_smokePM = weighted.mean(total_smokePM, pop_density)),
                 aes(y = total_smokePM, color = county,
                     xmin = as.Date(county_no*2, origin = "2000-01-01"),
                     xmax = as.Date(county_no*2+1.5, origin = "2000-01-01")),
                 size = 1.5,
                 inherit.aes = FALSE) +
  # add alpha = 0 points to average plots to get the zeros to align
  geom_point(data = left_join(calfire %>% filter(select_fires),
                              grid_fire_avgs %>% group_by(panel_name) %>% summarise(max = max(total_smokePM))),
             aes(y = -panel_fire_no*0.075*max), 
             x = as.Date(4, origin = "2000-01-01"), 
             inherit.aes = FALSE,
             alpha = 0) +
  # add line ranges on the bottom for the fires' start and end 
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
  # facet_grid(panel_name ~ type, scales = "free", space = "free_x") +
  scale_color_manual(values = MetBrewer::met.brewer("Juarez")[c(1,6,5,3)],
                     guide = "none") +
  scale_y_continuous(sec.axis = sec_axis(~., 
                                         name=expression(paste("Daily average (",mu, "g/", m^3, ")")))) +
  theme_classic() + xlab("") + ylab(expression(paste("smoke ", PM[2.5]," (",mu, "g/", m^3, ")"))) +
  theme(strip.background = element_blank(), 
        strip.text.x = element_blank(),
        panel.spacing.y = unit(1.5, "lines"), 
        panel.spacing.x = unit(-2, "lines"))

# make it into a gtable so we can edit
CA_fires_ts <- ggplot_gtable(ggplot_build(CA_fires_ts))

# make the average column narrower 
CA_fires_ts$widths[9] <- CA_fires_ts$widths[9]*0.25 

# remove right y axis on left panels and left y axis on right panels and x-axis from 2nd column
CA_fires_ts %<>% gtable_remove_grobs(c("axis-l-1-2", "axis-l-2-2", "axis-l-3-2", 
                                      "axis-r-1-1", "axis-r-2-1", "axis-r-3-1",
                                      "axis-b-2-1", "axis-b-2-2", "axis-b-2-3")) 

# repeat outside y axes on all plots 
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
  ggsave("./figures/figure3/CA_fires_2018_2020_timeseries.png",
         ., width = 6, height = 7)

# grid.draw(CA_fires_ts)
# gtable::gtable_show_layout(CA_fires_ts)
# CA_fires_ts$layout

# map of select CA fires
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
  ggsave("./figures/figure3/CA_fires_2018_2020_map.png",
         ., width = 4, height = 5)

# all calfires during the time series
fire_panels %>%
  purrr::pmap(function(panel_name, start_date, end_date){
    fig_name <- paste0("./figures/figure3/fire_map_", lubridate::year(start_date), ".png")
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


# {ggplot() + 
#     geom_sf(data = states %>% filter(NAME == "California")) + 
#     # geom_sf(data = counties %>% 
#     #           filter(NAME %in% c("Sacramento", "Santa Clara", "Fresno", "Sonoma")), 
#     #         aes(color = NAME, fill = NAME)) + 
#     # scale_color_manual(values = MetBrewer::met.brewer("Juarez")[c(1,6,5,3)],
#     #                    aesthetics = c("color", "fill")) + 
#     # ggnewscale::new_scale_fill() +
#     # ggnewscale::new_scale_color() +
#     geom_sf(data = calfire %>% filter(YEAR_ == 2020), 
#             color = NA,
#             fill = "black") + 
#     theme_void()} %>% 
#   ggsave("./figures/figure3/2020_complex_fire_map.png",
#          ., width = 4, height = 5)

# data.frame(fire_start = as.Date(c("2018-11-08", "2019-10-23", "2020-08-16")),
#            fire_end = as.Date(c("2018-11-25", "2019-11-06", "2020-11-12")),
#            fire_name = c("Camp Fire", "Kincade Fire", "August Complex"))


# comparison to other smoke PM options? just plumes, EPA data, reid, GEOS-chem? 
# epa_pm <- readRDS(paste0(data_path, "/3_intermediate/station_smokePM.rds"))
# epa_ll <- st_read(paste0(data_path, "/epa_station_locations"))
# smoke_days <- readRDS(paste0(data_path, "/3_intermediate/all_smoke_days.rds")) 
# 
# county_stations <- counties %>% 
#   filter(NAME %in% c("Sacramento", "Santa Clara", "Fresno", "Sonoma")) %>% 
#   select(NAME, geometry) %>% 
#   purrr::pmap_dfr(function(NAME, geometry){
#     data.frame(county = NAME, 
#                epa_id = epa_ll$id[st_intersects(epa_ll, geometry) %>% 
#                                     purrr::map_lgl(function(x){length(x) > 0})]) %>% 
#       return
#   })
# 
# epa_pm %>% 
#   ungroup %>% 
#   left_join(county_stations,
#              by = c("id" = "epa_id")) %>% 
#   filter(!is.na(county)) %>% 
#   mutate(panel_name = case_when(date >=  as.Date("2018-11-01") & date <=  as.Date("2018-11-30") ~ "November 2018",
#                                 date >=  as.Date("2019-10-20") & date <=  as.Date("2019-11-15") ~ "October - November 2019",
#                                 date >=  as.Date("2020-08-10") & date <=  as.Date("2020-11-15") ~ "Fall 2020", 
#                                 T ~ "")) %>% 
#   filter(panel_name != "") %>% 
#   ggplot(aes(x = date, y = smokePM, group = id, color = county)) + 
#   geom_line() + 
#   facet_wrap(~panel_name, ncol = 1, scales = "free") + 
#   theme_classic()
# 
# epa_pm %>% 
#   ungroup %>% 
#   left_join(county_stations,
#             by = c("id" = "epa_id")) %>% 
#   filter(!is.na(county)) %>% 
#   mutate(panel_name = case_when(date >=  as.Date("2018-11-01") & date <=  as.Date("2018-11-30") ~ "November 2018",
#                                 date >=  as.Date("2019-10-20") & date <=  as.Date("2019-11-15") ~ "October - November 2019",
#                                 date >=  as.Date("2020-08-10") & date <=  as.Date("2020-11-15") ~ "Fall 2020", 
#                                 T ~ "")) %>% 
#   filter(panel_name != "") %>% 
#   group_by(panel_name, id, county) %>% 
#   summarise(mean_smokePM = mean(smokePM), .groups = "drop") %>% 
#   {ggplot(data = ., aes(x = as.numeric(as.factor(county)), 
#              y = mean_smokePM, 
#              color = county)) + 
#   geom_point(alpha = 0.5) + 
#   geom_hline(data = group_by(., panel_name) %>% summarise(avg_smokePM = mean(mean_smokePM, na.rm = T)), 
#              aes(yintercept = avg_smokePM), 
#              linetype = "dashed") +
#   facet_wrap(~panel_name, ncol = 1, scales = "free") + 
#   theme_classic()}
# add reid predictions somehow? 

# data.frame(panel_name = c("November 2018", "October - November 2019", "Fall 2020"), 
#            start_date = as.Date(c("2018-11-01", "2019-10-20", "2020-08-10")), 
#            end_date = as.Date(c("2018-11-30", "2019-11-15", "2020-11-15"))) %>% 




