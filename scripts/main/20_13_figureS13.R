# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 13.
# ------------------------------------------------------------------------------
counties <- tigris::counties(cb = TRUE)
epa_data <- readRDS(file.path(path_data, "3_intermediate", "station_smokePM.rds")) %>% ungroup
epa_ll <- st_read(file.path(path_data, "epa_station_locations")) %>% 
  filter(id %in% unique(epa_data$id)) %>%
  rename(grid_id_10km = grid_10km) %>% 
  mutate(county = counties$NAME[st_intersects(geometry, 
                                              counties %>% st_transform(st_crs(geometry))) %>% 
                                  unlist])

fire_panels <- data.frame(panel_name = c("November 2018", "October - November 2019", "Fall 2020"), 
                          start_date = as.Date(c("2018-11-01", "2019-10-20", "2020-08-10")), 
                          end_date = as.Date(c("2018-11-30", "2019-11-15", "2020-11-15"))) %>% 
  mutate(panel_name = factor(panel_name, 
                             levels = panel_name, 
                             ordered = T))

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

obs_data <- epa_data %>% 
  left_join(epa_ll %>% 
              st_drop_geometry() %>% 
              select(id, county)) %>% 
  filter(county %in% c("Santa Clara", "Fresno", "Sacramento", "Sonoma")) %>% 
  left_join(fire_panels %>% 
              rowwise %>% 
              mutate(all_dates = list(seq.Date(start_date, end_date, by = "day"))) %>% 
              unnest_longer(all_dates), 
            by = c("date" = "all_dates")) %>% 
  filter(!is.na(panel_name)) 
  
obs_data %>%   
  mutate(id = as.factor(id)) %>%
  {ggplot(data = ., aes(x = date, y = smokePM, group = id, color = county)) + 
      geom_line() + 
      geom_linerange(data = left_join(calfire %>% filter(select_fires),
                                      obs_data %>% group_by(panel_name) %>% summarise(max = max(smokePM, na.rm = T))) %>% 
                       mutate(panel_name = factor(panel_name, levels = unique(panel_name), ordered = T)), 
                     aes(xmin = as.Date(ALARM_DATE), 
                         xmax = as.Date(CONT_DATE), 
                         y = -panel_fire_no*0.1*max), 
                     inherit.aes = FALSE) + 
      geom_label(data = left_join(calfire %>% filter(select_fires) %>% st_drop_geometry(),
                                  obs_data %>% group_by(panel_name) %>% summarise(max = max(smokePM, na.rm = T))) %>%
                   mutate(panel_name = factor(panel_name, levels = unique(panel_name), ordered = T),
                          mid_date = as.Date(ALARM_DATE) + floor(difftime(as.Date(CONT_DATE), as.Date(ALARM_DATE), units = "days")/2)),
                 aes(x = mid_date, y = -panel_fire_no*0.1*max, label = Fire), 
                 inherit.aes = FALSE, size = 3, 
                 label.size = NA, label.padding = unit(0.1, "lines")) +
      facet_wrap(~panel_name, scales = "free", 
                 nrow =3) + 
      scale_color_manual(values = MetBrewer::met.brewer("Juarez")[c(1,6,5,3)]) +
      theme_classic() + 
      theme(strip.background = element_blank(), 
            strip.text = element_text(size = 13, face = 2)) + 
      ylab(expression(paste("observed smoke ", PM[2.5]," (",mu, "g/", m^3, ")"))) + 
      xlab("")} %>% 
  ggsave(file.path(path_figures, "figureS13.png"), .,
         width = 6, height = 8)
