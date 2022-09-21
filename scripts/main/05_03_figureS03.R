# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 3.
# ------------------------------------------------------------------------------
# trends in plumes
plumes <- readRDS(file.path(path_data, "smoke", "smoke_plumes_sfdf_20050805_20220711.RDS"))
plumes <- readRDS(file.path(path_data, "smoke", "smoke_plumes_sfdf.RDS"))

plume_area <- st_area(plumes)

plumes %<>% st_drop_geometry() %>% 
  cbind(area = unclass(plume_area)/1e6)

plumes %>% 
  filter(area > 0) %>% 
  mutate(year_month = substr(date, 1, 6),
         date = as.Date(date, format = "%Y%m%d"), 
         month = lubridate::month(date), 
         year = lubridate::year(date)) %>% 
  filter(year >= 2006 & year <= 2020) %>%
  group_by(year_month) %>% 
  summarise(start_date = min(date),
            qminimum = min(area), 
            q5 = quantile(area, 0.05), 
            q10 = quantile(area, 0.10), 
            .groups = "drop") %>%
  pivot_longer(starts_with("q"), names_prefix = "q") %>% 
  mutate(name = ifelse(name %in% c("minimum", "max"), name, paste0(name, "th percentile"))) %>% 
  {ggplot() + 
      geom_vline(xintercept = as.Date("2017-12-28"),
                 color = "grey") + 
      geom_label(aes(x = as.Date("2017-12-28"),
                    y = pull(., value) %>% max,
                    label = "GOES-16"), 
                nudge_y = -5,
                nudge_x = -65,
                vjust = 1,
                fill = "white",
                color = "grey", 
                label.size = NA) + 
      geom_vline(xintercept = as.Date("2019-02-28"), 
                 color = "grey") +
      geom_label(aes(x = as.Date("2019-02-28"),
                    y = Inf,
                    label = "GOES-17"), 
                vjust = 1, 
                nudge_y = 20,
                nudge_x = 65,
                fill = "white",
                color = "grey", 
                label.size = NA) + 
      geom_line(data = .,
                aes(x = start_date,
                    y = value,
                    group = name,
                    color = name)) +
      geom_text(data = group_by(., name) %>% 
                  summarise(mean_val = last(value)) %>% 
                  cbind(start_date = as.Date("2021-01-01")), 
                aes(x = start_date, 
                    y = mean_val, 
                    label = name, 
                    color = name), 
                hjust = 0) +
      theme_classic() + 
      coord_cartesian(clip = "off") + 
      scale_color_manual(values = c("black", "darkgreen", "purple")) + 
      xlab("Date") + ylab(bquote("Plume size "(km^2))) +
      theme(legend.position = "none", 
            plot.margin = unit(c(0.75, 2.5, 0.5, 0.25), "cm"))} %>% 
  ggsave(file.path(path_figures, "figureS03.png"), ., 
         width = 6, height = 4)
