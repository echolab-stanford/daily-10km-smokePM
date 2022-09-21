source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots graphical abstract.
# ------------------------------------------------------------------------------
# Load smokePM prediction
smokePM_preds <- readRDS(file.path(path_output, "smokePM", "predictions", "combined", "smokePM_predictions_20060101_20201231.rds"))

# Load population density
pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv)

# Load 10 km grid
grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84")) %>% 
  mutate(area = st_area(geometry) %>% unclass) # area in m^2

# 
extremes_by_year <- smokePM_preds %>%
  mutate(year = lubridate::year(date)) %>% 
  {full_join(group_by(., year) %>% 
               summarise(days_over50 = sum(smokePM_pred > 50),
                         days_over100 = sum(smokePM_pred > 100), 
                         days_over200 = sum(smokePM_pred > 200), 
                         .groups = "drop"),
             group_by(., year, grid_id_10km) %>%  
               summarise(pop_over50 = any(smokePM_pred >50),
                         pop_over100 = any(smokePM_pred > 100), 
                         pop_over200 = any(smokePM_pred > 200),
                         .groups = "drop") %>% 
               left_join(pop %>% left_join(grid_10km) %>%
                           transmute(grid_id_10km = ID, pop = mean*area)) %>% 
               group_by(year) %>% 
               summarise(across(starts_with("pop_over"), ~sum(.x*pop)), 
                         .groups = "drop"))}
# change in extremes
extremes_by_year %>% 
  mutate(period = case_when(year >= 2006 & year <=2010 ~ "2006 - 2010", 
                            year >= 2016 & year <=2020 ~ "2016 - 2020",
                            T ~ as.character(NA))) %>% 
  filter(!is.na(period)) %>% 
  group_by(period) %>% 
  summarise(across(contains("over"), mean)) %>% 
  pivot_longer(contains("over")) %>% 
  separate(name, into = c("panel", "cutoff"), sep = "_") %>%
  mutate(panel = dplyr::recode_factor(panel, 
                                      "days" = "average annual number of grid cell-days",
                                      "pop" = "average annual population exposed\nto at least one day",
                                      .ordered = TRUE)) %>%
  {ggplot(data = ., 
          aes(x = period, y = value, group = cutoff, 
              color = cutoff)) + 
      geom_point() + 
      geom_line() + 
      geom_text(data = group_by(., panel, cutoff) %>% 
                  summarise(asinh_mid = sinh(mean(asinh(value))),
                            .groups = "drop") %>% 
                  mutate(cutoff_num = gsub("over", "", cutoff), 
                         label_top = paste0("paste(\'Days > ", cutoff_num, "\', mu, \'g\')")),
                aes(y = asinh_mid, label = label_top,
                    x = 2.1 + ifelse(cutoff == "over100", 0.03, 0) + 
                      ifelse(cutoff == "over50", 0.06, 0)), 
                color = "black", hjust = 0, vjust = 0, parse = TRUE) + 
      geom_text(data = group_by(., panel, cutoff) %>% 
                  summarise(asinh_mid = sinh(mean(asinh(value))),
                            mult = max(value)/min(value),
                            .groups = "drop") %>% 
                  mutate(mult_round = formatC(round(signif(mult, 2), 0),
                                              format="d", big.mark=","), 
                         label_bottom = paste0(mult_round, "x increase")),
                aes(y = asinh_mid, label = label_bottom, 
                    x = 2.1 + ifelse(cutoff == "over100", 0.03, 0) + 
                      ifelse(cutoff == "over50", 0.06, 0)), 
                color = "black", hjust = 0, vjust = 1) + 
      geom_linerange(data = group_by(., panel, cutoff) %>% 
                       summarise(min = min(value), 
                                 max = max(value), 
                                 .groups = "drop"), 
                     mapping = aes(ymin = min, ymax = max, 
                                   x = 2.05 + ifelse(cutoff == "over100", 0.03, 0) + 
                                     ifelse(cutoff == "over50", 0.06, 0), 
                                   color = cutoff), 
                     inherit.aes = FALSE) + 
      facet_wrap(~panel, scales = "free", nrow = 1, strip.position = "left") + 
      scale_y_continuous(trans = "pseudo_log", 
                         expand = expansion(mult = c(0.07, 0.05)),
                         breaks = c(0, 1, 5, 10, 50, 100, 500, 
                                    1000, 5000, 10000, 50000, 100000, 
                                    500000, 1000000, 5000000, 10000000, 
                                    20000000),
                         labels = c("0", "1", "5", "10", "50", "100", "500", 
                                    "1k", "5k", "10k", "50k", "100k", 
                                    "500k", "1m", "5m", "10m", "20m")) +
      scale_x_discrete(expand = expansion(mult = c(0.1, 0.05))) + 
      coord_cartesian(clip = "off") +
      scale_color_manual(values = c("#e8993b", "#913818", "#4a571e"), 
                         guide = "none") + 
      xlab("") + ylab("") + 
      theme_classic() + 
      theme(plot.margin = margin(0.5,5.5,0,-1, "lines"), 
            text = element_text(size = 15),
            axis.text.x = element_text(vjust = -0.5),
            strip.placement = "outside", 
            strip.background = element_blank(),
            strip.text = element_text(size = 12),
            panel.spacing.x = unit(6.25, "lines"))} %>% 
  ggsave(file.path(path_figures, "figure00.png"), ., 
         width = 8, height = 3.95)
