source("./scripts/0_config.R")
library(sf)
library(lubridate)

smokePM_preds <- readRDS(paste0(output_path, "/smokePM_predictions_2006_2020.rds")) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

pop <- list.files(paste0(data_path, "/2_from_EE/populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv)

grid_10km <- st_read(paste0(data_path, "/1_grids/grid_10km_wgs84")) %>% 
  mutate(area = st_area(geometry) %>% unclass) # area in m^2

# check the population adds up 
pop %>% 
  rename(pop_density = mean) %>% # people per m2
  left_join(grid_10km) %>% 
  mutate(pop = pop_density*area) %>% 
  summarise(pop = sum(pop))

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
                                      "days" = "avg annual number of grid cell-days",
                                      "pop" = "avg annual population exposed to 1+ days",
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
      theme(plot.margin = margin(0.5,6,0,0, "lines"), 
            strip.placement = "outside", 
            strip.background = element_blank(),
            strip.text = element_text(size = 10),
            panel.spacing.x = unit(5, "lines"))} %>% 
  ggsave("./figures/figure7/extremes_change.png", ., 
         width = 8, height = 3.25)


# map of chaning extremes
simple_states <- tigris::states(cb = TRUE) %>% 
  filter(!(STATEFP %in% nonContig_stateFIPS)) %>% 
  st_transform(crs = st_crs(grid_10km))

decadal_change <- smokePM_preds %>% 
  mutate(year = lubridate::year(date)) %>% 
  # calculate annual avg contribution of smokePM to daily PM
  group_by(year, grid_id_10km) %>% 
  summarise(annual_total_smokePM = sum(smokePM_pred), 
            annual_days_over50 = sum(smokePM_pred > 50), 
            annual_days_over100 = sum(smokePM_pred > 100), 
            .groups = "drop") %>% 
  mutate(y_days = 365 + leap_year(year)*1, 
         annual_daily_smokePM = annual_total_smokePM/y_days) %>%
  select(-annual_total_smokePM) %>% 
  # join with full set of years and grid cells to fill in zeros
  {left_join(expand.grid(year = unique(.$year),
                         grid_id_10km = unique(.$grid_id_10km)), 
             .)} %>% 
  replace_na(list(annual_daily_smokePM = 0, 
                  annual_days_over50 = 0, 
                  annual_days_over100 = 0)) %>% 
  # add the time periods to summarise over 
  mutate(period = case_when(year >= 2006 & year <=2010 ~ "years2006_2010", 
                            year >= 2016 & year <=2020 ~ "years2016_2020",
                            T ~ as.character(NA))) %>% 
  filter(!is.na(period)) %>% 
  # average the annual daily smokePM to the period
  group_by(period, grid_id_10km) %>% 
  summarise(across(starts_with("annual"), mean),
            .groups = "drop") %>%  
  # make a row for each metric, then column for each period, and difference the columns
  pivot_longer(starts_with("annual")) %>% 
  pivot_wider(names_from = period, values_from = value) %>% 
  mutate(decade_change = years2016_2020 - years2006_2010) %>%
  select(-starts_with("years")) %>% 
  pivot_wider(names_from = name, values_from = decade_change)

mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

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
  ggsave("./figures/figure7/extremes_map.png",. , 
         width = 5, height = 3)

