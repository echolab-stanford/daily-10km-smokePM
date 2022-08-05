library(tidyverse)
library(magrittr)
library(sf)

# restart using the data from Kara and Jen 
dt <- read.csv("./scratch/files_from_jen/drive-download-20220630T164247Z-001/IMPROVE_CSN_smokeday_data.csv") %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-", day), 
                        format = "%Y-%m-%d")) %>% 
  filter(date >= as.Date("2006-01-01"))

species <- c('PM2.5', 'OC','EC', 'SO4', 'Dust') 

# calcualte pm25 anomalies and species anomalies, 
# how much of the pm25 anomalies are explained/accounted for by the OC anomalies? 
source("./scripts/0_config.R")
species_anom <- dt %>% 
  select(Network, Site, date, all_of(species), smoke_day) %>% 
  pivot_longer(all_of(species)) %>% 
  filter(!is.na(value)) %>% 
  filter(!is.na(smoke_day)) %>%
  mutate(month = lubridate::month(date), 
         year = lubridate::year(date), 
         name = gsub("\\.", "", name)) %>%
  unite(net_site_species, Network, Site, name) %>% 
  {left_join(., 
             nonsmoke_medians(., value, smoke_day, net_site_species, month, year), 
             by = c("net_site_species", "month", "year"))} %>% 
  mutate(anom = value - value_med_3yr) %>% 
  separate(net_site_species, into = c("Network", "Site", "Species"), sep = "_") 

# site_states <- dt %>% select(Network, Site, State) %>% unique


# species_anom %>%
#   filter(nobs_3yr > 15) %>% 
#   select(-nobs_3yr) %>%
#   filter(Site != "240239000") %>% # has duplicate obs
#   pivot_wider(id_cols = c(Network, Site, date, smoke_day), 
#               values_from = c(anom, value, value_med_3yr), 
#               names_from = Species) %>% 
#   mutate(pct_anom_OC = anom_SO4/anom_PM25,
#          baselinePM = cut(value_med_3yr_PM25, 
#                           c(0, 5, 10, 20))) %>% 
#   left_join(site_states %>% mutate(Site = as.character(Site))) %>%
#   mutate(state_group = case_when(State %in% c("CA", "OR", "WA", "ID", "MT") ~ "west", 
#                                  State %in% c("AL", "GA", "FL", "MS") ~ "south", 
#                                  State %in% c("AR", "NM") ~ "SW")) %>%
#   ggplot(aes(x = pct_anom_OC, 
#              group = as.factor(smoke_day),
#              color = as.factor(smoke_day))) +
#   geom_density() + 
#   theme_classic() +
#   # facet_grid(paste0("PM anom:", cut(anom_PM25, c(0, 10, 50, 10000))) ~ paste0("smoke day:", as.factor(smoke_day)),
#   #            scales = "free_y") + 
#   scale_x_continuous(trans = "pseudo_log", 
#                      limits = c(-0.5, 1))

# species_anom %>%
#   filter(nobs_3yr > 15) %>% 
#   select(-nobs_3yr) %>%
#   filter(Site != "240239000") %>% # has duplicate obs
#   pivot_wider(id_cols = c(Network, Site, date, smoke_day), 
#               values_from = c(anom, value, value_med_3yr), 
#               names_from = Species) %>% 
#   mutate(pct_anom_OC = anom_OC/anom_PM25,
#          baselinePM = cut(value_med_3yr_PM25, 
#                           c(0, 5, 10, 20))) %>% 
#   left_join(site_states %>% mutate(Site = as.character(Site))) %>%
#   mutate(state_group = case_when(State %in% c("CA", "OR", "WA", "ID", "MT") ~ "west", 
#                                   State %in% c("AL", "GA", "FL", "MS") ~ "south", 
#                                   State %in% c("AR", "NM") ~ "SW")) %>%
#   ggplot(aes(x = pct_anom_OC, 
#              group = state_group,
#              color = state_group)) +
#   geom_density() + 
#   theme_classic() +
#   facet_grid(paste0("PM anom:", cut(anom_PM25, c(0, 10, 50, 10000))) ~ paste0("smoke day:", as.factor(smoke_day)),
#              scales = "free_y") + 
#   geom_vline(xintercept = 0, linetype = "dashed") + 
#   geom_vline(xintercept = 1, linetype = "dashed") + 
#   scale_x_continuous(trans = "pseudo_log", 
#                      limits = c(-0.5, 1))
  

# species_anom %>%
#   filter(nobs_3yr > 15) %>% 
#   select(-nobs_3yr) %>%
#   filter(Site != "240239000") %>% # has duplicate obs
#   pivot_wider(id_cols = c(Network, Site, date, smoke_day), 
#               values_from = c(anom, value, value_med_3yr), 
#               names_from = Species) %>% 
#   mutate(pct_anom_OC = anom_OC/anom_PM25,
#          baselinePM = cut(value_med_3yr_PM25, 
#                           c(0, 5, 10, 500))) %>%
#   left_join(site_states %>% mutate(Site = as.character(Site))) %>%
#   mutate(state_group = case_when(State %in% c("CA", "OR", "WA", "ID", "MT") ~ "west", 
#                                  State %in% c("AL", "GA", "FL", "MS") ~ "south", 
#                                  State %in% c("AR", "NM") ~ "SW")) %>%
#   filter(smoke_day == 1) %>%
#   filter(!is.na(value_med_3yr_PM25)) %>%
#   filter(anom_PM25 > 0) %>%
#   ggplot(aes(x = pct_anom_OC, 
#              group = state_group,
#              color = state_group)) +
#   geom_density() + 
#   theme_classic() +
#   facet_grid(paste0("PM anom:", cut(anom_PM25, c(0, 10, 50, 10000))) ~ paste0("baseline PM:", baselinePM),
#              scales = "free_y") + 
#   geom_vline(xintercept = 0, linetype = "dashed") + 
#   geom_vline(xintercept = 1, linetype = "dashed") + 
#   scale_x_continuous(trans = "pseudo_log", 
#                      limits = c(-0.5, 1))

site_nonsmokePM <- dt %>% 
  filter(!is.na(smoke_day)) %>%
  group_by(Network, Site, State, Latitude, Longitude, smoke_day) %>% 
  summarise(baselinePM = median(`PM2.5`, na.rm = T), 
            n = n(),
            .groups = "drop") %>% 
  pivot_wider(values_from = c(baselinePM, n), 
              names_from = smoke_day, 
              names_prefix  = "smokeday") %>% 
  filter(!is.na(n_smokeday1)) %>% 
  select(-starts_with("n_smokeday"), -baselinePM_smokeday1) %>% 
  rename(baselinePM = baselinePM_smokeday0) %>% 
  filter(State %in% c("HI", "AK") == F)

pct_anom <- species_anom %>%
  # filter(nobs_3yr > 15) %>% 
  select(-nobs_3yr) %>%
  filter(Site != "240239000") %>% # has duplicate obs
  pivot_wider(id_cols = c(Network, Site, date, smoke_day), 
              values_from = c(anom, value, value_med_3yr), 
              names_from = Species) %>% 
  mutate(#pct_anom_NH4 = anom_NH4/anom_PM25,
         pct_anom_SO4 = anom_SO4/anom_PM25,
         pct_anom_OC = anom_OC/anom_PM25,
         pct_anom_EC = anom_EC/anom_PM25,
         pct_anom_dust = anom_Dust/anom_PM25) %>%
  filter(!is.na(value_med_3yr_PM25)) %>%  
  select(starts_with("pct_anom"), anom_PM25, smoke_day, date, Site, Network) %>% 
  left_join(site_nonsmokePM %>% mutate(Site = as.character(Site))) %>% 
  mutate(baselinePM = ifelse(baselinePM > 8, "High", "Low")) %>%
  pivot_longer(starts_with("pct_anom")) %>% 
  mutate(day = case_when(smoke_day == 0  ~ "non-smoke day", 
                         smoke_day == 1 & anom_PM25 <= 0 ~ "smoke day,\nanom < 0", 
                         smoke_day == 1 & anom_PM25 <= 25 ~ "smoke day,\n0 < anom <= 25 ", 
                         smoke_day == 1 & anom_PM25 > 25 ~ "smoke day,\n25 < anom"),
         day = factor(day, levels = c("non-smoke day", "smoke day,\nanom < 0",
                                      "smoke day,\n0 < anom <= 25 ", "smoke day,\n25 < anom"),
                         ordered = T),
         name = gsub("pct_anom_", "", name))

pct_anom %>% 
  filter(anom_PM25 != 0) %>%
  filter(!is.na(baselinePM)) %>%
  filter(day != "smoke day,\nanom < 0") %>%
  filter(!is.na(value)) %>% 
  mutate(name = recode_factor(name, 
                              "dust" = "Dust", 
                              "EC" = "Elemental~carbon", 
                              "SO4" = "SO[4]", 
                              "OC" = "Organic~carbon", 
                              .ordered = T)) %>%
  {ggplot(data =., 
          aes(x = value,
             y = ..ndensity..,
             group = day,
             color = day,
             fill = day)) +
      geom_density(alpha = 0.1, alpha = 0.5, adjust = 2) +
      theme_classic() +
      facet_grid(paste0(baselinePM, "~baseline~PM[2.5]") ~ name,
                 scales = "free",
                 labeller = label_parsed) +
      geom_vline(xintercept = 0, color = "grey30", linetype = "dotted") +
      scale_x_continuous(limits = c(-0.5, 1), # results in 46580/635396 (~7%) obs being dropped
                         labels = scales::percent) +
      scale_color_manual(values = c("black", "blue", "red"),
                         aesthetics = c("color", "fill")) +
      labs(color = "",
           fill = "",
           y = "density") + 
      xlab(bquote(Percent~PM[2.5]~anomaly)) + 
      theme(panel.spacing.x = unit(1, "lines"), 
            strip.text = element_text(size = 12),
            strip.background = element_blank())} %>% 
  ggsave("./figures/supplement/SX_speciated_smoke_days.png", ., 
         width = 10, height = 6)

# pct_anom %>% 
#   filter(anom_PM25 != 0) %>%
#   filter(!is.na(baselinePM)) %>%
#   group_by(day, name, baselinePM) %>% 
#   summarise(n = n(),
#             mean = mean(value, na.rm = T), 
#             sd = sqrt(var(value, na.rm = T)),
#             q95 = quantile(value, 0.95, na.rm = T),
#             q5 = quantile(value, 0.05, na.rm = T), 
#             .groups = "drop") %>% 
#   filter(name != "SO4") %>%
#   ggplot(aes(x = name, 
#              y = mean, 
#              shape = baselinePM,
#              group = interaction(baselinePM, day),
#              color = day)) + 
#   geom_point(position = position_dodge(width = 0.5)) + 
#   geom_linerange(aes(ymin = mean + qnorm(0.025)*sd/sqrt(n), 
#                      ymax = mean + qnorm(0.975)*sd/sqrt(n)),
#                  position = position_dodge(width = 0.5)) + 
#   theme_classic() + 
#   scale_color_manual(values = c("black", "green", "blue", "red"),
#                      aesthetics = c("color", "fill")) +
#   scale_size_continuous(trans = "pseudo_log",
#                         breaks = c(55, 500, 5000, 50000)) + 
#   ylab("% PM2.5 anomaly") + xlab("Species") + 
#   ylim(-0.5, 1)

states <- tigris::states(cb = TRUE) %>% 
  st_transform(crs = 4326)

mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

site_nonsmokePM %>% 
  filter(!is.na(baselinePM)) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"))  %>%
  st_set_crs(4326) %>%
  mutate(baselinePM_class = ifelse(baselinePM > 8, "high", "low"), 
         baselinePM = pmin(baselinePM, 15)) %>%  
  {ggplot(data = ., 
          aes(color = baselinePM,
             fill = baselinePM,
             shape = Network)) + 
  geom_sf(data = states %>% filter(STATEFP %in% nonContig_stateFIPS == F), 
          fill = "white", inherit.aes = FALSE) +
  geom_sf() + 
  scale_color_gradientn(colors = c(cmocean::cmocean(name = "balance", start = 0, end = 0.4)(20),
                                   cmocean::cmocean(name = "balance", start = 0.6, end = 1)(20)),
                        #colors = c(cmocean::cmocean(name = "oxy", start = 0.2, end = 1, direction = -1)(20),
                        #           cmocean::cmocean(name = "oxy", start = 0, end = 0.19, direction = -1)(20)),
                        name = "",
                        aesthetics = c("fill", "color"), 
                        rescaler = mid_rescaler(8), 
                        breaks = c(5, 10, 15), 
                        labels = c("5", "10", ">15"),
                        guide = guide_colorbar(label.theme = element_text(size = 7), 
                                               barheight = 4, barwidth = 1)) + 
  theme_void() + 
  theme(legend.position = c(0.93, 0.25))} %>% 
  ggsave("./figures/supplement/SX_improve_csn_station_ll.png", 
         ., width = 6, height = 4)
