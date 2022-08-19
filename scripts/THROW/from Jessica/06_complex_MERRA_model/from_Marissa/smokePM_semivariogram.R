library(tidyverse)
library(magrittr)
library(sf)
library(gstat)

station_data <- station_data <- readRDS("./data/clean/epa_station_covariates.rds") %>%
  filter(!is.na(pm25)) %>% 
  mutate(smokePM = pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0))

station_ll <- sf::read_sf("./data/epa_station_locations", 
                          "epa_station_locations") %>% 
  rename(epa_id = id)

station_data %>% 
  group_by(date) %>% 
  summarise(date_smokePM_obs = sum(smokePM > 0)) %>% 
  str

# randomly select dates among days with at least 2 stations with >0 smoke PM
n_dates <- 50
test <- 
  station_data %>% 
  group_by(date) %>% 
  summarise(date_smokePM_obs = sum(smokePM > 0)) %>% 
  filter(date_smokePM_obs > 1) %>%
  pull(date) %>%
  sample(n_dates) %>% 
  purrr::map_dfr(function(vario_date){
    test1 <- station_data %>% 
      filter(date == vario_date) %>% 
      left_join(station_ll) %>% 
      st_as_sf()
    
    # model
    gstat::variogram(smokePM~1,
                     data = test1,
                     cutoff = 500,
                     width = 2.5) %>%
      gstat::fit.variogram(vgm("Sph")) %>%
      variogramLine(500) %>%
      cbind(date = vario_date) %>% return
    
    # raw semivariances?
    # gstat::variogram(smokePM~1, 
    #                  data = test1,
    #                  cutoff = 500, 
    #                  width = 2.5, 
    #                  covariogram = FALSE) %>%
    #   cbind(date = vario_date) %>% return
    
  })

# plot the semivariogram models and MERRA2 resolution
test %>% 
  mutate(date = as.factor(date)) %>% 
  group_by(date) %>% 
  mutate(scaled_gamma = gamma / max(gamma)) %>%
  ggplot(aes(x = dist, y = scaled_gamma, group = date)) + 
  geom_line(alpha = 0.5, color = "grey40") + 
  geom_vline(xintercept = 50, color = "red") + 
  labs(x = "distance (km)", y = "relative semivariance") + 
  theme_bw()

# plot raw semivariograms and MERRA2 resolution
test %>% 
  mutate(date = as.factor(date)) %>% 
  group_by(date) %>% 
  mutate(scaled_gamma = gamma / max(gamma)) %>%
  ggplot(aes(x = dist, y = gamma, 
             group = date, color = date)) + 
  geom_point(alpha = 0.5) + 
  geom_vline(xintercept = 50, color = "red") + 
  labs(x = "distance (km)", y = "semivariance") + 
  theme_bw() + 
  scale_y_continuous(trans = "pseudo_log")
