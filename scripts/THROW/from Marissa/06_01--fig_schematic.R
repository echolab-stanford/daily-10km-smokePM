library(tidyverse)
library(magrittr)
library(layer)
library(sf)
# library(raster)

source("scripts/0_config.R")

# set a date to use for the figure
fig_date <- as.Date("2020-09-20")

# 10 km grid
grid_10km <- sf::st_read(paste0(data_path, "/1_grids/grid_10km_wgs84"))

contigUS <- tigris::states(cb = TRUE) %>% filter(!(STATEFP %in% nonContig_stateFIPS))

# plumes 
# plumes <- readRDS(paste0(data_path, 
#                          "/smoke_days/grid_smoke_day_", 
#                          format(fig_date, "%Y"), 
#                          "_",
#                          format(fig_date, "%m"),
#                          ".rds")) %>% 
#   filter(date == fig_date) %>% str
#   dplyr::select(grid_id_10km = id_grid, smoke_day)

  # fire variables
tilted_grid <- tilt_map(grid_10km, 
                        x_tilt = 0.1, y_tilt = 1) %>% 
  select(grid_id_10km = value, 
         geometry)

plumes <- st_read("./scratch/plumes_20200920/") %>% 
  st_set_crs(st_crs(grid_10km))
# plumes[st_intersects(plumes, grid_10km) %>% purrr::map_dbl(length) %>% 
#          is_greater_than(0) %>% 
#          which,] %>% 


contig_plumes <- st_intersection(plumes, contigUS  %>% st_union() %>% st_transform(crs = st_crs(plumes)))
contig_plumes_tilt <- tilt_map(contig_plumes, 
                               x_tilt = 0.1, y_tilt = 1)

# smoke-days under clouds 
plume_day = readRDS(paste0("~/BurkeLab Dropbox/Data/smoke/10km_grid/grid_smoke_day_", 
                           format(fig_date, "%Y"), "_", 
                           format(fig_date, "%m"), ".rds")) %>% 
  filter(date == fig_date) %>% 
  filter(smoke_day == 1)
all_smoke_day = readRDS(paste0(data_path, "/3_intermediate/all_smoke_days_incl_cloudy.rds")) %>% 
  filter(date == fig_date)
cloudy_smoke_days <- setdiff(all_smoke_day$grid_id_10km, 
                             plume_day$id_grid)

aod_missing <- list.files(paste0(data_path, "/2_from_EE/maiac_AODmissings/"), 
                          pattern = paste0(format(fig_date, "%Y"),"0101"),
                          full.names = TRUE) %>% 
  purrr::map_dfr(function(x){read.csv(x) %>% filter(as.character(start_date) == format(fig_date, "%Y%m%d"))}) %>% 
  select(grid_id_10km = ID, value = mean)

aod_pred <- readRDS(paste0(output_path, 
                           "/AOD_predictions/10km_smoke_days/AOD_predictions_10km_", 
                           format(fig_date, "%Y%m%d"), 
                           ".rds")) %>% 
  select(grid_id_10km, 
         value = aod_anom_pred_1.00)
temp <- readRDS(paste0(data_path, 
                       "/ERA5_variables/Land/2m_temperature/USA/10km_grid/UTC-0600/daily_mean_of_1-hourly/grid_2m_temperature_daily_mean_", 
                       format(fig_date, "%Y"), 
                       "_",
                       format(fig_date, "%m"),
                       ".rds")) %>% 
  filter(date == fig_date) %>% 
  select(grid_id_10km = id_grid, 
         value = `2m_temperature`)

hysplit <- readRDS(paste0("~/BurkeLab Dropbox/Data/hms_hysplit/10km_grid_2006-2020/grid_trajectory_points_", 
                          format(fig_date, "%Y"), 
                          "_",
                          format(fig_date, "%m"),
                          ".rds")) %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  filter(date == fig_date) %>% 
  transmute(grid_id_10km = id_grid, 
            value = asinh(num_traj_points_height_1))

fire_dist <- readRDS(paste0(data_path, 
                            "/distance_to_fire_cluster/grid_distance_to_fire_cluster_",
                            format(fig_date, "%Y"), 
                            "_",
                            format(fig_date, "%m"),
                            ".rds")) %>% 
  filter(date == fig_date) %>% 
  select(grid_id_10km = id_grid, 
         value = km_dist)

elev <- read.csv(paste0(data_path, "/2_from_EE/elevation_avg_sd_10km_grid.csv")) %>% 
  select(grid_id_10km = ID, 
         value = mean)

gg_mapLayers <- list(list(i = 0, 
                          data = elev, 
                          color = "Terrain 2",
                          name = "elevation", 
                          rev_pal = FALSE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 1,
                          data = aod_pred,
                          color = "Emrld",
                          name = "maximum\npredicted AOD",
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 2, 
                          data = aod_missing, 
                          color = "YlGnBu", 
                          name = "% missing AOD", 
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 3,
                          data = hysplit,
                          color = "BuPu", #"Red-Yellow",
                          name = "trajectory points\nbelow 1.1km",
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 4, 
                          data = fire_dist, 
                          color = "Red-Yellow", 
                          name = "distance\nto fire",
                          rev_pal = FALSE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 5,
                          data = temp,
                          color = "YlOrRd",
                          name = "temperature",
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_sequential))  %>% 
  purrr::reduce(.f = function(prev_gg, layer_list){
    new_gg <- prev_gg + 
      {if(layer_list$i > 0) ggnewscale::new_scale_fill()} + 
      {if(layer_list$i > 0) ggnewscale::new_scale_color()} +
      geom_sf(data = tilted_grid %>% left_join(layer_list$data) %>% 
                mutate(geometry = geometry + layer_list$i*c(0, 20)), 
              aes(fill = value, color = value), 
              size = 0.5, alpha = 1) + 
      layer_list$scale_func(palette = layer_list$color, 
                            aesthetics = c("color", "fill"),
                            rev = layer_list$rev_pal,
                            name = layer_list$name, 
                            guide = guide_colorbar(ticks = FALSE,
                                                   label = FALSE,
                                                   title.position = "right",
                                                   barheight = 3.5, 
                                                   title.theme = element_text(size = 16),
                                                   order = 10 - layer_list$i)) 
    return(new_gg)
  },  .init = ggplot()) 

# png("./figures/test.png",
#     width = 720, height = 880)
{gg_mapLayers +
  ggnewscale::new_scale_fill() +
  ggnewscale::new_scale_color() +
  geom_sf(data = tilted_grid %>%
            mutate(geometry = geometry + 6*c(0, 20)),
          color = "grey30", fill = "grey30", inherit.aes = F) +
  geom_sf(data = contig_plumes_tilt %>% 
            mutate(geometry = geometry + 6*c(0, 20)), 
          aes(fill = "A", color = "A"), alpha = 0.5, 
          show.legend = "polygon", inherit.aes = F) + 
  geom_sf(data = tilted_grid %>% 
            filter(grid_id_10km %in% cloudy_smoke_days) %>%
            mutate(geometry = geometry + 6*c(0, 20)), 
          aes(fill = "B", color = "B"), alpha = 0.5, 
          show.legend = "polygon", inherit.aes = F) + 
  scale_fill_manual(values = c("A" = "grey60",
                               "B" = "red"), 
                    labels = c("plume", "clouds + trajectories"), 
                    name = "", 
                    aesthetics = c("fill", "color"), 
                    guide = guide_legend(order = 5,
                                         label.theme = element_text(size = 16))) + 
  theme_void()}%>% 
  ggsave("./figures/figure1/figure1_layers.png", ., 
      width = 7.2, height = 8.8)

# EPA stations ----
epa_ll <- read_sf(paste0(data_path, "/epa_station_locations")) %>% 
  rename(grid_id_10km = grid_10km)

# map of stations 
# get fold information from final smokePM training data set
station_folds <- readRDS(paste0(data_path, "/4_clean/smokePM_training.rds")) %>% 
  select(id, grid_id_10km, fold) %>% 
  unique

{ggplot() + 
  geom_sf(data = contigUS, 
          color = "grey40", fill = NA) + 
  geom_sf(data = epa_ll %>% 
            filter(!is.na(grid_id_10km)) %>% 
            left_join(station_folds) %>% 
            filter(!is.na(fold)) %>% 
            mutate(fold = as.factor(fold)), 
          aes(color = fold), size = 2) + 
  scale_color_manual(values = MetBrewer::met.brewer("Juarez")[c(1:5)],
                       # MetBrewer::met.brewer("Nattier")[c(1, 2, 3, 4, 6)],
                     # MetBrewer::met.brewer("Juarez")[c(1:3, 5:6)]
                     guide = "none") + 
  theme_void()} %>% 
  ggsave("./figures/figure1/figure1_stations.png", 
         ., width = 9, height = 6)


# times series in a few locations,
# these don't need to match the date anymore, can just find some nice looking 
# time series, maybe that correspond to specific fires 
station_PM <- readRDS(paste0(data_path, "/3_intermediate/station_smokePM.rds"))

purrr::map(c(300490026), #482570005), 
           function(epa_id){
             station_PM %>% 
               filter(id == epa_id) %>% 
               filter(date >= as.Date("2020-08-01") & 
                        date <= as.Date("2020-08-01") + as.difftime(87, units = "days")) %>% 
               group_by(id) %>% 
               mutate(maxPM = max(pm25)) %>% 
               {ggplot(data = ., aes(x = date)) + 
                   geom_line(aes(y = pm25), color = "black") + 
                   geom_line(aes(y = pm25_med_3yr), color = "blue", linetype = "dashed") + 
                   geom_line(aes(y = smokePM), color = "red", lwd = 1.5) + 
                   geom_point(aes(alpha = I(smoke_day), y = -0.07*maxPM), 
                              color = "grey45", cex = 3) + 
                   geom_vline(xintercept = fig_date, color = "grey30", linetype = "dashed") + 
                   geom_text(data = filter(., date == max(date)) %>% 
                               mutate(smoke_day = -0.07*maxPM) %>%
                               dplyr::select(id, date, pm25, pm25_med_3yr, smokePM, smoke_day) %>% 
                               pivot_longer(c(contains("pm"), smoke_day)) %>% 
                               mutate(name = recode(name, 
                                                    "pm25" = "total~PM[2.5]", #"total PM[2.5]",
                                                    "pm25_med_3yr" = "`non-smoke`~median", #"non-smoke median", 
                                                    "smokePM" = "smoke~PM[2.5]",
                                                    "smoke_day" = "smoke~day")),
                             aes(x = date + as.difftime(1, units = "days"), 
                                 y = ifelse(value >0, 1.1, 1)*value,
                                   # ifelse(id == 482570005 & value > 0, 1.25, 1)*value + 
                                   # ifelse(id == 482570005 & name == "total~PM[2.5]", 1.5, 0), 
                                 label = name, 
                                 color = name), 
                             size = 5,
                             parse = TRUE, 
                             hjust = 0) +
                   scale_color_manual(values = c("blue", "grey45", "red", "black"),
                                      guide = "none") +
                   theme_classic() + 
                   ylab(expression(paste(PM[2.5], " (", mu, "g/", m^3, ")"))) + 
                   xlab("") + 
                   scale_x_date(expand = expansion(add = c(0, 1)),
                                date_breaks = "1 month",
                                date_labels = "%b %Y") + 
                   coord_cartesian(clip = "off") +
                   theme(text = element_text(size = 16),
                         plot.margin = unit(c(0, 1.75, 0, 0), "in"))} %>% 
               ggsave(filename = paste0("./figures/figure1/figure1_smokePM_timeseries_", epa_id, ".png"), 
                      plot = ., 
                      width = 6, height = 3.5,
                      device = "png")
           })


# where are they located 
# states <- tigris::states()
# png("./figures/figure1/figure1_timeseries_loc.png")
# ggplot() + 
#   geom_sf(data = states %>% filter(STATEFP %in% nonContig_stateFIPS == FALSE), 
#           color = "black", fill = NA) + 
#   geom_sf(data = epa_ll %>% filter(id %in% c(300490026, 482570005)), color = "red")
# dev.off()

# smoke PM predictions
smokePM_pred <- readRDS(paste0("./output/smokePM_predictions/10km_smoke_days/smokePM_predictions_10km_", 
                               format(fig_date, "%Y%m%d"),
                               ".rds")) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

grid_10km %>% 
  left_join(smokePM_pred %>% 
              select(grid_id_10km, 
                     value = smokePM_pred),
            by = c("ID" = "grid_id_10km")) %>% 
  replace_na(list(value = 0)) %>%
  mutate(value = pmin(value, 150)) %>%
  {ggplot(data = .) + 
      geom_sf(aes(fill = value, color = value), 
              size = 0.5, alpha = 1) + 
      geom_sf(data = contigUS, 
              color = "grey40", fill = NA, lwd = 0.1) + 
      # colorspace::scale_color_continuous_sequential(palette = "Red-Yellow", 
      #                                               aesthetics = c("color", "fill"),
      #                                               guide = guide_colorbar(direction = "horizontal",
      #                                                                      title = expression(smoke~PM[2.5]),
      #                                                                      barwidth = 6, barheight = 1.5,
      #                                                                      title.theme = element_text(size = 20)),
      #                                               trans = "pseudo_log", 
      #                                               breaks = c(0, 5, 10, 25, 100, 250)) + 
      scale_color_gradientn(colors = viridis::inferno(100, begin = 0, end = 1, direction = 1),
                            name = expression(paste(mu, "g/", m^3)),
                            aesthetics = c("fill", "color"), 
                            values = scales::rescale(sinh(seq(0, 2, length.out = 100))),  
                            breaks = seq(0, 150, by = 50),
                            labels = c(seq(0, 100, by = 50), ">150")) + 
      theme_void() + 
      theme(legend.position = c(0.9, 0.32))} %>% 
  ggsave(filename = "./figures/figure1/figure1_smokePM_pred.png", 
         plot = ., width = 6, height = 4)
