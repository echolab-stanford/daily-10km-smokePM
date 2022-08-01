source("./scripts/0_config.R")
library(sf)

hysplit_date <- "2018-11-11"

hysplit <- list.files("~/BurkeLab Dropbox/Data/hms_hysplit/10km_grid_2006-2020",
                      pattern = paste0(format(as.Date(hysplit_date), "%Y"), 
                                       "_",
                                       format(as.Date(hysplit_date), "%m")),
                      full.names = T) %>% 
  readRDS %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  filter(date == as.Date(hysplit_date))

grid_10km <- st_read(paste0(data_path, "/1_grids/grid_10km_wgs84"))

mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}


hysplit %>% 
  select(ID = id_grid, num_traj_points_height_1) %>% 
  left_join(grid_10km %>% select(ID)) %>% 
  st_as_sf %>% 
  {ggplot(data = .) + 
      geom_sf(aes(fill = num_traj_points_height_1, color = num_traj_points_height_1)) + 
      scale_color_gradientn(colors = c(cmocean::cmocean(name = "oxy", start = 0.2, end = 0.79, direction = -1)(20),
                                       cmocean::cmocean(name = "oxy", start = 0, end = 0.19, direction = -1)(20)),
        # colors = colorspace::sequential_hcl(palette = "BuPu", n = 100, rev = T),
                            name = "trajectory points\nwithin 50km",
                            aesthetics = c("fill", "color"), 
                            rescaler = mid_rescaler(50),
                            # values = scales::rescale(sinh(seq(0, 5, length.out = 100))),
                            guide = guide_colorbar(title.theme = element_text(size = 7),
                                                   label.theme = element_text(size = 6.5), 
                                                   barheight = 4, barwidth = 1)) +
      theme_void() + 
      theme(legend.position = c(0.93, 0.35))} %>% 
  ggsave(filename = "./figures/supplement/S2_hysplit_lowest_gridded.png",
         ., height = 3.5, width = 5)
