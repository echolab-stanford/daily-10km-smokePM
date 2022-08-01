source("./scripts/0_config.R")
library(sf)

hysplit_date <- "2020-09-20"

states <- tigris::states(cb = TRUE) %>% 
  filter(!(STATEFP %in% nonContig_stateFIPS))

traj <- list.files(paste0("./output/hysplit_trajectory/", hysplit_date), full.names = T)[1] %>% 
  list.files(full.names = T) %>% 
  extract(4:6) %>% 
  map_dfr(function(x){readRDS(x) %>% mutate(file = x)})

height_data <- readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/6-day_misc/heights.rds")



# panel A, example hysplit traj from single point and hour 
traj %>% 
  separate(file, into = c(NA, NA, NA, NA, NA, "init_height"), sep = "/") %>% 
  separate(init_height, into = c(NA, "init_height"), sep = "hgt_") %>% 
  mutate(init_height = gsub("-144h.rds", "", init_height, fixed = T)) %>% 
{ggplot(data = .) + 
    geom_sf(data = states, inherit.aes = FALSE) + 
    geom_point(mapping = aes(x = lon, y = lat, 
                             color = height), size = 1) + 
    geom_path(aes(x = lon, y = lat, group = init_height), size = 0.2) +
    geom_text(data = filter(., hour_along == max(hour_along)), 
              aes(x = lon + ifelse(init_height == "2500", 0.5, -0.5),
                  y = lat, 
                  label = paste0(init_height, " m"),
                  hjust = ifelse(init_height == "2500", 0, 1))) +
    scale_color_viridis_c("height above\nground level (m)", 
                          option = "magma", direction = -1) + 
    theme_void()} %>% 
  ggsave(filename = "./figures/supplement/S1_hysplit_traj.png", .,
         width = 5, height = 3)

# panel B, distribution of heights for quintiles
height_quant <- quantile(height_data, probs = seq(0, 1, by = 0.2))

height_quant_df <- data.frame(height = height_quant/1e3, 
                              quant = names(height_quant)) %>% 
  mutate(label = paste0(quant, "\n", round(height, 1), " km")) %>% 

# height_hist <- table(height_data/1e3, 
set.seed(10001)
height_samp <- sample(1:length(height_data), size = 1e6, replace = F)

height_hist <- data.frame(height = height_data[height_samp]/1e3) %>% 
  # slice_sample(n = 1e6) %>%
  ggplot(aes(x = height, y = ..density..)) + 
  geom_histogram(boundary = 0) + 
  geom_vline(data = height_quant_df %>% filter(quant != "100%"), 
             aes(xintercept = height), 
             color = "#2b614e") + 
  geom_label(data = height_quant_df %>% filter(quant != "100%"), 
             aes(x = height, y = .3, label = label), 
             position = position_dodge(width = 0.2),
             label.size = 0, size = 2, label.padding = unit(0.1, "lines"),
             color = "#2b614e") + 
  xlab("height above ground level (km)") + ylab("density") + 
  scale_x_continuous(limits = c(NA, 12), expand = c(0.05, 0)) + 
  theme_classic()

ggsave(filename = "./figures/supplement/S1_hysplit_hist.png", height_hist,
       width = 5, height = 3)


# pabel C, counts in each height quintile in the 50km buffer 
hysplit <- list.files("~/BurkeLab Dropbox/Data/hms_hysplit/10km_grid_2006-2020",
                                     pattern = paste0(format(as.Date(hysplit_date), "%Y"), 
                                                      "_",
                                                      format(as.Date(hysplit_date), "%m")),
                                     full.names = T) %>% 
  readRDS %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  filter(date == as.Date(hysplit_date))

grid_10km <- st_read(paste0(data_path, "/1_grids/grid_10km_wgs84"))

hysplit %>% 
  select(ID = id_grid, contains("num_traj_points_height")) %>% 
  pivot_longer(starts_with("num")) %>% 
  separate(name, into = c(NA, NA, NA, NA, "n"), sep = "_", remove = FALSE, 
           convert = T) %>% 
  left_join(data.frame(height = unname(height_quant)/1e3) %>% 
              mutate(n = 1:n(),
                     label = paste0(round(height, 1), 
                                    " - ", 
                                    lead(round(height, 1)), 
                                    " km above ground level")) %>% 
              select(n, label)) %>% 
  left_join(grid_10km %>% select(ID)) %>% 
  st_as_sf %>% 
  {ggplot(data = .) + 
      geom_sf(aes(fill = value, color = value)) + 
      facet_wrap(~label, nrow = 1) + 
      scale_color_gradientn(colors = colorspace::sequential_hcl(palette = "BuPu", n = 100, rev = T),
                            name = "trajectory points\nwithin 50km",
                            aesthetics = c("fill", "color"), 
                            values = scales::rescale(sinh(seq(0, 5, length.out = 100))),
                            guide = guide_colorbar(title.theme = element_text(size = 7),
                                                   label.theme = element_text(size = 6.5), 
                                                   barheight = 4, barwidth = 1)) +
      theme_void() + 
      theme(strip.text = element_text(size = 8),
            panel.spacing = unit(-0.5, "lines"))} %>% 
  ggsave(filename = "./figures/supplement/S1_hysplit_gridded.png",. ,
         height = 3, width = 12)
  

