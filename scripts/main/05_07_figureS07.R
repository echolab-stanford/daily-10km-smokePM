# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 7.
# ------------------------------------------------------------------------------
aod_df <- readRDS(file.path(path_data, "4_clean", "aod_training.rds"))
grid_orig <- st_read(file.path(path_data, "1_grids", "1km_aod_grid_wgs84_training")) %>% 
  mutate(train_cells = (grid_id %in% unique(aod_df$grid_id_1km)))

states <- tigris::states(cb = TRUE) %>% 
  filter(!(STATEFP %in% nonContig_stateFIPS))

{ggplot() + 
    geom_sf(data = states, 
            color = "grey30", 
            lwd = 0.1) + 
    geom_sf(data = grid_orig, 
            aes(color = train_cells, 
                fill = train_cells)) + 
    scale_color_manual(name = "", 
                       values = c("black", "blue"), 
                       labels = c("original 5,000 cells", 
                                  "training locations"), 
                       aesthetics = c("color", "fill")) + 
    theme_void() + 
    theme(legend.position = c(0.75, 0.3), 
          legend.justification = "left", 
          legend.key.size = unit(0.4, 'cm'))} %>% 
  ggsave(file.path(path_figures, "figureS07.png"), 
         ., width = 5, height = 3.5)
