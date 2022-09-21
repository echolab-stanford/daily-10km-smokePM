source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 10.
# ------------------------------------------------------------------------------
monitors <- st_read(file.path(path_data, "epa_station_locations")) 
states <- tigris::states() %>% st_transform(st_crs(monitors))

monitors$state <- states$STUSPS[st_intersects(monitors, states) %>% unlist]

smokePM_data <- readRDS(file.path(path_data, "4_clean", "smokePM_training.rds")) %>% 
  select(id, date, smokePM, fold)

cv_preds <- list.files(file.path(path_output, "smokePM", "model"), 
                       pattern = "pred_fold", 
                       full.names = TRUE) %>% 
  purrr::map_dfr(function(x){
    readRDS(x) %>%
      mutate(test_fold = gsub("^smokePM_pred_fold|_drop|-aod_anom_pred|-traj_points|\\.rds$", "", basename(x)), 
             drop_var = gsub("^smokePM_pred_fold[0-4]_drop-?|\\.rds$", "", basename(x))) %>%
      return()
  })

model_metrics <- cv_preds %>%
  mutate(smokePM_pred = pmax(0, smokePM_pred)) %>%
  left_join(smokePM_data, by = c("id", "date", "fold")) %>% 
  mutate(test = fold == test_fold) %>%
  eval_metrics(models = drop_var, test_tune = test, 
               obs = smokePM, pred = smokePM_pred, loc_id = id) %>%
  pivot_longer(contains("rank") | contains("value")) %>% 
  separate(name, into = c("metric", "rank_value"), sep = "_(?=rank|value)") %>% 
  pivot_wider(values_from = value, names_from = rank_value) %>%
  separate(metric, into = c("metric", "subset"), sep = "_", extra = "merge") 

# comparison between full model, without AOD preds, and without HYSPLIT
model_metrics %>%
  filter(grepl("day", subset)) %>% 
  filter(metric %in% c("rmse", "r2", "wr2")) %>% 
  filter(test == TRUE) %>% 
  mutate(subset = recode_factor(subset, 
                                "day" = "all days", 
                                "day_sub50" = "days with\n< 50ug",
                                "day_over50" = "days with\n> 50ug", 
                                .ordered = T), 
         metric = recode_factor(metric, 
                                "r2" = "R^2", 
                                "wr2" = "within~R^2", 
                                "rmse" = "RMSE",
                                .ordered = TRUE),
         drop_var = case_when(drop_var == "" ~ "full model", 
                              drop_var =="aod_anom_pred" ~ "no AOD predictions", 
                              drop_var =="traj_points" ~ "no HYSPLIT"),
         drop_var = factor(drop_var, 
                           levels = c("full model", "no HYSPLIT", "no AOD predictions"), 
                           ordered = TRUE)) %>% 
  {ggplot(data = ., 
          aes(x = subset, y = value, color = drop_var,
              group = drop_var)) + 
      geom_point() + 
      geom_line() + 
      scale_x_discrete(expand = expansion(mult = 0.04)) + 
      ylim(0, NA) + 
      facet_wrap(~metric, scales = "free", strip.position = "left",
                 labeller = label_parsed) + 
      theme_classic() +
      theme(strip.placement = "outside",
            strip.background = element_blank(),
            panel.spacing = unit(-0.1, "lines"),
            strip.switch.pad.wrap = unit(-0.25, "lines")) + 
      scale_color_manual(name = "", values = MetBrewer::met.brewer("Homer2", 5)[c(1,2,4)]) + 
      xlab("") + ylab("")} %>% 
  ggsave(file.path(path_figures, "figureS10.png"), ., 
         width = 8, height= 3)

# change in model r2 from aod and hysplit inclusion
model_metrics %>%
  filter(subset == "day" & metric == "r2"  & test == TRUE) %>% 
  mutate(diff = max(value) - value) %>% View
