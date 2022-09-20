# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Calculates model performance metrics.
# ------------------------------------------------------------------------------
monitors <- st_read(file.path(path_data, "epa_station_locations")) 
states <- tigris::states() %>% st_transform(st_crs(monitors))

monitors$state <- states$STUSPS[st_intersects(monitors, states) %>% unlist]

smokePM_data <- readRDS(file.path(path_data, "4_clean", "smokePM_training.rds")) %>% 
  select(id, date, smokePM, fold)

cv_preds <- list.files(file.path(path_output, "smokePM_model"), 
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

# metrics on different sample for main model, table S10
model_metrics %>% 
  filter(drop_var == "") %>% 
  filter(test == TRUE & metric != "nobs" & metric != "me" &
           !grepl("bin", metric, ignore.case = TRUE)) %>% 
  select(-drop_var, -test, -rank) %>% 
  pivot_wider(names_from = metric, values_from = value)

# confirm two decimal places for RMSE on over 50 days
model_metrics %>% 
  filter(drop_var=="", test==T, metric=="rmse", subset=="day_over50") %>% 
  pull(value)
