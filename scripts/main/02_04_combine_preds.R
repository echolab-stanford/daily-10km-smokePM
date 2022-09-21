#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Combines smoke PM2.5 predictions.
#-------------------------------------------------------------------------------
smokePM_pred <- list.files(
  file.path(path_output, "smokePM", "predictions", "10km_smoke_days"), 
  full.names = TRUE
) %>% 
  map_dfr(readRDS)

smokePM_pred %<>% mutate(smokePM_pred = pmax(0, smokePM_pred))

saveRDS(smokePM_pred, 
        file.path(path_output, "smokePM", "predictions", "combined", 
                  paste0("smokePM_predictions_", 
                         format(min(smokePM_pred$date), "%Y%m%d"),
                         "_",
                         format(max(smokePM_pred$date), "%Y%m%d"),
                         ".rds")))
