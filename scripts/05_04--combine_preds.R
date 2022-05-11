source("./scripts/0_config.R")

smokePM_pred <- list.files("./output/smokePM_predictions/10km_smoke_days", 
                           full.names = TRUE) %>% 
  map_dfr(readRDS)
  
smokePM_pred %<>% mutate(smokePM_pred = pmax(0, smokePM_pred))

saveRDS(smokePM_pred, 
        paste0(output_path,
               "/smokePM_predictions_", 
               format(min(smokePM_pred$date), "%Y%m%d"),
               "_",
               format(max(smokePM_pred$date), "%Y%m%d"),
               ".rds"))
