source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(tidyr)
library(mlr)
library(ranger)
library(tigris)

#-------------------------------------------------------------------------------
# Fit Random Forest Without and With Spatial Re-weighting
# Written by Jessica
# Last edited September 2021
#-------------------------------------------------------------------------------
# Limit to West Coast
west_coast <- fips_codes %>% 
  select(state, state_code) %>% 
  distinct() %>% 
  filter(state %in% c("CA", "OR", "WA")) %>% 
  pull(state_code)

# Load data
dat_merged <- readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates_AOD.rds")) %>% 
  drop_na(pm25, smokePM) %>% 
  filter(state %in% west_coast) %>% 
  select(epa_id, date, smoke_day, state,
         smokePM, fold, month, temperature, precipitation, 
         aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
         elevation, lat, lon, km_dist, pbl_min, 
         wind_u, wind_v, wind, elevation_stdDev,
         mean_sea_level_pressure, surface_pressure, 
         dewpoint_2m_temperature, pbl_max, pbl, 
         aod) %>% 
  drop_na()
dat_preds <- dat_merged %>% select(epa_id, lon, lat, date, fold, state, month, smoke_day, smokePM)
dat_merged_id <- dat_merged %>% 
  filter(smoke_day == 1) %>% 
  select(-date, -state, -smoke_day)
dat_merged_sd <- dat_merged_id %>% select(-epa_id)
dat_merged <- dat_merged %>% select(-epa_id, -date, -state, -smoke_day, -smokePM, -fold)

try(log_file <- file("~/Desktop/spatial_reweighting.log", open = "at"))
sink(log_file, type="output", append = TRUE, split = TRUE)
sink(log_file, type="message", append = TRUE)

for (s in 1:2) {
  for (f in 1:5) {
    # Get spatial weights
    if (s == 1) {
      weights <- NULL
    } else if (s == 2) {
      weights <- dat_merged_id %>% 
        filter(fold != f) %>% 
        group_by(epa_id) %>% 
        mutate(weight = 1/n()) %>% 
        ungroup() %>% 
        pull(weight)
    }
    
    # Split train/test by fold
    dat_train <- dat_merged_sd %>% filter(fold != f)
    train_folds <- dat_train$fold
    dat_train <- dat_train %>% select(-fold)
    dat_test <- dat_merged_sd %>% filter(fold == f) %>% select(-fold)
    
    # Choose resampling strategy and define grid
    task <- makeRegrTask(data = dat_train,
                         target = "smokePM",
                         weights = weights,
                         blocking = factor(train_folds))
    learner <- makeLearner("regr.ranger")
    rsmpl_desc <- makeResampleDesc("CV", fixed = TRUE)
    nf <- length(names(dat_train)) - 1
    params <- makeParamSet(makeIntegerParam("mtry", 2, nf - 1),
                           makeIntegerParam("min.node.size", 10, 100),
                           makeDiscreteParam("sample.fraction", c(0.2, 0.4, 0.6, 0.8, 1)),
                           makeDiscreteParam("num.trees", 100))
    
    # Tune hyperparameters
    start_time <- get_start_time()
    set.seed(7569)
    res <- tuneParams(learner, task, rsmpl_desc, par.set = params,
                      control = makeTuneControlRandom(maxit = 25))
    print_time(start_time)
    
    # Fit random forest model
    set.seed(3473)
    mdl <- ranger(smokePM ~ .,
                  data = dat_train,
                  min.node.size = res$x$min.node.size,
                  mtry = res$x$mtry,
                  sample.fraction = res$x$sample.fraction,
                  num.trees = 1000,
                  importance = "impurity",
                  case.weights = weights)

    # Save model and predictions
    saveRDS(mdl, paste0(path_project, sprintf("spatial_reweighting_model_spec%s_fold%s.rds", s, f)))
    saveRDS(predict(mdl, dat_merged)$predictions, paste0(path_project, sprintf("spatial_reweighting_predictions_spec%s_fold%s.rds", s, f)))
  }
}
print(paste("Finished training and predicting:", Sys.time()))
sink(type = "output")
sink(type = "message")
close(log_file)

# Collect models and predictions
sd <- dat_preds$smoke_day == 1
mdls <- vector("list", 10)
m <- 1
for (s in 1:2) {
  for (f in 1:5) {
    mdls[[m]] <- readRDS(paste0(path_project, sprintf("spatial_reweighting_model_spec%s_fold%s.rds", s, f)))
    preds <- readRDS(paste0(path_project, sprintf("spatial_reweighting_predictions_spec%s_fold%s.rds", s, f)))
    col <- sprintf("pred_spec%s_fold%s", s, f)
    dat_preds[, col] <- ifelse(sd & (preds >= 0), preds, 0)
    m <- m + 1
  }
}

# Save collected models and predictions
saveRDS(mdls, paste0(path_project, "spatial_reweighting_random_forest_models.rds"))
saveRDS(dat_preds, paste0(path_project, "spatial_reweighting_random_forest_predictions.rds"))
