print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}

library(dplyr)
library(tidyr)
library(mlr)
library(ranger)

scratch <- Sys.getenv("SCRATCH")
path_aod <- paste0(scratch, "/aod/")

#-------------------------------------------------------------------------------
# Fit Random Forest Without and With AOD
# Written by Jessica
# Last edited August 2021
#-------------------------------------------------------------------------------
# Load data
dat_merged <- readRDS(paste0(path_aod, "epa_station_smokePM_full_panel_extendedCovariates_AOD.rds")) %>% 
  drop_na(pm25, smokePM) %>% 
  filter(smoke_day == 1) %>% 
  select(epa_id, date, 
         smokePM, fold, month, temperature, precipitation, 
         aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
         elevation, lat, lon, km_dist, pbl_min, 
         wind_u, wind_v, wind, elevation_stdDev,
         mean_sea_level_pressure, surface_pressure, 
         dewpoint_2m_temperature, pbl_max, pbl, 
         aod) %>% 
  drop_na()
dat_preds <- dat_merged %>% select(epa_id, date, fold, smokePM)
dat_merged <- dat_merged %>% select(-epa_id, -date)

for (s in 1:2) {
  dat_merged_s <- dat_merged
  if (s == 1) dat_merged_s <- dat_merged_s %>% select(-aod)
  
  for (f in 1:5) {
    # Split train/test by fold
    dat_train <- dat_merged_s %>% filter(fold != f)
    train_folds <- dat_train$fold
    dat_train <- dat_train %>% select(-fold)
    dat_test <- dat_merged_s %>% filter(fold == f) %>% select(-fold)
    
    # Choose resampling strategy and define grid
    task <- makeRegrTask(data = dat_train, 
                         target = "smokePM", 
                         blocking = factor(train_folds))
    learner <- makeLearner("regr.ranger")
    rsmpl_desc <- makeResampleDesc("CV", fixed = TRUE)
    nf <- length(names(dat_train)) - 1
    params <- makeParamSet(makeIntegerParam("mtry", 2, nf - 1),
                           makeIntegerParam("min.node.size", 10, 1000),
                           makeDiscreteParam("sample.fraction", c(0.2, 0.4, 0.6, 0.8, 1)),
                           makeDiscreteParam("num.trees", 100))
    
    # Tune hyperparameters
    start_time <- get_start_time()
    set.seed(7569)
    res <- tuneParams(learner, task, rsmpl_desc, par.set = params,
                      control = makeTuneControlRandom(maxit = 100))
    print_time(start_time)
    
    # Fit random forest model
    set.seed(3473)
    mdl <- ranger(smokePM ~ .,
                  data = dat_train,
                  min.node.size = res$x$min.node.size,
                  mtry = res$x$mtry,
                  sample.fraction = res$x$sample.fraction,
                  num.trees = 500,
                  importance = "impurity")
    
    # Save model and predictions
    saveRDS(mdl, paste0(path_aod, sprintf("AOD_model_spec%s_fold%s.rds", s, f)))
    saveRDS(predict(mdl, dat_merged)$predictions, paste0(path_aod, sprintf("AOD_predictions_spec%s_fold%s.rds", s, f)))
  }
}
print(Sys.time())

# Collect models and predictions
mdls <- vector("list", 10)
m <- 1
for (s in 1:2) {
  for (f in 1:5) {
    mdls[[m]] <- readRDS(paste0(path_aod, sprintf("AOD_model_spec%s_fold%s.rds", s, f)))
    preds <- readRDS(paste0(path_aod, sprintf("AOD_predictions_spec%s_fold%s.rds", s, f)))
    col <- sprintf("pred_spec%s_fold%s", s, f)
    dat_preds[, col] <- pmax(preds, 0)
    m <- m + 1
  }
}

# Save collected models and predictions
saveRDS(mdls, paste0(path_aod, "AOD_random_forest_models.rds"))
saveRDS(dat_preds, paste0(path_aod, "AOD_random_forest_predictions.rds"))
