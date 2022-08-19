source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(tidyr)
library(mlr)
library(ranger)
library(foreach)
library(doParallel)

num_cores <- 5

#-------------------------------------------------------------------------------
# Does Training on All Days Do Better Than Training on Only Smoke Days?
# Written by Jessica
# Last edited July 2021
# 
# Fit random forest using nested CV with all days and smoke days. Evaluate on 
# the held out test set for each fold using model predictions (lower bounded at 
# 0) on smoke days and 0 for non-smoke days with R2, within R2, and RMSE for 
# full set of days, smoke days, and days with smokePM > 50.
#-------------------------------------------------------------------------------
# Load data
dat_merged <- readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates.rds")) %>% 
  drop_na(pm25, smokePM) %>% 
  select(epa_id, date, 
         smokePM, smoke_day, fold, month, temperature, precipitation, 
         aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
         elevation, lat, lon, km_dist, pbl_min, 
         wind_u, wind_v, wind, elevation_stdDev,
         mean_sea_level_pressure, surface_pressure, 
         dewpoint_2m_temperature, pbl_max, pbl) %>% 
  drop_na()
dat_preds <- dat_merged %>% select(epa_id, date, smoke_day, fold, smokePM)
dat_merged <- dat_merged %>% select(-epa_id, -date)

# registerDoParallel(num_cores)
foreach(s = 1:2) %:% foreach(f = 1:5) %do% {
  dat_merged_s <- dat_merged
  if (s == 2) dat_merged_s <- dat_merged_s %>% filter(smoke_day == 1)
  
  # Split train/test by fold
  dat_train <- dat_merged_s %>% filter(fold != f)
  train_folds <- dat_train$fold
  dat_train <- dat_train %>% select(-fold, -smoke_day)
  dat_test <- dat_merged_s %>% filter(fold == f) %>% select(-fold, -smoke_day)
  
  # Choose resampling strategy and define grid
  task <- makeRegrTask(data = dat_train, 
                       target = "smokePM", 
                       blocking = factor(train_folds))
  learner <- makeLearner("regr.ranger")
  rsmpl_desc <- makeResampleDesc("CV", fixed = TRUE)
  nf <- length(names(dat_train)) - 1
  params <- makeParamSet(makeIntegerParam("mtry", 2, 3),#2, nf),
                         makeIntegerParam("min.node.size", 1000000, 1000001),#100, 1000),
                         makeDiscreteParam("sample.fraction", c(0.2, 0.3)),#c(0.4, 0.6, 0.8, 1)),
                         makeDiscreteParam("num.trees", 2))#100))
  
  # Tune hyperparameters
  # start_time <- get_start_time()
  # set.seed(7569)
  # res <- tuneParams(learner, task, rsmpl_desc, par.set = params,
  #                   control = makeTuneControlRandom(maxit = 1))
  # print_time(start_time)
  
  # Fit random forest model
  set.seed(3473)
  mdl <- ranger(smokePM ~ .,
                data = dat_train,
                min.node.size = 1000001,#res$x$min.node.size,
                mtry = 3,#res$x$mtry,
                sample.fraction = 0.3,#res$x$sample.fraction,
                num.trees = 2,#500,
                importance = "impurity")
  
  # Save model and predictions
  saveRDS(mdl, paste0(path_project, sprintf("sample_test_model_sample%s_fold%s.rds", s, f)))
  saveRDS(predict(mdl, dat_merged)$predictions, paste0(path_project, sprintf("sample_test_predictions_sample%s_fold%s.rds", s, f)))
}
# stopImplicitCluster()
print(Sys.time())

# Collect models and predictions
sd <- dat_preds$smoke_day == 1
mdls <- vector("list", 10)
m <- 1
for (s in 1:2) {
  for (f in 1:5) {
    mdls[[m]] <- readRDS(paste0(path_project, sprintf("sample_test_model_sample%s_fold%s.rds", s, f)))
    preds <- readRDS(paste0(path_project, sprintf("sample_test_predictions_sample%s_fold%s.rds", s, f)))
    col <- sprintf("pred_sample%s_fold%s", s, f)
    dat_preds[, col] <- ifelse(sd & (preds >= 0), preds, 0)
    m <- m + 1
  }
}

# Save collected models and predictions
saveRDS(mdls, paste0(path_project, "sample_test_random_forest_models.rds"))
saveRDS(dat_preds, paste0(path_project, "sample_test_random_forest_predictions.rds"))

# Calculate test R2 for each fold
for (f in 1:5) {
  df <- dat_preds %>% filter(fold == f) %>% select(smokePM, paste0("pred_sample2_fold", f))
  r2 <- cor(df$smokePM, df[, 2])^2
  print(paste("fold", f, ":", round(r2, 4)))
}
