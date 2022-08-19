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
library(foreach)
library(doParallel)

num_cores <- 5

scratch <- Sys.getenv("SCRATCH")
path_sample_test <- paste0(scratch, "/sample_test/")
node <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
s_ <- (node - 1) %/% 5 + 1
f_ <- node %% 5
f_ <- ifelse(f_ == 0, 5, f_)

#-------------------------------------------------------------------------------
# Does Training on All Days Do Better Than Training on Only Smoke Days?
# Written by Jessica
# Last edited July 2021
# 
# Fit random forest using nested CV with all days and smoke days. Evaluate on 
# the held out test set for each fold using model predictions (lower bounded at 
# 0) on smoke days and 0 for non-smoke days with R2, within R2, and RMSE for 
# full set of days, smoke days, and days with smokePM > 0.
#-------------------------------------------------------------------------------
# Load data
dat_merged <- readRDS(paste0(path_sample_test, "epa_station_smokePM_full_panel_extendedCovariates.rds")) %>% 
  drop_na(pm25, smokePM) %>% 
  select(epa_id, date, 
         smokePM, smoke_day, fold, month, temperature, precipitation, 
         aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
         elevation, lat, lon, km_dist, pbl_min, 
         wind_u, wind_v, wind, elevation_stdDev,
         mean_sea_level_pressure, surface_pressure, 
         dewpoint_2m_temperature, pbl_max, pbl) %>% 
  drop_na()
dat_preds <- dat_merged %>% select(epa_id, date, smoke_day, fold)
dat_merged <- dat_merged %>% select(-epa_id, -date)

# registerDoParallel(num_cores)
foreach(s = s_) %:% foreach(f = f_) %do% {
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
  mns2 <- ifelse(s == 1, 100000, 30000)
  params <- makeParamSet(makeIntegerParam("mtry", 2, nf - 2),
                         makeIntegerParam("min.node.size", 10000, mns2),
                         makeDiscreteParam("sample.fraction", c(0.4, 0.6, 0.8, 1)),
                         makeDiscreteParam("num.trees", 100))
  
  # Tune hyperparameters
  start_time <- get_start_time()
  set.seed(7569)
  res <- tuneParams(learner, task, rsmpl_desc, par.set = params,
                    control = makeTuneControlRandom(maxit = 10))
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
  saveRDS(mdl, paste0(path_sample_test, sprintf("sample_test_model_sample%s_fold%s.rds", s, f)))
  saveRDS(predict(mdl, dat_merged)$predictions, paste0(path_sample_test, sprintf("sample_test_predictions_sample%s_fold%s.rds", s, f)))
}
# stopImplicitCluster()
print(Sys.time())