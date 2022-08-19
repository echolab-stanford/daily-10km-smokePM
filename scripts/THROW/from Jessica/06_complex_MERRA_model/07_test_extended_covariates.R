source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(mlr)
library(ranger)
library(foreach)
library(doParallel)

num_cores <- 5

#-------------------------------------------------------------------------------
# Fit Random Forest Covariate Model on Extended Covariates Set
# Written by Jessica
# Last edited August 2021
# 
# spec 1 = smaller set of covariates
# spec 2 = extended set of covariates
# fold f = held out as test set
#-------------------------------------------------------------------------------
# Load data
dat_merged <- readRDS(paste0(path_project, "cleaned_station_covariates_smokedays_WestCoast_2006_mid2020.rds"))
dat_preds <- dat_merged %>% select(epa_id, date)

# registerDoParallel(num_cores)
foreach(s = 1:2) %:% foreach(f = 1:5) %do% {
  # Specify model
  features <- c("smokePM", "fold", "month", "temperature", "precipitation", 
                "aot_anom", "aot_anom_lag1", "aot_anom_lag2", "aot_anom_lag3", 
                "elevation", "lat", "lon", "km_dist", "pbl_min")
  if (s == 2) {
    features <- c(features, "wind_u", "wind_v", "wind", "elevation_stdDev",
                  "mean_sea_level_pressure", "surface_pressure", 
                  "dewpoint_2m_temperature", "pbl_max", "pbl")
  }
  dat_merged_s <- dat_merged %>% select(features)
  
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
  params <- makeParamSet(makeIntegerParam("mtry", 2, nf - 2),
                         makeIntegerParam("min.node.size", 10, 500),
                         makeDiscreteParam("sample.fraction", c(0.8, 1)),
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
  saveRDS(mdl, paste0(path_project, sprintf("model_spec%s_fold%s.rds", s, f)))
  saveRDS(predict(mdl, dat_merged)$predictions, paste0(path_project, sprintf("predictions_spec%s_fold%s.rds", s, f)))
}
# stopImplicitCluster()
print(Sys.time())

# Collect models and predictions
mdls <- vector("list", 10)
m <- 1
for (s in 1:2) {
  for (f in 1:5) {
    mdls[[m]] <- readRDS(paste0(path_project, sprintf("model_spec%s_fold%s.rds", s, f)))
    dat_preds[, sprintf("pred_spec%s_fold%s", s, f)] <- readRDS(paste0(path_project, sprintf("predictions_spec%s_fold%s.rds", s, f)))
    m <- m + 1
  }
}

# Save collected models and predictions
saveRDS(mdls, paste0(path_project, "random_forest_models.rds"))
saveRDS(dat_preds, paste0(path_project, "random_forest_predictions.rds"))

# Calculate test R2 for each fold
dat_preds <- full_join(dat_merged, dat_preds)
for (f in 1:5) {
  df <- dat_preds %>% filter(fold == f) %>% select(smokePM, paste0("pred_spec2_fold", f))
  r2 <- cor(df$smokePM, df[, 2])^2
  print(paste("fold", f, ":", round(r2, 4)))
}
