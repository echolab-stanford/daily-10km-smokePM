source("work/05_get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(tidyr)
library(mlr)
library(ranger)
library(foreach)
library(doParallel)

num_cores <- 5

#-------------------------------------------------------------------------------
# Does Plume Height from HYSPLIT Improve Daily-Level PM2.5 Prediction in 
# ML MERRA Model?
# Written by Jessica
# Last edited July 2021
# 
# spec 1 = smaller set of covariates
# spec 2 = extended set of covariates
# fold f = held out as test set
#-------------------------------------------------------------------------------
# Choose HYSPLIT data set
inj_heights <- "500+1500+2500"
agg <- "idw"
cut <- 30

s_ <- 1:2

# Load HYSPLIT data
dat_hysplit0 <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_overlap_48hr_EPA_smoke_AOT_covariates.rds"))
dat_hysplit <- dat_hysplit0 %>% 
  mutate(date = as.Date(date)) %>% 
  filter(injection_heights == inj_heights,
         agg_method == agg,
         cutoff == cut) %>% 
  select(epa_id = id_epa, date, height) %>% 
  drop_na(height)

# Load West Coast sample
# dat_west <- readRDS(paste0(path_project, "../complex_MERRA_model/cleaned_station_covariates_smokedays_WestCoast_2006_mid2020.rds"))

# Load EPA panel
# dat_epa <- readRDS(paste0(path_dropbox, "PM25/panel_station_pm_smoke_day_w_county.RDS")) %>% rename

# Load West Coast data frame with spec1 variables and folds
dat_west <- readRDS(paste0(path_project, "cleaned_station_covariates_smokedays_CA_2020.rds")) %>% 
  drop_na(temperature) %>% 
  select(epa_id, date,
         smokePM, fold, month, temperature, precipitation,
         aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3,
         elevation, lat, lon, km_dist, pbl_min)

# Merge HYSPLIT height with smoke and covariates
dat_merged <- dat_hysplit %>% inner_join(dat_west)

# Prepare data frame for predictions
dat_preds <- dat_merged %>% select(epa_id, date)

start_time <- get_start_time()
# registerDoParallel(num_cores)
foreach(s = s_) %:% foreach(f = 1:5) %do% {
  # Specify model
  features <- c("smokePM", "fold", "month", "temperature", "precipitation", 
                "aot_anom", "aot_anom_lag1", "aot_anom_lag2", "aot_anom_lag3", 
                "elevation", "lat", "lon", "km_dist", "pbl_min")
  if (s == 2) {
    features <- c(features, "height")
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
  params <- makeParamSet(makeIntegerParam("mtry", 2, nf),
                         makeIntegerParam("min.node.size", 5, 500),
                         makeDiscreteParam("sample.fraction", c(0.9, 1)),
                         makeDiscreteParam("num.trees", 200))
  
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
  saveRDS(mdl, paste0(path_project, sprintf("HYSPLIT_model_spec%s_fold%s.rds", s, f)))
  saveRDS(predict(mdl, dat_merged)$predictions, paste0(path_project, sprintf("HYSPLIT_predictions_spec%s_fold%s.rds", s, f)))
}
# stopImplicitCluster()
print_time(start_time)

# Collect models and predictions
mdls <- vector("list", 10)
m <- 1
for (s in s_) {
  for (f in 1:5) {
    mdls[[m]] <- readRDS(paste0(path_project, sprintf("HYSPLIT_model_spec%s_fold%s.rds", s, f)))
    dat_preds[, sprintf("pred_spec%s_fold%s", s, f)] <- readRDS(paste0(path_project, sprintf("HYSPLIT_predictions_spec%s_fold%s.rds", s, f)))
    m <- m + 1
  }
}

# Save collected models and predictions
saveRDS(mdls, paste0(path_project, "HYSPLIT_random_forest_models.rds"))
saveRDS(dat_preds, paste0(path_project, "HYSPLIT_random_forest_predictions.rds"))

# Calculate test R2 for each fold
dat_preds <- full_join(dat_merged, dat_preds)
for(s in s_) {
  for (f in 1:5) {
    df <- dat_preds %>% filter(fold == f) %>% select(smokePM, paste0("pred_spec", s, "_fold", f))
    r2 <- cor(df$smokePM, df[2])^2
    print(paste("fold", f, ":", round(r2, 4)))
  }
}

# Compare spec1 (w/o HYSPLIT) and spec2 (w/ HYSPLIT) test R2 by fold
df <- dat_preds %>% 
  select(epa_id, date, fold, smokePM, starts_with("pred")) %>% 
  pivot_longer(cols = starts_with("pred"),
               names_to = c("spec", "test_fold"),
               names_prefix = "pred_",
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = as.integer(substr(spec, 5, 5)),
         test_fold = as.integer(substr(test_fold, 5, 5))) %>% 
  filter(fold == test_fold) %>% 
  group_by(fold, spec) %>% 
  summarize(R2 = cor(smokePM, pred)^2) %>% 
  ungroup()

df %>% 
  pivot_wider(names_from = spec,
              names_prefix = "spec",
              values_from = R2) %>% 
  rename(noHY = spec1, withHY = spec2) %>% 
  mutate(noHY_minus_withHY = noHY - withHY)

# Compare spec1 (w/o HYSPLIT) and spec2 (w/ HYSPLIT) test RMSE by fold
df <- dat_preds %>% 
  select(epa_id, date, fold, smokePM, starts_with("pred")) %>% 
  pivot_longer(cols = starts_with("pred"),
               names_to = c("spec", "test_fold"),
               names_prefix = "pred_",
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = as.integer(substr(spec, 5, 5)),
         test_fold = as.integer(substr(test_fold, 5, 5))) %>% 
  filter(fold == test_fold) %>% 
  group_by(fold, spec) %>% 
  summarize(RMSE = sqrt(mean((smokePM - pred)^2))) %>% 
  ungroup()

df %>% 
  pivot_wider(names_from = spec,
              names_prefix = "spec",
              values_from = RMSE) %>% 
  rename(noHY = spec1, withHY = spec2) %>% 
  mutate(noHY_minus_withHY = noHY - withHY)
