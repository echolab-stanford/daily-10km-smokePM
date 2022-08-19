source("work/05_get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(tidyr)
library(mlr)
library(xgboost)

#-------------------------------------------------------------------------------
# Does Plume Height from HYSPLIT Improve Daily-Level PM2.5 Prediction in 
# ML MERRA Model?
# Written by Jessica
# Last edited August 2021
#-------------------------------------------------------------------------------
# Choose HYSPLIT data set
agg <- "idw"
cutoffs <- c(10, 30, 50, 80, 100)
injection_heights <- c("500+1500+2500", "500+1500", "500+2500", "1500+2500")
specs <- c("baseline", "n_traj_points", "height", "pressure")

# Load CA 2020 data frame with spec1 variables and folds
# dat_west <- readRDS(paste0(path_project, "cleaned_station_covariates_smokedays_CA_2020.rds"))
dat_panel <- readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates.rds")) %>% 
  drop_na(pm25, smokePM) %>% 
  filter(state == "06",
         year == 2020,
         smoke_day == 1) %>% 
  mutate(month = month %>% as.character() %>% as.numeric()) %>% 
  select(epa_id, date, smoke_day, 
         smokePM, fold, month, temperature, precipitation, 
         aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
         elevation, lat, lon, km_dist, pbl_min) %>% 
  drop_na()

# Load HYSPLIT data
dat_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_overlap_48hr_EPA_smoke_AOT_covariates.rds")) %>% 
  filter(agg_method == agg) %>% 
  mutate(date = as.Date(date)) %>% 
  select(date, epa_id = id_epa, agg_method, injection_heights, cutoff,
         height, pressure, n_traj_points)

# Merge HYSPLIT height with smoke and covariates
dat_merged <- dat_panel %>% left_join(dat_hysplit)
obs <- dat_merged %>% 
  count(date, epa_id) %>% 
  filter(n == 1) %>% 
  mutate(obs = paste0(epa_id, "_", date)) %>% 
  pull(obs)
no_match <- expand.grid(obs, cutoffs, injection_heights) %>% 
  separate(Var1, into = c("epa_id", "date"), sep = "_") %>% 
  rename(cutoff = Var2, 
         injection_heights = Var3) %>% 
  mutate(epa_id = as.integer(epa_id),
         date = as.Date(date),
         cutoff = as.numeric(cutoff),
         agg_method = agg, 
         injection_heights = as.character(injection_heights)) %>% 
  left_join(dat_merged %>% 
              select(-cutoff, -agg_method, -injection_heights, -height, 
                     -pressure, -n_traj_points))
dat_merged <- dat_merged %>% 
  drop_na(agg_method) %>% 
  bind_rows(no_match) %>% 
  mutate(n_traj_points = ifelse(is.na(n_traj_points), 0, n_traj_points))

# Prepare data frame for predictions
dat_preds <- dat_merged %>% 
  select(epa_id, date, smoke_day, fold, agg_method, injection_heights, cutoff, smokePM)

for (spec in specs) {
  features <- c("smokePM", "fold", "month", "temperature", "precipitation",
                "aot_anom", "aot_anom_lag1", "aot_anom_lag2", "aot_anom_lag3",
                "elevation", "lat", "lon", "km_dist", "pbl_min")
  ih <- injection_heights
  co <- cutoffs
  if (spec == "baseline") {
    ih <- injection_heights[1]
    co <- cutoffs[1]
  } else {
    features <- c(features, spec)
  }
  
  for (inj_heights in ih) {
    inj <- gsub("\\+", "-", inj_heights)
    
    for (cut in co) {
      dat_merged_s <- dat_merged %>% 
        filter(cutoff == cut,
               injection_heights == inj_heights) %>% 
        select(features)
      
      print(paste(spec, inj_heights, cut))
      start_time <- get_start_time()
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
        learner <- makeLearner("regr.xgboost")
        rsmpl_desc <- makeResampleDesc("CV", fixed = TRUE)
        nf <- length(names(dat_train)) - 1
        params <- makeParamSet(makeNumericParam("colsample_bytree", 2/nf, (nf  - 2)/nf),
                               makeIntegerParam("max_depth", 16, 670),
                               makeDiscreteParam("subsample", c(0.6, 0.8, 1)),
                               makeDiscreteParam("num_parallel_tree", 100))
        
        # Tune hyperparameters
        start_time <- get_start_time()
        set.seed(7569)
        res <- tuneParams(learner, task, rsmpl_desc, par.set = params,
                          control = makeTuneControlRandom(maxit = 100))
        print_time(start_time)
        
        # Fit random forest model
        set.seed(3473)
        mdl <- xgboost(data = dat_train %>% 
                         select(-smokePM) %>% 
                         as.matrix(), 
                       label = dat_train$smokePM, 
                       colsample_bytree = res$x$colsample_bytree, 
                       max_depth = res$x$max_depth, 
                       subsample = res$x$subsample, 
                       num_parallel_tree = 500, 
                       nrounds = 1,
                       missing = NA)
        
        # Save model and predictions
        if (spec == "baseline") {
          saveRDS(mdl, paste0(path_project, sprintf("using_xgboost/HYSPLIT_model_%s_%s.rds", spec, f)))
          saveRDS(predict(mdl, dat_merged %>% select(features, -smokePM, -fold) %>% as.matrix()), paste0(path_project, sprintf("using_xgboost/HYSPLIT_predictions_%s_%s.rds", spec, f)))
        } else {
          saveRDS(mdl, paste0(path_project, sprintf("using_xgboost/HYSPLIT_model_%s_%s_%s_%s.rds", spec, inj, cut, f)))
          saveRDS(predict(mdl, dat_merged %>% select(features, -smokePM, -fold) %>% as.matrix()), paste0(path_project, sprintf("using_xgboost/HYSPLIT_predictions_%s_%s_%s_%s.rds", spec, inj, cut, f)))
        }
      }
    }
  }
}
print(Sys.time())

# Collect models and predictions
nm <- 5 * (1 + (length(specs) - 1) * length(agg) * length(cutoffs) * length(injection_heights))
mdls <- vector("list", nm)
m <- 1
for (f in 1:5) {
  mdl <- readRDS(paste0(path_project, sprintf("using_xgboost/HYSPLIT_model_baseline_%s.rds", f)))
  mdls[[m]] <- list(model = mdl,
                    spec = "baseline",
                    fold = f)
  dat_preds[, sprintf("pred.baseline.%s", f)] <- readRDS(paste0(path_project, sprintf("using_xgboost/HYSPLIT_predictions_baseline_%s.rds", f)))
  m <- m + 1
}
for (spec in specs[-1]) {
  for (inj in gsub("\\+", "-", injection_heights)) {
    for (cut in cutoffs) {
      for (f in 1:5) {
        mdl <- readRDS(paste0(path_project, sprintf("using_xgboost/HYSPLIT_model_%s_%s_%s_%s.rds", spec, inj, cut, f)))
        mdls[[m]] <- list(model = mdl,
                          spec = spec,
                          fold = f,
                          injection_heights = gsub("-", "\\+", inj),
                          cutoff = cut)
        dat_preds[, sprintf("pred.%s.%s.%s.%s", spec, f, cut, gsub("-", "_", inj))] <- readRDS(paste0(path_project, sprintf("using_xgboost/HYSPLIT_predictions_%s_%s_%s_%s.rds", spec, inj, cut, f)))
        m <- m + 1
      }
    }
  }
}

# Save collected models and predictions
saveRDS(mdls, paste0(path_project, "using_xgboost/HYSPLIT_random_forest_models.rds"))
saveRDS(dat_preds, paste0(path_project, "using_xgboost/HYSPLIT_random_forest_predictions.rds"))
