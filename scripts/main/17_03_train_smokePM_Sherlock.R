source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Trains smoke PM2.5 model.
#-------------------------------------------------------------------------------
args <- commandArgs(TRUE)
cv_fold_num <- as.numeric(args[1])
drop_vars <- strsplit(args[2], split = "-")[[1]] # second arg should be any character strings of variables to be dropped separated by dashes, matches are on containment of that string in the column name

print(paste0("tuning model, excluding fold ", cv_fold_num))
print(paste0("excluding variables with names containing ",
             paste0(drop_vars, collapse = " or ")))

txt_progress_file <- file.path(path_output, "smokePM_model", 
                               paste0("smokePM_xgb_progress_fold", cv_fold_num, 
                                      paste0(c("_drop", drop_vars), collapse = "-"), ".txt"))
max_xgb_rounds <- 10000
bayes_opt_n_init <- 24
bayes_opt_n_iter <- 16

param_bounds <- list(
  eta = c(0.001, 0.1),
  gamma = c(0, 50),
  max_depth = c(2L, 25L),
  colsample_bytree = c(0.5, 1),
  subsample = c(0.25, 1),
  min_child_weight = c(1L, 50L))

if (Sys.getenv('SLURM_JOB_ID') != "") {
  usable.cores <- Sys.getenv("SLURM_NTASKS_PER_NODE")
} else {
  usable.cores <- 2
}

print(paste0("there are ", usable.cores, " usable cores"))

mod_data <- readRDS(file.path(path_data, "smokePM_training.rds")) %>% 
  filter(fold != cv_fold_num)

xgb_train_mat <- xgb.DMatrix(
  data = model.matrix.lm(~.-1, 
                         data = mod_data %>% 
                           select(month, lat, lon, 
                                  aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3,
                                  aod_anom_pred_0.00, aod_anom_pred_0.25, aod_anom_pred_0.50,
                                  aod_anom_pred_0.75, aod_anom_pred_1.00, aod_anom_pred_mean,
                                  AODmissing,
                                  num_traj_points_height_1, num_traj_points_height_2,
                                  num_traj_points_height_3, num_traj_points_height_4, 
                                  num_traj_points_height_5,
                                  fire_dist_km, closest_fire_area, closest_fire_num_points,
                                  pbl_min, pbl_max, pbl_mean,
                                  wind_u, wind_v, 
                                  dewpoint_temp_2m, temp_2m,  
                                  sea_level_pressure, surface_pressure, precip, 
                                  elevation_mean, elevation_stdDev, 
                                  developed, barren, forest, shrubland, cultivated, 
                                  wetlands, herbaceous, water) %>% 
                           select(-contains(drop_vars)),
                         na.action = "na.pass"), 
  label = mod_data %>% 
    pull(smokePM))


xgb_opt_fun <- function(eta, gamma, max_depth, subsample, colsample_bytree, 
                        min_child_weight, xgb_mat, cv_ind = NULL, 
                        rounds_max = 100, nfold = 4, nthread = 2,
                        progress_file = ""){
  if(progress_file != ""){
    write(paste0("eta = ", eta, 
                 ", gamma = ", gamma, 
                 ", max_depth = ", max_depth, 
                 ", colsample_bytree = ", colsample_bytree, 
                 ", subsample = ", subsample, 
                 ", min_child_weight = ", min_child_weight),
          file = progress_file,
          append = T)}
  
  set.seed(10001)
  mod_xgb_cv <- xgb.cv(
    params = list(booster = "gbtree",
                  eta = eta,
                  gamma = gamma,
                  max_depth = max_depth,
                  subsample = subsample,
                  colsample_bytree = colsample_bytree,
                  min_child_weight = min_child_weight, 
                  objective = "reg:squarederror",
                  eval_metric = "rmse"),
    data = xgb_mat,
    nrounds = rounds_max,
    nthread = nthread,
    nfold = nfold,
    folds = cv_ind,
    early_stopping_rounds = 10,
    print_every_n = 20,
    verbose = TRUE)
  
  if (progress_file != ""){
    write(paste0("nrounds = ", mod_xgb_cv$best_iteration, 
                 ", RMSE = ", mod_xgb_cv$evaluation_log %>% 
                   filter(iter == mod_xgb_cv$best_iteration) %>% 
                   pull(test_rmse_mean),
                 "\n"),
          file = progress_file,
          append = T)}
  
  list(Score = mod_xgb_cv$evaluation_log %>% 
         filter(iter == mod_xgb_cv$best_iteration) %>% 
         pull(test_rmse_mean) %>% 
         multiply_by(-1), 
       Pred = mod_xgb_cv$best_iteration) %>% 
    return
}


# define folds
cv_folds = mod_data %>% 
  mutate(row_no = row_number()) %>% 
  group_by(fold) %>% 
  summarise(row_nos = list(row_no)) %>% 
  pull(row_nos) 


# run bayesian optimization to find the best hyperparameters
write("", txt_progress_file)

tic <- Sys.time()
set.seed(10001)
bayes_opt_params <- BayesianOptimization(
  function(eta, gamma, max_depth, subsample,
           colsample_bytree, min_child_weight){
    xgb_opt_fun(eta, gamma, max_depth, subsample, 
                colsample_bytree, min_child_weight, 
                xgb_train_mat, cv_ind = cv_folds, 
                rounds_max = max_xgb_rounds, nfold = length(cv_folds), 
                nthread = min(usable.cores, 8),
                progress_file = txt_progress_file)},
  bounds = param_bounds,
  init_points = bayes_opt_n_init, # heuristic on how many points? 
  n_iter = bayes_opt_n_iter)
toc <- Sys.time()
toc - tic

saveRDS(bayes_opt_params, 
        file.path(path_output, "smokePM_model", 
                  paste0("smokePM_bayes_opt_params_fold", cv_fold_num, 
                         paste0(c("_drop", drop_vars), collapse = "-"),
                         ".rds")))

mod_gb_final <- xgb.train(
  params = c(as.list(bayes_opt_params$Best_Par),
             booster = "gbtree",
             objective = "reg:squarederror",
             eval_metric = "rmse"),
  data = xgb_train_mat,
  nrounds = bayes_opt_params$History %>%
    filter(Value == bayes_opt_params$Best_Value) %>%
    pull(Round) %>%
    magrittr::extract(unlist(bayes_opt_params$Pred), .),
  verbose = 1)

xgb.save(mod_gb_final,
         file.path(path_output, "smokePM_model", 
                   paste0("smokePM_mod_fold", cv_fold_num, 
                          paste0(c("_drop", drop_vars), collapse = "-"),
                          ".xgb")))

# predict for full data set and save predictions
pred_data <- readRDS(file.path(path_data, "smokePM_training.rds"))

xgb_pred_mat <- xgb.DMatrix(
  data = model.matrix.lm(~.-1, 
                         data = pred_data %>% 
                           select(month, lat, lon, 
                                  aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3,
                                  aod_anom_pred_0.00, aod_anom_pred_0.25, aod_anom_pred_0.50,
                                  aod_anom_pred_0.75, aod_anom_pred_1.00, aod_anom_pred_mean,
                                  AODmissing,
                                  num_traj_points_height_1, num_traj_points_height_2,
                                  num_traj_points_height_3, num_traj_points_height_4, 
                                  num_traj_points_height_5,
                                  fire_dist_km, closest_fire_area, closest_fire_num_points,
                                  pbl_min, pbl_max, pbl_mean,
                                  wind_u, wind_v, 
                                  dewpoint_temp_2m, temp_2m,  
                                  sea_level_pressure, surface_pressure, precip, 
                                  elevation_mean, elevation_stdDev, 
                                  developed, barren, forest, shrubland, cultivated, 
                                  wetlands, herbaceous, water) %>% 
                           select(-contains(drop_vars)),
                         na.action = "na.pass"))

preds <- pred_data %>% 
  select(id, date, fold) %>% 
  cbind(smokePM_pred = predict(mod_gb_final, xgb_pred_mat))

saveRDS(preds, 
        file.path(path_output, "smokePM_model", paste0("smokePM_pred_fold", cv_fold_num, 
                                      paste0(c("_drop", drop_vars), collapse = "-"), 
                                      ".rds")))

# time permitting, calculate and save the variable importance
var_import <-  xgb.importance(model = mod_gb_final)
feat_names <- model.matrix.lm(~.-1, 
                              data = mod_data %>% 
                                select(month, lat, lon, 
                                       aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3,
                                       aod_anom_pred_0.00, aod_anom_pred_0.25, aod_anom_pred_0.50,
                                       aod_anom_pred_0.75, aod_anom_pred_1.00, aod_anom_pred_mean,
                                       AODmissing,
                                       num_traj_points_height_1, num_traj_points_height_2,
                                       num_traj_points_height_3, num_traj_points_height_4, 
                                       num_traj_points_height_5,
                                       fire_dist_km, closest_fire_area, closest_fire_num_points,
                                       pbl_min, pbl_max, pbl_mean,
                                       wind_u, wind_v, 
                                       dewpoint_temp_2m, temp_2m,  
                                       sea_level_pressure, surface_pressure, precip, 
                                       elevation_mean, elevation_stdDev, 
                                       developed, barren, forest, shrubland, cultivated, 
                                       wetlands, herbaceous, water) %>% 
                                select(-contains(drop_vars)),
                              na.action = "na.pass") %>% colnames

saveRDS(list(variable_importance = var_import, 
             feature_names = feat_names), 
        file.path(path_output, "smokePM_model", 
                  paste0("smokePM_var_importance_fold", cv_fold_num, 
                         paste0(c("_drop", drop_vars), collapse = "-"),
                         ".rds")))
