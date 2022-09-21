# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Trains anomalous AOD model.
# ------------------------------------------------------------------------------
txt_progress_file <- file.path(path_output, "aod_xgb_progress.txt")
max_xgb_rounds <- 10000
bayes_opt_n_init <- 24
bayes_opt_n_iter <- 24

if (Sys.getenv('SLURM_JOB_ID') != ""){
  usable.cores <- Sys.getenv("SLURM_NTASKS_PER_NODE")
} else {
  usable.cores <- 2
}

mod_data <- readRDS(file.path(path_data, "4_clean", "aod_training.rds"))

xgb_opt_fun <- function(eta, gamma, max_depth, subsample, colsample_bytree, 
                        min_child_weight, xgb_mat, cv_ind = NULL, 
                        rounds_max = 100, nfold = 4, nthread = 2,
                        progress_file = ""){
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
  
  if(progress_file != ""){
    write(paste0("nrounds = ", mod_xgb_cv$best_iteration, 
                 ", eta = ", eta, 
                 ", gamma = ", gamma, 
                 ", max_depth = ", max_depth, 
                 ", colsample_bytree = ", colsample_bytree, 
                 ", subsample = ", subsample, 
                 ", min_child_weight = ", min_child_weight, 
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

xgb_train_mat <- xgb.DMatrix(
  data = model.matrix.lm(~.-1, 
                         data = mod_data %>% 
                           mutate(month = as.factor(month)) %>%
                           select(month, lat, lon, aot_anom, 
                                  aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
                                  fire_dist_km, closest_fire_area, closest_fire_num_points,
                                  pbl_min, pbl_max, pbl_mean,
                                  wind_u, wind_v, 
                                  dewpoint_temp_2m, temp_2m,  
                                  sea_level_pressure, surface_pressure, precip, 
                                  elevation_mean, elevation_stdDev, 
                                  developed, barren, forest, shrubland, cultivated, 
                                  wetlands, herbaceous, water),
                         na.action = "na.pass"), 
  label = mod_data %>% 
    pull(aod_anom))

# define folds
cv_folds = mod_data %>% 
  mutate(row_no = row_number()) %>% 
  group_by(fold) %>% 
  summarise(row_nos = list(row_no)) %>% 
  pull(row_nos) 


# run bayesian optimization to find the best hyperparameters
write("", txt_progress_file)

tic <- Sys.time()
print(paste0("Starting bayesian optimization at ", tic))
set.seed(10001)
bayes_opt_params <- BayesianOptimization(
  function(eta, gamma, max_depth, subsample,
           colsample_bytree, min_child_weight){
    xgb_opt_fun(eta, gamma, max_depth, subsample, 
                colsample_bytree, min_child_weight, 
                xgb_train_mat, cv_ind = cv_folds, 
                rounds_max = max_xgb_rounds, nfold = length(cv_folds), 
                nthread = usable.cores, 
                progress_file = txt_progress_file)},
  bounds = list(
    eta = c(0.0001, 0.2),
    gamma = c(0, 100),
    max_depth = c(2L, 50L),
    colsample_bytree = c(0.5, 1),
    subsample = c(0.25, 1),
    min_child_weight = c(1L, 50L)),
  init_points = bayes_opt_n_init, # heuristic on how many points? 
  n_iter = bayes_opt_n_iter)
toc <- Sys.time()
print(paste0("bayesian optimization completed at ", toc))
print(toc - tic)

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

saveRDS(bayes_opt_params, file.path(path_output, "aod_bayes_opt_params.rds"))
xgb.save(mod_gb_final, file.path(path_output, "aod_mod.xgb"))

# save variable importance information 
feat_names <- model.matrix.lm(~.-1, 
                              data = mod_data %>% 
                                mutate(month = as.factor(month)) %>%
                                select(month, lat, lon, aot_anom, 
                                       aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
                                       fire_dist_km, closest_fire_area, closest_fire_num_points,
                                       pbl_min, pbl_max, pbl_mean,
                                       wind_u, wind_v, 
                                       dewpoint_temp_2m, temp_2m,  
                                       sea_level_pressure, surface_pressure, precip, 
                                       elevation_mean, elevation_stdDev, 
                                       developed, barren, forest, shrubland, cultivated, 
                                       wetlands, herbaceous, water),
                              na.action = "na.pass") %>% colnames

var_import <- xgb.importance(model = mod_gb_final)

saveRDS(list(variable_importance = var_import, 
             feature_names = feat_names), 
        file.path(path_output, "AOD_var_importance.rds"))
