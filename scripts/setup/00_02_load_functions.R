# Timers
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









nonsmoke_medians <- function(data, 
                             main_var, 
                             smoke_day, 
                             spatial_unit, 
                             temporal_unit, 
                             temporal_trend){
  main_var <- enquo(main_var)
  smoke_day <- enquo(smoke_day)
  spatial_unit <- enquo(spatial_unit)
  temporal_unit <- enquo(temporal_unit)
  temporal_trend <- enquo(temporal_trend)
  
  new_name <- paste0(rlang::as_name(main_var), "_med_3yr")
  
  full_panel <- expand.grid(id = data %>% pull(!!spatial_unit) %>% unique, 
                            month = data %>% pull(!!temporal_unit) %>% unique, 
                            year = data %>% pull(!!temporal_trend) %>% unique) %>% 
    rename(!!spatial_unit := id, 
           !!temporal_unit := month, 
           !!temporal_trend := year) %>% 
    ungroup
  
  data %>% 
    filter(!is.na(!!main_var) & !!smoke_day == 0) %>%
    full_join(full_panel) %>%
    group_by(!!spatial_unit, !!temporal_unit, !!temporal_trend) %>%
    summarise(main_var = list(!!main_var),
              nobs = n(),
              .groups = "drop") %>%
    arrange(!!spatial_unit, !!temporal_unit, !!temporal_trend) %>%
    group_by(!!spatial_unit, !!temporal_unit) %>%
    mutate(main_var_lag = lag(main_var, n = 1, default = list(NA)),
           main_var_lead = lead(main_var, n = 1, default = list(NA)),
           nobs_lag = lag(nobs, n = 1, default = 0),
           nobs_lead = lead(nobs, n = 1, default = 0)) %>%
    ungroup %>%
    rowwise %>%
    mutate(main_var_3yr = list(c(main_var, main_var_lag, main_var_lead)),
           main_var_med_3yr = median(unlist(main_var_3yr), na.rm = T),
           nobs_3yr = nobs + nobs_lead + nobs_lag) %>%
    ungroup %>%
    transmute(!!spatial_unit, !!temporal_unit, !!temporal_trend,
              nobs_3yr,
              !!new_name := main_var_med_3yr)
}

eval_metrics <- function(data, models, test_tune, obs, pred, 
                         loc_id, bins = c(0, 5, 10, 20, 50, 100, 10000)){
  library(fixest)
  models <- enquo(models)
  test_tune <- enquo(test_tune)
  obs <- enquo(obs)
  pred <- enquo(pred)
  loc_id <- enquo(loc_id)
  
  data %<>% 
    rename(obs = !!obs, 
           pred = !!pred, 
           loc_id = !!loc_id) %>% 
    mutate(month = lubridate::month(date),
           year = lubridate::year(date),
           month_year = paste0(month, "_", year))
  day_eval <-  data %>%
    nest_by(!!models, !!test_tune) %>%
    # for each model and test vs train folds, calculate metrics
    mutate(wr2_day = r2(feols(obs ~ pred | loc_id + year,
                              data = data), "wr2"),
           r2_day = r2(feols(obs ~ pred,
                             data = data), "r2"),
           rmse_day = sqrt(mean((data$obs - data$pred)^2)),
           me_day = mean(data$pred - data$obs),
           nobs_day = (data %>% filter(!is.na(pred)) %>% nrow)) %>% 
    select(-data) 
  day_eval_sub50 <-  data %>%
    filter(obs < 50) %>% 
    nest_by(!!models, !!test_tune) %>%
    # for each model and test vs train folds, calculate metrics
    mutate(wr2_day_sub50 = r2(feols(obs ~ pred | loc_id + year,
                                    data = data), "wr2"),
           r2_day_sub50 = r2(feols(obs ~ pred,
                                   data = data), "r2"),
           rmse_day_sub50 = sqrt(mean((data$obs - data$pred)^2)),
           me_day_sub50 = mean(data$pred - data$obs),
           nobs_day_sub50 = (data %>% filter(!is.na(pred)) %>% nrow)) %>% 
    select(-data) 
  day_eval_over50 <-  data %>%
    filter(obs >= 50) %>% 
    nest_by(!!models, !!test_tune) %>%
    # for each model and test vs train folds, calculate metrics
    mutate(wr2_day_over50 = r2(feols(obs ~ pred | loc_id + year,
                                     data = data), "wr2"),
           r2_day_over50 = r2(feols(obs ~ pred,
                                    data = data), "r2"),
           rmse_day_over50 = sqrt(mean((data$obs - data$pred)^2)),
           me_day_over50 = mean(data$pred - data$obs),
           nobs_day_over50 = (data %>% filter(!is.na(pred)) %>% nrow)) %>% 
    select(-data) 
  month_eval <- data %>% 
    group_by(loc_id, month_year, !!models, !!test_tune) %>% 
    summarise(pred = mean(pred), 
              obs = mean(obs),
              year = unique(year),
              .groups = "drop") %>%
    nest_by(!!models, !!test_tune) %>%
    # for each model and test vs train folds, calculate metrics
    mutate(wr2_month = r2(feols(obs ~ pred | loc_id + year,
                                data = data), "wr2"),
           r2_month = r2(feols(obs ~ pred,
                               data = data), "r2"),
           rmse_month = sqrt(mean((data$obs - data$pred)^2)),
           me_month = mean(data$pred - data$obs),
           nobs_month = (data %>% filter(!is.na(pred)) %>% nrow)) %>% 
    select(-data)
  year_eval <- data %>% 
    group_by(loc_id, year, !!models, !!test_tune) %>% 
    summarise(pred = mean(pred), 
              obs = mean(obs),
              .groups = "drop") %>%
    nest_by(!!models, !!test_tune) %>%
    # for each model and test vs train folds, calculate metrics
    mutate(wr2_year = r2(feols(obs ~ pred | loc_id + year,
                               data = data), "wr2"),
           r2_year = r2(feols(obs ~ pred,
                              data = data), "r2"),
           rmse_year = sqrt(mean((data$obs - data$pred)^2)),
           me_year = mean(data$pred - data$obs),
           nobs_year = (data %>% filter(!is.na(pred)) %>% nrow)) %>% 
    select(-data)
  day_bin <- data %>% 
    mutate(obs_bin = cut(obs, breaks = bins, 
                         right = FALSE, include.lowest = TRUE, 
                         ordered_result = TRUE),
           pred_bin = cut(pred, breaks = bins, 
                          right = FALSE, include.lowest = TRUE, 
                          ordered_result = TRUE)) %>% 
    mutate(across(ends_with("_bin"), as.numeric),
           bin_dist = abs(obs_bin - pred_bin)) %>% 
    group_by(!!models, !!test_tune) %>%
    summarise(binDist_day = mean(bin_dist),
              pctCorrectBin_day = mean(obs_bin == pred_bin),
              wrongBinDist_day = weighted.mean(bin_dist, bin_dist > 0),
              .groups = "drop")
  
  full_join(day_eval,
            day_eval_sub50) %>% 
    full_join(day_eval_over50) %>% 
    full_join(month_eval) %>%
    full_join(year_eval) %>%
    full_join(day_bin) %>%
    group_by(!!test_tune) %>%
    # rank the rmse and r2 for each model fold that was left out
    mutate(across(contains("r2"), list(rank =~ data.table::frankv(.x, order = -1))),
           across(contains("rmse"), list(rank =~ data.table::frankv(.x, order = 1))),
           across(contains("me"), list(rank =~ data.table::frankv(abs(.x), order = 1))),
           across(contains("Dist"), list(rank =~ data.table::frankv(.x, order = 1))),
           across(contains("pctCorrect"), list(rank =~ data.table::frankv(.x, order = -1)))) %>%
    ungroup %>%
    # name the other columns with value to distinguish them from the rank columns
    dplyr::rename_with(function(x){paste0(x, "_value")}, !contains("rank") & !(!!test_tune) & !(!!models)) %>%
    return
}





mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}
