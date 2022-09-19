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






mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}
