source("scripts/0_config.R")
library(sf)
library(fixest)

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
  # month_bin <- data %>% 
  #   mutate(obs_bin = cut(obs, breaks = bins, 
  #                        right = TRUE, include.lowest = TRUE, 
  #                        ordered_result = TRUE),
  #          pred_bin = cut(pred, breaks = bins, 
  #                         right = TRUE, include.lowest = TRUE, 
  #                         ordered_result = TRUE)) %>% 
  #   group_by(loc_id, month_year, !!models, !!test_tune, )
  
  # return(day_bin)
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
  
  # group_by(mod_fold, test_train) %>%
  # # rank the rmse and r2 for each model fold that was left out
  # mutate(across(contains("r2"), list(rank =~ data.table::frankv(.x, order = -1))),
  #        across(contains("rmse"), list(rank =~ data.table::frankv(.x, order = 1)))) %>% 
  # ungroup %>% 
  # # name the other columns with value to distinguish them from the rank columns
  # dplyr::rename_with(function(x){paste0(x, "_value")}, !contains("rank") & !spec & !mod_fold & !test_train) %>% 
  # pivot_longer(c(contains("r2"), contains("rmse"))) %>% 
  # separate(name, into = c("measure", "eval_sample", "rank_value")) %>%
  # pivot_wider(names_from = rank_value) %>% 
  # mutate(measure = recode(measure,
  #                         "r2" = "R2",
  #                         "wr2" = "within R2",
  #                         "rmse" = "RMSE"),
  #        measure = factor(measure, levels = c("RMSE", "R2", "within R2"))) 
  # total monthly smoke pollution (r2, within r2, rmse of observed and predicted)
  # total annual smoke pollution (same as above)
  # binned smokePM (currently proposed bins are 0-5, 5-10, 10-20, 20-50, 50-100, 100+) where eval metric accounts for order in bins and calculates average distance between true bin and predicted bin for each monitor-day
  # distribution of monthly binned smokePM (still need to figure out exactly what this metric is, but should compare the observed vs predicted distribution of binned smoke PM at the monitor-month)
}

monitors <- st_read(paste0(data_path, "/epa_station_locations")) 
states <- tigris::states() %>% st_transform(st_crs(monitors))

monitors$state <- states$STUSPS[st_intersects(monitors, states) %>% unlist]

smokePM_data <- readRDS("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/data/4_clean/smokePM_training.rds") %>% 
  select(id, date, smokePM, fold)

cv_preds <- list.files("./output/smokePM_model", pattern = "pred_fold", 
                                 full.names = TRUE) %>% 
  purrr::map_dfr(function(x){
    readRDS(x) %>%
      mutate(test_fold = as.numeric(substr(x, 41, 41)),
             drop_var = gsub("\\.rds|-", "", substr(x, 47, 100000))) %>%
      return()
  })

model_metrics <- cv_preds %>%
  mutate(smokePM_pred = pmax(0, smokePM_pred)) %>%
  left_join(smokePM_data, by = c("id", "date", "fold")) %>% 
  mutate(test = fold == test_fold) %>%
  eval_metrics(models = drop_var, test_tune = test, 
               obs = smokePM, pred = smokePM_pred, loc_id = id) %>%
  pivot_longer(contains("rank") | contains("value")) %>% 
  separate(name, into = c("metric", "rank_value"), sep = "_(?=rank|value)") %>% 
  pivot_wider(values_from = value, names_from = rank_value) %>%
  separate(metric, into = c("metric", "subset"), sep = "_", extra = "merge") 


# metrics on different sample for main model, table SX
model_metrics %>% 
  filter(drop_var == "") %>% 
  filter(test == TRUE & metric != "nobs" & metric != "me" &
           !grepl("bin", metric, ignore.case = TRUE)) %>% 
  select(-drop_var, -test, -rank) %>% 
  pivot_wider(names_from = metric, values_from = value)

# comparison between full model, without AOD preds, and without HYSPLIT
model_metrics %>%
  filter(grepl("day", subset)) %>% 
  filter(metric %in% c("rmse", "r2", "wr2")) %>% 
  filter(test == TRUE) %>% 
  mutate(subset = recode_factor(subset, 
                                "day" = "all days", 
                                "day_sub50" = "days with\n< 50ug",
                                "day_over50" = "days with\n> 50ug", 
                                .ordered = T), 
         metric = recode_factor(metric, 
                                "r2" = "R^2", 
                                "wr2" = "within~R^2", 
                                "rmse" = "RMSE",
                                .ordered = TRUE),
         drop_var = case_when(drop_var == "" ~ "full model", 
                              drop_var =="aod_anom_pred" ~ "no AOD predictions", 
                              drop_var =="traj_points" ~ "no HYSPLIT"),
         drop_var = factor(drop_var, 
                           levels = c("full model", "no HYSPLIT", "no AOD predictions"), 
                           ordered = TRUE)) %>% 
  {ggplot(data = ., 
          aes(x = subset, y = value, color = drop_var,
             group = drop_var)) + 
  geom_point() + 
  geom_line() + 
  scale_x_discrete(expand = expansion(mult = 0.04)) + 
  ylim(0, NA) + 
  facet_wrap(~metric, scales = "free", strip.position = "left",
             labeller = label_parsed) + 
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        panel.spacing = unit(-0.1, "lines"),
        strip.switch.pad.wrap = unit(-0.25, "lines")) + 
  scale_color_manual(name = "", values = MetBrewer::met.brewer("Homer2", 5)[c(1,2,4)]) + 
  xlab("") + ylab("")} %>% 
  ggsave("./figures/supplement/SX_hysplit_aod_comparison.png", ., 
         width = 8, height= 3)

# change in model r2 from aod and hysplit inclusion
model_metrics %>%
  filter(subset == "day" & metric == "r2"  & test == TRUE) %>% 
  mutate(diff = max(value) - value) %>% View
