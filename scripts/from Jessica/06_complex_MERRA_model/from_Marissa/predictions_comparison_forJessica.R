library(tidyverse)
library(magrittr)
library(scales)
library(fixest)

# read in the predictions from Jessica 
RF_preds <- readRDS("../data/random_forest_predictions.rds") %>% 
  rename_with(function(x){paste0(x, "_rf")}, starts_with("pred"))

# reid and di 
reid <- readRDS("../data/reid_anom_df.rds") %>%
  select(epa_id = id, 
         date, 
         pm25_anom_reid = pred_pm25_anom) 

di <- readRDS("../data/di_anom_df.rds") %>% 
  select(epa_id, date, pm25_anom_di) 

# brt_preds <- mod_data %>% 
brt_preds <- readRDS("../output/brtLong_preds_spec2_2021Aug3.rds")

# need to add state information to get state month, etc
station_data <- readRDS("../data/clean/epa_station_covariates.rds") %>% 
  select(epa_id, state) %>%
  unique 

# dataframe of all the predictions 
preds <- brt_preds %>% 
  left_join(station_data) %>%
  left_join(RF_preds) %>% 
  left_join(reid) %>% 
  left_join(di) %>% 
  mutate(pred_reid = pmax(0, pm25_anom_reid), 
         pred_di = pmax(0, pm25_anom_di),
         month = lubridate::month(date), 
         year = lubridate::year(date)) %>% 
  unite(state_month, c(state, month), sep = "_", remove = FALSE) %>% 
  unite(state_year, c(state, year), sep = "_", remove = FALSE) %>%
  select(-starts_with("pm25")) 

# calculate summary statistics 
pred_sum <- preds %>% 
  na.omit() %>%
  # make each prediction its own row
  pivot_longer(starts_with("pred")) %>% 
  # pull the info from the prediction names
  separate(name, into  = c(NA, "spec", "mod_fold", "mod_type"), sep = "_")  %>%
  mutate(mod_fold = gsub("fold", "", mod_fold) %>% as.numeric(), 
         mod_fold = ifelse(is.na(mod_fold), fold, mod_fold), # this is for reid and di, we'll just call them test
         test_train = ifelse(mod_fold == fold, "test", "train"),
         pred = pmax(value, 0)) %>%  # truncate all predictions at zero
  # group by model + fold
  nest_by(spec, mod_fold, test_train, mod_type) %>% 
  # for each model and test vs train folds, calculate metrics
  mutate(wr2_smoke = r2(feols(smokePM ~ pred | epa_id + state_month + state_year,
                                 data = data), "wr2"),
         r2_smoke = r2(feols(smokePM ~ pred,
                                  data = data), "r2"),
         wr2_smoke50 = r2(feols(smokePM ~ pred | epa_id + state_month + state_year,
                                   data = data %>% filter(smokePM >= 50)), "wr2"),
         r2_smoke50 = r2(feols(smokePM ~ pred,
                                    data = data %>% filter(smokePM >= 50)), "r2"),
         rmse_smoke = sqrt(mean((data$smokePM - data$pred)^2)),
         rmse_smoke50 = sqrt(mean((data$smokePM[data$smokePM >= 50] - data$pred[data$smokePM >= 50])^2)),
         nobs = (data %>% filter(!is.na(pred)) %>% nrow)) %>% 
  # drop the data column
  select(-data) %>% 
  group_by(mod_fold, test_train) %>%
  # rank the rmse and r2 for each model fold that was left out
  mutate(across(contains("r2"), list(rank =~ data.table::frankv(.x, order = -1))),
         across(contains("rmse"), list(rank =~ data.table::frankv(.x, order = 1))))%>%
  ungroup %>% 
  # name the other columns with value to distinguish them from the rank columns
  dplyr::rename_with(function(x){paste0(x, "_value")}, !contains("rank") & !spec & !mod_fold & !test_train & !mod_type) %>% 
  pivot_longer(c(contains("r2"), contains("rmse"))) %>% 
  separate(name, into = c("measure", "eval_sample", "rank_value")) %>%
  pivot_wider(names_from = rank_value) %>% 
  mutate(measure = recode(measure,
                          "r2" = "R2",
                          "wr2" = "within R2",
                          "rmse" = "RMSE"),
         measure = factor(measure, levels = c("RMSE", "R2", "within R2")),
         model = ifelse(!is.na(mod_type), paste0(spec, "_", mod_type), spec),
         model = factor(model, levels = c("di", "reid", "spec1_rf", "spec2_rf", "spec2_brt", "spec2_brtLong"))) 

# looking only at the performance on test folds when they are left out
pred_sum %>% 
  filter(test_train == "test") %>% 
  ggplot(aes(y = model, x = eval_sample, fill = rank,
             label = round(value, 3))) +
  geom_tile() +
  geom_text() +
  facet_grid(measure ~ mod_fold) +
  ggtitle("Model comparison (CA, OR, WA; 2008 - 2016)") +
  labs(y = "model") +
  scale_fill_gradient2(midpoint = 3.5,
                       low = muted("blue", l = 50),
                       mid = "white",
                       high = muted("red", l = 50)) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# average across folds to get one performance number for each model type
pred_sum %>% 
  filter(test_train == "test") %>%
  group_by(model, measure, eval_sample) %>% 
  summarise(mean = mean(value)) %>% 
  pivot_wider(names_from = measure, values_from = mean) %>%  
  group_by(eval_sample) %>%
  mutate(across(contains("R2"), list(rank =~ data.table::frankv(.x, order = -1))),
         across(contains("RMSE"), list(rank =~ data.table::frankv(.x, order = 1))))%>%
  ungroup %>% 
  dplyr::rename_with(function(x){paste0(x, "_value")}, !contains("rank") & !model &!eval_sample) %>% 
  pivot_longer(c(contains("RMSE"), contains("R2"))) %>% 
  separate(name, into = c("measure", "rank_value"), sep = "_") %>% 
  pivot_wider(names_from = rank_value) %>% 
  ggplot(aes(y = model, 
             x = eval_sample, fill = rank,
             label = round(value, 3))) +
  geom_tile() +
  geom_text() +
  facet_wrap(~measure) +
  scale_fill_gradient2(midpoint = 3.5,
                       low = muted("blue", l = 50),
                       mid = "white",
                       high = muted("red", l = 50)) +
  theme_bw()

