
fe_epa_id <- fixef(mdl_ln)$epa_id %>% 
  as.list() %>% 
  stack() %>% 
  rename(epa_id = ind, fe_epa_id = values) %>% 
  mutate(epa_id_fac = epa_id,
         epa_id = as.numeric(as.character(epa_id)))
fe_month <- fixef(mdl_ln)$month %>% 
  as.list() %>% 
  stack() %>% 
  rename(month = ind, fe_month = values) %>% 
  mutate(month_fac = month,
         month = as.numeric(as.character(month)))
fe_year <- fixef(mdl_ln)$year %>% 
  as.list() %>% 
  stack() %>% 
  rename(year = ind, fe_year = values) %>% 
  mutate(year_fac = year,
         year = as.numeric(as.character(year)))

df <- dat_test %>% 
  list(fe_epa_id, fe_month, fe_year) %>% 
  reduce(left_join)
pred_ln2 <- predict(mdl_ln, dat_test) - 
  as.numeric(df$epa_id_fac)*df$fe_epa_id - 
  as.numeric(df$month_fac)*df$fe_month - 
  as.numeric(df$year_fac)*df$fe_year











































params2 <- makeParamSet(makeDiscreteParam("mtry", res$x$mtry),
                        makeDiscreteParam("min.node.size", res$x$min.node.size),
                        # makeIntegerParam("min.node.size", 5, 50),
                        makeDiscreteParam("sample.fraction", res$x$sample.fraction),
                        makeDiscreteParam("num.trees", 100))
# makeDiscreteParam("num.trees", c(120, 200, 300, 400, 500, 600, 700, 800)))

start_time <- get_start_time()
set.seed(7569)
res2 <- tuneParams(learner, task, rsmpl_desc, par.set = params2,
                   control = makeTuneControlGrid())
print_time(start_time)
beep_alert("Done tuning")







params4 <- makeParamSet(makeDiscreteParam("mtry", res$x$mtry),
                        makeDiscreteParam("min.node.size", c(5, 50, 250)),
                        # makeDiscreteParam("min.node.size", c(10, 100, 500, 1000)),
                        # makeIntegerParam("min.node.size", 5, 50),
                        makeDiscreteParam("sample.fraction", res$x$sample.fraction),
                        makeDiscreteParam("num.trees", 100))
# makeDiscreteParam("num.trees", c(120, 200, 300, 400, 500, 600, 700, 800)))

start_time <- get_start_time()
set.seed(7569)
res4 <- tuneParams(learner, task, rsmpl_desc, par.set = params4,
                   control = makeTuneControlGrid())
print_time(start_time)
beep_alert("Done tuning")












params5 <- makeParamSet(makeDiscreteParam("mtry", res$x$mtry),
                        makeDiscreteParam("min.node.size", 500),
                        # makeDiscreteParam("min.node.size", c(10, 100, 500, 1000)),
                        # makeIntegerParam("min.node.size", 5, 50),
                        makeDiscreteParam("sample.fraction", c(0.2, 0.4, 0.6, 0.8, 1)),
                        makeDiscreteParam("num.trees", 100))
# makeDiscreteParam("num.trees", c(120, 200, 300, 400, 500, 600, 700, 800)))

start_time <- get_start_time()
set.seed(7569)
res5 <- tuneParams(learner, task, rsmpl_desc, par.set = params5,
                   control = makeTuneControlGrid())
print_time(start_time)
beep_alert("Done tuning")


















params6 <- makeParamSet(makeDiscreteParam("mtry", c(2, 5, 9, 12)),
                        makeDiscreteParam("min.node.size", 500),
                        # makeDiscreteParam("min.node.size", c(10, 100, 500, 1000)),
                        # makeIntegerParam("min.node.size", 5, 50),
                        makeDiscreteParam("sample.fraction", 1),
                        makeDiscreteParam("num.trees", 100))
# makeDiscreteParam("num.trees", c(120, 200, 300, 400, 500, 600, 700, 800)))

start_time <- get_start_time()
set.seed(7569)
res6 <- tuneParams(learner, task, rsmpl_desc, par.set = params6,
                   control = makeTuneControlGrid())
print_time(start_time)
beep_alert("Done tuning")



























library(vip)

vip(mdls[[1]])
vip(mdls[[2]])
vip(mdls[[3]])
vip(mdls[[4]])
vip(mdls[[5]])
vip(mdls[[6]])
vip(mdls[[7]])
vip(mdls[[8]])
vip(mdls[[9]])
vip(mdls[[10]])


df <- inner_join(dat_preds, dat_merged %>% select(epa_id, date, smokePM, fold))

library(ggplot2)
library(ggpmisc)


ggplot(df %>% filter(fold == 5), aes(pred_spec2_fold5, smokePM)) + 
  geom_point() + 
  stat_poly_eq(formula = y ~ x, parse = TRUE)

data.frame(
  spec1 = c(0.55, 0.39, 0.51, 0.52, 0.64),
  spec2 = c(0.57, 0.43, 0.51, 0.53, 0.67)
)

set.seed(123)
mdl1 <- ranger(smokePM ~ .,
               dat_merged %>% 
                 filter(fold != 3) %>% 
                 select(c("smokePM", "month", "temperature", "precipitation", 
                          "aot_anom", "aot_anom_lag1", "aot_anom_lag2", "aot_anom_lag3", 
                          "elevation", "lat", "lon", "km_dist", "pbl_min")),
               importance = "impurity")
pred1 <-  predict(mdl1, dat_merged %>% filter(fold == 3))$predictions
df1 <- bind_cols(dat_merged %>% filter(fold == 3), 
                 data.frame(pred1))
summary(lm(smokePM ~ pred1,
           dat_merged %>% filter(fold == 3)))$r.squared

data.frame(
  spec1 = c(0.52, 0.54, 0.64)
)

set.seed(123)
mdl2 <- ranger(smokePM ~ .,
               dat_merged %>% 
                 filter(fold != 5) %>% 
                 select(c("smokePM", "month", "temperature", "precipitation", 
                          "aot_anom", "aot_anom_lag1", "aot_anom_lag2", "aot_anom_lag3", 
                          "elevation", "lat", "lon", "km_dist", "pbl_min", 
                          "wind_u", "wind_v", "wind", "elevation_stdDev",
                          "mean_sea_level_pressure", "surface_pressure", 
                          "dewpoint_2m_temperature", "pbl_max", "pbl")),
               importance = "impurity")





































s_ <- 2

# Get RF predictions without HYSPLIT
rf_merged <- readRDS(paste0("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/complex_MERRA_model/", "cleaned_station_covariates_smokedays_WestCoast_2006_mid2020.rds"))
rf_preds <- readRDS(paste0("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/complex_MERRA_model/", "random_forest_predictions.rds"))
rf_preds <- full_join(rf_merged, rf_preds)

# Filter to same station-days in sample w/ HYSPLIT
rf_preds <- rf_preds %>% 
  filter(paste(epa_id, date) %in% paste(dat_preds$epa_id, dat_preds$date))

for (s in s_) {
  for (f in 1:5) {
    df <- rf_preds %>% filter(fold == f) %>% select(smokePM, paste0("pred_spec", s, "_fold", f))
    r2 <- cor(df$smokePM, df[, 2])^2
    print(paste("fold", f, ":", round(r2, 4)))
  }
}

for (s in s_) {
  for (f in 1:5) {
    rf <- rf_preds %>% filter(fold == f) %>% select(smokePM, paste0("pred_spec", s, "_fold", f))
    r2_rf <- cor(rf$smokePM, rf[, 2])^2
    
    df <- dat_preds %>% filter(fold == f) %>% select(smokePM, paste0("pred_spec", s, "_fold", f))
    r2_hy <- cor(df$smokePM, df[, 2])^2
    
    if (f == 1) print(paste("      ", " ", "w/o HY", "    ", "w/ HY", "    ", "w/o - w/"))
    print(paste("fold", f, ":", round(r2_rf, 4), "    ", round(r2_hy, 4), "    ", round(r2_rf - r2_hy, 4)))
  }
}
































































df1 <- dat_preds  %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = c("spec", "test_fold"),
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = ifelse(spec == "spec1", "unweighted", "weighted"),
         test_fold = gsub("fold", "", test_fold) %>% as.integer(),
         set = ifelse(fold == test_fold, "test", "train") %>% factor(levels = c("train", "test"))) %>% 
  group_by(spec, set) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2")) %>% 
  ungroup()
df2 <- dat_preds %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = c("spec", "test_fold"),
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = ifelse(spec == "spec1", "unweighted", "weighted"),
         test_fold = gsub("fold", "", test_fold) %>% as.integer(),
         set = ifelse(fold == test_fold, "test", "train") %>% factor(levels = c("train", "test"))) %>% 
  group_by(spec, test_fold, set) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2")) %>% 
  ungroup()

df3 <- dat_preds %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = c("spec", "test_fold"),
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = ifelse(spec == "spec1", "unweighted", "weighted"),
         test_fold = gsub("fold", "", test_fold) %>% as.integer(),
         set = ifelse(fold == test_fold, "test", "train") %>% factor(levels = c("train", "test"))) %>% 
  group_by(spec, fold, test_fold, set) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2")) %>% 
  ungroup()

df0 <- bind_rows(df1, df2, df3)

ggplot() + 
  # geom_point(data = df3, mapping = aes(set, r2), color = "green") + 
  geom_point(data = df2, mapping = aes(set, r2), color = "red") +
  geom_point(data = df1, mapping = aes(set, r2)) + 
  facet_wrap(vars(spec))

ggplot() + 
  # geom_point(data = df3, mapping = aes(set, r2), color = "green") + 
  geom_point(data = df2, mapping = aes(set, r2), color = "red") +
  geom_point(data = df1, mapping = aes(set, r2)) + 
  geom_line(data = df1, mapping = aes(set, r2)) + 
  facet_wrap(vars(spec))



ggplot() + 
  geom_point(data = df2, mapping = aes(set, r2, color = spec)) +
  # geom_line(data = df2, mapping = aes(set, r2, group = interaction(spec, test_fold)), color = "red") + 
  # geom_point(data = df1, mapping = aes(set, r2)) + 
  geom_line(data = df1, mapping = aes(set, r2, group = spec, color = spec)) + 
  theme_classic()



df4 <- df2 %>% 
  pivot_wider(names_from = set,
              names_prefix = "r2_",
              values_from = r2)
ggplot(df4, aes(r2_train, r2_test)) + 
  geom_point(aes(color = spec))


library(GGally)
df0 <- bind_rows(df1, df2)
ggparcoord(df4, columns = 4:3, groupColumn = 1, scale="globalminmax") + 
  theme_classic() + 
  scale_y_continuous(name = "train",
                     sec.axis = dup_axis(name = "test")) + 
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) + 
  facet_wrap(vars(spec))

data <- iris

# Plot
ggparcoord(data,
           columns = 1:4, groupColumn = 5
) 

df10 <- dat_preds  %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = c("spec", "test_fold"),
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = ifelse(spec == "spec1", "unweighted", "weighted"),
         test_fold = gsub("fold", "", test_fold) %>% as.integer(),
         set = ifelse(fold == test_fold, "test", "train")) %>% 
  pivot_wider(names_from = "spec",
              names_prefix = "pred_",
              values_from = "pred") %>% 
  left_join(dat_di) %>% 
  mutate(pred_di = pmax(if_else(smoke_day == 1, pm25_anom_di, 0), 0),
         pred_di = if_else(is.na(pm25_anom_di), pm25_anom_di, pred_di)) %>% 
  drop_na(pred_di) %>% 
  left_join(dat_reid) %>%
  mutate(pred_reid = pmax(if_else(smoke_day == 1, pm25_anom_reid, 0), 0),
         pred_reid = if_else(is.na(pm25_anom_reid), pm25_anom_reid, pred_reid)) %>% 
  drop_na(pred_reid) %>% 
  select(-pm25_anom_di, -pm25_anom_reid) %>% 
  pivot_longer(cols = starts_with("pred"),
               names_to = "spec",
               names_prefix = "pred_",
               values_to = "pred") %>% 
  mutate(spec = factor(spec, levels = c("unweighted", "weighted", "di", "reid"))) %>% 
  filter(spec %in% c("unweighted", "weighted"))

