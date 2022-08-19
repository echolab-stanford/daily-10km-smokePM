source("work/05_get_HYSPLIT_height/00_utils.R")

library(dplyr)

#-------------------------------------------------------------------------------
# Does Plume Height from HYSPLIT Improve Daily-Level PM2.5 Prediction?
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
# Read in merged data
dat_merged <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_overlap_48hr_EPA_smoke_AOT_covariates.rds"))

dat_merged <- dat_merged %>% 
  mutate(date = as.Date(date))

# Choose aggregation method: nn, idw, or k (idw of knn)
agg <- "idw"

# Choose cutoff: 10, 30, 50, 80, or 100 km
cut <- 30

# NEED TO STANDARDIZE SAMPLE: aggregation method, cutoff, and injection heights
dat_merged %>% 
  group_by()

# Configure data frame based on the above
dat_merged1 <- dat_merged %>% filter(agg_method == agg, cutoff == cut)

# Drop obs where missing height
dat_merged1 <- dat_merged1 %>% drop_na(height)

df <- dat_merged1
mdl0 <- lm(pm25 ~ aot_anom + aot_anom:smoke_day,
           data = df)
mdl1 <- lm(pm25 ~ aot_anom + aot_anom:smoke_day + aot_anom:smoke_day:height,
           data = df)
mdl2 <- lm(pm25 ~ aot_anom + aot_anom:smoke_day + aot_anom:smoke_day:pressure,
           data = df)

suppressWarnings(stargazer(mdl0, mdl1, mdl2, type = "html"))


df$pred_aotanom_smokeday <- predict(mdl0)
df[!is.na(df$height), "pred_aotanom_smokeday_height"] <- predict(mdl1)
df[!is.na(df$pressure), "pred_aotanom_smokeday_pressure"] <- predict(mdl2)




t1 <- as.Date("2020-08-01")
t2 <- as.Date("2020-10-15")
event <- df %>% 
  filter(t1 <= date, date <= t2, cbsa_name == "San Francisco-Oakland-Hayward, CA") %>% 
  group_by(date) %>% 
  summarize(across(c(pm25, starts_with("pred")), mean, na.rm = TRUE)) %>% 
  pivot_longer(c(pm25, starts_with("pred_")), names_to = "mdl")

ggplot(data = event, mapping = aes(x = date, y = value)) +
  geom_line(mapping = aes(color = mdl)) +
  theme_classic()

etable(mdl0, mdl1, mdl2, drop.section = "fixef")




dfsd <- df %>% 
  filter(smoke_day == 1) %>% 
  pivot_longer(starts_with("pred_"), names_to = "mdl")
ggplot(data = dfsd, mapping = aes(x = value, y = pm25)) + 
  geom_point(aes(color = mdl), alpha = 0.2) + 
  facet_wrap(vars(mdl))


dfsd <- df %>% 
  filter(smoke_day == 1) %>% 
  select(pm25, starts_with("pred_"))
corr_dfsd <- cor(dfsd, use = "complete.obs")
ggcorrplot(corr_dfsd, lab = TRUE)








t1 <- as.Date("2020-08-01")
t2 <- as.Date("2020-11-30")
event <- df %>% 
  filter(t1 <= date, date <= t2, 
         county %in% c("Glenn", "Lake", "Mendocino", "Tehama", "Trinity", "Shasta")) %>% 
  group_by(date) %>% 
  summarize(across(c(pm25, starts_with("pred")), mean, na.rm = TRUE)) %>% 
  pivot_longer(c(pm25, starts_with("pred_")), names_to = "mdl")

ggplot(data = event, mapping = aes(x = date, y = value)) +
  geom_line(mapping = aes(color = mdl)) +
  theme_classic()








# New option "extr" for using within-trajectory extrema
agg <- "extr"
cut <- 30
dat_merged1 <- dat_merged0 %>% filter(agg_method == agg, cutoff == cut)
dat_merged1 <- dat_merged1 %>% drop_na(height)












# Read in data with background PM2.5 and get anomalous PM2.5
dat_covariates <- readRDS("/Users/jessssli/BurkeLab Dropbox/Data/PM25/epa_station_covariates.rds") %>% 
  mutate(pm25_anom = pm25 - pm25_med_3yr)
dat_merged_smoke <- dat_merged00 %>% 
  left_join(dat_covariates %>% select(epa_id, date, pm25, pm25_med_3yr, pm25_anom),
            by = c("id_epa" = "epa_id", "date", "pm25"))


agg <- "idw"
cut <- 30
dat_merged1 <- dat_merged_smoke %>% 
  filter(agg_method == agg, cutoff == cut) %>% 
  drop_na(height, pm25_anom)


hist(dat_merged1$pm25_anom)



df <- dat_merged1
mdl0 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day | id_epa + month,
              data = df)
mdl1 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:height | id_epa + month,
              data = df)
mdl2 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:pressure | id_epa + month,
              data = df)

etable(mdl0, mdl1, mdl2, drop.section = "fixef")


smoke_day <- df$smoke_day
aot_anom <- df$aot_anom
height <- df$height
pressure <- df$pressure

coefs <- coefficients(mdl0)
df$pred_baseline <- coefs["smoke_day"]*smoke_day + 
  coefs["smoke_day:aot_anom"]*aot_anom*smoke_day

coefs <- coefficients(mdl1)
df$pred_height <- coefs["smoke_day"]*smoke_day + 
  coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
  coefs["smoke_day:aot_anom:height"]*aot_anom*smoke_day*height

coefs <- coefficients(mdl2)
df$pred_pressure <- coefs["smoke_day"]*smoke_day + 
  coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
  coefs["smoke_day:aot_anom:pressure"]*aot_anom*smoke_day*pressure



dfsd <- df %>% 
  filter(smoke_day == 1) %>% 
  select(pm25_anom, starts_with("pred_"))
corr_dfsd <- cor(dfsd, use = "complete.obs")
ggcorrplot(corr_dfsd, lab = TRUE)


dfsd <- df %>% 
  filter(smoke_day == 1) %>% 
  pivot_longer(starts_with("pred_"), names_to = "mdl")
ggplot(data = dfsd, mapping = aes(x = value, y = pm25_anom)) + 
  geom_point(aes(color = mdl), alpha = 0.2) + 
  facet_wrap(vars(mdl))


t1 <- as.Date("2020-08-01")
t2 <- as.Date("2020-10-15")
event <- df %>% 
  filter(t1 <= date, date <= t2, cbsa_name == "San Francisco-Oakland-Hayward, CA") %>% 
  group_by(date) %>% 
  summarize(across(c(pm25_anom, starts_with("pred")), mean, na.rm = TRUE)) %>% 
  pivot_longer(c(pm25_anom, starts_with("pred_")), names_to = "mdl")

ggplot(data = event, mapping = aes(x = date, y = value)) +
  geom_line(mapping = aes(color = mdl)) +
  theme_classic()



t1 <- as.Date("2020-08-01")
t2 <- as.Date("2020-11-30")
event <- df %>% 
  filter(t1 <= date, date <= t2, 
         county %in% c("Glenn", "Lake", "Mendocino", "Tehama", "Trinity", "Shasta")) %>% 
  group_by(date) %>% 
  summarize(across(c(pm25_anom, starts_with("pred")), mean, na.rm = TRUE)) %>% 
  pivot_longer(c(pm25_anom, starts_with("pred_")), names_to = "mdl")

ggplot(data = event, mapping = aes(x = date, y = value)) +
  geom_line(mapping = aes(color = mdl)) +
  theme_classic()












dat_overlap48 <- dat_traj2
id_overlap48 <- unique(dat_overlap48$id_traj)
dat_linked <- dat_traj1
dat_traj <- dat_linked %>% 
  select(-id_hysplit) %>% 
  distinct() %>% 
  filter(id_traj %in% id_overlap48) %>% 
  mutate(height_i = as.factor(height_i))


ggplot(data = dat_traj, mapping = aes(x = height, group = height_i, fill = height_i)) + 
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = c(500, 1500, 2500), linetype = 2) + 
  theme_classic() + 
  labs(title = "Trajectory Points", x = "mAGL", y = "Density", fill = "Injection Height")





dat_traj3 <- dat_traj %>% 
  group_by(height_i, hour_along) %>% 
  summarize(height = mean(height)) %>% 
  ungroup()

set.seed(123)
rand <- dat_traj %>% 
  pull(id_traj) %>% 
  unique() %>% 
  sample(200)

ggplot(data = dat_traj %>% filter(id_traj %in% rand), mapping = aes(x = hour_along, y = height, group = id_traj)) + 
  geom_line(alpha = 0.2, color = "gray") + 
  geom_line(data = dat_traj3, mapping = aes(x = hour_along, y = height, group = height_i, color = height_i)) + 
  facet_wrap(vars(height_i)) +
  geom_hline(yintercept = c(500, 1500, 2500), linetype = 2) + 
  theme_classic()





dat_iheight <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_EPA_smoke_AOT_CA_2020_48hour_iheight.rds")) %>% 
  mutate(date = as.Date(date), 
         year = year(date),
         month = month(date),
         day = day(date))

# Drop obs not in California
loc_epa <- dat_iheight[c("lon_epa", "lat_epa")] %>% distinct()
o <- over(SpatialPoints(loc_epa), us_states)
dat_iheight <- dat_iheight %>% 
  left_join(bind_cols(loc_epa, o)) %>% 
  rename(state = NAME) %>% 
  filter(state == "California")



dat_iheight_smoke <- dat_iheight %>% 
  left_join(dat_covariates %>% select(epa_id, date, pm25, pm25_med_3yr, pm25_anom),
            by = c("id_epa" = "epa_id", "date", "pm25"))
dat_iheight_smoke <- dat_iheight_smoke %>% 
  bind_rows(dat_merged_smoke %>% mutate(injection_heights = "500+1500+2500"))



agg <- "idw"
cut <- 30
dat_merged1 <- dat_iheight_smoke %>% 
  filter(agg_method == agg, cutoff == cut) %>% 
  drop_na(height, pm25_anom)

# Get to same sample across injection height combinations
id012 <- dat_merged1 %>% 
  filter(injection_heights == "500+1500+2500") %>% 
  select(date, id_epa) %>% 
  distinct()
id01 <- dat_merged1 %>% 
  filter(injection_heights == "500+1500") %>% 
  select(date, id_epa) %>% 
  distinct()
id02 <- dat_merged1 %>% 
  filter(injection_heights == "500+2500") %>% 
  select(date, id_epa) %>% 
  distinct()
id12 <- dat_merged1 %>% 
  filter(injection_heights == "1500+2500") %>% 
  select(date, id_epa) %>% 
  distinct()

id_common <- reduce(list(id012, id01, id02, id12), inner_join)

dat_merged1 <- dat_merged1 %>% 
  filter(paste(date, id_epa) %in% paste(id_common$date, id_common$id_epa))




df <- dat_merged1
mdl0012 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day | id_epa + month,
                 data = df %>% filter(injection_heights == "500+1500+2500"))
mdl1012 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:height | id_epa + month,
                 data = df %>% filter(injection_heights == "500+1500+2500"))
mdl2012 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:pressure | id_epa + month,
                 data = df %>% filter(injection_heights == "500+1500+2500"))

mdl001 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day | id_epa + month,
                data = df %>% filter(injection_heights == "500+1500"))
mdl101 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:height | id_epa + month,
                data = df %>% filter(injection_heights == "500+1500"))
mdl201 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:pressure | id_epa + month,
                data = df %>% filter(injection_heights == "500+1500"))

mdl002 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day | id_epa + month,
                data = df %>% filter(injection_heights == "500+2500"))
mdl102 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:height | id_epa + month,
                data = df %>% filter(injection_heights == "500+2500"))
mdl202 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:pressure | id_epa + month,
                data = df %>% filter(injection_heights == "500+2500"))

mdl012 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day | id_epa + month,
                data = df %>% filter(injection_heights == "1500+2500"))
mdl112 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:height | id_epa + month,
                data = df %>% filter(injection_heights == "1500+2500"))
mdl212 <- feols(pm25_anom ~ smoke_day + aot_anom:smoke_day + aot_anom:smoke_day:pressure | id_epa + month,
                data = df %>% filter(injection_heights == "1500+2500"))


etable(mdl0012, mdl1012, mdl2012, 
       mdl001, mdl101, mdl201, 
       mdl002, mdl102, mdl202, 
       mdl012, mdl112, mdl212, 
       drop.section = "fixef")







smoke_day <- df$smoke_day
aot_anom <- df$aot_anom
height <- df$height
pressure <- df$pressure

df$pred_baseline <- NA
df$pred_height <- NA
df$pred_pressure <- NA

coefs <- coefficients(mdl0012)
i <- df$injection_heights == "500+1500+2500"
df$pred_baseline[i] <- (coefs["smoke_day"]*smoke_day + 
                          coefs["smoke_day:aot_anom"]*aot_anom*smoke_day)[i]
coefs <- coefficients(mdl1012)
df$pred_height[i] <- (coefs["smoke_day"]*smoke_day + 
                        coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
                        coefs["smoke_day:aot_anom:height"]*aot_anom*smoke_day*height)[i]
coefs <- coefficients(mdl2012)
df$pred_pressure[i] <- (coefs["smoke_day"]*smoke_day + 
                          coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
                          coefs["smoke_day:aot_anom:pressure"]*aot_anom*smoke_day*pressure)[i]

coefs <- coefficients(mdl001)
i <- df$injection_heights == "500+1500"
df$pred_baseline[i] <- (coefs["smoke_day"]*smoke_day + 
                          coefs["smoke_day:aot_anom"]*aot_anom*smoke_day)[i]
coefs <- coefficients(mdl101)
df$pred_height[i] <- (coefs["smoke_day"]*smoke_day + 
                        coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
                        coefs["smoke_day:aot_anom:height"]*aot_anom*smoke_day*height)[i]
coefs <- coefficients(mdl201)
df$pred_pressure[i] <- (coefs["smoke_day"]*smoke_day + 
                          coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
                          coefs["smoke_day:aot_anom:pressure"]*aot_anom*smoke_day*pressure)[i]

coefs <- coefficients(mdl002)
i <- df$injection_heights == "500+2500"
df$pred_baseline[i] <- (coefs["smoke_day"]*smoke_day + 
                          coefs["smoke_day:aot_anom"]*aot_anom*smoke_day)[i]
coefs <- coefficients(mdl102)
df$pred_height[i] <- (coefs["smoke_day"]*smoke_day + 
                        coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
                        coefs["smoke_day:aot_anom:height"]*aot_anom*smoke_day*height)[i]
coefs <- coefficients(mdl202)
df$pred_pressure[i] <- (coefs["smoke_day"]*smoke_day + 
                          coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
                          coefs["smoke_day:aot_anom:pressure"]*aot_anom*smoke_day*pressure)[i]

coefs <- coefficients(mdl012)
i <- df$injection_heights == "1500+2500"
df$pred_baseline[i] <- (coefs["smoke_day"]*smoke_day + 
                          coefs["smoke_day:aot_anom"]*aot_anom*smoke_day)[i]
coefs <- coefficients(mdl112)
df$pred_height[i] <- (coefs["smoke_day"]*smoke_day + 
                        coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
                        coefs["smoke_day:aot_anom:height"]*aot_anom*smoke_day*height)[i]
coefs <- coefficients(mdl212)
df$pred_pressure[i] <- (coefs["smoke_day"]*smoke_day + 
                          coefs["smoke_day:aot_anom"]*aot_anom*smoke_day + 
                          coefs["smoke_day:aot_anom:pressure"]*aot_anom*smoke_day*pressure)[i]






dfsd <- df %>% 
  select(date, id_epa, smoke_day, injection_heights, pm25_anom, starts_with("pred")) %>% 
  pivot_wider(names_from = injection_heights, values_from = c(pred_height, pred_pressure)) %>% 
  filter(smoke_day == 1) %>% 
  select(pm25_anom, starts_with("pred_"))
corr_dfsd <- cor(dfsd, use = "complete.obs")
ggcorrplot(corr_dfsd, lab = TRUE, title = "Smoke days")


dfsd <- dfsd %>% filter(pm25_anom > 100)
corr_dfsd <- cor(dfsd, use = "complete.obs")
ggcorrplot(corr_dfsd, lab = TRUE, title = "Smoke days with > 100 ug")



dfsd <- df %>%
  filter(smoke_day == 1) %>%
  pivot_longer(starts_with("pred_"), names_to = "mdl")
ggplot(data = dfsd %>% filter(mdl != "pred_baseline"), mapping = aes(x = value, y = pm25_anom)) +
  geom_point(aes(color = injection_heights), alpha = 0.2) +
  facet_wrap(vars(mdl))



