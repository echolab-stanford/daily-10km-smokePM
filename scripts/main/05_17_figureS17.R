source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 17.
# ------------------------------------------------------------------------------
counties <- tigris::counties() %>% 
  filter(STATEFP %in% nonContig_stateFIPS == F)

# population by grid cell
pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"),
                  full.names = T) %>% purrr::map_dfr(read.csv)

# population by census tract, aggregated to the county
acs <- list.files(file.path(path_data, "US Census Bureau", "American Community Survey", "5-Year"),
                    pattern = "2009|2019",
                    full.names = T) %>% 
  purrr::map_dfr(function(x){
    year = str_sub(x, -4, -1)
    paste0(x, "/tract/processed") %>% 
      list.files(full.names = T) %>% 
      purrr::map_dfr(readRDS) %>%
      mutate(year = year) %>%
      return
  }) %>% 
  filter(Total.Population > 0) 

acs %<>% select(GEOID, Total.Population, year) %>% 
  st_drop_geometry() %>%
  mutate(FIPS = substr(GEOID, 1, 5)) %>% 
  group_by(FIPS, year) %>% 
  summarise(pop = sum(Total.Population), 
            .groups = "drop")

# subnational gdp 
# downloaded from https://apps.bea.gov/regional/downloadzip.cfm, CAGDP9 under GDP dropdown on July 6, 2022
gdp <- read.csv(file.path(path_data, "CAGDP9", "CAGDP9__ALL_AREAS_2001_2020.csv")) %>% 
  filter(LineCode == 1) %>% 
  mutate(GeoFIPS = trimws(GeoFIPS)) %>% 
  # filter out counties outside of the contiguous US
  filter(substr(GeoFIPS, 1, 2) %in% nonContig_stateFIPS == F) %>% 
  # filter out the state totals
  filter(substr(GeoFIPS, 3, 5) != "000")

# only things that don't match are the counties in virginia 
# setdiff(gdp$GeoFIPS, counties$GEOID)
# setdiff(counties$GEOID, gdp$GeoFIPS)

# to match virginia countries between gdp data and acs/tigris files
va_gdp <- gdp %>% filter(substr(GeoFIPS, 1, 2) == "51") %>% 
  select(starts_with("Geo")) 

va_tigris <- counties %>% 
  filter(STATEFP == "51") %>% 
  select(GEOID, NAME, NAMELSAD) %>% 
  st_drop_geometry() %>% 
  filter(GEOID %in% va_gdp$GeoFIPS == F) %>% 
  mutate(NAME = ifelse(GEOID == "51600", paste0(NAME, " City"), NAME)) 

va_match <- va_gdp %>% 
  filter(grepl("+", GeoName, fixed = T)) %>% 
  mutate(GeoName = gsub(", VA*", "", GeoName, fixed = T)) %>% 
  separate(GeoName, into = c("unit1", "unit2", "unit3"), sep = "\\+|\\,", 
           remove = FALSE) %>% 
  mutate(across(starts_with("unit"), trimws)) %>% 
  left_join(va_tigris %>% 
              rename_with(function(x){paste0(x, "_unit1")}), 
            by = c("unit1" = "NAME_unit1")) %>% 
  left_join(va_tigris %>% 
              rename_with(function(x){paste0(x, "_unit2")}), 
            by = c("unit2" = "NAME_unit2")) %>% 
  left_join(va_tigris %>% 
              rename_with(function(x){paste0(x, "_unit3")}), 
            by = c("unit3" = "NAME_unit3")) %>% 
  select(gdp_FIPS = GeoFIPS, contains("GEOID")) %>% 
  pivot_longer(contains("GEOID"), values_to = "tigris_FIPS") %>% 
  select(-name) %>% 
  filter(!is.na(tigris_FIPS))

gdp %<>% select(GeoFIPS, GeoName, gdp_2009 = X2009, gdp_2019 = X2019) %>% 
  full_join(counties %>% 
              select(STATEFP, GEOID, NAME, NAMELSAD) %>% 
              full_join(acs %>% 
                          # recode Shannon County, SD to Oglala
                          # recode Bedford City which was merged into bedford county
                          mutate(FIPS = ifelse(FIPS == "46113", "46102", FIPS), 
                                 FIPS = ifelse(FIPS == "51515", "51019", FIPS)) %>%
                          group_by(FIPS, year) %>% 
                          summarise(pop = sum(pop)) %>%
                          pivot_wider(names_from = year, values_from = pop, names_prefix = "pop_"), 
                        by = c("GEOID" = "FIPS")) %>%
              left_join(va_match, by = c("GEOID" = "tigris_FIPS"))  %>%
              mutate(gdp_FIPS = coalesce(gdp_FIPS, GEOID)) %>% 
              group_by(gdp_FIPS) %>%
              summarise(NAME = paste0(NAME, collapse = ", "), 
                        pop_2009 = sum(pop_2009), 
                        pop_2019 = sum(pop_2019), 
                        do_union = TRUE, is_coverage = FALSE),
            by = c("GeoFIPS" = "gdp_FIPS")) 
gdp %<>% st_as_sf()
rm(va_gdp, va_tigris, va_match, counties, acs)

grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84")) %>% 
  st_transform(crs = st_crs(gdp))

cross = st_intersection(gdp,
                        grid_10km) %>% 
  select(GeoFIPS, grid_id_10km = ID) %>% 
  {cbind(st_drop_geometry(.), 
         area = st_area(.))} 

smokePM_pred <- readRDS(file.path(path_output, "smokePM", "predictions", "combined", "smokePM_predictions_20060101_20201231.rds"))

# calculate annual smoke PM by grid cell and time period ----
cell_smokePM <- smokePM_pred %>%
  mutate(year = lubridate::year(date)) %>%
  # calculate annual avg smokePM 
  group_by(year, grid_id_10km) %>%
  summarise(smokePM_total = sum(smokePM_pred),
            .groups = "drop") %>%
  mutate(y_days = 365 + lubridate::leap_year(year)*1, 
         smokePM_avg = smokePM_total/y_days) %>%
  select(-smokePM_total, -y_days) %>%
  # join with full set of years and grid cells to fill in zeros
  {left_join(expand.grid(year = unique(.$year),
                         grid_id_10km = unique(.$grid_id_10km)), 
             .)} %>% 
  replace_na(list(smokePM_avg = 0)) %>% 
  # add the time periods to summarise over 
  mutate(period = case_when(year >= 2006 & year <=2010 ~ "years2006_2010", 
                            year >= 2016 & year <=2020 ~ "years2016_2020",
                            T ~ as.character(NA))) %>% 
  filter(!is.na(period)) %>% 
  # average over the time period
  group_by(grid_id_10km, period) %>%
  summarise(smokePM_avg = mean(smokePM_avg),
            .groups = "drop") 

# calculate tract avgs, use pop density * intersection area for weights
tract_smokePM <- cell_smokePM %>%
  # join with tract crosswalk
  right_join(cross %>% mutate(area = unclass(area))) %>% 
  # add pop density by grid cell
  left_join(pop %>% rename(pop_density = mean), 
            by = c("grid_id_10km" = "ID")) %>% 
  filter(!(is.na(period) & GeoFIPS == "12087")) %>% # this just drops the grid cells in the florida keys for which we don't have predictions
  # group by tract IDs
  group_by(GeoFIPS, period) %>% 
  summarise(mean_smokePM = weighted.mean(smokePM_avg, area*pop_density, na.rm = TRUE),
            .groups = "drop") 

# make a data frame for fitting the splines
spline_dt <- rbind(cell_smokePM %>% 
                     left_join(pop %>% rename(covar = mean), 
                               by = c("grid_id_10km" = "ID")) %>% 
                     rename(unit = grid_id_10km) %>% 
                     mutate(covar = asinh(pmin(covar*1000^2, 1000)), # convert from people/m2 to asinh people/km^2
                            name = "Population~density~(population/km^2)"),
                   gdp %>% 
                     mutate(across(starts_with("gdp"), as.numeric)) %>%
                     pivot_longer(c(starts_with("gdp"), starts_with("pop")),
                                  names_sep = "_", names_to = c("name", "year")) %>%
                     pivot_wider(values_from = value, names_from = name) %>% 
                     mutate(gdp_per_cap = gdp/pop,
                            period = recode(year, 
                                            "2009" = "years2006_2010", 
                                            "2019" = "years2016_2020")) %>%
                     left_join(tract_smokePM) %>% 
                     transmute(unit = GeoFIPS, 
                               period, smokePM_avg = mean_smokePM, 
                               covar = pmin(gdp_per_cap, 150), 
                               name = "GDP~per~capita"))

# fit splines. bootstrapping to get confidence intervals ----
nboot = 1000
spline_df = 3
set.seed(1001)
spline_fits <- spline_dt %>% 
  nest_by(name, period) %>% 
  purrr::pmap_dfr(function(name, period, data){
    print(name)
    print(period)
    x_seq <- seq(to  = min(data$covar), from = max(data$covar), length.out = 100)
    fmla <- as.formula("smokePM_avg ~ ns(covar, df = spline_df)")
    replicate(nboot, {
      samp <- data[sample.int(n = nrow(data), replace = TRUE),]
      predict(lm(fmla, data = samp), data.frame(covar = x_seq))
    }) %>% 
      apply(1, quantile, c(0.025, 0.05, 0.5,  0.95, 0.975)) %>% 
      t %>% data.frame %>% 
      rename_with(function(x){paste0("q", gsub("X|\\.$" ,"", x))}) %>% 
      cbind(covar = x_seq, 
            name = name, 
            period = period,
            full_samp = predict(lm(fmla, data = data), data.frame(covar = x_seq))) %>% 
      select(name, period, covar, starts_with("q"), full_samp)
  })

# plot the splines and histograms ----
g_spline_hist <- spline_fits %>% 
  mutate(name = recode(name, 
                       "Population~density~(population/km^2)" = "Population~density~(per~km^2)", 
                       "GDP~per~capita" = "paste(\"GDP per capita ($1,000)\")")) %>%
  mutate(panel = factor("paste(\"smoke \", PM[2.5], \" (\", mu, \"g/\", m^3, \")\",)",
                        levels = c("paste(\"smoke \", PM[2.5], \" (\", mu, \"g/\", m^3, \")\",)", "density"), ordered = T)) %>%
  ggplot(aes(x = covar, y = full_samp, ymin = q2.5, ymax = q97.5,
             group = period, color = period, fill = period)) + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  geom_line(lwd = 1) + 
  geom_histogram(data = spline_dt %>%
                   mutate(name = recode(name, 
                                        "Population~density~(population/km^2)" = "Population~density~(per~km^2)",
                                        "GDP~per~capita" = "paste(\"GDP per capita ($1,000)\")")) %>%
                   filter(period == "years2016_2020") %>%
                   mutate(panel = factor("density",
                                         levels = c("paste(\"smoke \", PM[2.5], \" (\", mu, \"g/\", m^3, \")\",)", "density"), ordered = T)),
                 boundary = 0,
                 mapping = aes(x = covar,  y = ..ndensity..),
                 inherit.aes = FALSE) +
  facetscales::facet_grid_sc(panel ~ name, switch = "both",
                             labeller = label_parsed,
                             scales = list(y = list(`paste(\"smoke \", PM[2.5], \" (\", mu, \"g/\", m^3, \")\",)` = scale_y_continuous(),
                                                    `density` = scale_y_continuous(expand = expansion(c(0, NA)))),
                                           x = list(`paste(\"GDP per capita ($1,000)\")` = scale_x_continuous(limits = c(0, NA)),
                                                    # adjust pop density x axis for asinh transform
                                                    `Population~density~(per~km^2)` = scale_x_continuous(limits = c(0, NA),
                                                                                                                breaks = asinh(c(0, 2, 10, 100, 1000, 10000)),
                                                                                                                labels =       c(0, 2, 10, 100, 1000, 10000))))) +
  xlab("") + ylab("") +
  theme_classic() +
  scale_color_manual(values = c("#4C0121", 
                                "#00008B"),
                     aesthetics = c("fill", "color")) +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.x = element_text(margin = margin(1, 1, 1, 1, unit  = "pt")),
        strip.text.y = element_text(margin =  margin(1, 1, 1, 1, unit  = "pt")),
        text = element_text(size = 14),
        legend.position = "none", 
        panel.spacing.x = unit(15, unit = "points"),
        plot.margin = unit(c(0,0.025,-0.025, -0.01), "npc"))

# minor plot adjustments ----
# adjust plot to drop yaxis on histogram portion 
g_spline_hist %<>% ggplot_build() %>% ggplot_gtable() %>% 
  gtable_filter(pattern = "axis-l-2", invert = T) %>% 
  gtable_filter(pattern = "strip-l-2", invert = T)

# shrink the height of the histograms
g_spline_hist$heights[9] <- g_spline_hist$heights[9]*0.2 

# plot maps of the covariates ----
g_maps <- purrr::pmap(data.frame(x = c("gdp_per_cap", "pop_density"),
                                 sf_name = c("gdp", "grid_10km"),
                                 sf_unit_col = c("GeoFIPS", "ID"),
                                # x_pal = c("dense", rep("curl", 1)), #c("matter", "curl", "tarn", "balance"), 
                                rev_pal = c(FALSE, FALSE),
                                pal_quantiles = c(FALSE, FALSE), 
                                plot_cols = I(list(cmocean::cmocean("curl", clip = 0.1)(10),
                                                   scico::scico(10, palette = "bilbao")))),
                     function(x, sf_name, sf_unit_col, rev_pal, pal_quantiles, plot_cols){
                       print(x)
                       plot_dt <- spline_dt %>% 
                         mutate(name = recode(name, 
                                              "Population~density~(population/km^2)" = "pop_density", 
                                              "GDP~per~capita" = "gdp_per_cap")) %>% 
                         filter(period == "years2016_2020") %>% 
                         filter(name == x)
                       
                       plot_sf <- get(sf_name) %>% 
                         mutate(across(any_of(sf_unit_col), as.character))
                         
                       plot_dt %<>% left_join(plot_sf, by = c("unit" = sf_unit_col)) %>% 
                         st_as_sf
                       
                       if(rev_pal){plot_cols %<>% rev}
                       if(pal_quantiles){
                         col_values <- quantile(plot_dt$covar,
                                                probs = seq(0, 1, by = 0.1),
                                                na.rm = TRUE)/max(plot_dt$covar, na.rm = T) %>% unname
                       } else {col_values <- NULL}
                       if(x == "pop_density"){
                         leg_breaks = asinh(c(0, 2, 10, 100, 1000, 10000))
                         leg_labels = c(0, 2, 10, 100, 1000, 10000)
                       } else {
                         leg_breaks = waiver()
                         leg_labels = waiver()
                       }
                      plot_dt %>% 
                         ggplot() +
                         geom_sf(aes(color = covar,
                                     fill = covar), lwd = 0.05) + 
                         theme_void() + 
                         scale_color_gradientn(colors = plot_cols, 
                                               aesthetics = c("color", "fill"),
                                               values = col_values,
                                               guide = guide_colorbar(barwidth = unit(0.015, units = "npc"),
                                                                      barheight = unit(0.07, units = "npc"),), 
                                               name = "",
                                               breaks = leg_breaks, 
                                               labels = leg_labels) + 
                         theme(legend.position = c(0.1, -0.05),
                               legend.justification = c(0, 0),
                               legend.text = element_text(size = 8),
                               plot.margin = unit(c(0, -0.1, 0, -0.1), units = "npc"),
                               plot.background = element_rect(fill = "white",
                                                              color = NA)) 
                     })
# ggsave("./test.png", g_maps[[2]])

# add the maps to the gtable ----
g_spline_hist %>% 
  gtable_add_rows(heights = g_spline_hist %>% 
                    gtable_filter(pattern = "panel-1-1") %>% 
                    gtable_height()*0.8, 
                  pos = 0) %>% 
  # gtable_show_layout()
  gtable_add_grob(g_maps[[1]] %>% ggplot_build() %>% ggplot_gtable(),
                  t = 1, b = 2, l = 7, r = 7) %>%
  gtable_add_grob(g_maps[[2]] %>% ggplot_build() %>% ggplot_gtable(),
                  t = 1, b = 2, l = 9, r = 9) %>%
  ggsave(file.path(path_figures, "figureS17.png"), ., 
         width = 6, height = 4.5)
