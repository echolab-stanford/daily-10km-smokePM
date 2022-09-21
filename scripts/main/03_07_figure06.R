# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots smoke PM by covariates.
# ------------------------------------------------------------------------------
covar <- list.files(
  file.path(path_data, "US Census Bureau", "American Community Survey", "5-Year"), 
  pattern = "2009|2019", 
  full.names = T
) %>% 
  purrr::map_dfr(function(x){
    year = str_sub(x, -4, -1)
    paste0(x, "/tract/processed") %>% 
      list.files(full.names = T) %>% 
      purrr::map_dfr(readRDS) %>%
      mutate(year = year) %>%
      return
  }) %>% 
  filter(Total.Population > 0)

grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84")) %>% 
  st_transform(crs = st_crs(covar))

# saveRDS(covar, "~/Desktop/covar.rds")
# saveRDS(grid_10km, "~/Desktop/grid_10km.rds")
# on Sherlock with large amount of memory allocated, run st_intersection below

if (!file.exists(file.path(path_data, "crosswalk_Census_tracts_grid_10km.rds"))) {
  cross = st_intersection(covar,
                          grid_10km) %>% 
    select(GEOID, year, grid_id_10km = ID) %>% 
    {cbind(st_drop_geometry(.), 
           area = st_area(.))}
  saveRDS(cross, file.path(path_data, "crosswalk_Census_tracts_grid_10km.rds"))
} else {
  cross = readRDS(file.path(path_data, "crosswalk_Census_tracts_grid_10km.rds"))
}

smokePM_pred <- readRDS(file.path(path_output, "smokePM", "predictions", "combined", "smokePM_predictions_20060101_20201231.rds")) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

# calculate annual smoke PM by census tract and time period ----
# using area-weighted intersection with grid cells 
tract_smokePM <- smokePM_pred %>%
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
            .groups = "drop") %>% 
  # join with tract crosswalk
  right_join(cross %>% 
               mutate(period = ifelse(year == "2009", "years2006_2010", "years2016_2020"), 
                      area = unclass(area))) %>%
  # group by tract IDs, preserving the year variable for joining in the covariates
  group_by(GEOID, period, year) %>%
  summarise(mean_smokePM = weighted.mean(smokePM_avg, area),
            .groups = "drop") %>%
  # pair with census covariates
  full_join(covar %>% st_drop_geometry()) %>%
  mutate(Income.PerCapita.2020Dollars = ifelse(year == "2019", Income.PerCapita, Income.PerCapita.2020Dollars)) %>%
  mutate(Income.PerCapita.2020Dollars = pmin(1e5, Income.PerCapita.2020Dollars)/1e3) %>%
  select(GEOID, period, Percent.HL, Percent.NotHL.White, Percent.NotHL.Black, Total.Population,
         Income.PerCapita.2020Dollars, mean_smokePM) 

# fit splines. bootstrapping to get confidence intervals ----
nboot = 1000
spline_df = 3
set.seed(1001)
spline_fits <- tract_smokePM %>% 
  # pivot longer, making a different row for each covariate to make fitting splines easier on nested data frame
  pivot_longer(c(starts_with("Percent"), starts_with("Income"))) %>%
  mutate(name = recode(name,
                       "Income.PerCapita.2020Dollars" = "Income per capita ($1,000)",
                       "Percent.HL" = "Percent Hispanic",
                       "Percent.NotHL.Black" = "Percent Black",
                       "Percent.NotHL.White" = "Percent White")) %>%
  filter(!is.na(period)) %>%
  nest_by(name, period) %>% 
  purrr::pmap_dfr(function(name, period, data){
    print(name)
    print(period)
    data %<>% filter(!is.na(value))
    # knots <- quantile(data$value, seq(0.2, 0.8, by = 0.2), na.rm = T)
    x_seq <- seq(to  = min(data$value), from = max(data$value), length.out = 100)
    fmla <- as.formula("mean_smokePM ~ ns(value, df = spline_df)")
    replicate(nboot, {
      samp <- data[sample.int(n = nrow(data), replace = TRUE),]
      predict(lm(fmla, data = samp), data.frame(value = x_seq))
    }) %>% 
      apply(1, quantile, c(0.025, 0.05, 0.5,  0.95, 0.975)) %>% 
      t %>% data.frame %>% 
      rename_with(function(x){paste0("q", gsub("X|\\.$" ,"", x))}) %>% 
      cbind(value = x_seq, 
            name = name, 
            period = period,
            full_samp = predict(lm(fmla, data = data), data.frame(value = x_seq))) %>% 
      select(name, period, value, starts_with("q"), full_samp)
  })

# plot the splines and histograms ----
g_spline_hist <- spline_fits %>% 
  mutate(panel = factor("paste(\"smoke \", PM[2.5], \" (\", mu, \"g/\", m^3, \")\",)", 
                        levels = c("paste(\"smoke \", PM[2.5], \" (\", mu, \"g/\", m^3, \")\",)", "density"), ordered = T)) %>%
  mutate(name = gsub(" ", "~", name, fixed = T),
         name = ifelse(name == "Income~per~capita~($1,000)", "paste(\"Income per capita ($1,000)\")", name)) %>%
  ggplot(aes(x = value, y = full_samp, ymin = q2.5, ymax = q97.5,
             group = period, color = period, fill = period)) + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  geom_line(lwd = 1) + 
  geom_histogram(data = tract_smokePM %>%
                   pivot_longer(c(starts_with("Percent"), starts_with("Income"))) %>%
                   mutate(name = recode(name,
                                        "Income.PerCapita.2020Dollars" = "Income per capita ($1,000)",
                                        "Percent.HL" = "Percent Hispanic",
                                        "Percent.NotHL.Black" = "Percent Black",
                                        "Percent.NotHL.White" = "Percent White")) %>%
                   mutate(name = gsub(" ", "~", name, fixed = T),
                          name = ifelse(name == "Income~per~capita~($1,000)", "paste(\"Income per capita ($1,000)\")", name)) %>%
                   filter(!is.na(value) & period == "years2016_2020") %>%
                   mutate(panel = factor("density",
                                         levels = c("paste(\"smoke \", PM[2.5], \" (\", mu, \"g/\", m^3, \")\",)", "density"), ordered = T)),
                 boundary = 0,
                 mapping = aes(x = value,  y = ..ndensity..),
                 inherit.aes = FALSE) +
  facetscales::facet_grid_sc(panel ~ name, switch = "both",
                             labeller = label_parsed,
                             scales = list(y = list(`paste(\"smoke \", PM[2.5], \" (\", mu, \"g/\", m^3, \")\",)` = scale_y_continuous(),
                                                    `density` = scale_y_continuous(expand = expansion(c(0, NA)))),
                                           x = list(`paste(\"Income per capita ($1,000)\")` = scale_x_continuous(limits = c(0, NA)), 
                                                    `Percent~Black` = scale_x_continuous(labels = scales::percent_format()),
                                                    `Percent~Hispanic` = scale_x_continuous(labels = scales::percent_format()),
                                                    `Percent~White` = scale_x_continuous(labels = scales::percent_format())))) +
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
# g_spline_hist %<>% ggplot_build() %>% ggplot_gtable() %>% 
#   gtable_filter(pattern = "axis-l-3", invert = T) %>% 
#   gtable_filter(pattern = "strip-l-3", invert = T)
g_spline_hist %<>% ggplot_build() %>% ggplot_gtable() %>% 
  gtable_filter(pattern = "axis-l-2", invert = T) %>% 
  gtable_filter(pattern = "strip-l-2", invert = T)

# shrink the height of the histograms
g_spline_hist$heights[9] <- g_spline_hist$heights[9]*0.2 

# plot maps of the covariates ----
simp_tracts <- covar %>% 
  mutate(Income.PerCapita = pmin(1e5, Income.PerCapita)) %>%
  mutate(Income.PerCapita = Income.PerCapita/1e3) %>%
  select(all_of(c("GEOID", "Percent.HL", "Percent.NotHL.White", "Percent.NotHL.Black", "Income.PerCapita"))) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 0.001)

g_maps <- purrr::pmap(data.frame(x = c("Income.PerCapita", "Percent.NotHL.Black", 
                                       "Percent.HL", "Percent.NotHL.White"),
                                 x_pal = c("dense", rep("curl", 3)), #c("matter", "curl", "tarn", "balance"), 
                                 rev_pal = c(FALSE, FALSE, FALSE, FALSE),
                                 pal_quantiles = c(TRUE, FALSE, FALSE, FALSE), 
                                 plot_cols = I(list(scico::scico(10, palette = "bilbao"), #cmocean::cmocean("turbid", clip = 0.1)(10), # scico::scico(10, palette = 'vik'), # viridis::magma(10), #
                                                    cmocean::cmocean("curl", clip = 0.1)(10),
                                                    cmocean::cmocean("curl", clip = 0.1)(10),
                                                    cmocean::cmocean("curl", clip = 0.1)(10)))),
                      function(x, x_pal, rev_pal, pal_quantiles, plot_cols){
                        # plot_cols <- cmocean::cmocean(x_pal, clip = 0.1)(10)
                        
                        if(rev_pal){plot_cols %<>% rev}
                        if(pal_quantiles){
                          col_values <- quantile(simp_tracts[[x]],
                                                 probs = seq(0, 1, by = 0.1),
                                                 na.rm = TRUE)/max(simp_tracts[[x]], na.rm = T) %>% unname
                          # col_values <- scales::rescale(sinh(seq(0, 1.5, length.out = 10)))
                        } else {col_values <- NULL}
                        if(grepl("percent", x, ignore.case = TRUE)){
                          leg_labs = scales::percent_format()
                        } else{ leg_labs = waiver()}
                        # print(col_values)
                        simp_tracts %>% 
                          rename_with(function(x){"value"}, all_of(x)) %>% 
                          ggplot() +
                          geom_sf(aes(color = value,
                                      fill = value), lwd = 0.05) + 
                          theme_void() + 
                          scale_color_gradientn(colors = plot_cols, 
                                                aesthetics = c("color", "fill"),
                                                values = col_values,
                                                guide = guide_colorbar(barwidth = unit(0.015, units = "npc"),
                                                                       barheight = unit(0.07, units = "npc"),), 
                                                name = "", 
                                                labels = leg_labs) + 
                          theme(legend.position = c(0.1, -0.05),
                                legend.justification = c(0, 0),
                                legend.text = element_text(size = 8),
                                plot.margin = unit(rep(-0.1, 4), units = "npc"),
                                plot.background = element_rect(fill = "white",
                                                               color = NA)) 
                      })

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
  gtable_add_grob(g_maps[[3]] %>% ggplot_build() %>% ggplot_gtable(),
                  t = 1, b = 2, l = 11, r = 11) %>% 
  gtable_add_grob(g_maps[[4]] %>% ggplot_build() %>% ggplot_gtable(),
                  t = 1, b = 2, l = 13, r = 13) %>% 
  # Takes an hour or so
  ggsave(file.path(path_figures, "figure06a-d.png"), ., width = 11, height = 4.5)


# exposure of average person ----
tract_smokePM %>% 
  filter(!is.na(period)) %>% 
  filter(!is.na(mean_smokePM)) %>%
  filter(Total.Population > 0) %>% 
  mutate(Income_bins = cut(Income.PerCapita.2020Dollars, 
                           breaks = quantile(Income.PerCapita.2020Dollars, 
                                             probs = seq(0, 1, length.out = 6), 
                                             na.rm = T),
                           include.lowest = T) %>% as.numeric) %>% 
  mutate(Total.Income.top = ifelse(Income_bins == max(Income_bins, na.rm = T), Total.Population, 0), 
         Total.Income.bottom = ifelse(Income_bins == min(Income_bins, na.rm = T), Total.Population, 0)) %>% 
  replace_na(list(Total.Income.top = 0, 
                  Total.Income.bottom = 0)) %>% 
  mutate(across(starts_with("Percent"),
                ~.x*Total.Population, 
                .names = "Total.{.col}")) %>% 
  group_by(period) %>% 
  summarise(across(starts_with("Total"), 
                   ~weighted.mean(mean_smokePM, .x))) %>% 
  pivot_longer(!period) %>% 
  mutate(name = recode(name, 
                       "Total.Income.bottom" = "Lowest income", 
                       "Total.Income.top" = "Highest income", 
                       "Total.Percent.HL" = "Hispanic/Latino", 
                       "Total.Percent.NotHL.Black" = "Black", 
                       "Total.Percent.NotHL.White" = "White", 
                       "Total.Population" = "Average"), 
         period = recode(period, 
                         "years2006_2010" = "2006 - 2010", 
                         "years2016_2020" = "2016 - 2020")) %>% 
  {ggplot(data = ., 
          aes(x = period, y = value, group = name, color = name)) + 
      geom_point() + 
      geom_line() + 
      geom_text(data = filter(., period == "2016 - 2020"), 
                aes(x = period, 
                    y = value + case_when(name == "Highest income" ~ 0.007, 
                                          name == "Average" ~ 0.003, 
                                          name == "Lowest income" ~ -0.003, 
                                          name == "White" ~ -0.007, 
                                          T ~ 0), 
                    label = name), 
                nudge_x = 0.05,
                hjust = "left") + 
      scale_color_manual(values = c("black", MetBrewer::met.brewer("VanGogh2", 15)[c(1,3, 15,7, 9)])) +
      theme_classic() + 
      ylab(expression(paste("smoke ", PM[2.5]," (", mu, "g/", m^3, ")"))) + 
      xlab("") + 
      coord_cartesian(clip = "off") + 
      scale_x_discrete(expand = expansion(add = c(0.1))) + 
      theme(legend.position = "none", 
            plot.margin = margin(1, 75,1.5,1.2))} %>% 
  ggsave(file.path(path_figures, "figure06e.png"), ., width = 3.5*0.9, height = 5*0.9)
