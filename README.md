# daily-10km-smokePM
Repo supporting [Childs et al 2022 "Daily local-level estimates of ambient wildfire smoke PM<sub>2.5</sub> for the contiguous US"](https://doi.org/10.1021/acs.est.2c02934).

Results from the paper are in the `figures/clean` and `tables/clean` folders. Code to replicate results are in the `scripts` folder. Data, models, and predictions are in [Dropbox](https://www.dropbox.com/sh/cvl54kv4bsryme0/AAD8z2j-wcqZ_S7qc1jIcF7Na?dl=0).

## Final predictions
Daily smoke PM<sub>2.5</sub> predictions from Jan 1, 2006 to Dec 31, 2020 for the contiguous US can be [downloaded](https://www.dropbox.com/sh/wh45f4uf7gpb3ct/AAAycHq02lp1KfqxDed0SKxFa?dl=0) at the following spatial scales:
* [10 km grid](https://www.dropbox.com/sh/7vclx9rw0e7i0hq/AAD7GCLPbgGJZx0iQfwSWcxLa?dl=0)
* [County](https://www.dropbox.com/sh/hrd009uk34k7d6n/AADeUtrYp1iQWdAPwX5eKI-2a?dl=0)
* [Census tract ](https://www.dropbox.com/sh/atmtfc54zuknnob/AAA7AVRQP-GoIMHpxlvfN7RBa?dl=0)

### Descriptions of final prediction files
10 km grid
* `10km_grid/10km_grid_wgs84/`: this is a folder that contains the shapefile for the 10 km grid.
* `10km_grid/smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.rds`: this is a file that contains a data frame with the final set of daily smoke PM2.5 predictions at 10 km resolution from January 1, 2006 to December 31, 2020 for the contiguous US. All rows in this file are predictions on smoke days. Predictions on non-smoke days are by construction 0 $\mu g/m^3$ and not included in this file. A smoke PM2.5 prediction of 0 in this file means that the grid cell-day did have a smoke day but did not have elevated PM2.5. The full set of smoke PM2.5 predictions on both smoke days and non-smoke days can be obtained by setting the smoke PM2.5 prediction to 0 on grid cell-days in the 10 km grid and in the January 1, 2006-December 31, 2020 date range that are not in this file. For example, the R code below returns the full set of smoke PM2.5 predictions. Also note that in this file the grid ID is in the column called `grid_id_10km`, and this column matches to the column called `ID` in the 10 km grid shapefile.

```
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)

# Load smokePM predictions on smoke days
preds = readRDS("./final/10km_grid/smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.rds")

# Load 10 km grid
grid_10km = read_sf("./final/10km_grid/10km_grid_wgs84/10km_grid_wgs84.shp")

# Load full set of dates
dates = seq.Date(ymd("20060101"), ymd("20201231"), by = "day")

# Get full combination of grid cell-days
# Warning: this may require a large amount of memory
out = expand.grid(grid_id_10km = grid_10km$ID, date = dates)

# Match smokePM predictions on smoke days to grid cell-days
out = left_join(out, preds, by = c("grid_id_10km", "date"))

# Predict 0 for remaining grid cell-days, which are non-smoke days
out = mutate(out, smokePM_pred = replace_na(smokePM_pred, 0))
```

* `10km_grid/smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.csv`: this is the same as `smokePM2pt5_predictions_on_smokedays_daily_10km_20060101-20201231.rds`, except it is saved as a CSV file.

County
* `county/tl_2019_us_county/`: this is a folder that contains the shapefile for all US counties in 2019. Files were downloaded from the US Census Bureau TIGER/Line Shapefiles [website](https://www.census.gov/cgi-bin/geo/shapefiles/index.php). R users may also use the `tigris` package.
* `county/smokePM2pt5_predictions_daily_county_20060101-20201231.rds`: this is a file that contains a data frame with the final set of daily smoke PM2.5 predictions at the county level from January 1, 2006 to December 31, 2020 for the contiguous US. County-level smoke PM2.5 predictions are aggregated from smoke PM2.5 predictions at the 10 km resolution using population and area of intersection-weighted averaging. The `GEOID` column in this file corresponds to the `GEOID` column in the county shapefile. Example R code for merging to the county shapefile:
```
library(sf)
library(dplyr)

# Load smokePM predictions
preds = readRDS("./final/county/smokePM2pt5_predictions_daily_county_20060101-20201231.rds")

# Load counties
counties = read_sf("./final/county/tl_2019_us_county")

# Match smokePM predictions to counties
out = counties %>% right_join(preds, by = "GEOID")
```
* `county/smokePM2pt5_predictions_daily_county_20060101-20201231.csv`: this is the same as `smokePM2pt5_predictions_daily_county_20060101-20201231.rds`, except it is saved as a CSV file.

Census tract
* `tract/tracts/`: this is a folder that contains the shapefiles for all US census tracts by state/territory in 2019. Tract-level smoke PM2.5 predictions are aggregated from smoke PM2.5 predictions at the 10 km resolution using population and area of intersection-weighted averaging. Files were downloaded from the US Census Bureau TIGER/Line Shapefiles [website](https://www.census.gov/cgi-bin/geo/shapefiles/index.php). R users may also use the `tigris` package.
* `tract/smokePM2pt5_predictions_daily_tract_20060101-20201231.rds`: this is a file that contains a data frame with the final set of daily smoke PM2.5 predictions at the census tract level from January 1, 2006 to December 31, 2020 for the contiguous US. The `GEOID` column in this file corresponds to the `GEOID` column in the census tract shapefiles. Example R code for merging to the census tract shapefiles:
```
library(sf)
library(dplyr)

# Load smokePM predictions
preds = readRDS("./final/tract/smokePM2pt5_predictions_daily_tract_20060101-20201231.rds")

# Load census tracts
tracts = list.files("./final/tract/tracts", full.names = T, pattern = "\\.shp$")
tracts = lapply(tracts, read_sf)
tracts = bind_rows(tracts)

# Match smokePM predictions to census tracts
out = tracts %>% right_join(preds, by = "GEOID")
```
* `tract/smokePM2pt5_predictions_daily_tract_20060101-20201231.csv`: this is the same as `smokePM2pt5_predictions_daily_tract_20060101-20201231.rds`, except it is saved as a CSV file.

## How to replicate results
1. Download this repository.
2. Download data from [Dropbox](https://www.dropbox.com/sh/cvl54kv4bsryme0/AAD8z2j-wcqZ_S7qc1jIcF7Na?dl=0). Place files downloaded from Dropbox in the same folder as the downloaded GitHub repository.
3. Change settings in `scripts/setup/00_03_load_settings.R`:
    1. Set `gee_email` to your Google Earth Engine email.
    2. Set `key` to the value of your US Census Bureau API Key (which can be requested [here](https://api.census.gov/data/key_signup.html)).
    3. Set `num_cores` to the number of cores to use in parallel computing.
    4. Set `path_dropbox` to the location of the data downloaded from Dropbox.
    5. Set `path_github` to the location of this downloaded repository's root.
4. Install packages listed in `scripts/setup/00_01_load_packages.R`.
5. Set working directory to this downloaded repository's root.
6. Run scripts in `scripts/main`. Some scripts may require relatively large computer memory.
