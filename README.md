# daily-10km-smokePM
Repo supporting [Childs et al 2022 "Daily local-level estimates of ambient wildfire smoke PM<sub>2.5</sub> for the contiguous US"](https://doi.org/10.1021/acs.est.2c02934).

Results from the paper are in the `figures/clean` and `tables/clean` folders. Code to replicate results are in the `scripts` folder. Data, models, and predictions are in [Dropbox](https://www.dropbox.com/sh/e7m3313fb7sqxui/AABGu-jUO3Ps2isGHbB2EGfAa?dl=0).

## Final predictions
Daily smoke PM<sub>2.5</sub> predictions from Jan 1, 2006 to Dec 31, 2020 for the contiguous US can be [downloaded](https://www.dropbox.com/sh/16bwdnfbakvuf3x/AABlnrek080Qu9YnbkLkUk8ha?st=a2oli9on&dl=0) at the following spatial scales:
* [10 km grid](https://www.dropbox.com/sh/9mbcxy65crd5cex/AAAk2_1QE6A5rAS7M5We-iapa?st=f8gglo0m&dl=0)
* [County](https://www.dropbox.com/sh/tze93uz29lzyr0h/AABulMjC_l-b5YssncaK5u7ha?st=tujqcqjp&dl=0)
* [ZCTA5](https://www.dropbox.com/sh/6ynye9u1vl8idbj/AAAAjn61JpxEoyfbESsbQY9Ua?st=9kfa7mxr&dl=0)
* [Census tract](https://www.dropbox.com/sh/uw81pu5eh6o4q5v/AACZJplCr-EiLruAIFn0nm5Na?st=mz7gp2su&dl=0)

Data download is also available through [Harvard Dataverse](https://doi.org/10.7910/DVN/DJVMTV).

### Descriptions of final prediction files
10 km grid
* `10km_grid/10km_grid_wgs84/`: this is a folder that contains the shapefile for the 10 km grid.
* `10km_grid/smokePM2pt5_predictions_daily_10km_20060101-20201231.rds`: this is a file that contains a data frame with the final set of daily smoke PM2.5 predictions on smoke days at 10 km resolution from January 1, 2006 to December 31, 2020 for the contiguous US. The `grid_id_10km` column in this file corresponds to the `ID` column in the 10 km grid shapefile. All rows in this file are predictions on smoke days. Predictions on non-smoke days are by construction 0 $\mu g/m^3$ and not included in this file. A smoke PM2.5 prediction of 0 in this file means that the grid cell-day did have a smoke day but did not have elevated PM2.5. The full set of smoke PM2.5 predictions on both smoke days and non-smoke days can be obtained by setting the smoke PM2.5 prediction to 0 on grid cell-days in the 10 km grid and in the January 1, 2006-December 31, 2020 date range that are not in this file. For example, the R code below returns the full set of smoke PM2.5 predictions:

```
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)

# Load smokePM predictions on smoke days
preds = readRDS("./final/10km_grid/smokePM2pt5_predictions_daily_10km_20060101-20201231.rds")

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

* `10km_grid/smokePM2pt5_predictions_daily_10km_20060101-20201231.csv`: this is the same as `smokePM2pt5_predictions_daily_10km_20060101-20201231.rds`, except it is saved as a CSV file.

County
* `county/tl_2019_us_county/`: this is a folder that contains the shapefile for CONUS counties in 2019. Files were downloaded from the US Census Bureau TIGER/Line Shapefiles [website](https://www.census.gov/cgi-bin/geo/shapefiles/index.php). R users may also use the `tigris` package. This shapefile includes only counties within the spatial domain over which smoke PM2.5 predictions are made.

* `county/smokePM2pt5_predictions_daily_county_20060101-20201231.rds`: this is a file that contains a data frame with the final set of daily smoke PM2.5 predictions on smoke days at the county level from January 1, 2006 to December 31, 2020 for the contiguous US. County-level smoke PM2.5 predictions are aggregated from smoke PM2.5 predictions at the 10 km resolution using population and area of intersection-weighted averaging (see `scripts/main/02_06_gridded_predictions_to_county.R`). The `GEOID` column in this file corresponds to the `GEOID` column in the county shapefile. All rows in this file are predictions on smoke days. Predictions on non-smoke days are by construction 0 $\mu g/m^3$ and not included in this file. A smoke PM2.5 prediction of 0 in this file means that the county-day did have a smoke day but did not have elevated PM2.5. The full set of smoke PM2.5 predictions on both smoke days and non-smoke days can be obtained by setting the smoke PM2.5 prediction to 0 on county-days in the counties and in the January 1, 2006-December 31, 2020 date range that are not in this file. For example, the R code below returns the full set of smoke PM2.5 predictions:

```
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)

# Load smokePM predictions on smoke days
preds = readRDS("./final/county/smokePM2pt5_predictions_daily_county_20060101-20201231.rds")

# Load counties
counties = read_sf("./final/county/tl_2019_us_county")

# Load full set of dates
dates = seq.Date(ymd("20060101"), ymd("20201231"), by = "day")

# Get full combination of county-days
# Warning: this may require a large amount of memory
out = expand.grid(GEOID = counties$GEOID, date = dates)

# Match smokePM predictions on smoke days to county-days
out = left_join(out, preds, by = c("GEOID", "date"))

# Predict 0 for remaining county-days, which are non-smoke days
out = mutate(out, smokePM_pred = replace_na(smokePM_pred, 0))
```
* `county/smokePM2pt5_predictions_daily_county_20060101-20201231.csv`: this is the same as `smokePM2pt5_predictions_daily_county_20060101-20201231.rds`, except it is saved as a CSV file.

ZIP Code Tabulation Area (ZCTA5)
* `zcta/tl_2019_us_zcta510/`: ths is a folder that contains the shapefile for CONUS zip code tabulation areas in 2019. Files were downloaded from the US Census Bureau TIGER/Line Shapefiles [website](https://www.census.gov/cgi-bin/geo/shapefiles/index.php). R users may also use the `tigris` package. This shapefile includes only ZCTAs within the spatial domain over which smoke PM2.5 predictions are made.

* `zcta/smokePM2pt5_predictions_daily_zcta_20060101-20201231.rds`: this is a file that contains a data frame with the final set of daily smoke PM2.5 predictions on smoke days at the ZCTA5 level from January 1, 2006 to December 31, 2020 for the contiguous US. ZCTA-level smoke PM2.5 predictions are aggregated from smoke PM2.5 predictions at the 10 km resolution using population and area of intersection-weighted averaging (see `scripts/main/02_07_gridded_predictions_to_zip.R`).The `GEOID10` column in this file corresponds to the `GEOID10` column in the ZCTA shapefile. All rows in this file are predictions on smoke days. Predictions on non-smoke days are by construction 0 $\mu g/m^3$ and not included in this file. A smoke PM2.5 prediction of 0 in this file means that the ZCTA-day did have a smoke day but did not have elevated PM2.5. The full set of smoke PM2.5 predictions on both smoke days and non-smoke days can be obtained by setting the smoke PM2.5 prediction to 0 on ZCTA-days in the ZCTAs and in the January 1, 2006-December 31, 2020 date range that are not in this file. For example, the R code below returns the full set of smoke PM2.5 predictions:

```
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)

# Load smokePM predictions on smoke days
preds = readRDS("./final/zcta/smokePM2pt5_predictions_daily_zcta_20060101-20201231.rds")

# Load ZCTAs
zctas = read_sf("./final/zcta/tl_2019_us_zcta510")

# Load full set of dates
dates = seq.Date(ymd("20060101"), ymd("20201231"), by = "day")

# Get full combination of ZCTA-days
# Warning: this may require a large amount of memory
out = expand.grid(GEOID10 = zctas$GEOID10, date = dates)

# Match smokePM predictions on smoke days to ZCTA-days
out = left_join(out, preds, by = c("GEOID10", "date"))

# Predict 0 for remaining ZCTA-days, which are non-smoke days
out = mutate(out, smokePM_pred = replace_na(smokePM_pred, 0))
```

* `zcta/smokePM2pt5_predictions_daily_zcta_20060101-20201231.csv`: this is the same as `smokePM2pt5_predictions_daily_zcta_20060101-20201231.rds`, except it is saved as a CSV file.

Census tract
* `tract/tracts/`: this is a folder that contains the shapefiles for CONUS census tracts by state/territory in 2019. Files were downloaded from the US Census Bureau TIGER/Line Shapefiles [website](https://www.census.gov/cgi-bin/geo/shapefiles/index.php). R users may also use the `tigris` package. This shapefile includes only tracts within the spatial domain over which smoke PM2.5 predictions are made.

* `tract/smokePM2pt5_predictions_daily_tract_20060101-20201231.rds`: this is a file that contains a data frame with the final set of daily smoke PM2.5 predictions on smoke days at the tract level from January 1, 2006 to December 31, 2020 for the contiguous US. Tract-level smoke PM2.5 predictions are aggregated from smoke PM2.5 predictions at the 10 km resolution using population and area of intersection-weighted averaging (see `scripts/main/02_08_gridded_predictions_to_tract.R`). The `GEOID` column in this file corresponds to the `GEOID` column in the tract shapefiles. All rows in this file are predictions on smoke days. Predictions on non-smoke days are by construction 0 $\mu g/m^3$ and not included in this file. A smoke PM2.5 prediction of 0 in this file means that the tract-day did have a smoke day but did not have elevated PM2.5. The full set of smoke PM2.5 predictions on both smoke days and non-smoke days can be obtained by setting the smoke PM2.5 prediction to 0 on tract-days in the tracts and in the January 1, 2006-December 31, 2020 date range that are not in this file. For example, the R code below returns the full set of smoke PM2.5 predictions:

```
library(lubridate)
library(sf)
library(dplyr)
library(tidyr)

# Load smokePM predictions on smoke days
preds = readRDS("./final/tract/smokePM2pt5_predictions_daily_tract_20060101-20201231.rds")

# Load tracts
tracts = list.files("./final/tract/tracts", full.names = T, pattern = "\\.shp$")
tracts = lapply(tracts, read_sf)
tracts = bind_rows(tracts)

# Load full set of dates
dates = seq.Date(ymd("20060101"), ymd("20201231"), by = "day")

# Get full combination of tract-days
# Warning: this may require a large amount of memory
out = expand.grid(GEOID = tracts$GEOID, date = dates)

# Match smokePM predictions on smoke days to tract-days
out = left_join(out, preds, by = c("GEOID", "date"))

# Predict 0 for remaining tract-days, which are non-smoke days
out = mutate(out, smokePM_pred = replace_na(smokePM_pred, 0))
```

* `tract/smokePM2pt5_predictions_daily_tract_20060101-20201231.csv`: this is the same as `smokePM2pt5_predictions_daily_tract_20060101-20201231.rds`, except it is saved as a CSV file.

## How to replicate results
1. Download this repository.
2. Download the [Dropbox](https://www.dropbox.com/sh/e7m3313fb7sqxui/AABGu-jUO3Ps2isGHbB2EGfAa?dl=0) folder. Place files downloaded from Dropbox in the same folder as the downloaded GitHub repository.
3. Change settings in `scripts/setup/00_03_load_settings.R`:
    1. Set `gee_email` to your Google Earth Engine email.
    2. Set `key` to the value of your US Census Bureau API Key (which can be requested [here](https://api.census.gov/data/key_signup.html)).
    3. Set `num_cores` to the number of cores to use in parallel computing.
    4. Set `path_dropbox` to the location of the data downloaded from Dropbox.
    5. Set `path_github` to the location of this downloaded repository's root.
4. Install packages listed in `scripts/setup/00_01_load_packages.R`.
5. Set working directory to this downloaded repository's root.
6. Run scripts in `scripts/main`. Some scripts may require relatively large computer memory.
