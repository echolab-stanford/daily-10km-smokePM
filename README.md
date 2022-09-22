# daily-10km-smokePM
Repo supporting [Childs et al 2022 "Daily local-level estimates of ambient wildfire smoke PM<sub>2.5</sub> for the contiguous US"](https://doi.org/10.1021/acs.est.2c02934).

Results from the paper are in the `figures/clean` and `tables/clean` folders. Code to replicate results are in the `scripts` folder. Data, models, and predictions are in [Dropbox](https://www.dropbox.com/sh/cvl54kv4bsryme0/AAD8z2j-wcqZ_S7qc1jIcF7Na?dl=0).

## Final predictions
Daily smoke PM<sub>2.5</sub> predictions from Jan 1, 2006 to Dec 31, 2020 for the contiguous US can be [downloaded](https://www.dropbox.com/sh/wh45f4uf7gpb3ct/AAAycHq02lp1KfqxDed0SKxFa?dl=0) at the following spatial scales:
* [10 km grid](https://www.dropbox.com/sh/7vclx9rw0e7i0hq/AAD7GCLPbgGJZx0iQfwSWcxLa?dl=0)
* [County](https://www.dropbox.com/sh/hrd009uk34k7d6n/AADeUtrYp1iQWdAPwX5eKI-2a?dl=0)
* [Census tract ](https://www.dropbox.com/sh/atmtfc54zuknnob/AAA7AVRQP-GoIMHpxlvfN7RBa?dl=0)

## How to replicate results
1. Download this repository.
2. Download data from [Dropbox](https://www.dropbox.com/sh/cvl54kv4bsryme0/AAD8z2j-wcqZ_S7qc1jIcF7Na?dl=0). Place downloaded Dropbox data in the same folder as the downloaded GitHub repository.
3. Change settings in `scripts/setup/00_03_load_settings.R`:
    1. Set `gee_email` to your Google Earth Engine email.
    2. Set `key` to the value of your US Census Bureau API Key (which can be requested [here](https://api.census.gov/data/key_signup.html)).
    3. Set `num_cores` to the number of cores to use in parallel computing.
    4. Set `path_dropbox` to the location of the data downloaded from Dropbox.
    5. Set `path_github` to the location of this downloaded repository's root.
4. Install packages listed in `scripts/setup/00_01_load_packages.R`.
5. Set working directory to this downloaded repository's root.
6. Run scripts in `scripts/main`. Some scripts may require relatively large computer memory.
