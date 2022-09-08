#-------------------------------------------------------------------------------
# Set the name of the conda environment
py_env_name = "daily-10km-smokePM"
reticulate::conda_create(py_env_name)
reticulate::use_condaenv(py_env_name, required = T)
ee_install()

# Provide your Google Earth Engine email
gee_email = readRDS("~/Documents/Registrations/Google Earth Engine/GEE_email.rds") # "INSERT YOUR GEE EMAIL HERE, e.g. jdoe@stanford.edu"

# Set the number of cores to use in parallel computing
num_cores = 1 # default sequential

# Provide your US Census API Key
key <- readRDS("~/Documents/Registrations/US Census/us_census_data_api_key.rds") # "INSERT YOUR US CENSUS BUREAU API KEY HERE"
census_api_key(key)

#-------------------------------------------------------------------------------
# Set to location of Dropbox and GitHub folders
path_dropbox = "~/BurkeLab Dropbox/Projects/daily-10km-smokePM-testing/" # "INSERT PATH TO DROPBOX FOLDER HERE"
path_github = "~/Documents/GitHub/daily-10km-smokePM/" # "INSERT PATH TO GITHUB REPO HERE"

# File paths based on root folders above
path_tables = file.path(path_github, "tables", "raw")
path_figures = file.path(path_github, "figures", "raw")
path_setup = file.path(path_github, "scripts", "setup")
path_main = file.path(path_github, "scripts", "main")
path_supplementary = file.path(path_github, "scripts", "supplementary")

#-------------------------------------------------------------------------------
nonContig_stateFIPS <- c("02","60","66","15","72","78","69")
