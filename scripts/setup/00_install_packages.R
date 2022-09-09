
# devtools::install_github("zeehio/facetscales")
renv::install("zeehio/facetscales")




renv::install("reticulate")
# py_env_name = "daily-10km-smokePM"
# py_env_exists = py_env_name %in% reticulate::conda_list()$name
# if (!py_env_exists) {
#   reticulate::conda_create(py_env_name)
# }
# reticulate::use_condaenv(py_env_name, required = T)

renv::install("geojsonio")
renv::install("rgee")
rgee::ee_install()
# reticulate::virtualenv_install(envname = "rgee", packages = "geojsonio")
rgee::ee_check()
# Choose yes to storing the environment variables EARTHENGINE_PYTHON and EARTHENGINE_ENV in your .Renviron file
# Choose 1 to restart your R session
# reticulate::virtualenv_install(envname = "rgee", packages = c("numpy", "earthengine-api==0.1.262", "geojsonio"))
