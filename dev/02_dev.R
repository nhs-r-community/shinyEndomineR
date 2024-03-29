# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

stop() # in case I run this like an idiot

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package("ggTimeSeries")
usethis::use_package("data.table")
usethis::use_package("ggTimeSeries")
usethis::use_package("esquisse")
usethis::use_package("rpivotTable")
usethis::use_package("data.table")
usethis::use_package("purrr")
usethis::use_dev_package("EndoMineR", remote = "sebastiz/EndoMineR")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "clean_and_merge") # Name of the module
golem::add_module( name = "merge_data" ) # Name of the module
golem::add_module( name = "map_terms" ) # Name of the module
golem::add_module( name = "barretts" ) # Name of the module
golem::add_module( name = "polyps" ) # Name of the module
golem::add_module( name = "per_endoscopist" ) # Name of the module
golem::add_module( name = "custom" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test("load_data")

# Documentation

## Vignette ----
usethis::use_vignette("shinyEndomineR")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

