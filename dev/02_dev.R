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

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shiny" ) # To call each time you need a new package
usethis::use_package( "shiny.i18n" )
usethis::use_package( "shinyFiles" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinydashboardPlus" )
usethis::use_package( "shinythemes" )
usethis::use_package( "shinyjs" )
usethis::use_package( "config" )
usethis::use_package( "golem" )
usethis::use_package( "DT" )
usethis::use_package( "RANN" )
usethis::use_package( "cowplot" )
usethis::use_package( "geometry" )
usethis::use_package( "ggplot2" )
usethis::use_package( "ggthemes" )
usethis::use_package( "jsonlite" )
usethis::use_package( "lidR" )
usethis::use_package( "magrittr" )
usethis::use_package( "methods" )
usethis::use_package( "plotly" )
usethis::use_package( "raster" )
usethis::use_package( "rgl" )
usethis::use_package( "scales" )
usethis::use_package( "sf" )
usethis::use_package( "sp" )
usethis::use_package( "stringr" )
usethis::use_package( "processx" )
usethis::use_package( "attempt" )
usethis::use_package( "glue" )
usethis::use_package( "htmltools" )
usethis::use_package( "leaflet" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "project" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module

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
usethis::use_data_raw( name = "datatest", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("shinyCNES")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

