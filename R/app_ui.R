#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinythemes
#' @import shinyWidgets
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyFiles
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    # ui = tags$body(class="skin-blue sidebar-mini control-sidebar-open",
    
    shinydashboardPlus::dashboardPage(
      header = dashboardHeader(
        fixed = TRUE,
        title = tagList(
          span(class = "logo-lg", "Shiny CNES"),
          img(src = "www/cnes.svg")
        )
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("Project selection", tabName = "project", icon = icon("gears")),
          menuItem("Product selection", tabName = "product", icon = icon("gears")),
          menuItem("Processing selection", tabName = "opt_processing", icon = icon("gears")),
          menuItem("Spectral indices selection", tabName = "index", icon = icon("calculator")),
          menuItem("RGB images selection", tabName = "rgb", icon = icon("calculator")),
          menuItem("TILES Processing", tabName = "tiles_processing", icon = icon("refresh")),
          menuItem("SRTM Processing", tabName = "srtm_processing", icon = icon("refresh")),
          menuItem("WorldClim Processing", tabName = "worldclim_processing", icon = icon("refresh")),
          menuItem("Prevision result", tabName = "launch_prevision", icon = icon("calendar"))
        )
      ),
      body = dashboardBody(
        tabItems(
          # First tab content
          tabItem(
            tabName = "project",
            mod_project_ui("product"),
            mod_temporal_map_ui("product")
          ),
          # tab 2
          tabItem(
            tabName = "product",
            mod_product_type_ui("product"),
            mod_collection_type_ui("product"),
            mod_platform_type_ui("product"),
            mod_level_ui("product")
          ),
          # tab 3
          tabItem(
            tabName = "opt_processing",
            mod_save_options_ui("product"),
            mod_temporal_parameters_ui("product"),
            mod_cloud_mask_ui("product"),
            mod_processing_options_ui("product")
          ),
          # tab 4
          tabItem(
            tabName = "index",
            mod_spectral_indice_ui("product")
          ),
          # tab 5
          tabItem(
            tabName = "rgb",
            mod_rgb_image_ui("product")
          ),
          # tab 6
          tabItem(
            tabName = "tiles_processing",
            mod_tiles_processing_ui("product")
          ),
          # tab 7
          tabItem(
            tabName = "srtm_processing",
            mod_srtm_processing_ui("product")
          ),
          # tab 8
          tabItem(
            tabName = "worldclim_processing",
            mod_worldclim_processing_ui("product")
          ),
          # tab 9
          tabItem(
            tabName = "launch_prevision",
            mod_launch_prevision_ui("launch_prevision")
          )
        )
      ),
      controlbar = dashboardControlbar(
        skin = "dark",
        collapsed = FALSE,
        controlbarMenu(
          id = "right_menu",
          controlbarItem(
            "Parameters",
            mod_param_ui("product")
          )
        )
      ),
      footer = dashboardFooter(
        left = "By Pascal Obstetar",
        right = "Dijon, 2020"
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinyCNES'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

