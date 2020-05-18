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
    
    dashboardPagePlus(
      header = dashboardHeaderPlus(
        fixed = TRUE,
        title = tagList(
          span(class = "logo-lg", "Shiny CNES"),
          img(src = "www/cnes.svg")
        ),
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "bars"
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("Project selection", tabName = "project", icon = icon("gears")),
          menuItem("Product selection", tabName = "product", icon = icon("gears")),
          menuItem("Processing selection", tabName = "opt_processing", icon = icon("gears")),
          menuItem("Spectral indices selection", tabName = "index", icon = icon("calculator")),
          menuItem("RGB images selection", tabName = "rgb", icon = icon("calculator")),
          menuItem("Processing result", tabName = "launch_processing", icon = icon("refresh")),
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
            mod_rgb_image_ui("rgb")
          ),
          # tab 6
          tabItem(
            tabName = "launch_processing",
            mod_launch_processing_ui("launch_processing")
          ),
          # tab 7
          tabItem(
            tabName = "launch_prevision",
            mod_launch_prevision_ui("launch_prevision")
          )
        )
      ),
      rightsidebar = rightSidebar(
        background = "dark",
        rightSidebarTabContent(
          id = 1,
          icon = "map-marked-alt",
          active = TRUE,
          mod_param_ui("product")
        )
      ),
      footer = dashboardFooter(
        left_text = "By Pascal Obstetar",
        right_text = "Dijon, 2020"
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

