#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  # create reative values
  r <- reactiveValues(
    project_name = NULL,
    path_project = NULL
  )
  
  callModule(mod_project_server, "product", rv = r)
  # callModule(mod_cloud_mask_server, "product", rv = r)
  # callModule(mod_level_server, "product", rv = r)
  # callModule(mod_platform_type_server, "product", rv = r)
  # callModule(mod_product_type_server, "product", rv = r)
  # callModule(mod_save_options_server, "product", rv = r)
  # callModule(mod_param_server, "param", rv = r)
  # callModule(mod_temporal_parameters_server, "query", rv = r)
  # callModule(mod_temporal_map_server, "query", rv = r)
  # callModule(mod_spectral_indice_server, "index", rv = r)
  # callModule(mod_processing_options_server, "process", rv = r)
  # callModule(mod_rgb_image_server, "rgb", rv = r)
  # callModule(mod_launch_processing_server, "launch_processing", rv = r)
  # callModule(mod_launch_prevision_server, "launch_prevision", rv = r)

}
