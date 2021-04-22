#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # List the first level callModules here

  # create reactive values
  r <- reactiveValues(
    project_name = NULL,
    path_project = NULL,
    # extract and import tiles kml
    s2tiles = s2_tiles(),
    s2orbits = s2_orbits()
  )

  # tab1
  param <- callModule(mod_param_server, "product", rv = r)
  callModule(mod_temporal_map_server, "product", rv = r)
  callModule(mod_project_server, "product", rv = r)
  # tab 2
  callModule(mod_product_type_server, "product", rv = r)
  callModule(mod_collection_type_server, "product", rv = r)
  callModule(mod_platform_type_server, "product", rv = r)
  callModule(mod_level_server, "product", rv = r)
  # tab3
  callModule(mod_save_options_server, "product", rv = r)
  callModule(mod_temporal_parameters_server, "product", rv = r)
  callModule(mod_cloud_mask_server, "product", rv = r)
  callModule(mod_processing_options_server, "product", rv = r)
  # tab4
  callModule(mod_spectral_indice_server, "product", rv = r)
  # tab 5
  callModule(mod_rgb_image_server, "product", rv = r)
  # tab 6
  callModule(mod_tiles_processing_server, "product", rv = r)
  # tab 7
  callModule(mod_srtm_processing_server, "product", rv = r)
  # tab 8
  callModule(mod_worldclim_processing_server, "product", rv = r)
  # tab 9
  callModule(mod_launch_prevision_server, "launch_prevision", rv = r)
}
