#' rgb_image UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rgb_image_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "rgb",
    h3(i18n$t("RGB images selection")),
    fluidRow(
      box(
        title = i18n$t("RGB images"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        uiOutput(ns("checkbox_list_rgb")),
        div(
          style = "display:inline-block;padding-top:10px;padding-right:10px;",
          actionButton(
            ns("new_rgb"),
            label = "\u2000Define custom RGB image",
            icon = icon("plus")
          )
        ),
        div(
          style = "display:inline-block;padding-top:10px;",
          actionButton(
            ns("rm_rgb"),
            label = "\u2000Remove unselected RGB from list",
            icon = icon("trash-alt")
          )
        ) # end div
      ) # end box
    ) # end fluidrow
  ) # end taglist
}
    
#' rgb_image Server Function
#'
#' @noRd 
mod_rgb_image_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_rgb_image_ui("rgb_image_ui_1")
    
## To be copied in the server
# callModule(mod_rgb_image_server, "rgb_image_ui_1")
 
