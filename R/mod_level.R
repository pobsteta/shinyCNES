#' level UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_level_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    fluidRow(
      #### SENTINEL2 ####
      conditionalPanel(
        condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'sentinel2'", ns("product"), ns("theiacollection")),
        box(
          title = i18n$t("Level"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          radioButtons(
            ns("theiaplatformsentinellevel"), NULL,
            choiceNames = list(
              span(
                a("LEVEL1C",
                  href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                  target = "_blank"
                )
              ),
              span(
                a("LEVEL2A",
                  href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                  target = "_blank"
                )
              ),
              span(
                a("LEVEL3A",
                  href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                  target = "_blank"
                )
              )
            ),
            choiceValues = list("l1c", "l2a", "l3a"),
            selected = "l2a",
            inline = FALSE
          ) # end radiobutton
        ) # end box
      ) # end conditionnalPanel
    ) # end fluidrow
  ) # end taglist
}
    
#' level Server Function
#'
#' @noRd 
mod_level_server <- function(input, output, session, rv){
  ns <- session$ns
  
  # observe({
  #   rv$theiaplatformsentinellevel <- input$theiaplatformsentinellevel
  # })
 
}
    
## To be copied in the UI
# mod_level_ui("level_ui_1")
    
## To be copied in the server
# callModule(mod_level_server, "level_ui_1")
 
