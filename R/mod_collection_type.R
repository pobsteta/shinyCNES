#' collection_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_collection_type_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    fluidRow(
      box(
        title = i18n$t("Type of collection"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        ### theia ###
        conditionalPanel(
          condition = sprintf("input['%s'] == 'theia'", ns("product")),
          radioButtons(
            ns("theiacollection"), NULL,
            choiceNames = list(
              span(
                a("Landsat",
                  href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=3487",
                  target = "_blank"
                )
              ),
              span(
                a("SpotWorldHeritage",
                  href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=12923",
                  target = "_blank"
                )
              ),
              span(
                a("SENTINEL2",
                  href = "https://theia.cnes.fr/atdistrib/documents/PSC-NT-411-0362-CNES_01_00_SENTINEL-2A_L2A_Products_Description.pdf",
                  target = "_blank"
                )
              ),
              span(
                a("Snow",
                  href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=10748#fr",
                  target = "_blank"
                )
              ),
              span(
                a("VENUS",
                  href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=12984",
                  target = "_blank"
                )
              )
            ),
            choiceValues = list("landsat", "spotworldheritage", "sentinel2", "snow", "venus"),
            selected = "sentinel2",
            inline = FALSE
          ) # end radiobutton
        ), # end conditionalpanel
        
        ### peps ###
        conditionalPanel(
          condition = sprintf("input['%s'] == 'peps'", ns("product")),
          # condition = "input.product == 'peps'",
          radioButtons(
            ns("pepscollection"), NULL,
            choiceNames = list(
              span(
                a("S1",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                  target = "_blank"
                )
              ),
              span(
                a("S2",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              ),
              span(
                a("S2ST",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              ),
              span(
                a("S3",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel3",
                  target = "_blank"
                )
              )
            ),
            choiceValues = list("s1", "s2", "s2st", "s3"),
            selected = "s2",
            inline = FALSE
          ) # end radiobutton
        ) # end conditionalpanel
      ) # end box
    ) # end fluidrow
  ) # end taglist
}
    
#' collection_type Server Function
#'
#' @noRd 
mod_collection_type_server <- function(input, output, session, rv){
  ns <- session$ns
  
  # observe({
  #   rv$theiacollection <- input$theiacollection
  #   rv$pepscollection <- input$pepscollection
  # })
 
}
    
## To be copied in the UI
# mod_collection_type_ui("collection_type_ui_1")
    
## To be copied in the server
# callModule(mod_collection_type_server, "collection_type_ui_1")
 
