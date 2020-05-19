#' product_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_product_type_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "product",
    h3(i18n$t("Product options")),
    fluidRow(
      box(
        title = i18n$t("Type of products"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        radioButtons(
          ns("product"), NULL,
          choiceNames = list(
            span(
              i18n$t("THEIA product "),
              a("Pleiades, Spots, Sentinelle, Venus",
                href = "http://www.theia-land.fr/fr",
                target = "_blank"
              ),
              i18n$t(" (download)")
            ),
            span(
              i18n$t("PEPS product "),
              a("Sentinelle (S1, S2, S2ST, S3)",
                href = "https://peps.cnes.fr/rocket/#/home",
                target = "_blank"
              ),
              i18n$t(" (download)")
            )
          ),
          choiceValues = list("theia", "peps"),
          selected = "theia",
          inline = FALSE
        ) # end radiobutton
      ) # end box
    ) # end fluidrow
  ) # end taglist
}
    
#' product_type Server Function
#'
#' @noRd 
mod_product_type_server <- function(input, output, session, rv){
  ns <- session$ns
  
  # for save parameters
  observe({
    rv$product <- input$product
  })

}
    
## To be copied in the UI
# mod_product_type_ui("product_type_ui_1")
    
## To be copied in the server
# callModule(mod_product_type_server, "product_type_ui_1")
 
