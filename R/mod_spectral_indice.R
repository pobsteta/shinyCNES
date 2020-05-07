#' spectral_indice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spectral_indice_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "index",
    h3(i18n$t("Select spectral indices")),
    conditionalPanel(
      condition = "input.product == 'theia' | input.product == 'peps'",
      fluidRow(
        box(
          title = i18n$t("Spectral indices selection"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          fluidRow(
            column(
              width = 6,
              radioButtons(
                "index_source",
                label = span(
                  i18n$t("Build indices from:\u2000"),
                  actionLink("help_index_source", icon("question-circle"))
                ),
                choices = list(
                  "BOA" = "BOA",
                  "TOA" = "TOA"
                ),
                selected = "BOA",
                inline = TRUE
              )
            ), # end column
            column(
              width = 6,
              selectInput(
                "index_datatype",
                label = i18n$t("Data type"),
                choices = list(
                  "Byte" = "Byte",
                  "Integer (16 bits)" = "Int16",
                  "Float (32 bits)" = "Float32",
                  "Float (64 bits)" = "Float64"
                ),
                selected = "Int16"
              )
            ) # end column
          ), # end fluidrow
          hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),
          fluidRow(
            column(
              width = 5,
              textInput("filter_indices", i18n$t("Filter indices"))
            ),
            column(
              width = 7,
              checkboxInput(
                "verified_indices",
                label = span(
                  i18n$t("Show only verified indices\u2000"),
                  actionLink("note_list_indices", icon("warning"))
                ),
                value = TRUE
              )
            ) # end column
          ), # end fluidrow
          uiOutput("check_indices")
        ), # end box
        uiOutput("show_formula")
      ) # end fluidrow
    ) # end of conditionalpanel on tab_index
  ) # end taglist
}
    
#' spectral_indice Server Function
#'
#' @noRd 
mod_spectral_indice_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_spectral_indice_ui("spectral_indice_ui_1")
    
## To be copied in the server
# callModule(mod_spectral_indice_server, "spectral_indice_ui_1")
 
