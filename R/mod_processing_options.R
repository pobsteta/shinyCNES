#' processing_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_processing_options_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    fluidRow(
      box(
        title = i18n$t("Processing options"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        column(
          width = 4,
          selectInput(
            "processing_order",
            span(
              "Processing order\u2000",
              actionLink("help_processing_order", icon("question-circle"))
            ),
            choices = list(
              "Process by groups" = "by_groups",
              "Process by date" = "by_date",
              "Mixed processing" = "mixed",
              "Process step by step" = "by_step"
            ),
            selected = "by_groups",
            width = "200%"
          ),
        ),
        column(
          width = 4,
          radioButtons(
            "parallel",
            label = span("Parallel computation?"),
            choiceNames = list("Yes", "No"),
            choiceValues = list(TRUE, FALSE),
            selected = TRUE,
            inline = TRUE
          )
        ),
        column(
          width = 4,
          conditionalPanel(
            condition = "input.parallel == 'TRUE'",
            div(
              sliderInput(
                "n_cores", 
                label = "Number of CPU cores",
                min = 1, 
                max = parallel::detectCores(),
                value = parallel::detectCores()-1,
                step = 1,
                ticks = FALSE
              ) # end slider
            ) # end div
          ) # end conditionalpanel
        ) # end column
      ) # end box
    ) # end of fluidRow
  ) # end taglist
}
    
#' processing_options Server Function
#'
#' @noRd 
mod_processing_options_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_processing_options_ui("processing_options_ui_1")
    
## To be copied in the server
# callModule(mod_processing_options_server, "processing_options_ui_1")
 
