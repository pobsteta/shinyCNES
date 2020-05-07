#' temporal_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stringr str_sub
mod_temporal_parameters_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "query",
    h3(i18n$t("Calc extent")),
    # Boxes need to be put in a row (or column)
    # first box Temporal parameters
    fluidRow(
      box(
        title = i18n$t("Temporal parameters"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        conditionalPanel(
          condition = "input.online == 'FALSE'",
          radioButtons("query_time",
                       label = i18n$t("Use temporal filter?"),
                       choiceNames = list(
                         i18n$t("Yes"),
                         i18n$t("No (process all the input THEIA images)")
                       ),
                       choiceValues = list(TRUE, FALSE),
                       selected = TRUE,
                       inline = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.query_time == 'TRUE'",
          column(
            width = 6,
            dateRangeInput("timewindow", label = i18n$t("Time interval"), language = stringr::str_sub(Sys.getlocale("LC_TIME"), 1, 2))
          ),
          column(
            width = 6,
            radioButtons(
              "timeperiod",
              label = span(
                i18n$t("Time period type\u2000"),
                actionLink("help_time_period", icon("question-circle"))
              ),
              choiceNames = list(
                span(
                  i18n$t("Full")
                ),
                span(
                  i18n$t("Seasonal")
                )
              ),
              choiceValues = list("full", "seasonal"),
              selected = "full",
              inline = TRUE
            ) # end radiobutton
          ) # end of column
        ) # end of conditionalpanel
      ) # end of box
    ) # end of fluidRow/box "Temporal parameters"
  ) # end taglist
}
    
#' temporal_parameters Server Function
#'
#' @noRd 
mod_temporal_parameters_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_temporal_parameters_ui("temporal_parameters_ui_1")
    
## To be copied in the server
# callModule(mod_temporal_parameters_server, "temporal_parameters_ui_1")
 
