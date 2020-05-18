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
#' 
mod_temporal_parameters_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    # first box Temporal parameters
    fluidRow(
      box(
        title = i18n$t("Temporal parameters"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        conditionalPanel(
          condition = sprintf("input['%s'] == 'FALSE'", ns("online")),
          radioButtons(ns("query_time"),
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
          condition = sprintf("input['%s'] == 'TRUE'", ns("query_time")),
          column(
            width = 6,
            dateRangeInput(ns("timewindow"),
                           label = i18n$t("Time interval"),
                           language = stringr::str_sub(Sys.getlocale("LC_TIME"), 1, 2))
          ),
          column(
            width = 6,
            radioButtons(
              ns("timeperiod"),
              label = span(
                i18n$t("Time period type\u2000"),
                actionLink(ns("help_time_period"), icon("question-circle"))
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
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  observeEvent(input$help_time_period, {
    showModal(modalDialog(
      title = i18n$t("Time period type"),
      p(HTML(
        i18n$t("<strong>Full</strong>:"),
        i18n$t("the specified time window is entirely processed"),
        i18n$t("(e.g., specifying a range from 2016-05-01 to 2018-09-30 will return"),
        i18n$t("all the products in this time window which match the other parameters).")
      )),
      p(HTML(
        i18n$t("<strong>Seasonal</strong>:"),
        i18n$t("the specified time window is processed from the first year to the"),
        i18n$t("last year, in the seasonal time windows from the first"),
        i18n$t("Julian day to the second Julian day"),
        i18n$t("(e.g., specifying a range from 2016-05-01 to 2018-09-30 will return"),
        i18n$t("all the products from 2016-05-01 to 2016-09-30, from 2017-05-01 to"),
        i18n$t("2017-09-30 and from 2018-05-01 to 2018-09-30,"),
        i18n$t("which also match the other parameters).")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
 
}
    
## To be copied in the UI
# mod_temporal_parameters_ui("temporal_parameters_ui_1")
    
## To be copied in the server
# callModule(mod_temporal_parameters_server, "temporal_parameters_ui_1")
 
