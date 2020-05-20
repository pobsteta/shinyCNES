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
            ns("processing_order"),
            span(
              i18n$t("Processing order\u2000"),
              actionLink(ns("help_processing_order"), icon("question-circle"))
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
            ns("parallel"),
            label = span(i18n$t("Parallel computation?")),
            choiceNames = list("Yes", "No"),
            choiceValues = list(TRUE, FALSE),
            selected = TRUE,
            inline = TRUE
          )
        ),
        column(
          width = 4,
          conditionalPanel(
            condition = sprintf("input['%s'] == 'TRUE'", ns("parallel")),
            div(
              sliderInput(
                ns("n_cores"), 
                label = i18n$t("Number of CPU cores"),
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
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  # for save parameters
  observe({
    rv$processing_order <- input$processing_order
    rv$parallel <- input$parallel
    rv$n_cores <- input$n_cores
  })
  
  observeEvent(input$help_processing_order, {
    showModal(modalDialog(
      title = i18n$t("Processing order"),
      p(HTML(
        i18n$t("The order used to execute the processing chain affects the speed"),
        i18n$t("of computation and the usage of system resources."),
        i18n$t("Changing this setting can be useful to optimise system performance,"),
        i18n$t("particularly if the user is processing a high amount of data"),
        i18n$t("(a large area of interest and/or an extensive time window).")
      )),
      p(HTML(
        i18n$t("The four available orders are described below.")
      )),
      p(HTML(
        i18n$t("<strong>Process by groups</strong> (default):"),
        i18n$t("it provides a good compromise between processing speed and disk usage."),
        i18n$t("Processing is done as follows:<ul>"),
        i18n$t("<li>the list of required archives and output product names is computed;</li>"),
        i18n$t("<li>the required dates are grouped in <em>g</em> groups, where"),
        i18n$t("<em>g</em> is the number of dates divided by the number of CPU;</li>"),
        i18n$t("<li>groups are then processed sequentially; for each group:<ul>"),
        i18n$t("<li>the required archives are downloaded;</li>"),
        i18n$t("<li>Calculate is applied in parallel using one core per archive;</li>"),
        i18n$t("<li>the remaining processing operations are executed using parallel"),
        i18n$t("(one core for each date).</li></ul></ul>")
      )),
      p(HTML(
        i18n$t("<strong>Process by date</strong>:"),
        i18n$t("this allows minimising the requirements of disk usage"),
        i18n$t("(in particular if archives are deleted after processing)."),
        i18n$t("It is similar to the default execution, but each group is composed"),
        i18n$t("by a single date: so the disk space occupied by archives"),
        i18n$t("and temporary files is lower,"),
        i18n$t("but it is generally slower than the default one because"),
        i18n$t("parallel computation over dates for products generation is not possible.")
      )),
      p(HTML(
        i18n$t("<strong>Mixed processing</strong>:"),
        i18n$t("this allows maximising CPU usage and processing speed."),
        i18n$t("The cycle on groups is ignored, and all the required archives are"),
        i18n$t("first of all downloaded and/or produced, and then dates are"),
        i18n$t("processed in parallel."),
        i18n$t("This mode is faster than the default mode, but it requires"),
        i18n$t("all archives to be downloaded and processed before performing"),
        i18n$t("subsequent steps, thus increasing disk space requirements.")
      )),
      p(HTML(
        i18n$t("<strong>Process step by step</strong>:"),
        i18n$t("this is the legacy mode, in which the cycle on groups is ignored"),
        i18n$t("as well as the parallel computation over dates."),
        i18n$t("All archives are first downloaded/processed,"),
        i18n$t("then the processing steps are performed sequentially."),
        i18n$t("This mode is similar to the previous one in terms of disk usage"),
        i18n$t("but it is slightly slower; its advantage are the lower RAM requirements.")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
 
}
    
## To be copied in the UI
# mod_processing_options_ui("processing_options_ui_1")
    
## To be copied in the server
# callModule(mod_processing_options_server, "processing_options_ui_1")
 
