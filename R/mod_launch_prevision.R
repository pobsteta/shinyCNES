#' launch_prevision UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_launch_prevision_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "launch_prevision",
    h3(i18n$t("Prevision result")),
    fluidRow(
      box(
        title = i18n$t("Prevision map"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        # proejct name and button
        column(
          width = 4,
          # Buttons to load the extent with modal dialogs
          span(
            div(
              style = "display:inline-block;vertical-align:top;",
              strong(i18n$t("Name of project: \u00a0"))
            ),
            div(
              style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
              verbatimTextOutput("project_name_verbatim")
            )
          ) # end of span
        ), # end of column
        column(
          width = 4,
          div(
            style = "display:inline-block;vertical-align:top;",
            strong(i18n$t("Directory for model: \u00a0"))
          ),
          div(
            style = "display:inline-block;vertical-align:top;",
            htmlOutput("path_model_errormess")
          ),
          div(
            div(
              style = "display:inline-block;vertical-align:top;width:50pt;",
              shinyDirButton("path_model_sel", "Select", i18n$t("Specify directory for the model"))
            ),
            div(
              style = "display:inline-block;vertical-align:top;width:calc(99% - 50pt - 3px);",
              textInput("path_model_textin", NULL, "")
            )
          )
        ),
        column(
          width = 4,
          # Buttons to load the extent with modal dialogs
          span(
            div(
              style = "display:inline-block;vertical-align:top;",
              strong(i18n$t("Click on button: \u00a0"))
            ),
            div(
              style = "padding-top:5px;padding-bottom:10px;",
              actionButton(
                "button_refresh_map_prevision",
                label = i18n$t("\u2000Reload the extent on map"),
                width = 176,
                icon = icon("retweet")
              )
            ) # end of div
          ) # end of span
        ), # end of column
        # Map
        leafletOutput("view_map_prevision", height = 800, width = "100%")
      ) # end of box
    ) # end of fluidrow
  ) # end taglist
}
    
#' launch_prevision Server Function
#'
#' @noRd 
mod_launch_prevision_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_launch_prevision_ui("launch_prevision_ui_1")
    
## To be copied in the server
# callModule(mod_launch_prevision_server, "launch_prevision_ui_1")
 
