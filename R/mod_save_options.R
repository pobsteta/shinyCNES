#' save_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_save_options_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "process",
    h3(i18n$t("Processing options")),
    fluidRow(
      # Box saving options
      box(
        title = i18n$t("Saving options"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        ### saving options ###
        # mode on/off line
        column(
          width = 4,
          # online_mode (online/offline mode)
          radioButtons(
            "online",
            label = span(
              i18n$t("Download mode\u2000"),
              actionLink("help_online", icon("question-circle"))
            ),
            choiceNames = list(
              i18n$t("Online"),
              i18n$t("Offline")
            ),
            choiceValues = list(TRUE, FALSE),
            selected = TRUE,
            inline = TRUE
          ) # end radiobutton
        ), # end column
        column(
          width = 4,
          # overwrite products
          radioButtons(
            "overwrite_product",
            label = span(
              i18n$t("Overwrite existing products ?\u2000"),
              actionLink("help_overwrite_product", icon("question-circle"))
            ),
            choiceNames = list(
              i18n$t("Yes"),
              i18n$t("No")
            ),
            choiceValues = list(TRUE, FALSE),
            selected = TRUE,
            inline = TRUE
          ) # end radiobutton
        ), # end column
        column(
          width = 4,
          # Theia credentials
          conditionalPanel(
            condition = "input.online == 'TRUE'",
            conditionalPanel(
              condition = "input.product == 'theia'",
              div(
                style = "padding-bottom:10px;",
                actionButton(
                  "theia",
                  label = i18n$t("\u2000Login in THEIA"),
                  icon = icon("user-circle")
                )
              )
            ), # end conditionalpanel
            conditionalPanel(
              condition = "input.product == 'peps'",
              div(
                style = "padding-bottom:10px;",
                actionButton(
                  "peps",
                  label = i18n$t("\u2000Login in PEPS"),
                  icon = icon("user-circle")
                )
              )
            ) # end conditionalpanel
          ) # end conditionalpanel
        ) # end column
      ) # end box
    ) # end fluidrow
  ) # end taglist
}
    
#' save_options Server Function
#'
#' @noRd 
mod_save_options_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_save_options_ui("save_options_ui_1")
    
## To be copied in the server
# callModule(mod_save_options_server, "save_options_ui_1")
 
