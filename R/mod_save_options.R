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
            ns("online"),
            label = span(
              i18n$t("Download mode\u2000"),
              actionLink(ns("help_online"), icon("question-circle"))
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
            ns("overwrite_product"),
            label = span(
              i18n$t("Overwrite existing products ?\u2000"),
              actionLink(ns("help_overwrite_product"), icon("question-circle"))
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
            condition = sprintf("input['%s'] == 'TRUE'", ns("online")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'theia'", ns("product")),
              div(
                style = "padding-bottom:10px;",
                actionButton(
                  ns("theia"),
                  label = i18n$t("\u2000Login in THEIA"),
                  icon = icon("user-circle")
                )
              )
            ), # end conditionalpanel
            conditionalPanel(
              condition = sprintf("input['%s'] == 'peps'", ns("product")),
              div(
                style = "padding-bottom:10px;",
                actionButton(
                  ns("peps"),
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
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  observeEvent(input$help_online, {
    showModal(modalDialog(
      title = i18n$t("Download mode"),
      p(HTML(
        i18n$t("Selecting <strong>Online</strong> mode, the user must specify"),
        i18n$t("a spatial extent and a temporal window (in 'Parameters"),
        i18n$t("temporal'), and the list of required products is searched"),
        i18n$t("online (an internet connection is required);"),
        i18n$t("missing archives products are then downloaded.")
      )),
      p(HTML(
        i18n$t("In <strong>Offline</strong> mode, only already available archives"),
        i18n$t("products are used (level-2A images can be produced locally"),
        i18n$t("with an algorithm if the corresponding level-1C images are available);"),
        i18n$t("the user can still filter them spatially and temporally,"),
        i18n$t("but this is not mandatory (if no parameters are specified,"),
        i18n$t("all the archives images are processed)."))),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_overwrite_product, {
    showModal(modalDialog(
      title = i18n$t("Overwrite existing products?"),
      p(HTML(
        i18n$t("<strong>Yes</strong>:"),
        i18n$t("re-download all images matching the parameters set in"),
        i18n$t("'Temporal parameters' and re-apply algorithms if needed.")
      )),
      p(HTML(
        i18n$t("<strong>No</strong>:"),
        i18n$t("skip download and/or atmospheric correction for existing images.")
      )),
      em(i18n$t("This option is not available if nor download neither atmospheric"),
         i18n$t("correction are required.")),
      easyClose = TRUE,
      footer = NULL
    ))
  })
 
}
    
## To be copied in the UI
# mod_save_options_ui("save_options_ui_1")
    
## To be copied in the server
# callModule(mod_save_options_server, "save_options_ui_1")
 
