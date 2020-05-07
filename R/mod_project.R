# Module UI

#' @title   mod_project_ui and mod_project_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_project
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shiny.i18n Translator
#' @importFrom shinyFiles shinyDirButton
#'
mod_project_ui <- function(id) {
  ns <- NS(id)

  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  tagList(
    tabName = "project",
    h3(i18n$t("Project options")),
    fluidRow(
      box(
        title = i18n$t("Project"),
        # title = textOutput(ns("title_project")),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        column(
          width = 2,
          div(
            style = "display:inline-block;vertical-align:top;",
            strong(i18n$t("Name of project: \u00a0"))
          ),
          div(
            style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
            textInput(ns("project_name"), NULL, "")
          )
        ),
        column(
          width = 10,
          div(
            style = "display:inline-block;vertical-align:top;",
            strong(i18n$t("Directory for project: \u00a0"))
          ),
          div(
            style = "display:inline-block;vertical-align:top;",
            htmlOutput(ns("path_project_errorness"))
          ),
          div(
            div(
              style = "display:inline-block;vertical-align:top;width:50pt;",
              shinyDirButton(ns("path_project_sel"), "Select", i18n$t("Save parameters of the project as"))
            ),
            div(
              style = "display:inline-block;vertical-align:top;width:calc(99% - 50pt - 3px);",
              textInput(ns("path_project_textin"), NULL, "")
            )
          )
        ) # end column
      ) # end box
    ) # end fluidrow
  ) # end taglist
}

# Module Server

#' @rdname mod_project
#' @export
#' @keywords internal
#' @importFrom shiny.i18n Translator
#' @importFrom shiny updateTextInput
#' @importFrom shinyFiles getVolumes shinyDirChoose
#' @importFrom stats na.omit

mod_project_server <- function(input, output, session, rv) {
  ns <- session$ns

  volumes <- c("Home" = path.expand("~"), shinyFiles::getVolumes()())

  observe({
    shinyDirChoose(
      input,
      "path_project_sel",
      roots = volumes,
      session = session
    )
  })

  observe({
    rv$project_name <- input$project_name
  })

  observe({
    rv$path_project <- parseDirPath(volumes, input$path_project_sel)
    updateTextInput(session = session, "path_project_textin", value = parseDirPath(volumes, input$path_project_sel))
  })

  observeEvent(c(input$project_name, input$path_project_textin), {
    rv$project_name <- input$project_name
    rv$path_project <- input$path_project_textin
    rv$path_data <- file.path(input$path_project_textin, input$project_name, "data")
    if (nchar(rv$path_data) > 6) {
      # get plot directory
      rv$nom <- stringr::str_extract(
        tools::file_path_sans_ext(
          basename(
            list.files(rv$path_data, pattern = "gpkg$", full.names = TRUE)
          )
        ),
        "[a-zA-Z]{2}_[0-9]+"
      ) %>%
        na.omit() %>%
        data.frame()
      updateSelectInput(session, "plotin", choices = c("Choisir" = "", rv$nom))
    }
  })

  observe({
    output$path_project_errorness <- path_check(input$path_project_textin)
  })
}

## To be copied in the UI
# mod_project_ui("project_ui_1")

## To be copied in the server
# callModule(mod_project_server, "project_ui_1")
