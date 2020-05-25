# Module UI

#' @title   mod_param_ui and mod_param_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_param
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @importFrom shiny.i18n Translator
#'
mod_param_ui <- function(id) {
  ns <- NS(id)

  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  tagList(
    ### button
    div(
      style = "position:absolute;top:100px;",
      # server-side buttons
      p(
        style = "margin-top:15pt;",
        shinySaveButton(
          ns("export_param"),
          i18n$t("Save parameters as"),
          i18n$t("Save parameters of the project as"),
          icon = icon("upload"),
          style = "width:196px",
          filetype = list(json = "json"),
          class = "darkbutton"
        )
      ),
      p(
        style = "margin-top:5pt;",
        shinyFilesButton(
          ns("import_param"),
          i18n$t("Load parameters"),
          i18n$t("Import a JSON file with parameters of the project"),
          icon = icon("download"),
          style = "width:196px",
          multiple = FALSE,
          class = "darkbutton"
        )
      ),
      p(
        style = "margin-top:0pt;width:calc(90% - 50pt - 3px);",
        actionButton(
          ns("exit_gui"),
          label = i18n$t("\u2000Close application"),
          icon = icon("close"),
          width = 196,
          class = "darkbutton"
        )
      )
    ) # end div
  ) # end taglist
}

# Module Server

#' @rdname mod_param
#' @export
#' @keywords internal
#' @import shinyFiles
#' @importFrom jsonlite toJSON
#' @importFrom shiny updateTextInput withProgress NS stopApp
#' @importFrom shinyFiles parseFilePaths shinyFileSave parseSavePath getVolumes
#'
mod_param_server <- function(input, output, session, rv) {
  ns <- session$ns
  
  rv$out <- FALSE

  volumes <- c("Home" = path.expand("~"), shinyFiles::getVolumes()())

  # if Export is pressed, export the values (using server-side button)
  shinyFiles::shinyFileSave(
    input,
    "export_param",
    roots = volumes,
    session = session,
    filetypes = c("JSON" = "json")
  )

  observeEvent(input$export_param, {
    req(rv)
    export_param_path <- parseSavePath(volumes, input$export_param)
    if (nrow(export_param_path) > 0) {
      return_list <- create_return_list(rv = rv) # run creation of return_list
      check_param_result <- check_param(return_list)
      if (check_param_result) {
        writeLines(
          jsonlite::toJSON(return_list, pretty = TRUE),
          as.character(export_param_path$datapath)
        )
      }
    }
  })

  # if Import is pressed, read a json object (using server-side button)
  shinyFileChoose(
    input,
    "import_param",
    roots = volumes,
    session = session,
    filetypes = c("JSON" = "json")
  )

  observeEvent(input$import_param, {
    req(rv)
    import_param_path <- parseFilePaths(volumes, input$import_param)
    rv$imported_param <- if (nrow(import_param_path) > 0) {
      import_param_path$datapath %>%
        as.character() %>%
        readLines() %>%
        fromJSON()
    } else {
      NULL
    }
    if (!is.null(rv$imported_param)) {
      out <- import_param_list(session = session, rv = rv$imported_param)
      # rv$extent <- rv$imported_param$extent
    }
    rv$imported_param <- NULL
  })

  # if Exit is pressend, exit from GUI
  observeEvent(input$exit_gui, {
    stopApp()
  })

  # Closing the connection when window is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  return (rv)
}

## To be copied in the UI
# mod_param_ui("param_ui_1")

## To be copied in the server
# callModule(mod_param_server, "param_ui_1")
