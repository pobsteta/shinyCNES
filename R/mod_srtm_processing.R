#' srtm_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_srtm_processing_ui <- function(id) {
  ns <- NS(id)

  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  tagList(
    tabName = "srtm_processing",
    h3(i18n$t("Processing SRTM")),
    fluidRow(
      box(
        title = i18n$t("Options SRTM"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        column(
          width = 4,
          pickerInput(
            ns("indice_checkbox"),
            span(i18n$t("Indices derivated"), 
                 actionLink(ns("help_srtm_indices"), icon("question-circle"))
            ),
            choices = c("exposition", "pente", "ombrage", "rugosite", "TRI", "TPI", "TWI", "geomorphon", "cplan", "cprof", "carea", "log10_carea"),
            options = list(
              `selected-text-format` = "count > 6",
              `live-search` = TRUE,
              `actions-box` = TRUE,
              title = i18n$t("All overlapping indices")
            ),
            multiple = TRUE
          ) # end pickerinput
        ), # end column
        column(width = 4,
               # textinput for multiple of calc derivated
               textInput(ns("srtm_multiple"), 
                         label = span(
                           i18n$t("Multiple to derivate\u2000"),
                           actionLink(ns("help_srtm_multiple"), icon("question-circle"))
                         ),
                         value = "9,15,33"
               )
        ), # end column
        column(width = 4,
               span(
                 div(
                   style = "padding-top:5px;padding-bottom:10px;",
                   actionButton(
                     ns("esa"),
                     label = i18n$t("\u2000Login in ESA/NASA"),
                     icon = icon("user-circle")
                   )
                 ) # end div
               ) # end span
        ), # end column
        column(width = 12,
               div(
                 actionButton(
                   ns("goSRTM"),
                   label = "\u2000SRTM",
                   icon = icon("upload"),
                   class = "darkbutton"
                 )
               ) # end div
        ) # end column
      ), # end box
      box(
        title = i18n$t("Tiles"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        box(
          title = i18n$t("Tiles SRTM"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          selectInput(ns("listimgsrtm"), i18n$t("Tiles SRTM:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            plotOutput(ns("imgsrtm"), height = 500, width = 500)
          ), # end div
          textInput(ns("list_imgsrtms"), label = "")
        ), # end box
        box(
          title = i18n$t("Multiple SRTM derivated"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          column(
            width = 12,
            selectInput(ns("listimgsrtmmultiple"), i18n$t("Multiple tiles derivated:"), c("No tile" = "")),
            div(
              style = "display:inline-block;horizontal-align:center;",
              plotOutput(ns("imgsrtmmultiple"), height = 500, width = 500)
            ),
            textInput(ns("list_imgsrtmmultiples"), label = "")
          ) # end column
        ), # end box
        box(
          title = i18n$t("Simple SRTM derivated"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          column(
            width = 12,
            selectInput(ns("listimgsrtmsimple"), i18n$t("Simple tiles derivated:"), c("No tile" = "")),
            div(
              style = "display:inline-block;horizontal-align:center;",
              plotOutput(ns("imgsrtmsimple"), height = 500, width = 500)
            ),
            textInput(ns("list_imgsrtmsimples"), label = "")
          ) # end column
        ) # end box
      ) # end box
    ) # end fluidrow 
  ) # end tabitem tab_srtm_processing
}

#' srtm_processing Server Function
#'
#' @noRd
mod_srtm_processing_server <- function(input, output, session, rv) {
  ns <- session$ns
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  # for save parameters
  observe({
    rv$project_name <- input$project_name
    rv$path_project <- input$path_project_textin
    rv$path_tif <- file.path(rv$path_project, "/projets/", rv$project_name, "/tif")
    rv$srtm_multiple <- unlist(strsplit(input$srtm_multiple, ","))
    rv$list_imgtile <- unlist(strsplit(input$list_imgtiles, ","))
    rv$list_imgrgb <- unlist(strsplit(input$list_imgrgbs, ","))
    rv$list_imgindice <- unlist(strsplit(input$list_imgindices, ","))
    rv$list_imgsrtm <- unlist(strsplit(input$list_imgsrtms, ","))
    rv$list_imgsrtmsimple <- unlist(strsplit(input$list_imgsrtmsimples, ","))
    rv$list_imgsrtmmultiple <- unlist(strsplit(input$list_imgsrtmmultiples, ","))
  })
  
  observeEvent(input$help_srtm_multiple, {
    showModal(modalDialog(
      title = i18n$t("SRTM multiple"),
      p(HTML(
        i18n$t("Indicate a list of the multiples to be used to calculate the data"),
        i18n$t("derived from the SRTM data, separated by a comma. These multiples"),
        i18n$t("will be used for the calculation of the TPI, IRR, Roughness, Slope,"),
        i18n$t("Morphology indices.")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # if goSRTM is pressend
  observeEvent(input$goSRTM, {
    req(rv)
    res <- srtm(session = session,
                spatial_extent = rv$extent, 
                path_project = rv$path_project,
                path_tif = rv$path_tif,
                # list_indice = input$indice_checkbox,
                srtm_multiple = input$srtm_multiple,
                time_interval = rv$timewindow)
    #### image list SRTM ####
    updateSelectInput(session, "listimgsrtm", choices = c("Choose a picture" = "", res[1]))
    updateTextInput(session, "list_imgsrtms", value = res[1])
    #### image list SRTM simple ####
    updateSelectInput(session, "listimgsrtmsimple", choices = c("Choose a picture" = "", res[2:8]))
    updateTextInput(session, "list_imgsrtmsimples", value = res[2:8])
    #### image list SRTM multiple ####
    updateSelectInput(session, "listimgsrtmmultiple", choices = c("Choose a picture" = "", res[9:length(res)]))
    updateTextInput(session, "list_imgsrtmmultiples", value = res[9:length(res)])
  })
  
  # image srtm
  output$imgsrtm <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      s2_preview_srtm(
        path_data = rv$path_project,
        project_name = rv$project_name,
        path_tif = rv$path_tif,
        tile = input$listimgsrtm
      )
    }
  })
  
  # image srtm simple
  output$imgsrtmsimple <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      s2_preview_srtm(
        path_data = rv$path_project,
        project_name = rv$project_name,
        path_tif = rv$path_tif,
        tile = input$listimgsrtmsimple
      )
    }
  })
  
  # image srtm multiple
  output$imgsrtmmultiple <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      s2_preview_srtm(
        path_data = rv$path_project,
        project_name = rv$project_name,
        path_tif = rv$path_tif,
        tile = input$listimgsrtmmultiple
      )
    }
  })
  
  # Edit esa credentials login
  observeEvent(input$esa, {
    # open the modalDialog
    showModal(esa_modal(
      username = if (!is.null(input$esa_username)) {
        input$esa_username
      } else {
        NA
      },
      password = if (!is.null(input$esa_password)) {
        input$esa_password
      } else {
        NA
      }, 
      session = session
    ))
    # initialise the shinyFiles Save as button
    observe({
      apiesa_path_prev <- rv$apiesa_path
      shinyFileSave(input, "apiesa_path_sel", roots = rv$volumes, session = session)
    })
  })
  # save user/password
  observeEvent(input$save_apiesa, {
    write_esa_login(
      input$esa_username, input$esa_password,
      apiesa_path = if (!is.na(rv$apiesa_path)) {
        as.character(rv$apiesa_path)
      } else {
        NA
      }
    )
    removeModal()
  })
  
}


## To be copied in the UI
# mod_srtm_processing_ui("srtm_processing_ui_1")

## To be copied in the server
# callModule(mod_srtm_processing_server, "srtm_processing_ui_1")
