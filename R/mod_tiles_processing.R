#' tiles_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tiles_processing_ui <- function(id) {
  ns <- NS(id)

  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  tagList(
    tabName = "tiles_processing",
    h3(i18n$t("Processing tiles")),
    fluidRow(
      box(
        title = i18n$t("Tiles"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        box(
          title = i18n$t("Tiles downloaded"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          p(
            style = "margin-top:0pt;",
            actionButton(
              ns("goDownload"),
              label = i18n$t("\u2000Starts downloading"),
              icon = icon("cog", class = "fa-spin"),
              class = "darkbutton"
            )
          ),
          selectInput(ns("listimgtile"), i18n$t("Tiles downloaded:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            plotOutput(ns("imgtile"), height = 500, width = 500),
          ), # end div
          textInput(ns("list_imgtiles"), label = "")
        ), # end box
        box(
          title = i18n$t("Tiles RGB"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          p(
            style = "margin-top:0pt;",
            actionButton(
              ns("goRGB"),
              label = i18n$t("\u2000Starts RGB calc"),
              icon = icon("cog", class = "fa-spin"),
              class = "darkbutton"
            )
          ),
          selectInput(ns("listimgrgb"), i18n$t("Tiles RGB:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            plotOutput(ns("imgrgb"), height = 500, width = 500)
          ), # end div
          textInput(ns("list_imgrgbs"), label = "")
        ), # end box
        box(
          title = i18n$t("Tiles indice"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          p(
            style = "margin-top:0pt;",
            actionButton(
              ns("goIndices"),
              label = i18n$t("\u2000Starts indices calc"),
              icon = icon("cog", class = "fa-spin"),
              class = "darkbutton"
            )
          ),
          selectInput(ns("listimgindice"), i18n$t("Tiles indice:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            plotOutput(ns("imgindice"), height = 500, width = 500)
          ),
          textInput(ns("list_imgindices"), label = "")
        ) # end box
      ), # end box
    ), # end fluidrow 1
    br(),
    fluidRow(
      box(
        title = i18n$t("Presence/Absence - Tiles TIF - Mask"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        box(
          title = i18n$t("Presence/Absence"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          # button
          column(
            width = 12,
            # Buttons to load the extent with modal dialogs
            strong(i18n$t("Specify the extent:\u2000")),
            span(
              div(
                style = "padding-top:5px;padding-bottom:10px;",
                actionButton(
                  ns("button_extent_vectfile_pa"),
                  label = i18n$t("\u2000Load a gpkg file"),
                  width = 176,
                  icon = icon("upload")
                ),
                actionButton(
                  ns("button_refresh_map_pa"),
                  label = i18n$t("\u2000Reload map extent"),
                  width = 176,
                  icon = icon("retweet")
                )
              ) # end of div
            ) # end of span
          ), # end of column
          # Map presence/absence
          leafletOutput(ns("view_map_presabs"), height = 500, width = "100%")
        ), # end box
        box(
          title = i18n$t("Mask shapefile"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          # button
          column(
            width = 12,
            # Buttons to load the extent with modal dialogs
            strong(i18n$t("Specify the extent:\u2000")),
            span(
              div(
                style = "padding-top:5px;padding-bottom:10px;",
                actionButton(
                  ns("button_extent_vectfile_mask"),
                  label = i18n$t("\u2000Load a gpkg file"),
                  width = 176,
                  icon = icon("upload")
                ),
                actionButton(
                  ns("button_refresh_map_mask"),
                  label = i18n$t("\u2000Reload map extent"),
                  width = 176,
                  icon = icon("retweet")
                )
              ) # end of div
            ) # end of span
          ), # end of column
          # Map presence/absence
          leafletOutput(ns("view_map_mask"), height = 500, width = "100%")
        ) # end box
      ) # end box
    ), # end fluidrow 2
    br(),
    column(12, textOutput(ns("text00")))
  ) # end tabitem tab_tiles_processing
}

#' tiles_processing Server Function
#'
#' @noRd
mod_tiles_processing_server <- function(input, output, session, rv) {
  ns <- session$ns
  
  # for save parameters
  observe({
    rv$project_name <- input$project_name
    rv$path_project <- input$path_project_textin
    rv$path_rgb <- file.path(rv$path_project, "/projets/", rv$project_name, "/rgb")
    rv$list_imgtile <-   unlist(strsplit(input$list_imgtiles, ","))
    rv$list_imgrgb <-   unlist(strsplit(input$list_imgrgbs, ","))
    rv$list_imgindice <-   unlist(strsplit(input$list_imgindices, ","))
  })

  # if goDownload is pressend
  observeEvent(input$goDownload, {
    req(rv)
    til <- s2_list(
      session = session,
      spatial_extent = rv$extent,
      tiles = rv$s2tiles_selected,
      orbit = rv$s2orbits_selected,
      time_interval = rv$timewindow,
      time_period = rv$timeperiod,
      level = rv$theiaplatformsentinellevel,
      platform = rv$theiaplatformsentinel,
      maxcloud = rv$max_masked_perc,
      collection = rv$theiacollection,
      path_to_download = rv$path_project,
      project_name = rv$project_name
    )
    #### image list Tiles ####
    updateSelectInput(session, "listimgtile", choices = c("Choose a picture" = "", til$tile))
    updateTextInput(session, "list_imgtiles", value = til$tile)
    #### image list RGB ####
    updateSelectInput(session, "listimgrgb", choices = c("Choose a picture" = "", paste(rv$list_rgb, til$tile, sep = "_")))
    updateTextInput(session, "list_imgrgbs", value = paste(rv$list_rgb, til$tile, sep = "_"))
    #### image list Indice ####
    updateSelectInput(session, "listimgindice", choices = c("Choose a picture" = "", paste(rv$list_indice, til$tile, sep = "_")))
    updateTextInput(session, "list_imgindices", value = paste(rv$list_indice, til$tile, sep = "_"))
  })

  # image tile
  output$imgtile <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      s2_preview_img(
        path_data = rv$path_project,
        project_name = rv$project_name,
        collection = rv$theiaplatformsentinellevel,
        tile = input$listimgtile
      )
    }
  })
  
  # if goRGB is pressend
  observeEvent(input$goRGB, {
    req(rv)
    rgb <- s2_rgb(path_data = rv$path_project,
                  path_rgb = file.path(rv$path_project, "rgb"),
                  project_name = rv$project_name,
                  collection = rv$theiaplatformsentinellevel,
                  tiles = rv$list_imgtile,
                  rgbs = rv$list_rgb)
    #### image list RGB ####
    updateSelectInput(session, "listimgrgb", choices = c("Choose a picture" = "", rgb))
    updateTextInput(session, "list_imgrgbs", value = rgb)
  })
  
  # image rgb
  output$imgrgb <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      s2_preview_rgb(
        path_data = rv$path_project,
        project_name = rv$project_name,
        collection = rv$theiaplatformsentinellevel,
        tile = input$listimgrgb
      )
    }
  })
  
  # if goIndices is pressend
  observeEvent(input$goIndices, {
    req(rv)
    indice <- s2_indice(path_data = rv$path_project,
                     path_indice = file.path(rv$path_project, "indice"),
                     project_name = rv$project_name,
                     collection = rv$theiaplatformsentinellevel,
                     tiles = rv$list_imgtile,
                     indices = rv$list_indice)
    #### image list Indice ####
    updateSelectInput(session, "listimgindice", choices = c("Choose a picture" = "", indice))
    updateTextInput(session, "list_imgindices", value = indice)
  })
  
  # image rgb
  output$imgindice <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      s2_preview_indice(
        path_data = rv$path_project,
        project_name = rv$project_name,
        collection = rv$theiaplatformsentinellevel,
        tile = input$listimgindice
      )
    }
  })
  
}


## To be copied in the UI
# mod_tiles_processing_ui("tiles_processing_ui_1")

## To be copied in the server
# callModule(mod_tiles_processing_server, "tiles_processing_ui_1")
