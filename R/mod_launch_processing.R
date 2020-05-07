#' launch_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_launch_processing_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "launch_processing",
    h3(i18n$t("Processing result")),
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
          selectInput("listimage01", i18n$t("Tiles downloaded:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            imageOutput("image01", height = 500, width = 500)
          ) # end div
        ), # end box
        box(
          title = i18n$t("Tiles RGB"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          selectInput("listimage02", i18n$t("Tiles RGB:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            imageOutput("image02", height = 500, width = 500)
          ) # end div
        ), # end box
        box(
          title = i18n$t("Tiles indice"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          selectInput("listimage03", i18n$t("Tiles indice:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            imageOutput("image03", height = 500, width = 500)
          ) # end div
        ) # end box
      ) # end box
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
                  "button_extent_vectfile_pa",
                  label = i18n$t("\u2000Load a vector file"),
                  width = 176,
                  icon = icon("upload")
                ),
                # actionButton(
                #   "button_extent_draw_pa",
                #   label = i18n$t("\u2000Draw it on the map"),
                #   width = 176,
                #   icon = icon("paint-brush")
                # ),
                actionButton(
                  "button_refresh_map_pa",
                  label = i18n$t("\u2000Reload the extent on map"),
                  width = 176,
                  icon = icon("retweet")
                )
              ) # end of div
            ) # end of span
          ), # end of column
          # Map presence/absence
          leafletOutput("view_map_presabs", height = 500, width = "100%")
        ), # end box
        box(
          title = i18n$t("Tiles TIF"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          selectInput("listimage04", i18n$t("Tiles TIF:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            imageOutput("image04", height = 500, width = 500)
          ) # end div
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
                  "button_extent_vectfile_mask",
                  label = i18n$t("\u2000Load a vector file"),
                  width = 176,
                  icon = icon("upload")
                ),
                # actionButton(
                #   "button_extent_draw_mask",
                #   label = i18n$t("\u2000Draw it on the map"),
                #   width = 176,
                #   icon = icon("paint-brush")
                # ),
                actionButton(
                  "button_refresh_map_mask",
                  label = i18n$t("\u2000Reload the extent on map"),
                  width = 176,
                  icon = icon("retweet")
                )
              ) # end of div
            ) # end of span
          ), # end of column
          # Map presence/absence
          leafletOutput("view_map_mask", height = 500, width = "100%")
        ) # end box
      ) # end box
    ), # end fluidrow 2
    br(),
    column(12, textOutput("text00"))
  ) # end tabitem tab_launch_processing
}
    
#' launch_processing Server Function
#'
#' @noRd 
mod_launch_processing_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_launch_processing_ui("launch_processing_ui_1")
    
## To be copied in the server
# callModule(mod_launch_processing_server, "launch_processing_ui_1")
 
