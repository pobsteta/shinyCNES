#' temporal_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom leaflet leafletOutput
#' 
mod_temporal_map_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    # second box Map
    fluidRow(
      box(
        title = i18n$t("Map"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        conditionalPanel(
          condition = "input.online == 'FALSE'",
          radioButtons("query_space",
                       label = i18n$t("Use spatial filter/clip?"),
                       choiceNames = list(
                         i18n$t("Yes"),
                         i18n$t("No (process all the input THEIA images)")
                       ),
                       choiceValues = list(TRUE, FALSE),
                       selected = TRUE,
                       inline = TRUE
          )
        ), # end of conditionalpanel
        fluidRow(
            column(
              width = 5,
              # Buttons to load the extent with modal dialogs
              strong(i18n$t("Specify the extent:\u2000")),
              column(
                width = 12,
                span(
                  div(
                    style = "padding-top:5px;padding-bottom:10px;",
                    actionButton(
                      "button_extent_forest",
                      label = i18n$t("\u2000Choose on a list"),
                      width = 196,
                      icon = icon("list-ul")
                    ),
                    actionButton(
                      "button_extent_bbox",
                      label = i18n$t("\u2000Specify a bounding box"),
                      width = 196,
                      icon = icon("object-group")
                    ),
                    actionButton(
                      "button_extent_vectfile",
                      label = i18n$t("\u2000Load a vector file"),
                      width = 196,
                      icon = icon("upload")
                    )
                  ), # end div
                  div(
                    actionButton(
                      "button_extent_draw",
                      label = i18n$t("\u2000Draw it on the map"),
                      width = 196,
                      icon = icon("paint-brush")
                    ),
                    actionButton(
                      "button_refresh_map",
                      label = i18n$t("\u2000Reload map extent"),
                      width = 196,
                      icon = icon("retweet")
                    )
                  ) # end of div
                ) # end span
              ) # end column
            ), # end column
            column(
              width = 7,
              # Select tiles and orbits
              column(
                width = 6,
                pickerInput(
                  "tiles_checkbox", "Tiles selected",
                  choices = character(0),
                  options = list(
                    `selected-text-format` = "count > 3",
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    title = "All overlapping tiles"
                  ),
                  multiple = TRUE
                ) # end pickerinput
              ), # end column
              column(
                width = 6,
                pickerInput(
                  "orbits_checkbox",
                  span(
                    "Orbits selected\u2000",
                    actionLink("help_orbits", icon("question-circle"))
                  ),
                  choices = str_pad2(1:143, 3, "left", "0"),
                  options = list(
                    `selected-text-format` = "count > 6",
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    title = "All overlapping orbits"
                  ),
                  multiple = TRUE
                ) # end pickerinput
              ) # end column
            ), # end column
            column(
              width = 12,
              # Map
              leaflet::leafletOutput("view_map", height = 600, width = "100%")
            ) # end of column
        ) # end of fluidrow
      ) # end of box
    ) # end of fluidrow
  ) # end taglist
}
    
#' temporal_map Server Function
#'
#' @noRd 
mod_temporal_map_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_temporal_map_ui("temporal_map_ui_1")
    
## To be copied in the server
# callModule(mod_temporal_map_server, "temporal_map_ui_1")
 
