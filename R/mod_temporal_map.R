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
          condition = sprintf("input['%s'] == 'FALSE'", ns("online")),
          radioButtons(ns("query_space"),
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
                      ns("button_extent_gpkg"),
                      label = i18n$t("\u2000Load a gpkg file"),
                      width = 196,
                      icon = icon("upload")
                    ),
                    actionButton(
                      ns("button_extent_draw"),
                      label = i18n$t("\u2000Draw it on the map"),
                      width = 196,
                      icon = icon("paint-brush")
                    ),
                    actionButton(
                      ns("button_refresh_map"),
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
                  ns("tiles_checkbox"),
                  i18n$t("Tiles selected"),
                  choices = character(0),
                  options = list(
                    `selected-text-format` = "count > 6",
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    title = i18n$t("All overlapping tiles")
                  ),
                  multiple = TRUE
                ) # end pickerinput
              ), # end column
              column(
                width = 6,
                pickerInput(
                  ns("orbits_checkbox"),
                  span(
                    i18n$t("Orbits selected\u2000"),
                    actionLink(ns("help_orbits"), icon("question-circle"))
                  ),
                  choices = str_pad2(1:143, 3, "left", "0"),
                  options = list(
                    `selected-text-format` = "count > 10",
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    title = i18n$t("All overlapping orbits")
                  ),
                  multiple = TRUE
                ) # end pickerinput
              ) # end column
            ), # end column
            column(
              width = 12,
              # Map
              leaflet::leafletOutput(ns("view_map_extent"), height = 600, width = "100%"),
              textInput(ns("list_tiles"), label = "")
            ) # end of column
        ) # end of fluidrow
      ) # end of box
    ) # end of fluidrow
  ) # end taglist
}
    
#' temporal_map Server Function
#'
#' @noRd 
#' 
#' @importFrom sf st_geometry_type st_as_sf
#' @importFrom shinyjs enable disable
#' @importFrom leaflet removeShape leafletProxy removeLayersControl
#' 
mod_temporal_map_server <- function(input, output, session, rv){
  ns <- session$ns
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  # create a new map for extent view
  react_map <- reactiveVal({
    basemap <- base_map(session = session, map = "view_map_extent")
    removeLayersControl(basemap)
    addLayersControl(
      basemap,
      baseGroups = c("OpenStreetMap", "OpenTopoMap", "Satellite"),
      overlayGroups = c("Light names", "Dark names", "S2 tiles", "Extent"),
      options = layersControlOptions(collapsed = TRUE)
    )
  })
  
  output$view_map_extent <- renderLeaflet({
    react_map()
  })
  
  observeEvent(input$path_gpkg_sel, {
    uploaded_exts <- gsub(
      "^.+\\.(.+)$", "\\1",
      input$path_gpkg_sel$name
    )
    # checks if it is a gpkg file
    if (uploaded_exts %not_in% c("gpkg")) {
      # if a not gpkg file was chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = i18n$t("Invalid gpkg"),
        text = paste(
          i18n$t("Please select a gpkg file")
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$gpkg_path <- ""
      # checks if class vector is a polygon
    } else if (all(st_geometry_type(st_read(input$path_gpkg_sel$datapath, quiet = TRUE)) %in% c("POINT"))) {
      # if a not polygon gpkg file was chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = i18n$t("Invalid polygon"),
        text = paste(
          i18n$t("Please select a gpkg polygon.")
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$gpkg_path <- ""
    } else if (length(uploaded_exts) == 1 && uploaded_exts %in% c("gpkg")) {
      # if a single file was chosen and it is a geopackage file, use it
      rv$gpkg_path <- input$path_gpkg_sel$datapath
    }
  })
  
  # create a new map (to be shown in modal dialog)
  react_map_gpkg <- reactiveVal({
    base_map(session = session, map = "view_map_gpkg")
  })
  
  output$view_map_gpkg <- renderLeaflet({
    react_map_gpkg()
  })
  
  # Open modal dialog to load the gpkg file
  observeEvent(input$button_extent_gpkg, {
    rv$gpkg_path <- ""
    showModal(load_extent_gpkg(rv, session))
  })
  
  # load the gpkg on the map
  observeEvent(rv$gpkg_path, {
    # Check that the vector is valid
    rv$gpkg_polygon <- tryCatch(
      {
        x <- st_read(rv$gpkg_path, quiet = TRUE) %>%
          st_transform(4326)
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      },
      error = function(e) {
        x <- st_polygon()
        attr(x, "valid") <- FALSE
        x
      }
    )
    if (attr(rv$gpkg_polygon, "valid")) {
      # if the gpkg is valid, update the map modal
      rv$gpkg_polygon_ll <- st_transform(rv$gpkg_polygon, 4326)
      leafletProxy("view_map_gpkg") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$gpkg_polygon_ll)[, "X"]),
          lat1 = min(st_coordinates(rv$gpkg_polygon_ll)[, "Y"]),
          lng2 = max(st_coordinates(rv$gpkg_polygon_ll)[, "X"]),
          lat2 = max(st_coordinates(rv$gpkg_polygon_ll)[, "Y"])
        ) %>%
        addPolygons(
          data = rv$gpkg_polygon_ll,
          group = "Extent",
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        )
    } else {
      # if the gpkg is not valid, reset the map
      react_map_gpkg(base_map(session = session, map = "view_map_gpkg"))
    }
  })
  
  # use bbox
  observeEvent(input$save_extent_gpkg, {
    withProgress(message = i18n$t("Creating the extent"), value = 0, {
      gpkg_valid <- update_extent(rv = rv, extent_source = "gpkg", session = session)
      if (gpkg_valid$out) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = i18n$t("Please specify a valid gpkg file."),
          text = NULL,
          type = "error",
          btn_labels = "Ok"
        )
      }
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })
  
  # Draw mode ####
  
  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_draw, {
    # create a new namespace every time the button is pushed,
    # in order not to make mess between modules
    extent_ns_name <- paste0("editor_", sample(1E9, 1))
    extent_ns <- ns(extent_ns_name)
    rv$extent_edits <- callModule(editModPoly, extent_ns_name, base_map(session = session, map = "view_map_draw"), rv = rv)
    
    # show the modal dialog
    showModal(load_extent_draw(extent_ns_name = extent_ns_name, session = session, rv = rv))
  })
  
  # use bbox
  observeEvent(input$save_extent_draw, {
    withProgress(message = i18n$t("Creating the extent"), value = 0, {
      drawn_valid <- update_extent(extent_source = "draw", rv = rv, session = session)
      if (drawn_valid$out) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = i18n$t("Please draw a valid extent."),
          text = NULL,
          type = "error",
          btn_labels = "Ok"
        )
      }
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })
  
  #- Update tile colours when single tiles are [de]activated -#
  observeEvent(input$tiles_checkbox, ignoreNULL = FALSE, {
    if (!is.null(input$tiles_checkbox)) {
      rv$draw_tiles_overlapping_selected <- rv$s2tiles %>% dplyr::filter(tile_id %in% input$tiles_checkbox)
      l <- unlist(strsplit(input$list_tiles, ","))
      ul <- l[!(l %in% input$tiles_checkbox)] 
      rv$draw_tiles_overlapping_unselected <- rv$s2tiles %>% dplyr::filter(tile_id %in% ul)
      leafletProxy("view_map_extent") %>% 
        addPolygons(
          data = rv$draw_tiles_overlapping_selected,
          group = "S2 tiles",
          options = pathOptions(pane = "tiles_selected"),
          layerId = ~tile_id,
          label = ~tile_id,
          labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "orange",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkred"
        )
      if (length(ul) > 0L) {
        leafletProxy("view_map_extent") %>% 
          addPolygons(
            data = rv$draw_tiles_overlapping_unselected,
            group = "S2 tiles",
            options = pathOptions(pane = "tiles_notselected"),
            layerId = ~tile_id,
            label = ~tile_id,
            labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
            fill = TRUE,
            fillColor = "lightgrey",
            fillOpacity = .3,
            stroke = TRUE,
            weight = 3,
            color = "grey"
          )
      }
    } else {
      l <- unlist(strsplit(input$list_tiles, ","))
      ul <- l[!(l %in% input$tiles_checkbox)] 
      rv$draw_tiles_overlapping_unselected <- rv$s2tiles %>% dplyr::filter(tile_id %in% ul)
      if (length(l) > 1L) {
        leafletProxy("view_map_extent") %>%
          addPolygons(
            data = rv$draw_tiles_overlapping_unselected,
            group = "S2 tiles",
            options = pathOptions(pane = "tiles_notselected"),
            layerId = ~tile_id,
            label = ~tile_id,
            labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
            fill = TRUE,
            fillColor = "lightgrey",
            fillOpacity = .3,
            stroke = TRUE,
            weight = 3,
            color = "grey"
          )
      }
    }
  })
  
  
  #- Refresh the extent map if required -#
  observeEvent(input$button_refresh_map, {
    withProgress(message = i18n$t("\u2000Reload map extent"), value = 0, {
      refresh <- update_extent(extent_source = "fake", map = "view_map_extent", rv = rv, session = session)
      rv <- refresh
      updatePickerInput(
        session, 
        "tiles_checkbox",
        selected = ""
      )
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })
  
  observeEvent(input$help_orbits, {
    showModal(modalDialog(
      title = i18n$t("Tiles and orbits selected"),
      p(HTML(
        i18n$t("Selectors 'Tiles selected' and 'Orbits selected' allow to"),
        i18n$t("restrict the processing to the specified tiles and <a href="),
        i18n$t("'https://sentinel.esa.int/web/sentinel/missions/sentinel-2/satellite-description/orbit'"),
        i18n$t("target='_blank'>orbits</a>."),
        i18n$t("The list of tiles which can be selected is dynamically updated"),
        i18n$t("basing on the selected extent"),
        i18n$t("(only tiles overlapping the extent are shown),"),
        i18n$t("as well as the colour of tiles shown in the map is dynamically set"),
        i18n$t("(selected tiles are shown in red, unselected ones in grey)."),
        i18n$t("The list of orbits is static, and orbits are not shown"),
        i18n$t("on the map.")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  return(rv)
 
}
    
## To be copied in the UI
# mod_temporal_map_ui("temporal_map_ui_1")
    
## To be copied in the server
# callModule(mod_temporal_map_server, "temporal_map_ui_1")
 
