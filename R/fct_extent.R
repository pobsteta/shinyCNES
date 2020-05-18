#' update_extent
#'
#' @name update_extent
#'
#' @param rv Reactive value
#' @param extent_source Extent source
#' @param custom_source Custom source
#' @param map Map
#' @param session Session
#'
#' @return Update the map
#' @export
#'
#' @importFrom leaflet leafletProxy clearShapes addPolygons showGroup fitBounds pathOptions labelOptions
#' @importFrom sf st_polygon st_read st_coordinates st_zm st_geometry st_transform
#' @importFrom dplyr filter
#'
update_extent <- function(rv, extent_source, custom_source = NA, map = "view_map_extent", session) {
  ns <- session$ns
  
  # 1. define rv$extent
  if (extent_source == "gpkg") {
    # gpkg mode
    # check if polygon is valid
    if (attr(rv$gpkg_polygon, "valid")) {
      rv$extent <- rv$gpkg_polygon
      attr(rv$extent, "new") <- TRUE
    } else {
      rv$out <- FALSE
      return(rv)
    }
  } else if (extent_source == "draw") {
    # draw mode
    # namespace for extent selection
    sel_draw <- if (!is.null(rv$extent_edits()$finished)) {
      x <- rv$extent_edits()$finished
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    } else {
      x <- st_polygon()
      attr(x, "valid") <- FALSE
      x
    }
    if (!attr(sel_draw, "valid")) {
      rv$out <- FALSE
      return(rv)
    }
    rv$extent <- sel_draw
  } else if (extent_source == "imported") {
    # imported from parameters
    sel_imported_extent <- if (is.null(custom_source) | anyNA(custom_source)) {
      x <- st_polygon()
      attr(x, "valid") <- FALSE
      x
    } else {
      x <- if (is.character(custom_source)) {
        st_read(custom_source, quiet = TRUE)
      } else {
        custom_source
      }
      x <- st_transform(st_zm(x), 4326)
      names(sf::st_geometry(x)) <- NULL
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    }

    rv$extent <- sel_imported_extent
    if (!attr(sel_imported_extent, "valid")) {
      rv$out <- FALSE
      return(rv)
    }
    rv$extent <- sel_imported_extent
  } else {
    # for any other value of extent_source, use the existing rv$extent
    if (is.null(rv$extent)) {
      rv$out <- FALSE
      return(rv)
    } else if (!attr(rv$extent, "valid")) {
      rv$out <- FALSE
      return(rv)
    } else {
      attr(rv$extent, "new") <- FALSE
    }
  }

  # 2. Update the list of overlapping tiles and the tiles on the map
  if (length(rv$extent) > 0) {
    rv$draw_tiles_overlapping <- tiles_intersects(rv$extent, all = TRUE, out_format = "sf", .s2tiles = rv$s2tiles)
    names(sf::st_geometry(rv$draw_tiles_overlapping)) <- NULL

    if (attr(rv$extent, "new")) {
      updatePickerInput(
        session, 
        "tiles_checkbox",
        choices = rv$draw_tiles_overlapping$tile_id,
        selected = ""
      )
      updateTextInput(session, "list_tiles", value = rv$draw_tiles_overlapping$tile_id)
    }

    # reset and update the map
    rv$draw_tiles_overlapping_ll <- st_transform(rv$draw_tiles_overlapping, 4326)
    rv$extent_ll <- st_transform(rv$extent, 4326)
    leafletProxy(mapId = ns(map), session = session) %>%
      clearShapes() %>%
      fitBounds(
        lng1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[, "X"]),
        lat1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[, "Y"]),
        lng2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[, "X"]),
        lat2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[, "Y"])
      ) %>%
      addPolygons(
        data = rv$draw_tiles_overlapping_ll,
        group = "S2 tiles",
        layerId = ~tile_id,
        label = ~tile_id,
        labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
        options = pathOptions(pane = "tiles_notselected"),
        fill = TRUE,
        fillColor = "lightgrey",
        fillOpacity = .3,
        stroke = TRUE,
        weight = 3,
        color = "grey"
      ) %>%
      # add extent
      addPolygons(
        data = rv$extent_ll,
        group = "Extent",
        layerId = "Extent",
        fill = TRUE,
        fillColor = "blue",
        fillOpacity = .3,
        stroke = TRUE,
        weight = 3,
        color = "darkgreen"
      ) %>%
      showGroup(c("OpenTopoMap"))
  } else {
    rv$draw_tiles_overlapping <- NULL
    # empty the list of tiles
    updatePickerInput(session, "tiles_checkbox", choices = character(0))
    # reset the map
    # react_map(base_map())
  }

  rv$out <- TRUE
  return(rv)
}

#' Create base_map
#'
#' @name base_map
#'
#' @param rv Reactive value
#' @param map Map
#' @param session Session
#'
#' @return Base map
#' @export
#'
#' @importFrom leaflet leaflet addTiles addLayersControl hideGroup layersControlOptions addMapPane
#'
base_map <- function(rv, map, session) {
  ns <- session$ns
  leaflet(ns(map)) %>%
    # add tiles
    addTiles(group = "OpenStreetMap") %>%
    addTiles("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
      group = "OpenTopoMap"
    ) %>%
    addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
      group = "Satellite"
    ) %>%
    addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_only_labels/{z}/{x}/{y}.png",
      group = "Light names"
    ) %>%
    addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_only_labels/{z}/{x}/{y}.png",
      group = "Dark names"
    ) %>%
    addMapPane("extent", zIndex = 430) %>% 
    addMapPane("tiles_selected", zIndex = 420) %>% 
    addMapPane("tiles_notselected", zIndex = 410) %>% 
    # view and controls
    addLayersControl(
      baseGroups = c("OpenStreetMap", "OpenTopoMap", "Satellite"),
      overlayGroups = c("Light names", "Dark names", "Extent"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    hideGroup(c("Light names", "Dark names"))
}

#' load_extent_gpkg
#'
#' @name load_extent_gpkg
#'
#' @param rv Reactive values
#' @param session Session
#'
#' @importFrom shiny htmlOutput modalButton modalDialog helpText fileInput
#' actionButton
#' @importFrom leaflet leafletOutput
#' @export
load_extent_gpkg <- function(rv, session) {
  ns <- session$ns
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  modalDialog(
    title = i18n$t("Select vector gpkg file"),
    size = "m",
    helpText(em(
      p(i18n$t("Choose the vector gpkg file to be used as extent.")),
      p(
        i18n$t("To upload a geopackage, select the related file"),
        i18n$t("(at most the .gpkg must be present).")
      )
    )),
    fileInput(ns("path_gpkg_sel"),
      i18n$t("Select"),
      multiple = FALSE
    ),
    div(
      style = "display:inline-block;vertical-align:top;",
      htmlOutput(ns("path_gpkg_errormess"))
    ),
    leafletOutput(ns("view_map_gpkg"), height = 400, width = "100%"),
    easyClose = FALSE,
    footer = tagList(
      actionButton(ns("save_extent_gpkg"),
        strong(i18n$t("\u2000Ok")),
        icon = icon("check")
      ),
      modalButton(i18n$t("\u2000Cancel"), icon = icon("ban"))
    )
  )
}

#' load_extent_draw
#'
#' @name load_extent_draw
#'
#' @param extent_ns_name Name of the namespace to be used
#' @param rv Reactive values
#' @param session Session
#'
#' @importFrom shiny actionButton modalButton modalDialog helpText
#' @importFrom mapedit editModUI
#' @export
load_extent_draw <- function(extent_ns_name, rv, session) {
  ns <- session$ns
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  modalDialog(
    title = i18n$t("Draw the extent"),
    size = "l",
    helpText(em(i18n$t("Use the tools on the left to draw the extent of your calc."))),
    ns(editModUI(extent_ns_name, height = 500, width = "100%")),
    easyClose = FALSE,
    footer = tagList(
      actionButton(ns("save_extent_draw"), strong(i18n$t("\u2000Ok")),
        icon
        = icon("check")
      ),
      modalButton(i18n$t("\u2000Cancel"), icon = icon("ban"))
    )
  )
}
