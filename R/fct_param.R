#' import_param_list
#'
#' @param rv Reactivevalue list
#' @param session Shiny session
#'
#' @return A param list
#' @importFrom shiny updateTextInput setProgress withProgress
#' @importFrom shiny.i18n Translator
#' @importFrom shinyjs delay
#' @export
#'
import_param_list <- function(session, rv) {
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  # Add a progress bar while importing
  withProgress(message = i18n$t("Loading parameters"), value = 0, {
    # set directory and name of project
    # project ####
    setProgress(0.1)
    updateTextInput(session = session, "project_name", value = rv$project_name)
    updateTextInput(session = session, "path_project_textin", value = rv$path_project)
    # product ####
    setProgress(0.2)
    updateRadioButtons(session, "product", selected = rv$product)
    # product ####
    # collection ####
    if (rv$product == "theia") {
      updateRadioButtons(session, "theiacollection", selected = rv$theiacollection)
    } else {
      updateRadioButtons(session, "pepscollection", selected = rv$pepscollection)
    }
    # platform and level ####
    if (rv$theiacollection == "landsat") {
      updateRadioButtons(session, "theiaplatformlandsat", selected = rv$theiaplatformlandsat)
    } else if (rv$theiacollection == "spotworldheritage") {
      updateRadioButtons(session, "theiaplatformspotworldheritage", selected = rv$theiaplatformspotworldheritage)
    } else if (rv$theiacollection == "sentinel2") {
      updateRadioButtons(session, "theiaplatformsentinel", selected = rv$theiaplatformsentinel)
      # level
      updateRadioButtons(session, "theiaplatformsentinellevel", selected = rv$theiaplatformsentinellevel)
    } else if (rv$theiacollection == "venus") {
      updateRadioButtons(session, "theiaplatformvenus", selected = rv$theiaplatformvenus)
    }
    # product ####
    # saving options ####
    updateRadioButtons(session, "online", selected = rv$online)
    updateRadioButtons(session, "overwrite_product", selected = rv$overwrite_product)
    setProgress(0.3)
    # cloud mask ####
    updateRadioButtons(session, "atm_mask", selected = rv$atm_mask)
    updateSelectInput(session, "atm_mask_type", selected = rv$atm_mask_type)
    updateSliderInput(session, "max_masked_perc", value = rv$max_masked_perc)
    updateRadioButtons(session, "mask_apply_smooth", selected = rv$mask_apply_smooth)
    updateNumericInput(session, "mask_smooth", value = rv$mask_smooth)
    updateNumericInput(session, "mask_buffer", value = rv$mask_buffer)
    updateCheckboxGroupInput(session, "atm_mask_custom", selected = rv$atm_mask_custom)
    # processing options ####
    updateSelectInput(session, "processing_order", selected = rv$processing_order)
    updateRadioButtons(session, "parallel", selected = rv$parallel)
    updateSliderInput(session, "n_cores", value = rv$n_cores)
    # spatio-temporal selection ####
    if (anyNA(rv$timewindow)) {
      updateRadioButtons(session, "query_time", selected = FALSE)
    } else {
      updateRadioButtons(session, "query_time", selected = TRUE)
      updateDateRangeInput(session, "timewindow", start = rv$timewindow[1], end = rv$timewindow[2])
      updateRadioButtons(session, "timeperiod", selected = rv$timeperiod)
    }
    if (anyNA(rv$extent) & rv$online == FALSE) {
      updateRadioButtons(session, "query_space", selected = FALSE)
    } else {
      updateRadioButtons(session, "query_space", selected = TRUE)
    }
    # product ####
    setProgress(0.4)
    
    # update extent (at the end, not to interfer with other events
    # (the delay is required to update the map after the map is charged)
    # shinyjs::delay(5E3, {
      update_extent(extent_source = "imported", rv = rv, custom_source = rv$extent, session = session)
      updatePickerInput(session, "tiles_checkbox", selected = if(length(nn(rv$s2tiles_selected))>0) {rv$s2tiles_selected} else {NA})
      # rv$update_tiles_on_map <- sample(1E6, 1) # update the [un]selected tiles on the map
    # })
    updatePickerInput(session, "orbits_checkbox", selected = if(length(nn(rv$s2orbits_selected))>0) {rv$s2orbits_selected} else {NA})

    setProgress(1)
  })
}


#' create_return_list
#'
#'
#' @param rv Reactive value
#'
#' @return List
#' @importFrom geojsonio geojson_json
#' @importFrom sf st_transform
#' @export
#'
create_return_list <- function(rv) {
  
  req(rv)
  rl <- list()

  # Project name
  rl$project_name <- rv$project_name
  # Project directory
  rl$path_project <- rv$path_project
  # create repository if project don't exist
  rep <- file.path(rv$path_project, rv$project_name)
  if (!dir.exists(rep)) {
    dir.create(rep, showWarnings = FALSE, recursive = TRUE)
  }
  # name of path are paste from path_project + project_name
  rl$path_data <- if (rl$project_name != "") {
    res <- paste0(rep, "/data")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path of entire tiled products
  rl$path_tiles <- if (rl$project_name != "") {
    res <- paste0(rep, "/projets/", rv$project_name, "/tiles")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path of prediction calc
  rl$path_pred <- if (rl$project_name != "") {
    res <- paste0(rep, "/projets/", rv$project_name, "/pred/sdm")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE, recursive = TRUE)
    }
    res
  } else {
    NA
  }
  # path of mosaic calc
  rl$path_mosaic <- if (rl$project_name != "") {
    res <- paste0(rep, "/mosaic")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path of translate
  rl$path_translate <- if (rl$project_name != "") {
    res <- paste0(rep, "/translate")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path of merged
  rl$path_merged <- if (rl$project_name != "") {
    res <- paste0(rep, "/projets/", rv$project_name, "/merged")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path of tif
  rl$path_tif <- if (rl$project_name != "") {
    res <- paste0(rep, "/projets/", rv$project_name, "/tif")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path of warped
  rl$path_warped <- if (rl$project_name != "") {
    res <- paste0(rep, "/warped")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path of masked
  rl$path_masked <- if (rl$project_name != "") {
    res <- paste0(rep, "/masked")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path out calc
  rl$path_out <- if (rl$project_name != "") {
    res <- paste0(rep, "/projets/", rv$project_name, "/out")
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE)
    }
    res
  } else {
    NA
  }
  # path of rgb calc
  rl$path_rgb <- if (rl$project_name != "") {
    res <- paste0(rep, "/rgb")
    res2 <- paste0(rep, "/projets/", rv$project_name, "/rgb/jpg")
    if (!dir.exists(res) | !dir.exists(res2)) {
      dir.create(res, showWarnings = FALSE, recursive = TRUE)
      dir.create(res2, showWarnings = FALSE, recursive = TRUE)
    }
    res
  } else {
    NA
  }
  # path of spectral indices
  rl$path_indices <- if (rl$project_name != "") {
    res <- paste0(rep, "/indices")
    res2 <- paste0(rep, "/projets/", rv$project_name, "/indices/jpg")
    if (!dir.exists(res) | !dir.exists(res2)) {
      dir.create(res, showWarnings = FALSE, recursive = TRUE)
      dir.create(res2, showWarnings = FALSE, recursive = TRUE)
    }
    res
  } else {
    NA
  }

  # theia to download THEIA product, peps to download PEPS product
  # product ####
  rl$product <- rv$product
  # collection landsat, spotworldheritage, sentinel2, snow, venus
  # collection ####
  rl$theiacollection <- if (rl$product == "theia") {
    rv$theiacollection
  } else {
    NA
  }
  # s1, s2, s2st, s3
  rl$pepscollection <- if (rl$product == "peps") {
    rv$pepscollection
  } else {
    NA
  }
  # platform ####
  # landsat5, landsat7, landsat8
  rl$theiaplatformlandsat <- if (rl$product == "theia" & rl$theiacollection == "landsat") {
    rv$theiaplatformlandsat
  } else {
    NA
  }
  # spot1, spot2, spot3, spot4, spot5
  rl$theiaplatformspotworldheritage <- if (rl$product == "theia" & rl$theiacollection == "spotworldheritage") {
    rv$theiaplatformspotworldheritage
  } else {
    NA
  }
  # s2a, s2b
  rl$theiaplatformsentinel <- if (rl$product == "theia" & rl$theiacollection == "sentinel2") {
    rv$theiaplatformsentinel
  } else {
    NA
  }
  # venus
  rl$theiaplatformvenus <- if (rl$product == "theia" & rl$theiacollection == "venus") {
    rv$theiaplatformvenus
  } else {
    NA
  }
  # level ####
  rl$theiaplatformsentinellevel <- if (rl$product == "theia" & rl$theiacollection == "sentinel2") {
    rv$theiaplatformsentinellevel
  } else {
    NA
  }
  # saving options ####
  rl$online <- as.logical(rv$online) # TRUE if online mode, FALSE if offline mode
  rl$overwrite_product <- as.logical(rv$overwrite_product) # TRUE to overwrite existing product, FALSE otherwise
  # cloud mask ####
  rl$atm_mask <- as.logical(rv$atm_mask) # TRUE if atm_mask mode, FALSE otherwise
  rl$atm_mask_type <- rv$atm_mask_type
  rl$atm_mask_custom <- rv$atm_mask_custom
  rl$max_masked_perc <- rv$max_masked_perc
  rl$mask_apply_smooth <- as.logical(rv$mask_apply_smooth)
  rl$mask_smooth <- rv$mask_smooth
  rl$mask_buffer <- rv$mask_buffer
  # processing options ####
  rl$processing_order <- rv$processing_order
  rl$parallel <- as.logical(rv$parallel)
  rl$n_cores <- rv$n_cores
  # temporal parameters ####
  rl$query_time <- rv$query_time
  # spatio-temporal selection #
  rl$timewindow <- if (rv$query_time == TRUE) { # range of dates
    rv$timewindow
  } else {
    NA
  }
  rl$timeperiod <- if (rv$query_time == TRUE) { # range of dates
    rv$timeperiod # "full" or "seasonal"
  } else {
    "full"
  }
 
  # polygon extent
  rl$extent <- if (!is.null(rv$extent) & !is.na(rv$extent)) {
    rv$extent %>%
      st_transform(4326) %>%
      geojson_json(pretty = TRUE)
  } else {
    NA
  }
  # selected tile IDs
  rl$s2tiles_selected <- if (rv$query_space == TRUE & length(nn(rv$tiles_checkbox)>0)) {
    rv$tiles_checkbox
  } else {
    NA
  } 
  # selected orbit IDs
  rl$s2orbits_selected <- if (rv$query_space == TRUE & length(nn(rv$orbits_checkbox)>0)) {
    rv$orbits_checkbox
  } else {
    NA
  } 

  return(rl)
}

#' check_param
#'
#' @param param_list List param
#' @param rv Reactive values
#'
#' @return True or False
#' @importFrom shiny.i18n Translator
#' @export
#'
check_param <- function(param_list, rv) {
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  error_list <- check_param_list(param_list, type = "string", correct = FALSE)
  if (!is.null(error_list)) {
    # if errors occur:
    # build modal dialog
    check_param_modal <- modalDialog(
      title = i18n$t("Parameter errors"),
      size = "m",
      if (length(error_list) == 1) {
        tagList(
          p(
            i18n$t("A parameter has not been correctly set:"),
            br(), error_list
          ),
          p(i18n$t("Please edit it using the GUI before continuing."))
        )
      } else {
        tagList(
          p(HTML(
            i18n$t("Some parameters have not been correctly set:"),
            "<ul><li>",
            paste(error_list, collapse = "</li><li>"),
            "</li></ul>"
          )),
          p(i18n$t("Please edit them using the GUI before continuing."))
        )
      },
      easyClose = TRUE,
      footer = NULL
    )
    # show modal dialog
    showModal(check_param_modal)
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' @title Check a parameter list
#' @description Check that the parameter list (or JSON parameter file)
#'  is in the correct format, and then speficied values are coherent with
#'  parameters.
#' @param pm List of parameters or path of a JSON parameter file.
#' @param type Type of the output (see [print_message] for details).
#' @param correct Logical: if TRUE (default), the function corrects
#'  some incoherences (e.g. timewindow of length 1 is transformed in length 2)
#'  and returns the corrected list as output; if false, only checking is
#'  performed, and the output is NULL if no errors occur.
#' @return In case of errors, depending on `type` argument, output can be
#'  a vector of errors (if `type = 'string'`),
#'  the first error occurred (if `type = 'error'`)
#'  or a set of warnings (if `type = 'warning'`).
#'  If no errors occur, output is the corrected parameter list if
#'  `correct = TRUE` or NULL otherwise.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom shiny showModal
#' @importFrom methods is
#' @importFrom stringr str_pad
#' @importFrom shiny.i18n Translator
#' @author Pascal Obstetar (2020) \email{pascal.obstetar@@onf.fr}
#' @note License: GPL 3.0
#' @export
check_param_list <- function(pm, type = "string", correct = TRUE) {
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  # to avoid NOTE on check
  . <- NULL

  # check the format of pm object
  if (is(pm, "character")) {
    if (file.exists(pm)) {
      # load json parameter file
      pm <- jsonlite::fromJSON(pm)
    } else {
      shiny::showModal(modalDialog(
        title = i18n$t("Unable to download"),
        paste0(i18n$t("The file "), pm, i18n$t(" does not exist.")),
        easyClose = TRUE,
        footer = NULL
      ))
      correct <- FALSE
    }
  } else if (!is(pm, "list")) {
    shiny::showModal(modalDialog(
      title = i18n$t("Unable to download"),
      paste0("\"", deparse(substitute(pm)), "\"", i18n$t("must be a list or a path of a JSON parameter file.")),
      easyClose = TRUE,
      footer = NULL
    ))
    correct <- FALSE
  }

  # TODO check the names of the content of the list

  # check is empty project name
  if (pm$project_name == "") {
    shiny::showModal(modalDialog(
      title = i18n$t("Unable to download"),
      i18n$t("The name of project is empty!"),
      easyClose = TRUE,
      footer = NULL
    ))
    correct <- FALSE
  }

  if (correct == TRUE) {
    return(pm)
  } else {
    return(invisible(NULL))
  }
}
