#' @title s2_list
#'
#' @param spatial_extent Spatial extent
#' @param tile Tiles
#' @param orbit Orbit
#' @param time_interval Time interval
#' @param time_period Time period
#' @param level Level
#' @param platform Plateform
#' @param maxcloud Max cloud cover
#' @param collection Collection
#' @param path_to_download Path to the project
#'
#' @return List of products
#' @export
#'
#' @importFrom theiaR TheiaCollection
s2_list <- function(session,
                    spatial_extent = NULL,
                    tiles = NULL,
                    orbit = NULL, # spatial parameters
                    time_interval = NULL,
                    time_period = "full", # temporal parameters
                    level = "L2A",
                    platform = "SENTINEL2A",
                    maxcloud = 101,
                    collection = "SENTINEL",
                    path_to_download = "~",
                    download = TRUE,
                    project_name = NULL,
                    progress_bar = TRUE) {
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  # search theiaR path
  # myauth, ce fichier contient deux lignes, la premiere est l'ID pour
  # se connecter et la deuxieme, le mot de passe. inscription sur
  # https://sso.theia-land.fr/theia/register/register.xhtml
  theia_download <- find.package("theiaR")
  myauth <- file.path(theia_download, "auth_theia.txt")

  if (is.null(spatial_extent)) {
    if (progress_bar) {
      sendSweetAlert(
        session,
        title = i18n$t("Spatial_extent is NULL !"),
        text = paste(
          i18n$t("Please select a gpkg polygon!")
        ),
        type = "error",
        btn_labels = "Ok"
      )
    } else {
      print_message(
        type = "message",
        i18n$t("Spatial_extent is NULL !")
      )
    }
    return(NULL)
  } else {
    spatext <- spatial_extent %>%
      sf::st_geometry() %>%
      sf::st_bbox()
  }

  # pass lat,lon if the bounding box is a point or line; latmin,latmax,lonmin,lonmax if it is a rectangle
  if (spatext["xmin"] == spatext["xmax"] || spatext["ymin"] == spatext["ymax"]) {
    lon <- mean(spatext["xmin"], spatext["xmax"])
    lat <- mean(spatext["ymin"], spatext["ymax"])
    lonmi <- lonma <- latmi <- latma <- NULL
  } else {
    lonmi <- spatext["xmin"]
    lonma <- spatext["xmax"]
    latmi <- spatext["ymin"]
    latma <- spatext["ymax"]
    lon <- lat <- NULL
  }

  # checks on dates
  # TODO add checks on format
  if (length(time_interval) == 1) {
    time_interval <- rep(time_interval, 2)
  }
  # split time_interval in case of seasonal download
  time_intervals <- if (time_period == "full") {
    data.frame(
      "start" = strftime(time_interval[1], "%Y-%m-%d"),
      "end" = strftime(time_interval[2], "%Y-%m-%d"),
      stringsAsFactors = FALSE
    )
  } else if (time_period == "seasonal") {
    data.frame(
      "start" = strftime(seq(time_interval[1], time_interval[2], by = "year"), "%Y-%m-%d"),
      "end" = strftime(rev(seq(time_interval[2], time_interval[1], by = "-1 year")), "%Y-%m-%d"),
      stringsAsFactors = FALSE
    )
  }

  # convert orbits to integer
  if (is.null(orbit)) {
    orb <- list(NULL)
  } else {
    orb <- as.integer(gsub(".(...)$", "\\1", orbit))
    if (anyNA(orbit)) {
      orb <- list(NULL)
    }
  }

  # set level
  lev <- switch(level,
    l1c = "LEVEL1C",
    l2a = "LEVEL2A",
    l3a = "LEVEL3A",
    "LEVEL2A"
  )

  # set collection
  col <- switch(collection,
    landsat = "LANDSAT",
    spotworldheritage = "SpotWorldHeritage",
    sentinel2 = "SENTINEL2",
    snow = "Snow",
    venus = "VENUS",
    "SENTINEL2"
  )

  # set platform
  pla <- switch(platform,
    landsat5 = "LANDSAT5",
    landsat7 = "LANDSAT7",
    landsat8 = "LANDSAT8",
    spot1 = "SPOT1",
    spot2 = "SPOT2",
    spot3 = "SPOT3",
    spot4 = "SPOT4",
    spot5 = "SPOT5",
    s2a = "SENTINEL2A",
    s2b = "SENTINEL2B",
    venus = "VENUS",
    "SENTINEL2A"
  )

  # create a list containing the query
  if (level == "l3a") {
    myquery <- list(
      collection = col,
      level = lev,
      latmin = latmi,
      latmax = latma,
      lonmin = lonmi,
      lonmax = lonma,
      start.date = time_intervals$start,
      end.date = time_intervals$end
    )
  } else {
    myquery <- list(
      collection = col,
      platform = pla,
      level = lev,
      latmin = latmi,
      latmax = latma,
      lonmin = lonmi,
      lonmax = lonma,
      start.date = time_intervals$start,
      end.date = time_intervals$end
    )
  }
  
  
  if (progress_bar) {
    # Add a progress bar while importing
    withProgress(message = i18n$t("Connect to THEIA server..."), value = 0, {
      # connexion au serveur THEIA
      mycollection <- tryCatch(theiaR::TheiaCollection$new(
        query = myquery,
        dir.path = file.path(path_to_download, "data"),
        check = TRUE
      ),
      error = function(e) print(i18n$t("No tiles matching search criteria!"))
      )
      setProgress(value = 1)
    })
  } else {
    # connexion au serveur THEIA
    mycollection <- tryCatch(theiaR::TheiaCollection$new(
      query = myquery,
      dir.path = file.path(path_to_download, "data"),
      check = TRUE
    ),
    error = function(e) print("No tiles matching search criteria!")
    )
  }
  
  if (class(mycollection)[1] == "TheiaCollection") {
    # filter mycollection with tiles
    out <- mycollection$status %>%
      filter(grepl(paste(tiles, collapse = "|"), tile))
  } else {
    if (progress_bar) {
      sendSweetAlert(
        session,
        title = i18n$t("THEIA search"),
        text = paste(
          i18n$t("No tiles matching search criteria!")
        ),
        type = "error",
        btn_labels = "Ok"
      )
    } else {
      print_message(
        type = "message",
        i18n$t("No tiles matching search criteria!")
      )
    }
    return(NULL)
  }

  if (download) {
    # telechargement des dalles si elles ne sont pas encore telechargees
    files <- out
    w <- getOption("warn")
    on.exit(options("warn" = w))
    options("warn" = -1)
    if (progress_bar) {
      # Add a progress bar while importing
      withProgress(message = i18n$t("Loading tiles..."), value = 0, {
        for (f in seq(1:nrow(files))) {
          if (files$correct[f] == TRUE) {
            # Increment the progress bar, and update the detail text.
            incProgress(1 / nrow(files), detail = paste(files$tile[f], i18n$t(".zip is already downloaded ! ("), f, "/", nrow(files), ")"))
            Sys.sleep(0.25)
          } else {
            # Increment the progress bar, and update the detail text.
            incProgress(1 / nrow(files), detail = paste(files$tile[f], i18n$t(".zip is being downloaded...("), f, "/", nrow(files), ")"))
            Sys.sleep(0.25)
            tryCatch(mycollection$download(auth = myauth),
              error = function(e) print(paste(files$tile[f], i18n$t("did not downloaded ! ("), f, "/", nrow(files), ")"))
            )
          }
        } # end for
      }) # end withprogress
    } else {
      for (f in seq(1:nrow(files))) {
        if (files$correct[f] == TRUE) {
          message(paste(files$tile[f], ".zip is already downloaded ! ("), f, "/", nrow(files), ")")
        } else {
          message(paste(files$tile[f], ".zip is being downloaded...("), f, "/", nrow(files), ")")
          tryCatch(mycollection$download(auth = myauth),
            error = function(e) print(paste(files$tile[f], "did not downloaded ! ("), f, "/", nrow(files), ")")
          )
        }
      } # end for
    }
  } # endif

  # affichage du statut des dalles a telecharger
  return(out)
}
