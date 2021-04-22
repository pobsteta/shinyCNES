#' srtm
#'
#' @param progress_bar Progress bar if shiny run
#' @param time_interval Time interval
#' @param spatial_extent Spatial extent
#' @param session Session
#' @param path_project Path project
#' @param path_tif Path tif
#' @param srtm_multiple Multiple SRTM
#'
#' @return
#' @export
#'
srtm <- function(session,
                 path_project,
                 path_tif,
                 srtm_multiple = "9,15,33",
                 time_interval = NULL,
                 spatial_extent = NULL,
                 progress_bar = TRUE) {
  # login to NASA/ESA services
  apiesa <- read_esa_login()
  username <- apiesa[1]
  password <- apiesa[2]
  getSpatialData::login_earthdata(username = username, password = password)

  # spatial extent set AOI
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
    # Define an area of interest (AOI):
    aoi <- spatial_extent %>%
      sf::st_geometry() %>%
      sf::st_transform("EPSG:2154")
    getSpatialData::set_aoi(aoi)
    # Spatial mask of extent in ESPG 4326 (WGS84)
    extent_aoi_4326 <- sf::st_bbox(aoi %>% st_transform(4326))
  }

  # time interval
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

  if (progress_bar) {
    i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
    i18n$set_translation_language("fr")
    # Add a progress bar while importing
    withProgress(message = i18n$t("Connect to NASA server..."), value = 0, {
      # get records
      records <- getSpatialData::get_records(
        time_range = c(time_intervals$start, time_intervals$end),
        products = c("SRTM_global_1arc_V001")
      )

      # Download and georeference the previews for all records:
      records <- getSpatialData::get_data(records, dir_out = file.path(path_project))
      setProgress(1)
    }) # end withprogress
  } else {
    # get records
    message("Download NASA SRTM...")
    records <- getSpatialData::get_records(
      time_range = c(time_intervals$start, time_intervals$end),
      products = c("SRTM_global_1arc_V001")
    )

    # Download and georeference the previews for all records:
    records <- getSpatialData::get_data(records, dir_out = file.path(path_project), as_sf = FALSE)
  }

  list_zip <- records$dataset_file %>% unlist(use.names = FALSE)

  # unzip all files
  list_hgt <- NULL
  for (f in seq_along(list_zip)) {
    lf <- grep("\\.hgt$", unzip(list_zip[f], list = TRUE)$Name, value = TRUE)
    unzip(list_zip[f], files = lf, list = FALSE, exdir = dirname(list_zip[f]))
    list_hgt <- c(list_hgt, lf)
  }

  # list all hgt
  list_hgt <- as.vector(outer(dirname(list_zip[f]), list_hgt, paste, sep = "/"))

  # build a vrt file
  output.vrt <- paste(tempfile(), ".vrt", sep = "")
  gdalUtils::gdalbuildvrt(
    gdalfile = list_hgt, 
    output.vrt = output.vrt, 
    separate = FALSE,
    verbose = FALSE,
    overwrite = TRUE)
  
  # read the vrt
  s <- stars::read_stars(output.vrt)
  
  # crop
  lc <- s[extent_aoi_4326]
  names(lc) <- "altitude.tif"
  
  if (progress_bar) {
    i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
    i18n$set_translation_language("fr")
    # Add a progress bar while importing
    # Altitude
    withProgress(message = i18n$t("Write altitude.tif..."), value = 0, {
      # write the tif
      stars::write_stars(lc, dsn = file.path(path_tif, names(lc)), driver = "GTiff", progress = TRUE, update = FALSE)
      setProgress(1)
    })
    out <- "altitude"
    # Exposition
    withProgress(message = i18n$t("Write exposition.tif..."), value = 0, {
      exposition <- gdalUtils::gdaldem(
        mode = "aspect",
        input_dem = file.path(path_tif, "altitude.tif"),
        output = file.path(path_tif, "exposition.tif"),
        output_Raster = TRUE,
        alg = "ZevenbergenThorne",
        b = 1,
        compute_edges = TRUE,
        zero_for_flat = TRUE,
        verbose = TRUE
      )
    setProgress(1)
    })
    out <- c(out, "exposition")
    
    # Pente
    withProgress(message = i18n$t("Write pente.tif..."), value = 0, {
      pente <- gdalUtils::gdaldem(
        mode = "slope",
        input_dem = file.path(path_tif, "altitude.tif"),
        output = file.path(path_tif, "pente.tif"),
        output_Raster = TRUE,
        alg = "ZevenbergenThorne",
        compute_edges = TRUE,
        b = 1,
        s = 1.0,
        p = TRUE,
        verbose = TRUE
      )
      setProgress(1)
    })
    out <- c(out, "pente")
    
    # Ombrage
    withProgress(message = i18n$t("Write ombrage.tif..."), value = 0, {
      ombrage <- gdalUtils::gdaldem(
        mode = "hillshade",
        input_dem = file.path(path_tif, "altitude.tif"),
        output = file.path(path_tif, "ombrage.tif"),
        output_Raster = TRUE,
        alg = "ZevenbergenThorne",
        b = 1,
        z = 1.0,
        s = 1.0,
        alt = 45.0,
        verbose = TRUE
      )
      setProgress(1)
    })
    out <- c(out, "ombrage")
    
    # Rugosite
    withProgress(message = i18n$t("Write rugosite.tif..."), value = 0, {
      rugosite <- gdalUtils::gdaldem(
        mode = "roughness",
        input_dem = file.path(path_tif, "altitude.tif"),
        output = file.path(path_tif, "rugosite.tif"),
        output_Raster = TRUE,
        compute_edges = TRUE,
        verbose = TRUE
      )
      setProgress(1)
    })
    out <- c(out, "rugosite")
    
    # TRI
    withProgress(message = i18n$t("Write tri.tif..."), value = 0, {
      tri <- gdalUtils::gdaldem(
        mode = "TRI",
        input_dem = file.path(path_tif, "altitude.tif"),
        output = file.path(path_tif, "tri.tif"),
        output_Raster = TRUE,
        compute_edges = TRUE,
        b = 1,
        verbose = TRUE
      )
      setProgress(1)
    })
    out <- c(out, "tri")
    
    # TPI
    withProgress(message = i18n$t("Write tpi.tif..."), value = 0, {
      tpi <- gdalUtils::gdaldem(
        mode = "TPI",
        input_dem = file.path(path_tif, "altitude.tif"),
        output = file.path(path_tif, "tpi.tif"),
        output_Raster = TRUE,
        compute_edges = TRUE,
        b = 1,
        verbose = TRUE
      )
      setProgress(1)
    })
    out <- c(out, "tpi")
    
    # Geomorphon
    withProgress(message = i18n$t("Write geomorphon.tif..."), value = 0, {
      geomorphon <- qgis::grass7_r_geomorphon(
        elevation = file.path(path_tif, "altitude.tif"),
        forms = file.path(path_tif, "geomorphon.tif")
      )
      setProgress(1)
    })
    out <- c(out, "geomorphon")
      
    #### list multiple
    s <- strsplit(srtm_multiple, ",")[[1]]
    
    # Pente
    withProgress(message = i18n$t("Write multiple pente..."), value = 0, {
      # calc mutiple pente indices
      for (r in seq(s)) {
        incProgress(1 / length(s), detail = paste0("Write pente", s[r], ".tif (", r, "/", length(s), ")"))
        Sys.sleep(0.25)
        pentem <- qgis::grass7_r_neighbors(
          input = pente,
          method = "average",
          size = as.integer(s[r]),
          .c = TRUE,
          output = file.path(path_tif, paste0("pente", s[r], ".tif"))
        )
        out <- c(out, paste0("pente", s[r]))
      } # end for
    })
    
    # Rugosite
    withProgress(message = i18n$t("Write multiple rhougness..."), value = 0, {
      # calc mutiple rhougness indices
      for (r in seq(s)) {
        incProgress(1 / length(s), detail = paste0("Write rhougness", s[r], ".tif (", r, "/", length(s), ")"))
        Sys.sleep(0.25)
        rugositem <- qgis::grass7_r_neighbors(
          input = rugosite,
          method = "average",
          size = as.integer(s[r]),
          .c = TRUE,
          output = file.path(path_tif, paste0("rugosite", s[r], ".tif"))
        )
        out <- c(out, paste0("rugosite", s[r]))
      } # end for
    })
    # TRI
    withProgress(message = i18n$t("Write multiple tri..."), value = 0, {
      # calc mutiple rhougness indices
      for (r in seq(s)) {
        incProgress(1 / length(s), detail = paste0("Write tri", s[r], ".tif (", r, "/", length(s), ")"))
        Sys.sleep(0.25)
        trim <- qgis::grass7_r_neighbors(
          input = tri,
          method = "average",
          size = as.integer(s[r]),
          .c = TRUE,
          output = file.path(path_tif, paste0("tri", s[r], ".tif"))
        )
        out <- c(out, paste0("tri", s[r]))
      } # end for
    })
    # TPI
    withProgress(message = i18n$t("Write multiple tpi..."), value = 0, {
      # calc mutiple rhougness indices
      for (r in seq(s)) {
        incProgress(1 / length(s), detail = paste0("Write tpi", s[r], ".tif (", r, "/", length(s), ")"))
        Sys.sleep(0.25)
        tpim <- qgis::grass7_r_neighbors(
          input = tpi,
          method = "average",
          size = as.integer(s[r]),
          .c = TRUE,
          output = file.path(path_tif, paste0("tpi", s[r], ".tif"))
        )
        out <- c(out, paste0("tpi", s[r]))
      } # end for
    })
    # Geomorphon
    withProgress(message = i18n$t("Write multiple geomorphon..."), value = 0, {
      # calc mutiple rhougness indices
      for (r in seq(s)) {
        incProgress(1 / length(s), detail = paste0("Write geomorphon", s[r], ".tif (", r, "/", length(s), ")"))
        Sys.sleep(0.25)
        geomorphonm <- qgis::grass7_r_geomorphon(
          elevation = file.path(path_tif, "altitude.tif"),
          forms = file.path(path_tif, paste0("geomorphon", s[r], ".tif")),
          search = as.integer(s[r])
        )
        out <- c(out, paste0("geomorphon", s[r]))
      } # end for
    })
    
  } else {
    message("Write altitude.tif...")
    stars::write_stars(lc, dsn = file.path(path_tif, names(lc)), driver = "GTiff", progress = TRUE, update = FALSE)
    out <- "altitude"
    message("Write exposition.tif...")
    exposition <- gdalUtils::gdaldem(
      mode = "aspect",
      input_dem = file.path(path_tif, "altitude.tif"),
      output = file.path(path_tif, "exposition.tif"),
      output_Raster = TRUE,
      alg = "ZevenbergenThorne",
      b = 1,
      compute_edges = TRUE,
      zero_for_flat = TRUE,
      verbose = TRUE
    )
    out <- c(out, "exposition")
    message("Write ombrage.tif...")
    ombrage <- gdalUtils::gdaldem(
      mode = "hillshade",
      input_dem = file.path(path_tif, "altitude.tif"),
      output = file.path(path_tif, "ombrage.tif"),
      output_Raster = TRUE,
      alg = "ZevenbergenThorne",
      b = 1,
      z = 1.0,
      s = 1.0,
      alt = 45.0,
      verbose = TRUE
    )
    out <- c(out, "ombrage")
    message("Write rugosite.tif...")
    rugosite <- gdalUtils::gdaldem(
      mode = "roughness",
      input_dem = file.path(path_tif, "altitude.tif"),
      output = file.path(path_tif, "rugosite.tif"),
      output_Raster = TRUE,
      compute_edges = TRUE,
      verbose = TRUE
    )
    out <- c(out, "rugosite")
    # TRI
    message("Write tri.tif...")
    tri <- gdalUtils::gdaldem(
      mode = "TRI",
      input_dem = file.path(path_tif, "altitude.tif"),
      output = file.path(path_tif, "tri.tif"),
      output_Raster = TRUE,
      compute_edges = TRUE,
      b = 1,
      verbose = TRUE
    )
    out <- c(out, "tri")
    # TPI
    message("Write tpi.tif...")
    tpi <- gdalUtils::gdaldem(
      mode = "TPI",
      input_dem = file.path(path_tif, "altitude.tif"),
      output = file.path(path_tif, "tpi.tif"),
      output_Raster = TRUE,
      compute_edges = TRUE,
      b = 1,
      verbose = TRUE
    )
    out <- c(out, "tpi")
    message("Write geomorphon.tif...")
    geomorphon <- qgis::grass7_r_geomorphon(
      elevation = file.path(path_tif, "altitude.tif"),
      forms = file.path(path_tif, "geomorphon.tif")
    )
    out <- c(out, "geomorphon")
    #### list multiple
    s <- strsplit(srtm_multiple, ",")[[1]]
    # calc mutiple rhougness indices
    s <- strsplit(srtm_multiple, ",")[[1]]
    for (r in seq(s)) {
      message(paste0("Write rugosite", s[r], ".tif..."))
      rugositem <- qgis::grass7_r_neighbors(
        input = rugosite,
        method = "average",
        size = as.integer(s[r]),
        .c = TRUE,
        output = file.path(path_tif, paste0("rugosite", s[r], ".tif"))
      )
      out <- c(out, paste0("rugosite", s[r]))
    }
    
    for (r in seq(s)) {
      message(paste0("Write tri", s[r], ".tif..."))
      trim <- qgis::grass7_r_neighbors(
        input = tri,
        method = "average",
        size = as.integer(s[r]),
        .c = TRUE,
        output = file.path(path_tif, paste0("tri", s[r], ".tif"))
      )
      out <- c(out, paste0("tri", s[r]))
    } # end for
    
    # TPI
    # calc mutiple rhougness indices
    for (r in seq(s)) {
      message(paste0("Write tpi", s[r], ".tif..."))
      tpim <- qgis::grass7_r_neighbors(
        input = tpi,
        method = "average",
        size = as.integer(s[r]),
        .c = TRUE,
        output = file.path(path_tif, paste0("tpi", s[r], ".tif"))
      )
      out <- c(out, paste0("tpi", s[r]))
    } # end for

    # Geomorphon
    # calc mutiple rhougness indices
    for (r in seq(s)) {
      message(paste0("Write geomorphon", s[r], ".tif..."))
      geomorphonm <- qgis::grass7_r_geomorphon(
        elevation = file.path(path_tif, "altitude.tif"),
        forms = file.path(path_tif, paste0("geomorphon", s[r], ".tif")),
        search = as.integer(s[r])
      )
      out <- c(out, paste0("geomorphon", s[r]))
    } # end for
    
  }
  return(out)
}
