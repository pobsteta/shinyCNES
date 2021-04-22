#' srtm
#'
#' @param progress_bar Progress bar if shiny run
#' @param time_interval Time interval
#' @param spatial_extent Spatial extent
#' @param session Session
#' @param path_project Path project
#' @param path_tif Path tif
#' @param srtm_multiple Multiple SRTM
#' @param list_indice List of derivated indices. Default vector from 'exposition' and 'ombrage'
#'
#' @return
#' @export
#'
srtm <- function(session,
                 path_project,
                 path_tif,
                 list_indice = c("exposition", "ombrage"),
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
      i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
      i18n$set_translation_language("fr")
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
        "Spatial_extent is NULL !"
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
    overwrite = TRUE
  )

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
    if ("exposition" %in% list_indice) {
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
    }

    # Pente
    if ("pente" %in% list_indice) {
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
    }

    # Ombrage
    if ("ombrage" %in% list_indice) {
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
    }

    # Rugosite
    if ("rigosite" %in% list_indice) {
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
    }

    # TRI
    if ("TRI" %in% list_indice) {
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
    }

    # TPI
    if ("TPI" %in% list_indice) {
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
    }

    # Geomorphon
    if ("geomorphon" %in% list_indice) {
      withProgress(message = i18n$t("Write geomorphon.tif..."), value = 0, {
        geomorphon <- qgis::grass7_r_geomorphon(
          elevation = file.path(path_tif, "altitude.tif"),
          forms = file.path(path_tif, "geomorphon.tif")
        )
        setProgress(1)
      })
      out <- c(out, "geomorphon")
    }

    #### RSAGA
    env <- rsaga.env()
    # TWI
    if ("TWI" %in% list_indice) {
      withProgress(message = i18n$t("Write twi.tif..."), value = 0, {
        RSAGA::rsaga.wetness.index(
          in.dem = file.path(path_tif, "altitude.tif"),
          out.wetness.index = file.path(tempdir(), "twi"),
          env = env
        )
        twi <- raster::raster(file.path(tempdir(), "twi.sdat"))
        raster::writeRaster(twi, 
                            filename = file.path(path_tif, "twi.tif"),
                            overwrite = TRUE
        )
        setProgress(1)
      })
      out <- c(out, "twi")
    }

    # plan curvature
    if ("cplan" %in% list_indice) {
      withProgress(message = i18n$t("Write cplan.tif..."), value = 0, {
        RSAGA::rsaga.plan.curvature(
          in.dem = file.path(path_tif, "altitude.tif"),
          out.hcurv = file.path(tempdir(), "cplan"),
          method = "poly2zevenbergen",
          env = env
        )
        cplan <- raster::raster(file.path(tempdir(), "cplan.sdat"))
        raster::writeRaster(cplan, 
                            filename = file.path(path_tif, "cplan.tif"),
                            overwrite = TRUE
        )
        setProgress(1)
      })
      out <- c(out, "cplan")
    }

    # profile curvature
    if ("cprof" %in% list_indice) {
      withProgress(message = i18n$t("Write cprof.tif..."), value = 0, {
        RSAGA::rsaga.profile.curvature(
          in.dem = file.path(path_tif, "altitude.tif"),
          out.vcurv = file.path(tempdir(), "cprof"),
          method = "poly2zevenbergen",
          env = env
        )
        cprof <- raster::raster(file.path(tempdir(), "cprof.sdat"))
        raster::writeRaster(cprof, 
                            filename = file.path(path_tif, "cprof.tif"),
                            overwrite = TRUE
        )
        setProgress(1)
      })
      out <- c(out, "cprof")
    }

    # contributing area
    if ("carea" %in% list_indice) {
      withProgress(message = i18n$t("Write carea.tif..."), value = 0, {
        RSAGA::rsaga.topdown.processing(
          in.dem = file.path(path_tif, "altitude.tif"),
          out.carea = file.path(tempdir(), "carea"),
          method = "mfd",
          env = env
        )
        carea <- raster::raster(file.path(tempdir(), "carea.sdat"))
        raster::writeRaster(carea, 
                            filename = file.path(path_tif, "carea.tif"),
                            overwrite = TRUE
        )
        setProgress(1)
      })
      out <- c(out, "carea")
    }

    # log10_carea
    if ("log10_carea" %in% list_indice) {
      withProgress(message = i18n$t("Write log10_carea.tif..."), value = 0, {
        RSAGA::rsaga.topdown.processing(
          in.dem = file.path(path_tif, "altitude.tif"),
          out.carea = file.path(tempdir(), "carea"),
          method = "mfd",
          env = env
        )
        RSAGA::rsaga.grid.calculus(
          in.grids = file.path(tempdir(), "carea"),
          out.grid = file.path(tempdir(), "log10_carea"),
          formula = ~ log(a),
          env = env
        )
        log10_carea <- raster::raster(file.path(tempdir(), "log10_carea.sdat"))
        raster::writeRaster(log10_carea, 
                            filename = file.path(path_tif, "log10_carea.tif"),
                            overwrite = TRUE
        )
        setProgress(1)
      })
      out <- c(out, "log10_carea")
    }

    #### list multiple
    s <- strsplit(srtm_multiple, ",")[[1]]

    # Exposition
    if ("exposition" %in% list_indice) {
      withProgress(message = i18n$t("Write multiple exposition..."), value = 0, {
        # calc mutiple pente indices
        for (r in seq(s)) {
          incProgress(1 / length(s), detail = paste0("Write exposition", s[r], ".tif (", r, "/", length(s), ")"))
          Sys.sleep(0.25)
          expositionm <- qgis::grass7_r_neighbors(
            input = exposition,
            method = "average",
            size = as.integer(s[r]),
            .c = TRUE,
            output = file.path(path_tif, paste0("exposition", s[r], ".tif"))
          )
          out <- c(out, paste0("exposition", s[r]))
        } # end for
      })
    }

    # Pente
    if ("pente" %in% list_indice) {
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
    }

    # Ombrage
    if ("ombrage" %in% list_indice) {
      withProgress(message = i18n$t("Write multiple ombrage..."), value = 0, {
        # calc mutiple ombrage indices
        for (r in seq(s)) {
          incProgress(1 / length(s), detail = paste0("Write ombrage", s[r], ".tif (", r, "/", length(s), ")"))
          Sys.sleep(0.25)
          pentem <- qgis::grass7_r_neighbors(
            input = ombrage,
            method = "average",
            size = as.integer(s[r]),
            .c = TRUE,
            output = file.path(path_tif, paste0("ombrage", s[r], ".tif"))
          )
          out <- c(out, paste0("ombrage", s[r]))
        } # end for
      })
    }

    # Rugosite
    if ("rugosite" %in% list_indice) {
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
    }

    # TRI
    if ("TRI" %in% list_indice) {
      withProgress(message = i18n$t("Write multiple tri..."), value = 0, {
        # calc mutiple TRI indices
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
    }

    # TPI
    if ("TPI" %in% list_indice) {
      withProgress(message = i18n$t("Write multiple tpi..."), value = 0, {
        # calc mutiple TPI indices
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
    }

    # Geomorphon
    if ("geomorphon" %in% list_indice) {
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
    }

    # TWI
    if ("TWI" %in% list_indice) {
      withProgress(message = i18n$t("Write multiple twi..."), value = 0, {
        # calc mutiple TWI indices
        for (r in seq(s)) {
          incProgress(1 / length(s), detail = paste0("Write twi", s[r], ".tif (", r, "/", length(s), ")"))
          Sys.sleep(0.25)
          twim <- qgis::grass7_r_neighbors(
            input = twi,
            method = "average",
            size = as.integer(s[r]),
            .c = TRUE,
            output = file.path(path_tif, paste0("twi", s[r], ".tif"))
          )
          out <- c(out, paste0("twi", s[r]))
        } # end for
      })
    }

    # cplan
    if ("cplan" %in% list_indice) {
      withProgress(message = i18n$t("Write multiple cplan..."), value = 0, {
        # calc mutiple cplan indices
        for (r in seq(s)) {
          incProgress(1 / length(s), detail = paste0("Write cplan", s[r], ".tif (", r, "/", length(s), ")"))
          Sys.sleep(0.25)
          twim <- qgis::grass7_r_neighbors(
            input = cplan,
            method = "average",
            size = as.integer(s[r]),
            .c = TRUE,
            output = file.path(path_tif, paste0("cplan", s[r], ".tif"))
          )
          out <- c(out, paste0("cplan", s[r]))
        } # end for
      })
    }

    # carea
    if ("carea" %in% list_indice) {
      withProgress(message = i18n$t("Write multiple carea..."), value = 0, {
        # calc mutiple carea indices
        for (r in seq(s)) {
          incProgress(1 / length(s), detail = paste0("Write carea", s[r], ".tif (", r, "/", length(s), ")"))
          Sys.sleep(0.25)
          twim <- qgis::grass7_r_neighbors(
            input = carea,
            method = "average",
            size = as.integer(s[r]),
            .c = TRUE,
            output = file.path(path_tif, paste0("carea", s[r], ".tif"))
          )
          out <- c(out, paste0("carea", s[r]))
        } # end for
      })
    }

    # log10_carea
    if ("log10_carea" %in% list_indice) {
      withProgress(message = i18n$t("Write multiple log10_carea..."), value = 0, {
        # calc mutiple log10_carea indices
        for (r in seq(s)) {
          incProgress(1 / length(s), detail = paste0("Write log10_carea", s[r], ".tif (", r, "/", length(s), ")"))
          Sys.sleep(0.25)
          twim <- qgis::grass7_r_neighbors(
            input = log10_carea,
            method = "average",
            size = as.integer(s[r]),
            .c = TRUE,
            output = file.path(path_tif, paste0("log10_carea", s[r], ".tif"))
          )
          out <- c(out, paste0("log10_carea", s[r]))
        } # end for
      })
    }
  } else {
    message("Write altitude.tif...")
    stars::write_stars(lc, dsn = file.path(path_tif, names(lc)), driver = "GTiff", progress = TRUE, update = FALSE)
    out <- "altitude"
    if ("exposition" %in% list_indice) {
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
    }
    if ("ombrage" %in% list_indice) {
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
    }
    if ("rugosite" %in% list_indice) {
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
    }
    # TRI
    if ("TRI" %in% list_indice) {
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
    }
    # TPI
    if ("TPI" %in% list_indice) {
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
    }
    if ("geomorphon" %in% list_indice) {
      message("Write geomorphon.tif...")
      geomorphon <- qgis::grass7_r_geomorphon(
        elevation = file.path(path_tif, "altitude.tif"),
        forms = file.path(path_tif, "geomorphon.tif")
      )
      out <- c(out, "geomorphon")
    }
    #### RSAGA
    env <- rsaga.env()
    # TWI
    if ("TWI" %in% list_indice) {
      message("Write twi.tif...")
      RSAGA::rsaga.wetness.index(
        in.dem = file.path(path_tif, "altitude.tif"),
        out.wetness.index = file.path(tempdir(), "twi"),
        env = env
      )
      twi <- raster::raster(file.path(tempdir(), "twi.sdat"))
      raster::writeRaster(twi, 
                          filename = file.path(path_tif, "twi.tif"),
                          overwrite = TRUE)
      out <- c(out, "twi")
    }
    # plan curvature
    if ("cplan" %in% list_indice) {
      message("Write cplan.tif...")
      RSAGA::rsaga.plan.curvature(
        in.dem = file.path(path_tif, "altitude.tif"),
        out.hcurv = file.path(tempdir(), "cplan"),
        method = "poly2zevenbergen",
        env = env
      )
      cplan <- raster::raster(file.path(tempdir(), "cplan.sdat"))
      raster::writeRaster(cplan, 
                          filename = file.path(path_tif, "cplan.tif"),
                          overwrite = TRUE)
      out <- c(out, "cplan")
    }

    # profile curvature
    if ("cprof" %in% list_indice) {
      message("Write cprof.tif...")
      RSAGA::rsaga.profile.curvature(
        in.dem = file.path(path_tif, "altitude.tif"),
        out.vcurv = file.path(tempdir(), "cprof"),
        method = "poly2zevenbergen",
        env = env
      )
      cprof <- raster::raster(file.path(tempdir(), "cprof.sdat"))
      raster::writeRaster(cprof, 
                          filename = file.path(path_tif, "cprof.tif"),
                          overwrite = TRUE)
      out <- c(out, "cprof")
    }

    # contributing area
    if ("carea" %in% list_indice) {
      message("Write carea.tif...")
      RSAGA::rsaga.topdown.processing(
        in.dem = file.path(path_tif, "altitude.tif"),
        out.carea = file.path(tempdir(), "carea"),
        method = "mfd",
        env = env
      )
      carea <- raster::raster(file.path(tempdir(), "carea.sdat"))
      raster::writeRaster(carea, 
                          filename = file.path(path_tif, "carea.tif"),
                          overwrite = TRUE)
      out <- c(out, "carea")
    }

    # log10_carea
    if ("log10_carea" %in% list_indice) {
      message("Write log10_carea.tif...")
      RSAGA::rsaga.topdown.processing(
        in.dem = file.path(path_tif, "altitude.tif"),
        out.carea = file.path(tempdir(), "carea"),
        method = "mfd",
        env = env
      )
      RSAGA::rsaga.grid.calculus(
        in.grids = file.path(tempdir(), "carea"),
        out.grid = file.path(tempdir(), "log10_carea"),
        formula = ~ log(a),
        env = env
      )
      log10_carea <- raster::raster(file.path(tempdir(), "log10_carea.sdat"))
      raster::writeRaster(log10_carea, 
                          filename = file.path(path_tif, "log10_carea.tif"),
                          overwrite = TRUE)
      out <- c(out, "log10_carea")
    }

    #### list multiple
    s <- strsplit(srtm_multiple, ",")[[1]]
    # calc mutiple rhougness indices
    if ("rugosite" %in% list_indice) {
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
    }
    if ("TRI" %in% list_indice) {
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
    }

    # TPI
    if ("TPI" %in% list_indice) {
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
    }

    # Geomorphon
    if ("geomorphon" %in% list_indice) {
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

    # TWI
    if ("TWI" %in% list_indice) {
      # calc mutiple TWI indices
      for (r in seq(s)) {
        message(paste0("Write twi", s[r], ".tif (", r, "/", length(s), ")"))
        twim <- qgis::grass7_r_neighbors(
          input = twi,
          method = "average",
          size = as.integer(s[r]),
          .c = TRUE,
          output = file.path(path_tif, paste0("twi", s[r], ".tif"))
        )
        out <- c(out, paste0("twi", s[r]))
      } # end for
    }

    # cplan
    if ("cplan" %in% list_indice) {
      # calc mutiple cplan indices
      for (r in seq(s)) {
        message(paste0("Write cplan", s[r], ".tif (", r, "/", length(s), ")"))
        twim <- qgis::grass7_r_neighbors(
          input = cplan,
          method = "average",
          size = as.integer(s[r]),
          .c = TRUE,
          output = file.path(path_tif, paste0("cplan", s[r], ".tif"))
        )
        out <- c(out, paste0("cplan", s[r]))
      } # end for
    }

    # carea
    if ("carea" %in% list_indice) {
      # calc mutiple carea indices
      for (r in seq(s)) {
        message(paste0("Write carea", s[r], ".tif (", r, "/", length(s), ")"))
        twim <- qgis::grass7_r_neighbors(
          input = carea,
          method = "average",
          size = as.integer(s[r]),
          .c = TRUE,
          output = file.path(path_tif, paste0("carea", s[r], ".tif"))
        )
        out <- c(out, paste0("carea", s[r]))
      } # end for
    }

    # log10_carea
    if ("log10_carea" %in% list_indice) {
      # calc mutiple log10_carea indices
      for (r in seq(s)) {
        message(paste0("Write log10_carea", s[r], ".tif (", r, "/", length(s), ")"))
        twim <- qgis::grass7_r_neighbors(
          input = log10_carea,
          method = "average",
          size = as.integer(s[r]),
          .c = TRUE,
          output = file.path(path_tif, paste0("log10_carea", s[r], ".tif"))
        )
        out <- c(out, paste0("log10_carea", s[r]))
      } # end for
    }
  }
  return(out)
}
