#' getDataWorldClim
#'
#' @param name 
#' @param download 
#' @param path 
#' @param ... 
#' @param period 
#' @param gcm 
#' @param ssp 
#'
#' @return
#' @export
#'
getDataWorldClim <- function(name = "worldclim", 
                             download = TRUE, 
                             path = "~", 
                             period = "1970-2000", 
                             gcm = "BCC-CSM2-MR", 
                             ssp = 126,
                             progress_bar = TRUE,
                             ...) {
  if (name == "worldclim") {
    .worldclim(..., download = download, path = path, period = period, gcm = gcm, ssp = ssp, progress_bar = progress_bar)
  } else {
    stop(name, " not recognized as a valid name.")
  }
}

.download <- function(url, filename) {
  httr::GET(url, 
            httr::write_disk(filename, overwrite = TRUE),
            httr::progress()
  )
}

.worldclim <- function(var, res, period = "1970-2000", path, download = TRUE, gcm = "BCC-CSM2-MR", ssp = 126, progress_bar = TRUE) {
  if (!dir.exists(path)) {
    stop("Path don't exist!")
  }
  if (period %in% c("1970-2000")) {
    if (!res %in% c(0.5, 2.5, 5, 10)) {
      stop("resolution should be one of: 0.5, 2.5, 5, 10")
    }
    stopifnot(var %in% c("tmin", "tmax", "tavg", "prec", "srad", "wind", "vapr", "bio", "elev"))
    path <- paste0(path, "/wc", res, "/base/")
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    
    if (res == 0.5) {
      zip <- paste0("wc2.1_", "30", "s_", var, ".zip")
    } else {
      zip <- paste0("wc2.1_", res, "m_", var, ".zip")
    }
    theurl <- paste0("http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/", zip)
    zipfile <- stringr::str_replace(file.path(path, zip), ".zip", "_1970-2000.zip")
  } else if (period %in% c("1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018")) {
    if (!res %in% c(2.5)) {
      stop("resolution should be 2.5")
    }
    stopifnot(var %in% c("tmin", "tmax", "prec"))
    path <- paste(path, "/wc", res, "/hist/", sep = "")
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    
    zip <- paste0("wc2.1_", res, "m_", var, "_", period, ".zip")
    
    theurl <- paste0("http://biogeo.ucdavis.edu/data/worldclim/v2.1/hist/", zip)
    zipfile <- file.path(path, zip)
  } else if (period %in% c("2021-2040", "2041-2060", "2061-2080", "2081-2100")) {
    if (!res %in% c(2.5, 5, 10)) {
      stop("resolution should be one of: 2.5, 5, 10")
    }
    stopifnot(var %in% c("tmin", "tmax", "prec", "bio"))
    
    path <- paste(path, "/wc", res, "/fut/", sep = "")
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    
    if (gcm %in% c("BCC-CSM2-MR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5", "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0")) {
      zip <- paste0("wc2.1_", res, "m_", var, "_", gcm, "_ssp", ssp, "_", period, ".zip")
    } else {
      stop("gcm should be one of: BCC-CSM2-MR, CNRM-CM6-1, CNRM-ESM2-1, CanESM5, GFDL-ESM4, IPSL-CM6A-LR, MIROC-ES2L, MIROC6, MRI-ESM2-0")
    }
    
    if (ssp %in% c(126, 245, 370, 585)) {
      zip <- paste0("wc2.1_", res, "m_", var, "_", gcm, "_ssp", ssp, "_", period, ".zip")
    } else {
      stop("ssp should be one of: 126, 245, 370, 585")
    }

    theurl <- paste0("http://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/", res, "m/", zip)
    zipfile <- file.path(path, zip)
  } else {
    stop("period should be one of: 1970-2000, 1960-1969, 1970-1979, 1980-1989, 1990-1999, 2000-2009, 2010-2018")
  }
  
  if (progress_bar) {
    i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
    i18n$set_translation_language("fr")
    # Add a progress bar while importing
    withProgress(message = i18n$t("Launch WordlClim downloading..."), value = 0, {
      if (!file.exists(zipfile)) {
        if (download) {
          incProgress(1, detail = paste("\n Download", zip, "file..."))
          Sys.sleep(0.25)
          .download(theurl, zipfile)
          if (!file.exists(zipfile)) {
            incProgress(1, detail = "\n Could not download file -- perhaps it does not exist")
            Sys.sleep(0.25)
          }
        } else {
          incProgress(1, detail = "File not available locally. Use 'download = TRUE'")
          Sys.sleep(0.25)
        }
      } else {
        incProgress(1 / length(tiles), detail = paste(zip, "exist in", path))
        Sys.sleep(0.25)
      }
    }) # end withprogress
  } else {
    if (!file.exists(zipfile)) {
      if (download) {
        message(paste("\n Download", zip, "file..."))
        .download(theurl, zipfile)
        if (!file.exists(zipfile)) {
          message("\n Could not download file -- perhaps it does not exist")
        }
      } else {
        message("File not available locally. Use 'download = TRUE'")
      }
    } else {
      message(paste(zip, "exist in", path))
    }
  }
}




