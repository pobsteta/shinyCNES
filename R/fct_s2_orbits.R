#' @title Load Sentinel-2 orbits
#' @description Load the vector object of the Sentinel-2 orbits.
#' @return An sf spatial object containing the extent of the orbits.
#' @importFrom sf st_read
#' @importFrom utils unzip
#' @author Pascal Obstetar, (2019) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' @export
#' 
s2_orbits <- function() {
  # extract and import tiles kml
  s2orbits_kmz <- system.file("data-raw/vector/s2_orbits.kmz", package = "shinyCNES")
  s2orbits_kml <- gsub("\\.kmz$", ".kml", s2orbits_kmz)
  if (!file.exists(s2orbits_kml)) {
    unzip(zipfile = s2orbits_kmz, files = basename(s2orbits_kml), exdir = dirname(s2orbits_kml), unzip = "internal")
  }
  s2orbits <- sf::st_read(s2orbits_kml, stringsAsFactors = FALSE, quiet = TRUE) %>% st_zm(drop = TRUE)
  s2orbits[, !names(s2orbits) %in% c("Relative_Orbit", "geometry")] <- NULL
  names(s2orbits) <- c("orbit_id", "geometry")
  s2orbits$orbit_id <- sprintf("O%03d", as.integer(s2orbits$orbit_id))
  return(s2orbits)
}
