#' @title Select the orbit intersecting the extent
#' @description Function which returns the orbit IDs of the Sentinel-2 orbits
#'  which overlap a provided extent.
#' @param tiles `sf` object with the tiles spatial extent.
#' @param all logical: if TRUE, all the orbits overlapping the extent are
#'  provided;
#'  if FALSE (default), unnecessary orbits are skipped.
#'  Unnecessary orbits are orbits which overlaps the extent for an area already
#'  covered by another orbit.
#'  In case the extent is all included in an overlapping area, only one of the
#'  two candidate orbits is returned (the first in alphabetical order).
#' @param out_format character: if "sf", the spatial object of the overlapping orbits
#'  is returned; if "id" (default), a character vector with the orbit IDs.
#' @param .s2orbits output of [s2_orbits()] function (it is possible to pass it
#'  in order to speed up the execution;
#'  otherwise leave to NULL and it will be generated within the function).
#' @return the orbits intersecting the extent (see argument `out_format`).
#' @export
#' @importFrom sf st_area st_crs st_difference st_geometry 
#'  st_intersects st_transform st_union
#' @author Pascal Obstetar (2020) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' 
orbits_intersects <- function(tiles, all = FALSE, out_format = "id", .s2orbits = NULL) {
  # Load S2 orbit
  s2orbits <- if (is.null(.s2orbits)) {
    s2_orbits()
  } else if (st_crs(.s2orbits) == st_crs(4326)) {
    .s2orbits
  } else {
    st_transform(.s2orbits, 4326)
  }
  
  # Extent to longlat
  tiles <- st_transform(tiles, 4326)
  if (is(tiles,"sf")) {
    tiles <- st_geometry(tiles)
  }
  
  # Select all the orbits which intersects the extent
  orbits_intersecting_all <- s2orbits[unique(unlist(suppressMessages(
    st_intersects(tiles, s2orbits)
  ))),]
  
  orbits_intersects <- rep(TRUE, nrow(orbits_intersecting_all))
  
  # Cycle on orbits
  if (length(orbits_intersects) > 1 & all == FALSE) {
    for (i in rev(seq_len(nrow(orbits_intersecting_all)))) {
      sel_orbit <- orbits_intersecting_all[i,]
      # intersection between extent and portion of orbits not overlapping other orbits
      # (with the exception of already discharged ones)
      sel_orbit_notoverlap <- if (
        any(orbits_intersects & orbits_intersecting_all$orbit_id != sel_orbit$orbit_id)
      ) {
        suppressMessages(st_difference(
          st_geometry(orbits_intersecting_all), 
          st_union(
            orbits_intersecting_all[orbits_intersects & orbits_intersecting_all$orbit_id != sel_orbit$orbit_id,]
          )
        ))
      } else {
        st_geometry(orbits_intersecting_all)
      }
      # Check if any portion ot the extent is exclusive of sel_orbit
      orbits_intersects[i] <- any(suppressMessages(
        st_intersects(extent, sel_orbit_notoverlap, sparse = FALSE)
      ))
    }
  }
  
  # Return the required output
  if (out_format == "id") {
    orbits_intersecting_all[orbits_intersects,]$orbit_id
  } else {
    orbits_intersecting_all[orbits_intersects,]
  }
  
}
