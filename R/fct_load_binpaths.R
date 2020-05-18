#' @title Load the paths of external executables
#' @description Internal function to load the paths of executables 
#'  from the JSON where they are saved when installed.
#' @param bins Character vector with one of more of the following values:
#'  "gdal", sen2cor", "aria2", "python".
#'  If an executable corresponding to the passed `bins` value is not found
#'  in the JSON, it is checked (when possible).
#' @return The list of the paths
#'
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}, Pascal Obstetar, (2020) \email{pascal.obstetar@@gmail.com}
#' @note License: GPL 3.0
#' @importFrom jsonlite fromJSON
#' @importFrom xfun normalize_path
#' @export
#' @examples
#' \donttest{
#' # Load only existing paths
#' binpaths <- load_binpaths()
#' binpaths
#' }
#' 
#' \dontrun{
#' # Load paths, forcing to check GDAL and sen2cor
#' binpaths <- load_binpaths(c("gdal", "sen2cor"))
#' binpaths
#' }


load_binpaths <- function(bins = NULL) {
  
  # Define where the JSON with the paths is
  binpaths_file <- file.path(
    if (dir.exists(xfun::normalize_path("~/.shinycnes"))) {
      xfun::normalize_path("~/.shinycnes")
    } else {
      tempdir()
    },
    "paths.json"
  )
  
  # If it exists, load it; otherwise, create empty
  binpaths <- if (file.exists(binpaths_file)) {
    jsonlite::fromJSON(binpaths_file)
  } else {
    list()
  }
  
  ## Check that the required binaries are present
  
  # Check GDAL
  if ("gdal" %in% bins & is.null(binpaths$gdalinfo)) {
    check_gdal()
    binpaths <- jsonlite::fromJSON(binpaths_file)
  }
  
  # Return the list
  attr(binpaths, "path") <- binpaths_file
  binpaths
  
}
