#' switch_band
#'
#' @param x 
#'
#' @return
#' @export
#'
switch_band <- function(x) {
  out <- switch(x,
                b = "B11",
                c = "B12",
                paste0("B", x)
  )
}