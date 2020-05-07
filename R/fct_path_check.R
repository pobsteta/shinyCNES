#' path_check
#'
#' @param path Directory of projetc
#'
#' @return If path ok
#' @export
#'
path_check <- function(path) {
  if (length(path) > 0 & path[1] != "") {
    if (!dir.exists(path)) {
      return(renderUI(span(
        style = "color:red",
        "\u2718 (the directory does not exist)"
      )))
    } else if (file.access(path, mode = 2) < 0) {
      return(renderUI(span(
        style = "color:red",
        "\u2718 (the directory is not writable)"
      )))
    } else {
      return(renderUI(span(
        style = "color:darkgreen",
        "\u2714"
      )))
    }
  } else {
    return(renderText(""))
  }
}
