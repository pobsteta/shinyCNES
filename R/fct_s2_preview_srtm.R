#' s2_preview_srtm
#'
#' @param path_data Path data
#' @param project_name project name
#' @param tile Tile
#' @param progress_bar Progress bar if shiny run
#' @param path Path
#'
#' @return
#' @export
#'
s2_preview_srtm <- function(path_data = "~",
                            project_name,
                            path_tif = "~",
                            tile = NULL,
                            progress_bar = TRUE) {

  if (nchar(tile) > 1) {
    if (progress_bar) {
      i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
      i18n$set_translation_language("fr")
      # Add a progress bar while importing
      withProgress(message = i18n$t("Loading preview..."), value = 0, {
        setProgress(value = 0.1)
        # read srtm altitude file
        s_srtm_preview <- stars::read_stars(file.path(path_tif, paste0(tile, ".tif"))) %>% 
          plot(axes = FALSE, 
               key.pos = NULL
          )
        setProgress(value = 1)
      }) # end with progress
    } else {
      # calcul preview at 100m de resolution
      # read srtm altitude file
      s_srtm_preview <- stars::read_stars(file.path(path_tif, paste0(tile, ".tif"))) %>% 
        plot(axes = FALSE, 
             key.pos = NULL
        )
    }
  } # end if
}
