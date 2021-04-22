#' s2_preview_rgb
#'
#' @param path_data Path data
#'
#' @return
#' @export
#'
s2_preview_rgb <- function(path_data = "~", 
                           project_name, 
                           collection = "Sentinel2_L2A_THEIA", 
                           tile = NULL,
                           progress_bar = TRUE) {
  collection <- ifelse(collection == "l3a", "Sentinel2_L3A_THEIA", "Sentinel2_L2A_THEIA")

  if (nchar(tile) > 1) {
    # on optimise avec le nombre de coeur du PC
    gdalcubes::gdalcubes_set_threads(parallel::detectCores() - 1)
    # On recherche les fichiers à importer dans le cube
    files <- list.files(file.path(path_data, "data"),
                        recursive = TRUE,
                        full.names = TRUE,
                        pattern = ".zip"
    )
    # on cherche le fichier tile
    files_list <- files[basename(tools::file_path_sans_ext(files)) %in% str_sub(tile, 9)]
    # On cree la collection des images, on la sauvegarde
    rgb_col <- gdalcubes::create_image_collection(files_list,
                                       collection,
                                       unroll_archives = TRUE
    )

    # Creation de la vue jour EPSG 2154
    # vue jour a 100m de resolution
    vued <- gdalcubes::cube_view(
      srs = "EPSG:2154",
      extent = rgb_col,
      nx = 250, # nombre de pixel en x
      ny = 250, # nombre de pixel en y
      dt = "P1D", # time a 1 jour selon norme ISO8601
      keep.asp = TRUE
    )
    
    # search the band of the tile
    band1 <- switch_band(str_sub(tile, 4, 4))
    band2 <- switch_band(str_sub(tile, 5, 5))
    band3 <- switch_band(str_sub(tile, 6, 6))
    
    if (progress_bar) {
      i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
      i18n$set_translation_language("fr")
      # Add a progress bar while importing
      withProgress(message = i18n$t("Loading preview..."), value = 0, {
        setProgress(value = 0.1)
        # calcul preview at 100m de resolution
        s_rgb_preview <- tryCatch(gdalcubes::raster_cube(rgb_col, vued) %>%
                                    gdalcubes::select_bands(c(band1, band2, band3)) %>%
                                    plot(rgb = 1:3, axes = FALSE),
                                  error = function(e) print(i18n$t("Input cube has no band!"))
        )
        setProgress(value = 1)
      }) # end with progress
    } else {
      # calcul preview at 100m de resolution
      s_rgb_preview <- tryCatch(gdalcubes::raster_cube(rgb_col, vued) %>%
                                  gdalcubes::select_bands(c(band1, band2, band3)) %>%
                                  plot(rgb = 1:3, axes = FALSE),
                                error = function(e) print("Input cube has no band!")
      )
    }
  } # end if
}
  