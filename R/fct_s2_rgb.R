#' s2_preview
#'
#' @param path_data Path data
#'
#' @return
#' @export
#'
s2_rgb <- function(path_data = "~", 
                   path_rgb = "~",
                   project_name, 
                   collection = "Sentinel2_L2A_THEIA", 
                   tiles = NULL,
                   rgbs = NULL,
                   progress_bar = TRUE) {
  collection <- ifelse(collection == "l3a", "Sentinel2_L3A_THEIA", "Sentinel2_L2A_THEIA")
  if (!is.null(tiles) & !is.null(rgbs)) {
    # on optimise avec le nombre de coeur du PC
    gdalcubes::gdalcubes_set_threads(parallel::detectCores() - 1)
    # On recherche les fichiers à importer dans le cube
    files <- list.files(file.path(path_data, "data"),
                        recursive = TRUE,
                        full.names = TRUE,
                        pattern = ".zip"
    )
    if (progress_bar) {
      i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
      i18n$set_translation_language("fr")
      # Add a progress bar while importing
      withProgress(message = i18n$t("Launch RGB calc..."), value = 0, {
        for (t in seq(length(tiles))) {
          # on cherche les tiles
          files_list <- files[basename(tools::file_path_sans_ext(files)) %in% tiles[t]]
          # On cree la collection des images, on la sauvegarde
          rgb_col <- gdalcubes::create_image_collection(files_list,
                                                        collection,
                                                        # file.path(path_db, "rgb_collection.db"),
                                                        unroll_archives = TRUE
          )
          
          # Creation de la vue jour EPSG 2154
          # vue jour a 100m de resolution
          vued <- gdalcubes::cube_view(
            srs = "EPSG:2154",
            extent = rgb_col,
            nx = 250, # nombre de pixel en x
            ny = 250, # nombre de pixel en y
            # dx = 10, # resolution spatial 10m en x
            # dy = 10, # resolution spatial 10m en y
            dt = "P1D", # time a 1 jour selon norme ISO8601
            keep.asp = TRUE
          )
          
          incProgress(1 / length(tiles), detail = paste0(tiles[t], " (", t, "/", length(tiles), ")"))
          Sys.sleep(0.25)
          
          for (r in seq(length(rgbs))) {
            band1 <- swb(str_sub(rgbs[r], 4, 4))
            band2 <- swb(str_sub(rgbs[r], 5, 5))
            band3 <- swb(str_sub(rgbs[r], 6, 6))
            # calcul preview at 10m de resolution
            s_rgb <- tryCatch(gdalcubes::raster_cube(rgb_col, vued) %>%
                                gdalcubes::select_bands(c(band1, band2, band3)),
                              error = function(e) print(i18n$t("Input cube has no band!"))
            )
            # verify if file don't exist
            if (length(list.files(path = file.path(path_rgb), pattern = paste0(rgbs[r], "_", tiles[t], "_"))) == 0) {
              s_rgb %>% 
                gdalcubes::write_tif(dir = path_rgb, prefix = paste0(rgbs[r], "_", tiles[t], "_"))
            } # end if
          } # end for
        } # end for
      }) # end with progress
    } else {
      for (t in seq(length(tiles))) {
        # on cherche les tiles
        files_list <- files[basename(tools::file_path_sans_ext(files)) %in% tiles[t]]
        # On cree la collection des images, on la sauvegarde
        rgb_col <- gdalcubes::create_image_collection(files_list,
                                                      collection,
                                                      # file.path(path_db, "rgb_collection.db"),
                                                      unroll_archives = TRUE
        )
        
        # Creation de la vue jour EPSG 2154
        # vue jour a 100m de resolution
        vued <- gdalcubes::cube_view(
          srs = "EPSG:2154",
          extent = rgb_col,
          nx = 250, # nombre de pixel en x
          ny = 250, # nombre de pixel en y
          # dx = 10, # resolution spatial 10m en x
          # dy = 10, # resolution spatial 10m en y
          dt = "P1D", # time a 1 jour selon norme ISO8601
          keep.asp = TRUE
        )
        
        for (r in seq(length(rgbs))) {
          band1 <- swb(str_sub(rgbs[r], 4, 4))
          band2 <- swb(str_sub(rgbs[r], 5, 5))
          band3 <- swb(str_sub(rgbs[r], 6, 6))
          # calcul preview at 100m de resolution
          message(paste0("Write: ", rgbs[r], "_", tiles[t], ".tif"))
          s_rgb <- tryCatch(gdalcubes::raster_cube(rgb_col, vued) %>%
                              gdalcubes::select_bands(c(band1, band2, band3)),
                            error = function(e) print("Input cube has no band!")
          )
          # verify if file don't exist
          if (length(list.files(path = file.path(path_rgb), pattern = paste0(rgbs[r], "_", tiles[t], "_"))) == 0) {
            s_rgb %>% 
              gdalcubes::write_tif(dir = path_rgb, prefix = paste0(rgbs[r], "_", tiles[t], "_"))
          } # end if
        } # end for
      } # end for
    } # end else
  } # end if
  return(as.vector(outer(rgbs, tiles, paste, sep = "_")))
}
  