#' s2_indice
#'
#' @param path_data Path data
#' @param project_name Project name
#' @param collection Collection
#' @param tiles Tiles
#' @param indices Indices
#' @param progress_bar Progress bar if shiny run
#' @param path_indice Path indice
#'
#' @return
#' @export
#'
s2_indice <- function(path_data = "~", 
                      path_indice = "~",
                      project_name, 
                      collection = "Sentinel2_L2A_THEIA", 
                      tiles = NULL,
                      indices = NULL,
                      progress_bar = TRUE) {
  collection <- ifelse(collection == "l3a", "Sentinel2_L3A_THEIA", "Sentinel2_L2A_THEIA")
  # generate indices.json if missing and read it
  create_indices_db()
  indices_db <- list_indices(
    c("n_index","name","longname","s2_formula","a","b","c","d"),
    all = TRUE
  )
  
  # function to create formula and bands for gdalcubes
  formula <- function(s2_formula, a, b, c, d) {
    stringr::str_replace_all(s2_formula, c("band_" = "B", "par_a" = as.character(a), "par_b" = as.character(b), "par_c" = as.character(c), "par_d" = as.character(d)))
  }
  formula_v <- Vectorize(formula)
  
  bands <- function(formule) {
    bds <- stringr::str_extract_all(formule, "B[0-9]{1,2}")
    unique(bds[[1]]) %>% 
      paste(collapse = ",")
  }
  bands_v <- Vectorize(bands)
  
  # indices selectionnes avec forumle pour gdalcubes
  indices_sel <- indices_db %>% 
    dplyr::filter(name %in% indices) %>% 
    dplyr::mutate(formule = as.character(formula_v(s2_formula, a = a, b = b, c = c, d = d)),
                  bands = as.character(bands_v(formule))
    )

  # on optimise avec le nombre de coeur du PC
  gdalcubes::gdalcubes_set_threads(parallel::detectCores() - 1)
  if (!is.null(tiles) & !is.null(indices)) {
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
          indices_col <- gdalcubes::create_image_collection(files_list,
                                                            collection,
                                                            unroll_archives = TRUE
          )
          # Creation de la vue jour EPSG 2154
          # vue jour a 100m de resolution
          vued <- gdalcubes::cube_view(
            srs = "EPSG:2154",
            extent = indices_col,
            nx = 250, # nombre de pixel en x
            ny = 250, # nombre de pixel en y
            # dx = 10, # resolution spatial 10m en x
            # dy = 10, # resolution spatial 10m en y
            dt = "P1D", # time a 1 jour selon norme ISO8601
            keep.asp = TRUE
          )
          
          incProgress(1 / length(tiles), detail = paste0(tiles[t], " (", t, "/", length(tiles), ")"))
          Sys.sleep(0.25)
          # calcul preview at 10m de resolution
          s_indices <- tryCatch(gdalcubes::raster_cube(indices_col, vued) %>%
                                  gdalcubes::select_bands(unique(strsplit(paste(indices_sel$bands, collapse = ","), ",")[[1]])) %>%
                                  gdalcubes::apply_pixel(indices_sel$formule, names = indices_sel$name),
                                error = function(e) print(i18n$t("Input cube has no band!"))
          )
          for (sel in 1:nrow(indices_sel)) {
            # verify if file don't exist
            if (length(list.files(path = file.path(path_indice), pattern = paste0(indices_sel$name[sel], "_", tiles[t], "_"))) == 0) {
              s_indices %>% 
                gdalcubes::select_bands(indices_sel$name[sel]) %>%
                gdalcubes::write_tif(dir = path_indice, prefix = paste0(indices_sel$name[sel], "_", tiles[t], "_"))
            }
          } # end for
        } # end for
      }) # end with progress
    } else {
      for (t in seq(length(tiles))) {
        # on cherche les tiles
        files_list <- files[basename(tools::file_path_sans_ext(files)) %in% tiles[t]]
        # On cree la collection des images, on la sauvegarde
        indices_col <- gdalcubes::create_image_collection(files_list,
                                                          collection,
                                                          unroll_archives = TRUE
        )
        # Creation de la vue jour EPSG 2154
        # vue jour a 100m de resolution
        vued <- gdalcubes::cube_view(
          srs = "EPSG:2154",
          extent = indices_col,
          nx = 250, # nombre de pixel en x
          ny = 250, # nombre de pixel en y
          # dx = 10, # resolution spatial 10m en x
          # dy = 10, # resolution spatial 10m en y
          dt = "P1D", # time a 1 jour selon norme ISO8601
          keep.asp = TRUE
        )
        # calcul preview at 10m de resolution
        s_indices <- tryCatch(gdalcubes::raster_cube(indices_col, vued) %>%
                                gdalcubes::select_bands(unique(strsplit(paste(indices_sel$bands, collapse = ","), ",")[[1]])) %>%
                                gdalcubes::apply_pixel(indices_sel$formule, names = indices_sel$name),
                              error = function(e) print("Input cube has no band!")
        )
        # write indices in path_indice
        for (sel in 1:nrow(indices_sel)) {
          # verify if file don't exist
          if (length(list.files(path = file.path(path_indice), pattern = paste0(indices_sel$name[sel], "_", tiles[t], "_"))) == 0) {
            message(paste0("Write: ", indices_sel$name[sel], "_", tiles[t], ".tif"))
            s_indices %>% 
              gdalcubes::select_bands(indices_sel$name[i]) %>%
              gdalcubes::write_tif(dir = path_indice, prefix = paste0(indices_sel$name[sel], "_", tiles[t], "_"))
          } # end if
        } # end for
      } # end for
    } # end else
  } # end if
  return(as.vector(outer(indices, tiles, paste, sep = "_")))
}
  