#' s2_preview_indice
#'
#' @param path_data Path data
#' @param project_name Project name
#' @param collection Collection
#' @param tile Tile
#' @param progress_bar Progress bar if shiny run
#'
#' @return
#' @export
#'
s2_preview_indice <- function(path_data = "~",
                              project_name,
                              collection = "Sentinel2_L2A_THEIA",
                              tile = NULL,
                              progress_bar = TRUE) {
  collection <- ifelse(collection == "l3a", "Sentinel2_L3A_THEIA", "Sentinel2_L2A_THEIA")
  if (nchar(tile) > 1) {
    # generate indices.json if missing and read it
    create_indices_db()
    indices_db <- list_indices(
      c("n_index", "name", "longname", "s2_formula", "a", "b", "c", "d"),
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
      dplyr::filter(name %in% str_sub(tile, 1, stringr::str_locate(pattern = "_", tile)[1] - 1)) %>%
      dplyr::mutate(
        formule = as.character(formula_v(s2_formula, a = a, b = b, c = c, d = d)),
        bands = as.character(bands_v(formule))
      )
    # on optimise avec le nombre de coeur du PC
    gdalcubes::gdalcubes_set_threads(parallel::detectCores() - 1)
    # On recherche les fichiers à importer dans le cube
    files <- list.files(file.path(path_data, "data"),
      recursive = TRUE,
      full.names = TRUE,
      pattern = ".zip"
    )
    # on cherche le fichier tile
    files_list <- files[basename(tools::file_path_sans_ext(files)) %in% str_sub(tile, stringr::str_locate(pattern = "_", tile)[1] + 1)]
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
      dt = "P1D", # time a 1 jour selon norme ISO8601
      keep.asp = TRUE
    )

    if (progress_bar) {
      i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
      i18n$set_translation_language("fr")
      # Add a progress bar while importing
      withProgress(message = i18n$t("Loading preview..."), value = 0, {
        setProgress(value = 0.1)
        # calcul preview at 100m de resolution
        s_indices_preview <- tryCatch(gdalcubes::raster_cube(indices_col, vued) %>%
          gdalcubes::select_bands(unique(strsplit(paste(indices_sel$bands, collapse = ","), ",")[[1]])) %>%
          gdalcubes::apply_pixel(indices_sel$formule, names = indices_sel$name) %>%
            plot(zlim = c(-1, 1), bands = 1, axes = FALSE, col = viridis::viridis),
        error = function(e) print(i18n$t("Input cube has no band!"))
        )
        setProgress(value = 1)
      }) # end with progress
    } else {
      # calcul preview at 100m de resolution
      s_indices_preview <- tryCatch(gdalcubes::raster_cube(indices_col, vued) %>%
                                      gdalcubes::select_bands(unique(strsplit(paste(indices_sel$bands, collapse = ","), ",")[[1]])) %>%
                                      gdalcubes::apply_pixel(indices_sel$formule, names = indices_sel$name) %>% 
                                      plot(zlim = c(-1, 1), bands = 1, axes = FALSE, col = viridis::viridis),
      error = function(e) print("Input cube has no band!")
      )
    }
  } # end if
}
