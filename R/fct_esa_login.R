#' read_esa_login
#'
#' @param apiesa_path 
#'
#' @return
#' @export
#'
read_esa_login <- function(apiesa_path = NA) {
  if (is.na(apiesa_path)) {
    # import getSpatialData
    if ("getSpatialData" %in% rownames(installed.packages()) == FALSE) {
      install.packages("getSpatialData")
    }
    esa_download <- find.package("getSpatialData")
    apiesa_path <- file.path(esa_download, "auth_esa.txt")
  }
  # return user and password
  if (file.exists(apiesa_path)) {
    readLines(apiesa_path) %>%
      strsplit(" ") %>%
      unlist()
  } else {
    # if apiesa_path does not exists, return default credentials
    c("email", "password")
  }
}



#' @title write_esa_login
#'
#' @param username ESA login
#' @param apiesa_path 
#' @param password ESA password
#'
#' @export
write_esa_login <- function(username, password, apiesa_path = NA) {
  # import getSpatialData
  if ("getSpatialData" %in% rownames(installed.packages()) == FALSE) {
    install.packages("getSpatialData")
  }
  esa_download <- find.package("getSpatialData")
  apiesa_path <- file.path(esa_download, "auth_esa.txt")
  # write credentials
  writeLines(paste(username, password, sep = "\n"), apiesa_path)
}



#' @title esa_modal
#'
#' @param username ESA login
#' @param password ESA password
#' @export
esa_modal <- function(username = NA, password = NA, session) {
  ns <- session$ns
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  # read esa user/password
  if (anyNA(c(username, password))) {
    apiesa <- read_esa_login()
    username <- apiesa[1]
    password <- apiesa[2]
  }
  modalDialog(title = i18n$t("Set NASA/ESA connection"), 
              size = "s", 
              textInput(ns("esa_username"), 
                        i18n$t("Username"), 
                        username), 
              passwordInput(ns("esa_password"), 
                            i18n$t("Password"),
                            password),
              a(i18n$t("Register new account"), 
                href = "https://urs.earthdata.nasa.gov/users/new", 
                target = "_blank"), 
              "\u2000—\u2000", 
              a(i18n$t("Forgot password?"),
                href = "https://urs.earthdata.nasa.gov/reset_passwords/new", 
                target = "_blank"),
              easyClose = FALSE, 
              footer = tagList(
                div(style = "display:inline-block;vertical-align:top;", 
                    actionButton(ns("save_apiesa"),
                                 i18n$t("\u2000Save"), 
                                 icon = icon("save"))
                ),
                div(style = "display:inline-block;vertical-align:top;", 
                    modalButton(i18n$t("\u2000Cancel"),
                                icon = icon("ban")
                    )
                )
              )
  )
}
