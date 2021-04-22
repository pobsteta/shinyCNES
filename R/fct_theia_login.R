#' @title read_theia_login
#'
#' @param apitheia_path Path api THEIA
#' @export
read_theia_login <- function(apitheia_path = NA) {
  # if apitheia_path is not specified, retrieve from the current theiaR installation
  if (is.na(apitheia_path)) {
    # import theiaR
    if ("theiaR" %in% rownames(installed.packages()) == FALSE) {
      install.packages("theiaR")
    }
    theia_download <- find.package("theiaR")
    apitheia_path <- file.path(theia_download, "auth_theia.txt")
  }
  # return user and password
  if (file.exists(apitheia_path)) {
    readLines(apitheia_path) %>%
      strsplit(" ") %>%
      unlist()
  } else {
    # if apitheia does not exists, return default credentials
    c("email", "password")
  }
}



#' @title write_theia_login
#'
#' @param username Theia login
#' @param password Theia password
#' @param apitheia_path Path theia
#' @export
write_theia_login <- function(username, password, apitheia_path = NA) {
  # if apitheia_path is not specified, retrieve from the current theiaR installation
  # import theiaR
  if ("theiaR" %in% rownames(installed.packages()) == FALSE) {
    install.packages("theiaR")
  }
  theia_download <- find.package("theiaR")
  apitheia_path <- file.path(theia_download, "auth_theia.txt")
  # write credentials
  writeLines(paste(username, password, sep = "\n"), apitheia_path)
}



#' @title theia_modal
#'
#' @param username Theia login
#' @param password Theia password
#' @export
theia_modal <- function(username = NA, password = NA, session) {
  ns <- session$ns
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  # read theia user/password
  if (anyNA(c(username, password))) {
    apitheia <- read_theia_login()
    username <- apitheia[1]
    password <- apitheia[2]
  }
  modalDialog(title = i18n$t("Set Theia connection"), 
              size = "s", 
              textInput(ns("theia_username"), 
                        i18n$t("Username"), 
                        username), 
              passwordInput(ns("theia_password"), 
                            i18n$t("Password"),
                            password),
              a(i18n$t("Register new account"), 
                href = "https://sso.theia-land.fr/theia/register/register.xhtml", 
                target = "_blank"), 
              "\u2000—\u2000", 
              a(i18n$t("Forgot password?"),
                href = "https://sso.theia-land.fr/theia/profile/recovery.xhtml;jsessionid=49E3F76B9E96191C4ADDD3EE5298E366", 
                target = "_blank"),
              easyClose = FALSE, 
              footer = tagList(
                div(style = "display:inline-block;vertical-align:top;", 
                    actionButton(ns("save_apitheia"),
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
