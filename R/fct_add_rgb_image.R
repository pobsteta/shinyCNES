#' @title Add RGB product
#' @description Modal dialog to define an RGB image.
#' @param session Session
#' @param s2_bands 2-length list (one for TOA, one for BOA),
#'  each element being a list of S2 bands, as defined in [s2_gui].
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}, Pascal Obstetar (2020) \email{pascal.obstetar@@gamil.com}
#' @note License: GPL 3.0
#' @importFrom shiny actionButton column fluidRow HTML icon modalButton
#'  modalDialog radioButtons sliderInput strong tagList
#' @importFrom shinyWidgets pickerInput

add_rgb_image <- function(s2_bands, session) {
  ns <- session$ns
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  modalDialog(
    title = i18n$t("Set a new RGB image"),
    size = "m",
    
    # setSliderColor(c("", "red", "darkgreen", "blue"), c(2, 3, 4)),
    # # this is 2, 3, 4 becaluse sliderInput 1 is "max_masked_perc".
    # # in case other sliderInput are added, check this!
    
    radioButtons(
      ns("newrgb_source"),
      label = i18n$t("RGB source:"),
      choices = list("BOA" = "BOA", "TOA" = "TOA"),
      selected = "BOA",
      inline = TRUE
    ),
    
    fluidRow(
      column(
        width = 4,
        pickerInput(
          ns("band_r"), 
          label = HTML("<span style='color:red;'>Red band</span>"),
          choices = s2_bands[["BOA"]], 
          selected = "band4"#,
          # options = list(style = "btn-danger")
        )
      ),
      column(
        width = 8,
        sliderInput(
          ns("band_r_range"), 
          label = HTML("<span style='color:red;'>Range (reflectance)</span>"),
          min = 0, max = 1, 
          value = c(0, 0.25)
        )
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        pickerInput(
          ns("band_g"), 
          label = HTML("<span style='color:darkgreen;'>Green band</span>"),
          choices = s2_bands[["BOA"]], 
          selected = "band3"#,
          # options = list(style = "btn-danger")
        )
      ),
      column(
        width = 8,
        sliderInput(
          ns("band_g_range"), 
          label = HTML("<span style='color:darkgreen;'>Range (reflectance)</span>"),
          min = 0, max = 1, 
          value = c(0, 0.25)
        )
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        pickerInput(
          ns("band_b"), 
          label = HTML("<span style='color:blue;'>Blue band</span>"),
          choices = s2_bands[["BOA"]], 
          selected = "band2"#,
          # options = list(style = "btn-primary")
        )
      ),
      column(
        width = 8,
        sliderInput(
          ns("band_b_range"), 
          label = HTML("<span style='color:blue;'>Range (reflectance)</span>"),
          min = 0, max = 1, 
          value = c(0, 0.25)
        )
      )
    ),
    
    easyClose = FALSE,
    footer = tagList(
      actionButton(ns("add_new_rgb"), strong("\u2000Add"), icon=icon("check")),
      modalButton("\u2000Cancel", icon = icon("ban"))
    )
  )
}
