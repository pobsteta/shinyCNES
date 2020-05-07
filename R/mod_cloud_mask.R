#' cloud_mask UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cloud_mask_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    fluidRow(
      ### cloud mask
      box(
        title = i18n$t("Cloud mask"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        radioButtons(
          "atm_mask",
          label = span(
            i18n$t("Mask cloud-covered pixels?\u2000"),
            actionLink("help_mask", icon("question-circle"))
          ),
          choiceNames = list(
            i18n$t("Yes"),
            i18n$t("No")
          ),
          choiceValues = list(TRUE, FALSE),
          selected = FALSE,
          inline = TRUE
        ), # end of radiobuttons
        
        conditionalPanel(
          condition = "input.atm_mask == 'TRUE'",
          selectInput(
            "atm_mask_type",
            label = span(
              i18n$t("Apply mask to:\u2000"),
              actionLink("help_mask_classes", icon("question-circle"))
            ),
            choices = list(
              "No data" = "nodata",
              "No data and clouds (high probability)" = "cloud_high_proba",
              "No data and clouds (high-medium prob.)" = "cloud_medium_proba",
              "No data and clouds (any probability)" = "cloud_low_proba",
              "No data, clouds and shadows" = "cloud_and_shadow",
              "All except clear-sky" = "clear_sky",
              "All except land surface" = "land",
              "Custom mask" = "custom"
            ),
            selected = "cloud_medium_proba"
          ), # end of selectinput
          
          conditionalPanel(
            condition = "input.atm_mask_type == 'custom'",
            checkboxGroupInput(
              "atm_mask_custom", i18n$t("Select the classes to mask:"),
              choiceNames = list(
                HTML("<font style=\"family:monospace;background-color:#000000;color:white;\">\u20020\u2002</font>\u2002No data"),
                HTML("<font style=\"family:monospace;background-color:#FF0000;color:white;\">\u20021\u2002</font>\u2002Saturated or defective"),
                HTML("<font style=\"family:monospace;background-color:#424142;color:white;\">\u20022\u2002</font>\u2002Dark area pixels"),
                HTML("<font style=\"family:monospace;background-color:#633400;color:white;\">\u20023\u2002</font>\u2002Cloud shadows"),
                HTML("<font style=\"family:monospace;background-color:#29f329;color:black;\">\u20024\u2002</font>\u2002Vegetation"),
                HTML("<font style=\"family:monospace;background-color:#ffff00;color:black;\">\u20025\u2002</font>\u2002Bare soils"),
                HTML("<font style=\"family:monospace;background-color:#0000ff;color:white;\">\u20026\u2002</font>\u2002Water"),
                HTML("<font style=\"family:monospace;background-color:#7b7d7b;color:white;\">\u20027\u2002</font>\u2002Cloud (low probability)"),
                HTML("<font style=\"family:monospace;background-color:#bdbebd;color:black;\">\u20028\u2002</font>\u2002Cloud (medium probability)"),
                HTML("<font style=\"family:monospace;background-color:#ffffff;color:black;\">\u20029\u2002</font>\u2002Cloud (high probability)"),
                HTML("<font style=\"family:monospace;background-color:#63cbff;color:black;\">\u200510\u2005</font>\u2002Thin cirrus"),
                HTML("<font style=\"family:monospace;background-color:#ff9aff;color:black;\">\u200511\u2005</font>\u2002Snow")
              ),
              choiceValues = as.list(0:11),
              selected = list(0, 1, 8, 9)
            )
          ), # end of conditionalpanel
          
          sliderInput(
            "max_masked_perc",
            label = span(
              i18n$t("Maximum allowed cloud cover"),
              actionLink("help_masked_perc", icon("question-circle"))
            ),
            min = 0, max = 100, value = 100,
            step = 1, post = "%"
          ), # end of sliderinput
          
          radioButtons(
            "mask_apply_smooth",
            label = span(
              i18n$t("Smooth / bufferize the cloud-covered surface?\u2000"),
              actionLink("help_mask_smooth", icon("question-circle"))
            ),
            choiceNames = list(
              i18n$t("Yes"),
              i18n$t("No")
            ),
            choiceValues = list(TRUE, FALSE),
            selected = FALSE,
            inline = TRUE
          ), # end of radiobuttons
          
          conditionalPanel(
            condition = "input.mask_apply_smooth == 'TRUE'",
            fluidRow(
              column(
                width = 6,
                numericInput("mask_smooth",
                             i18n$t("Smooth (m)"),
                             value = 250,
                             min = 0
                )
              ),
              column(
                width = 6,
                numericInput("mask_buffer",
                             i18n$t("Buffer (m)"),
                             value = 250
                )
              ) # end of column
            ) # end of smooth/buffer fluidRow
          ) # end of conditionalPanel mask_apply_smooth
        ) # end of conditionalPanel atm_mask
      ) # end of fluidRow/box "Atmospheric mask"
    ) # end fluidrow cloud mask
  ) # end taglist
}
    
#' cloud_mask Server Function
#'
#' @noRd 
mod_cloud_mask_server <- function(input, output, session, rv){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_cloud_mask_ui("cloud_mask_ui_1")
    
## To be copied in the server
# callModule(mod_cloud_mask_server, "cloud_mask_ui_1")
 
