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
          ns("atm_mask"),
          label = span(
            i18n$t("Mask cloud-covered pixels?\u2000"),
            actionLink(ns("help_mask"), icon("question-circle"))
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
          condition = sprintf("input['%s'] == 'TRUE'", ns("atm_mask")),
          selectInput(
            ns("atm_mask_type"),
            label = span(
              i18n$t("Apply mask to:\u2000"),
              actionLink(ns("help_mask_classes"), icon("question-circle"))
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
            condition = sprintf("input['%s'] == 'custom'", ns("atm_mask_type")),
            checkboxGroupInput(
              ns("atm_mask_custom"),
              i18n$t("Select the classes to mask:"),
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
            ns("max_masked_perc"),
            label = span(
              i18n$t("Maximum allowed cloud cover"),
              actionLink(ns("help_masked_perc"), icon("question-circle"))
            ),
            min = 0, max = 100, value = 100,
            step = 1, post = "%"
          ), # end of sliderinput
          
          radioButtons(
            ns("mask_apply_smooth"),
            label = span(
              i18n$t("Smooth / bufferize the cloud-covered surface?\u2000"),
              actionLink(ns("help_mask_smooth"), icon("question-circle"))
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
            condition = sprintf("input['%s'] == 'TRUE'", ns("mask_apply_smooth")),
            fluidRow(
              column(
                width = 6,
                numericInput(ns("mask_smooth"),
                             i18n$t("Smooth (m)"),
                             value = 250,
                             min = 0
                )
              ),
              column(
                width = 6,
                numericInput(ns("mask_buffer"),
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
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  observeEvent(input$help_mask, {
    showModal(modalDialog(
      title = i18n$t("Mask cloud-covered pixels?"),
      p(HTML(
        i18n$t("<strong>Yes</strong>:"),
        i18n$t("the pixels classified as clouds are set to NA."),
        i18n$t("The Surface Classification Map (SCL) included within Level-2A"),
        i18n$t("products is used to mask clouds."),
        i18n$t("Use the selector below to define which classes have to be considered"),
        i18n$t("as clouds.")
      )),
      p(HTML(
        i18n$t("<strong>No</strong>:"),
        i18n$t("this step is not performed.")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_mask_classes, {
    showModal(modalDialog(
      title = i18n$t("Apply mask to:"),
      p(HTML(
        i18n$t("Select which type of mask have to be applied to rasters.")
      )),
      p(HTML(
        i18n$t("It is possible to choose between seven predefinite masks,"),
        i18n$t("or set a custom one.")
      )),
      p(HTML(
        i18n$t("The predefinite ones are shown in order of masking rate:<ul>"),
        i18n$t("<li><strong>No data:</strong> only missing values, i.e. areas without"),
        i18n$t("data (class 0 within SCL, corresponding to areas not covered"),
        i18n$t("by the current Sentinel orbit) or classified as 'Saturated or"),
        i18n$t("defective' (class 1), are set to NA;</li>"),
        i18n$t("<li><strong>No data and clouds (high probability):</strong>"),
        i18n$t("areas classified as missing values (classes 0 and 1) or highly"),
        i18n$t("probably cloudy (class 9) are set to NA;</li>"),
        i18n$t("<li><strong>No data and clouds (high-medium prob.):</strong>"),
        i18n$t("areas classified as missing values (classes 0 and 1) or with a high"),
        i18n$t("or medium cloud probability (resp. classes 9 and 8) "),
        i18n$t("are set to NA;</li>"),
        i18n$t("<li><strong>No data and clouds (any probability):</strong>"),
        i18n$t("areas classified as missing values (classes 0 and 1) or cloudly"),
        i18n$t("(classes 7, 8 and 9) are set to NA;</li>"),
        i18n$t("<li><strong>No data, clouds and shadows:</strong>"),
        i18n$t("areas classified as missing values (classes 0 and 1), cloudly"),
        i18n$t("(classes 7, 8 and 9) or shady (classes 2 and 3) are set to NA;</li>"),
        i18n$t("<li><strong>All except clear-sky:</strong>"),
        i18n$t("areas classified as missing values (classes 0 and 1), cloudly"),
        i18n$t("(classes 7, 8 and 9), shady (classes 2 and 3) and as cirrus"),
        i18n$t("(class 10) are set to NA (in other words, only pixels classified"),
        i18n$t("as vegetation (class 4), bare soil (class 5), water (class 6) and"),
        i18n$t("snow (class 11) are maintained);</li>"),
        i18n$t("<li><strong>All except land surface:</strong>"),
        i18n$t("areas classified as missing values (classes 0 and 1), cloudly"),
        i18n$t("(classes 7, 8 and 9), shady (classes 2 and 3), as cirrus (class 10),"),
        i18n$t("water (class 6) and snow (class 11) are set to NA (in other words,"),
        i18n$t("only pixels classified as vegetation (class 4) and bare soil"),
        i18n$t("(class 5) are maintained);</li>"),
        "</ul>"
      )),
      p(HTML(
        i18n$t("If none of them is suitable for the user, it is possible to define"),
        i18n$t("a custom mask by manually selecting the classes to be masked."),
        i18n$t("See the <a href='https://earth.esa.int/web/sentinel/technical-guides/sentinel-2-msi/level-2a/algorithm'"),
        i18n$t("target='_blank'>classification algorithm</a> for further details.")
      )),
      p(HTML(
        i18n$t("Notice that this functionality does not ensure to correctly mask"),
        i18n$t("all the clouds: in fact, the SCL product is an automatic"),
        i18n$t("classification performed by Sen2Cor, and it is subject to errors"),
        i18n$t("(see i.e. <a href='https://elib.dlr.de/119324/1/S2-validation_meeting_Main-Knorn_20180128.pdf'"),
        i18n$t("target='_blank'>this"),
        i18n$t("report</a> and <a href='https://labo.obs-mip.fr/multitemp/quantitative-comparison-of-cloud-masks-from-maccsmaja-sen2cor-and-geosys-hand-made/'"),
        i18n$t("target='_blank'>this"),
        i18n$t("report</a>).")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_masked_perc, {
    showModal(modalDialog(
      title = i18n$t("Maximum allowed cloud cover"),
      p(HTML(
        i18n$t("Set the maximum percentage of areas classified as cloudy which"),
        i18n$t("should be tolerate."),
        i18n$t("If a higher value is computed, the output image is not produced.")
      )),
      p(HTML(
        i18n$t("Notice that the cloud covered surface is computed"),
        i18n$t("as the percentage of non-cloudy pixels on the total number"),
        i18n$t("of pixels included in the extent."),
        i18n$t("So, if the user chosed not to mask outside the polygons used as extent,"),
        i18n$t("the considered pixels are the pixels included in the bounding box"),
        i18n$t("of the extent;"),
        i18n$t("conversely, if the area outside the polygons has been masked,"),
        i18n$t("the percentage is computed only on pixels within the polygons."),
        i18n$t("Moreover, if the user chosed not to clip on the extent,"),
        i18n$t("the percentage is computed on all the pixels of the tile.")
      )),
      p(HTML(
        i18n$t("Notice also two more details:<ul>"),
        i18n$t("<li>the percentage is always computed on pixels, even if the user"),
        i18n$t("chosed to smooth the mask and/or to apply a buffer;</li>"),
        i18n$t("<li>pixels outside the current Sentinel-2 orbit (class 0 of SCL map)"),
        i18n$t("are threated as other cloud-masked pixels; i.e., an image which is"),
        i18n$t("50% nodata and with a 10% of the remaining pixels classified as cloudy"),
        i18n$t("will be produced only if the maximum allowed cloud surface is set"),
        i18n$t("> 55%.</li></ul>")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_mask_smooth, {
    showModal(modalDialog(
      title = i18n$t("Smooth / bufferize the cloud-covered surface?"),
      size = "l",
      p(HTML(
        i18n$t("By default, the cloud mask is applied at pixel level"),
        i18n$t("(pixels classified as cloudy are masked singularly)."),
        i18n$t("In this way, output image could appear grainy."),
        i18n$t("Moreover, sometimes pixels adiacent to masked areas could appear"),
        i18n$t("cloudy in turn, so it could be useful to mask also cloud borders.")
      )),
      p(HTML(
        i18n$t("To prevent producing grainy output images, cloud masks can be "),
        i18n$t("smoothed before applying them to output images."),
        i18n$t("This has the effect to remove isolated masked pixels (holes)"),
        i18n$t(" and to mask isolated non-masked pixels (isles)."),
        i18n$t("In order to mask cloud borders, a buffer can be also applied to masks."),
        i18n$t("Using higher buffer radius will produce images with a higher"),
        i18n$t("probability to be clean, but with a higher data loss.")
      )),
      a(href="www/mask_types.jpg", target="_blank",
        img(src="www/mask_types.jpg", width = "100%")),
      p(HTML(
        i18n$t("The image above shows the effect of different smoothing / buffer"),
        i18n$t("values (click on the figure to enlarge it)."),
        i18n$t("Panel 1 shows a scene which was masked without applying any smoothing"),
        i18n$t("nor a buffer; panels 2 to 9 shows different combination of"),
        i18n$t("smoothing and buffer radiuses."),
        i18n$t("Applying a buffer without smoothing the mask results in emphatising"),
        i18n$t("isolated masked pixels (panels 2 and 3), so this is a conservative"),
        i18n$t("choice that implicates some data loss."),
        i18n$t("Conversely, using a smoothing radius allows not to loose isolated"),
        i18n$t("masked pixels (e.g. small urban areas), but in this way it is"),
        i18n$t("probably to maintain a high number of cloudy pixels (panels 4 and 7);"),
        i18n$t("for this reason, it is commonly recommended to use a smoothing radius"),
        i18n$t("in combination with a buffer radius with a similar magnitude (panel 5).")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
 
}
    
## To be copied in the UI
# mod_cloud_mask_ui("cloud_mask_ui_1")
    
## To be copied in the server
# callModule(mod_cloud_mask_server, "cloud_mask_ui_1")
 
