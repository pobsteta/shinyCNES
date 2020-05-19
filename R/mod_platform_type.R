#' platform_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_platform_type_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    fluidRow(
      #### LANDSAT ####
      conditionalPanel(
        condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'landsat'", ns("product"), ns("theiacollection")),
        box(
          title = i18n$t("Type of platform"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          radioButtons(
            ns("theiaplatformlandsat"), NULL,
            choiceNames = list(
              span(
                a("LANDSAT5",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                  target = "_blank"
                )
              ),
              span(
                a("LANDSAT7",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              ),
              span(
                a("LANDSAT8",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              )
            ),
            choiceValues = list("landsat5", "landsat7", "landsat8"),
            selected = "landsat8",
            inline = FALSE
          ) # end radiobutton
        ) # end box
      ), # end conditionalpanel
      #### SPOTWORLDHERITAGE ####
      conditionalPanel(
        condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'spotworldheritage'", ns("product"), ns("theiacollection")),
        box(
          title = i18n$t("Type of platform"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          radioButtons(
            ns("theiaplatformspotworldheritage"), NULL,
            choiceNames = list(
              span(
                a("SPOT1",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                  target = "_blank"
                )
              ),
              span(
                a("SPOT2",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              ),
              span(
                a("SPOT3",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              ),
              span(
                a("SPOT4",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              ),
              span(
                a("SPOT5",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              )
            ),
            choiceValues = list("spot1", "spot2", "spot3", "spot4", "spot5"),
            selected = "spot5",
            inline = FALSE
          ) # end radiobutton
        ) # end box
      ), # end conditionalPanel
      #### SENTINEL2 ####
      conditionalPanel(
        condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'sentinel2'", ns("product"), ns("theiacollection")),
        box(
          title = i18n$t("Type of platform"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          radioButtons(
            ns("theiaplatformsentinel"), NULL,
            choiceNames = list(
              span(
                a("SENTINEL2A",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                  target = "_blank"
                )
              ),
              span(
                a("SENTINEL2B",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              )
            ),
            choiceValues = list("s2a", "s2b"),
            selected = "s2b",
            inline = FALSE
          ) # end radiobutton
        ) # end box
      ), # end conditionalpanel
      #### VENUS ####
      conditionalPanel(
        condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'venus'", ns("product"), ns("theiacollection")),
        box(
          title = i18n$t("Type of platform"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          radioButtons(
            ns("theiaplatformvenus"), NULL,
            choiceNames = list(
              span(
                a("VENUS",
                  href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                  target = "_blank"
                )
              )
            ),
            choiceValues = list("venus"),
            selected = "venus",
            inline = FALSE
          ) # end radiobutton
        ) # end box
      ) # end conditionalpanel
    ) # end fluidrow
  ) # end taglist
}
    
#' platform_type Server Function
#'
#' @noRd 
mod_platform_type_server <- function(input, output, session, rv){
  ns <- session$ns
  
  # for save parameters
  observe({
    rv$theiaplatformlandsat <- input$theiaplatformlandsat
    rv$theiaplatformspotworldheritage <- input$theiaplatformspotworldheritage
    rv$theiaplatformsentinel <- input$theiaplatformsentinel
    rv$theiaplatformvenus <- input$theiaplatformvenus
  })
 
}
    
## To be copied in the UI
# mod_platform_type_ui("platform_type_ui_1")
    
## To be copied in the server
# callModule(mod_platform_type_server, "platform_type_ui_1")
 
