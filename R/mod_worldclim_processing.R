#' worldclim_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_worldclim_processing_ui <- function(id) {
  ns <- NS(id)

  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  tagList(
    tabName = "worldclim_processing",
    h3(i18n$t("Processing WorldClim")),
    fluidRow(
      box(
        title = span(i18n$t("Options WorldClim"), 
                     actionLink(ns("help_worldclim"), icon("question-circle"))
        ),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        column(
          width = 3,
          pickerInput(
            ns("period_checkbox"),
            span(i18n$t("Periods selected"), 
                 actionLink(ns("help_worldclim_periods"), icon("question-circle"))
            ),
            choices = c("1970-2000", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2018", "2021-2040", "2041-2060", "2061-2080", "2081-2100"),
            options = list(
              `selected-text-format` = "count > 6",
              `live-search` = TRUE,
              `actions-box` = TRUE,
              title = i18n$t("All overlapping periods")
            ),
            multiple = TRUE
          ) # end pickerinput
        ), # end column
        column(
          width = 2,
          pickerInput(
            ns("resolution_checkbox"),
            span(i18n$t("Resolution selected"),
                 actionLink(ns("help_worldclim_resolutions"), icon("question-circle"))
            ),
            choices = c("0.5", "2.5", "5", "10"),
            options = list(
              `selected-text-format` = "count > 6",
              `live-search` = TRUE,
              `actions-box` = TRUE,
              title = i18n$t("All overlapping revolutions")
            ),
            multiple = TRUE
          ) # end pickerinput
        ), # end column
        column(
          width = 2,
          pickerInput(
            ns("variable_checkbox"),
            span(i18n$t("Variables selected"),
                 actionLink(ns("help_worldclim_variables"), icon("question-circle"))
            ),
            choices = c("tmin", "tmax", "tavg", "prec", "srad", "wind", "vapr", "bio", "elev"),
            options = list(
              `selected-text-format` = "count > 6",
              `live-search` = TRUE,
              `actions-box` = TRUE,
              title = i18n$t("All overlapping variables")
            ),
            multiple = TRUE
          ) # end pickerinput
        ), # end column
        column(
          width = 3,
          pickerInput(
            ns("gcm_checkbox"),
            span(i18n$t("GCM selected"),
                 actionLink(ns("help_worldclim_gcms"), icon("question-circle"))
            ),
            choices = c("BCC-CSM2-MR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5", "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0"),
            options = list(
              `selected-text-format` = "count > 6",
              `live-search` = TRUE,
              `actions-box` = TRUE,
              title = i18n$t("All overlapping gcm")
            ),
            multiple = TRUE
          ) # end pickerinput
        ), # end column
        column(
          width = 2,
          pickerInput(
            ns("ssp_checkbox"),
            span(i18n$t("SSP selected"),
                 actionLink(ns("help_worldclim_ssp"), icon("question-circle"))
            ),
            choices = c("126", "245", "370", "585"),
            options = list(
              `selected-text-format` = "count > 6",
              `live-search` = TRUE,
              `actions-box` = TRUE,
              title = i18n$t("All overlapping ssp")
            ),
            multiple = TRUE
          ) # end pickerinput
        ), # end column
        column(
          width = 4,
          span(
            div(
              style = "padding-top:5px;padding-bottom:10px;",
              actionButton(
                ns("goWorldClim"),
                label = i18n$t("WorldClim\u2000"),
                icon = icon("upload"),
                class = "darkbutton"
              )
            ) # end div
          ) # end span
        ), # end column
      ), # end box
      box(
        title = i18n$t("Tiles"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        box(
          title = i18n$t("Tiles WorldClim Past"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          selectInput(ns("listimgwcpast"), i18n$t("Tiles WorldClim past:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            plotOutput(ns("imgwcpast"), height = 500, width = 500)
          ), # end div
          textInput(ns("list_imgwcpasts"), label = "")
        ), # end box
        box(
          title = i18n$t("Tiles WorldClim Present"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          selectInput(ns("listimgwcpresent"), i18n$t("Tiles WorldClim present:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            plotOutput(ns("imgwcpresent"), height = 500, width = 500)
          ), # end div
          textInput(ns("list_imgwcpresents"), label = "")
        ), # end box
        box(
          title = i18n$t("Tiles WorldClim Future"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 4,
          selectInput(ns("listimgwcfuture"), i18n$t("Tiles WorldClim future:"), c("No tile" = "")),
          div(
            style = "display:inline-block;horizontal-align:center;",
            plotOutput(ns("imgwcfuture"), height = 500, width = 500)
          ), # end div
          textInput(ns("list_imgwcfutures"), label = "")
        ) # end box
      ) # end box
    ) # end fluidrow 
  ) # end tabitem tab_srtm_processing
}

#' worldclim_processing Server Function
#'
#' @noRd
mod_worldclim_processing_server <- function(input, output, session, rv) {
  ns <- session$ns
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  # for save parameters
  observe({
    rv$project_name <- input$project_name
    rv$path_project <- input$path_project_textin
    rv$path_worldclim <- file.path(rv$path_project, "/worldclim")
    rv$list_imgwcpast <- unlist(strsplit(input$list_imgwcpasts, ","))
    rv$list_imgwcpresent <- unlist(strsplit(input$list_imgwcpresents, ","))
    rv$list_imgwcfuture <- unlist(strsplit(input$list_imgwcfutures, ","))
  })
  
  observeEvent(input$help_worldclim, {
    showModal(modalDialog(
      title = i18n$t("WorldClim"),
      p(HTML(
        i18n$t("This is WorldClim version 2.1 climate data for 1970-2000."),
        i18n$t("This version was released in January 2020.")
      )),
      p(HTML(
        i18n$t("There are monthly climate data for minimum, mean, and maximum"),
        i18n$t("temperature, precipitation, solar radiation, wind speed, water"),
        i18n$t("vapor pressure, and for total precipitation. There are also 19"),
        i18n$t("“bioclimatic” variables.")
      )),
      p(HTML(
        i18n$t("The data is available at the four spatial resolutions, between"), 
        i18n$t("30 seconds (~1 km2) to 10 minutes (~340 km2). Each download is"), 
        i18n$t("a “zip” file containing 12 GeoTiff (.tif) files, one for each"),
        i18n$t("month of the year (January is 1; December is 12).")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_worldclim_periods, {
    showModal(modalDialog(
      title = i18n$t("WorldClim"),
      p(HTML(
        i18n$t("This is WorldClim version 2.1 climate data for 1970-2000."), 
        i18n$t("This version was released in January 2020.")
      )),
      p(HTML(
        i18n$t("11 periods : 1970-2000, 1960-1969, 1970-1979, 1980-1989, 1990-1999, 2000-2009, 2010-2018, 2021-2040, 2041-2060, 2061-2080, 2081-2100"),
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_worldclim_resolutions, {
    showModal(modalDialog(
      title = i18n$t("WorldClim"),
      p(HTML(
        i18n$t("The data is available at the four spatial resolutions,"), 
        i18n$t("between 30 seconds (~1 km2), 2.5 minutes (~86 km2),"),
        i18n$t("5 minutes (~171 km2) and 10 minutes (~340 km2)")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_worldclim_variables, {
    showModal(modalDialog(
      title = i18n$t("WorldClim"),
      p(HTML(
        i18n$t("Bioclimatic variables are derived from the monthly temperature"),
        i18n$t("and rainfall values in order to generate more biologically"),
        i18n$t("meaningful variables. These are often used in species distribution"),
        i18n$t("modeling and related ecological modeling techniques. The bioclimatic"),
        i18n$t("variables represent annual trends (e.g., mean annual temperature,"),
        i18n$t("annual precipitation) seasonality (e.g., annual range in temperature"),
        i18n$t("and precipitation) and extreme or limiting environmental factors"),
        i18n$t("(e.g., temperature of the coldest and warmest month, and precipitation"),
        i18n$t("of the wet and dry quarters). A quarter is a period of three months"),
        i18n$t("(1/4 of the year).")
      )),
      p(HTML(
        i18n$t("They are coded as follows:<ul>"),
        i18n$t("<li>BIO1 = Annual Mean Temperature</li>"),
        i18n$t("<li>BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))</li>"),
        i18n$t("<li>BIO3 = Isothermality (BIO2/BIO7) (×100)</li>"),
        i18n$t("<li>BIO4 = Temperature Seasonality (standard deviation ×100)</li>"),
        i18n$t("<li>BIO5 = Max Temperature of Warmest Month</li>"),
        i18n$t("<li>BIO6 = Min Temperature of Coldest Month</li>"),
        i18n$t("<li>BIO7 = Temperature Annual Range (BIO5-BIO6)</li>"),
        i18n$t("<li>BIO8 = Mean Temperature of Wettest Quarter</li>"),
        i18n$t("<li>BIO9 = Mean Temperature of Driest Quarter</li>"),
        i18n$t("<li>BIO10 = Mean Temperature of Warmest Quarter</li>"),
        i18n$t("<li>BIO11 = Mean Temperature of Coldest Quarter</li>"),
        i18n$t("<li>BIO12 = Annual Precipitation</li>"),
        i18n$t("<li>BIO13 = Precipitation of Wettest Month</li>"),
        i18n$t("<li>BIO14 = Precipitation of Driest Month</li>"),
        i18n$t("<li>BIO15 = Precipitation Seasonality (Coefficient of Variation)</li>"),
        i18n$t("<li>BIO16 = Precipitation of Wettest Quarter</li>"),
        i18n$t("<li>BIO17 = Precipitation of Driest Quarter</li>"),
        i18n$t("<li>BIO18 = Precipitation of Warmest Quarter</li>"),
        i18n$t("<li>BIO19 = Precipitation of Coldest Quarter</li></ul>")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_worldclim_gcms, {
    showModal(modalDialog(
      title = i18n$t("WorldClim"),
      p(HTML(
        i18n$t("The data available here are CMIP6 downscaled future climate"),
        i18n$t("projections. The downscaling and calibration (bias correction)"),
        i18n$t("was done with WorldClim v2.1 as baseline climate.")
      )),
      p(HTML(
        i18n$t("Monthly values of minimum temperature, maximum temperature, and"),
        i18n$t("precipitation were processed for nine global climate models (GCMs):"),
        i18n$t("BCC-CSM2-MR, CNRM-CM6-1, CNRM-ESM2-1, CanESM5, GFDL-ESM4, IPSL-CM6A-LR,"),
        i18n$t("MIROC-ES2L, MIROC6, MRI-ESM2-0, and for four Shared Socio-economic"),
        i18n$t("Pathways (SSPs): 126, 245, 370 and 585.)")
      )),
      p(HTML(
        i18n$t("The monthly values were averages over 20 year periods (2021-2040"),
        i18n$t(", 241-2060, 2061-2080, 2081-2100). The following spatial resolutions"),
        i18n$t("are available (expressed as minutes of a degree of longitude and"),
        i18n$t("latitude): 10 minutes, 5 minutes, 2.5 minutes.")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_worldclim_ssp, {
    showModal(modalDialog(
      title = i18n$t("WorldClim"),
      p(HTML(
        i18n$t("The data available here are CMIP6 downscaled future climate"),
        i18n$t("projections. The downscaling and calibration (bias correction)"),
        i18n$t("was done with WorldClim v2.1 as baseline climate.")
      )),
      p(HTML(
        i18n$t("Monthly values of minimum temperature, maximum temperature, and"),
        i18n$t("precipitation were processed for nine global climate models (GCMs):"),
        i18n$t("BCC-CSM2-MR, CNRM-CM6-1, CNRM-ESM2-1, CanESM5, GFDL-ESM4, IPSL-CM6A-LR,"),
        i18n$t("MIROC-ES2L, MIROC6, MRI-ESM2-0, and for four Shared Socio-economic"),
        i18n$t("Pathways (SSPs): 126, 245, 370 and 585.)")
      )),
      p(HTML(
        i18n$t("The monthly values were averages over 20 year periods (2021-2040"),
        i18n$t(", 241-2060, 2061-2080, 2081-2100). The following spatial resolutions"),
        i18n$t("are available (expressed as minutes of a degree of longitude and"),
        i18n$t("latitude): 10 minutes, 5 minutes, 2.5 minutes.")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # if goWorldClim is pressend
  observeEvent(input$goWorldClim, {
    req(rv)
    s <- input$gcm_checkbox
    print(file.path(rv$path_project, "worldclim"))
    if (!all(is.null(c(input$resolution_checkbox, input$variable_checkbox, input$period_checkbox, input$gcm_checkbox, input$ssp_checkbox)))) {
      # for present and past
      for (r in input$resolution_checkbox) {
        for (v in input$variable_checkbox) {
          if (is.null(input$period_checkbox)) {
            wc <- tryCatch(getDataWorldClim("worldclim", 
                                            var = v, 
                                            res = r, 
                                            path = file.path(rv$path_project, "worldclim"), 
                                            download = TRUE
            ),
            error = function(e) print(i18n$t("Error in worldclim function!"))
            )
          } else {
            for (p in input$period_checkbox) {
              print(paste("present/past period:", v, r, p))
              wc <- tryCatch(getDataWorldClim("worldclim", 
                                                      var = v, 
                                                      res = r,
                                                      period = p,
                                                      path = file.path(rv$path_project, "worldclim"),  
                                                      download = TRUE
              ),
              error = function(e) print(i18n$t("Error in worldclim function!"))
              )
            }
          }
        }
      }
      # for future
      for (r in input$resolution_checkbox) {
        for (v in input$variable_checkbox) {
          if (is.null(input$period_checkbox)) {
            for (g in input$gcm_checkbox) {
              for (s in input$ssp_checkbox) {
                wc <- tryCatch(getDataWorldClim("worldclim", 
                                                        var = v, 
                                                        res = r, 
                                                        gcm = g,
                                                        ssp = s,
                                                        path = file.path(rv$path_project, "worldclim"), 
                                                        download = TRUE
                ),
                error = function(e) print(i18n$t("Error in worldclim function!"))
                )
              } # end for s
            } # end for g
          } else {
            for (p in input$period_checkbox) {
              for (g in input$gcm_checkbox) {
                for (s in input$ssp_checkbox) {
                  print(paste("future period:", v, r, p, g, s))
                  wc <- tryCatch(getDataWorldClim("worldclim", 
                                                          var = v, 
                                                          res = r,
                                                          period = p,
                                                          gcm = g,
                                                          ssp = s,
                                                          path = file.path(rv$path_project, "worldclim"),  
                                                          download = TRUE
                  ),
                  error = function(e) print(i18n$t("Error in worldclim function!"))
                  )
                } # end for s
              } # end for g
            } # end for p
          } # end else
        } # end for v
      } # end for r
    } else {
      wc <- NULL
    }
    
    print(paste("wc", wc))
    #### image list WorldClim ####
    # updateSelectInput(session, "listimgwcpast", choices = c("Choose a picture" = "", wc[1]))
    # updateTextInput(session, "list_imgpasts", value = res[1])
    # #### image list SRTM simple ####
    # updateSelectInput(session, "listimgwcpresent", choices = c("Choose a picture" = "", wc[2:8]))
    # updateTextInput(session, "list_imgwcpresents", value = res[2:8])
    # #### image list SRTM multiple ####
    # updateSelectInput(session, "listimgwcfuture", choices = c("Choose a picture" = "", wc[9:length(wc)]))
    # updateTextInput(session, "list_imgwcfutures", value = wc[9:length(wc)])
  })
  
  # image worldclim past
  output$imgwcpast <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      # s2_preview_worldclim(
      #   path_data = rv$path_project,
      #   project_name = rv$project_name,
      #   path_worldclim = rv$path_worldclim,
      #   tile = input$listimgwcpast
      # )
    }
  })
  
  # image worldclim present
  output$imgwcpresent <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      # s2_preview_worldclim(
      #   path_data = rv$path_project,
      #   project_name = rv$project_name,
      #   path_worldclim = rv$path_worldclim,
      #   tile = input$listimgwcpresent
      # )
    }
  })
  
  # image worldclim future
  output$imgwcfuture <- renderPlot({
    if (!is.null(file.path(rv$path_project, rv$project_name))) {
      # s2_preview_worldclim(
      #   path_data = rv$path_project,
      #   project_name = rv$project_name,
      #   path_worldclim = rv$path_worldclim,
      #   tile = input$listimgswcfuture
      # )
    }
  })
  
}

## To be copied in the UI
# mod_worldclim_processing_ui("worldclim_processing_ui_1")

## To be copied in the server
# callModule(mod_worldclim_processing_server, "worldclim_processing_ui_1")
