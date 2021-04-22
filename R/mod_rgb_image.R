#' rgb_image UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rgb_image_ui <- function(id) {
  ns <- NS(id)

  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  tagList(
    tabName = "rgb",
    h3(i18n$t("RGB images selection")),
    fluidRow(
      box(
        title = i18n$t("RGB images"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        uiOutput(ns("checkbox_list_rgb")),
        textInput(ns("list_checkbox_rgb"), label = ""),
        div(
          style = "display:inline-block;padding-top:10px;padding-right:10px;",
          actionButton(
            ns("new_rgb"),
            label = i18n$t("\u2000Define custom RGB image"),
            icon = icon("plus")
          )
        ),
        div(
          style = "display:inline-block;padding-top:10px;",
          actionButton(
            ns("rm_rgb"),
            label = i18n$t("\u2000Remove unselected RGB from list"),
            icon = icon("trash-alt")
          )
        ) # end div
      ) # end box
    ) # end fluidrow
  ) # end taglist
}

#' rgb_image Server Function
#'
#' @noRd
mod_rgb_image_server <- function(input, output, session, rv) {
  ns <- session$ns

  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  # for save parameters
  observe({
    rv$list_rgb <- input$list_rgb
    updateTextInput(session, "list_checkbox_rgb", 
                    value = sort(nn(input$list_rgb)))
  })

  rgb_req <- reactive({
    !is.null(rv$list_rgb_ranges)
  })
  # convert in output value to be used in conditionalPanel
  output$rgb_req <- renderText(rgb_req())
  # options to update these values also if not visible
  outputOptions(output, "rgb_req", suspendWhenHidden = FALSE)

  # list with the names of Sentinel-2 bands
  s2_bands <- list("TOA" = list(
    "Band 1 (443 nm)" = "band1",
    "Band 2 (490 nm)" = "band2",
    "Band 3 (560 nm)" = "band3",
    "Band 4 (665 nm)" = "band4",
    "Band 5 (705 nm)" = "band5",
    "Band 6 (740 nm)" = "band6",
    "Band 7 (783 nm)" = "band7",
    "Band 8 (842 nm)" = "band8",
    "Band 9 (940 nm)" = "band9",
    "Band 10 (1375 nm)" = "band10",
    "Band 11 (1610 nm)" = "band11",
    "Band 12 (2190 nm)" = "band12"
  ))
  s2_bands[["BOA"]] <- s2_bands[["TOA"]][c(1:9, 11:12)]

  # Define default RGB images
  rv$list_rgb_ranges <- list(
    "RGB432B" = c(0, 2500),
    "RGB843B" = matrix(c(0, 0, 0, 7500, 2500, 2500), ncol = 2),
    "RGBb84B" = matrix(c(0, 0, 0, 7500, 7500, 2500), ncol = 2)
  )

  # Open modalDialog to add a new RGB
  observeEvent(input$new_rgb, {
    showModal(add_rgb_image(s2_bands = s2_bands, session = session))
  })
  # Add the new defined RGB
  observeEvent(input$add_new_rgb, {
    newrgb_ranges <- if (all(c(
      input$band_r_range == input$band_g_range,
      input$band_g_range == input$band_b_range
    ))) {
      list(input$band_r_range * 1E4)
    } else {
      list(t(matrix(
        c(input$band_r_range, input$band_g_range, input$band_b_range) * 1E4,
        nrow = 2
      )))
    }
    names(newrgb_ranges) <- paste0(
      "RGB",
      paste(as.hexmode(c(
        as.integer(gsub("^band", "", input$band_r)),
        as.integer(gsub("^band", "", input$band_g)),
        as.integer(gsub("^band", "", input$band_b))
      )), collapse = ""),
      substr(input$newrgb_source, 1, 1)
    )
    rv$list_rgb_ranges <- append(newrgb_ranges, rv$list_rgb_ranges)
    # new added RGB replaces existing one, if it is was already present
    rv$list_rgb_ranges <- rv$list_rgb_ranges[!duplicated(names(rv$list_rgb_ranges))]
    # order by name
    rv$list_rgb_ranges <- rv$list_rgb_ranges[order(names(rv$list_rgb_ranges))]
    removeModal()
  })

  # List of defined RGB images
  output$checkbox_list_rgb <- renderUI({
    checkboxGroupInput(
      ns("list_rgb"),
      label = span(
        i18n$t("RGB images:\u2000"),
        actionLink(ns("help_rgb"), icon("question-circle"))
      ),
      choiceNames = lapply(names(rv$list_rgb_ranges), function(x) {
        ranges <- if (length(rv$list_rgb_ranges[[x]]) == 6) {
          c("min_r" = 1, "min_g" = 2, "min_b" = 3, "max_r" = 4, "max_g" = 5, "max_b" = 6)
        } else if (length(rv$list_rgb_ranges[[x]]) == 2) {
          c("min_r" = 1, "min_g" = 1, "min_b" = 1, "max_r" = 2, "max_g" = 2, "max_b" = 2)
        }
        HTML(paste0(
          "<strong>", x, ":</strong> ",
          "<ul><li><i>source:</i> ", substr(x, 7, 7), "OA</li>",
          "<li><span style='color:red;'><i>red: </i>",
          "band ", strtoi(paste0("0x", substr(x, 4, 4))),
          " (reflectance range ",
          rv$list_rgb_ranges[[x]][ranges["min_r"]] / 1E4, " \u2013 ",
          rv$list_rgb_ranges[[x]][ranges["max_r"]] / 1E4, ")</span></li>",
          "<li><span style='color:darkgreen;'><i>green: </i>",
          "band ", strtoi(paste0("0x", substr(x, 5, 5))),
          " (reflectance range ",
          rv$list_rgb_ranges[[x]][ranges["min_g"]] / 1E4, " \u2013 ",
          rv$list_rgb_ranges[[x]][ranges["max_g"]] / 1E4, ")</span></li>",
          "<li><span style='color:blue;'><i>blue: </i>",
          "band ", strtoi(paste0("0x", substr(x, 6, 6))),
          " (reflectance range ",
          rv$list_rgb_ranges[[x]][ranges["min_b"]] / 1E4, " \u2013 ",
          rv$list_rgb_ranges[[x]][ranges["max_b"]] / 1E4, ")</span></li></ul>"
        ))
      }),
      choiceValues = as.list(names(rv$list_rgb_ranges)),
      selected = rv$list_rgb
    )
  })

  outputOptions(output, "checkbox_list_rgb", suspendWhenHidden = FALSE)
  # this to avoid errors in case a json were imported before activating checkbox_list_rgb
  # TODO: with this trick, also indices_rv could be avoid and simplified

  # Remove selected RGB images
  observeEvent(input$rm_rgb, {
    rv$list_rgb_ranges <- rv$list_rgb_ranges[names(rv$list_rgb_ranges) %in% input$list_rgb]
  })

  # Update RGB bands and ranges when changing RGB source
  observeEvent(input$newrgb_source, {
    updatePickerInput(
      session,
      "band_r",
      choices = s2_bands[[input$newrgb_source]],
      selected = input$band_r
    )
    updatePickerInput(
      session,
      "band_g",
      choices = s2_bands[[input$newrgb_source]],
      selected = input$band_g
    )
    updatePickerInput(
      session,
      "band_b",
      choices = s2_bands[[input$newrgb_source]],
      selected = input$band_b
    )
  })
  observeEvent(input$band_r, {
    updateSliderInput(
      session,
      "band_r_range",
      value = if (input$band_r %in% paste0("band", 1:5)) {
        c(0, 0.25)
      } else if (input$band_r %in% paste0("band", 6:12)) {
        c(0, 0.75)
      }
    )
  })
  observeEvent(input$band_g, {
    updateSliderInput(
      session,
      "band_g_range",
      value = if (input$band_g %in% paste0("band", 1:5)) {
        c(0, 0.25)
      } else if (input$band_g %in% paste0("band", 6:12)) {
        c(0, 0.75)
      }
    )
  })
  observeEvent(input$band_b, {
    updateSliderInput(
      session,
      "band_b_range",
      value = if (input$band_b %in% paste0("band", 1:5)) {
        c(0, 0.25)
      } else if (input$band_b %in% paste0("band", 6:12)) {
        c(0, 0.75)
      }
    )
  })

  observeEvent(input$help_rgb, {
    showModal(modalDialog(
      title = i18n$t("RGB images:"),
      p(HTML(
        i18n$t("RGB images can be built using different spectral bands."),
        i18n$t("To distinguish each other, the naming convention"),
        i18n$t("'RGB<strong>rgb</strong><strong>X</strong>', where:"),
        i18n$t("<ul><li><strong>r</strong>, <strong>g</strong> and <strong>b</strong>"),
        i18n$t("are the the number of the bands to be used respectively for red,"),
        i18n$t("green and blue, in hexadecimal format"),
        i18n$t("(so i.e. band 8 is 8, band 11 is b).</li></ul>"),
        i18n$t("<ul><li><strong>X</strong> is B (if source is BOA)"),
        i18n$t("or T (is source is TOA);</li></ul>")
      )),
      p(HTML(
        i18n$t("By default, three RGB images are defined:"),
        i18n$t("<ul><li><strong>RGB432B</strong> is the true colour image,"),
        i18n$t("computed from BOA, with all the three bands rescaled from"),
        i18n$t("reflectance 0 (black) to 0.25 (white);</li>"),
        i18n$t("<li><strong>RGB843B</strong> is the standard false colour image,"),
        i18n$t("where NIR is represented as red, red as green and green as blue;"),
        i18n$t("it is computed from BOA, and NIR band is rescaled from"),
        i18n$t("reflectance 0 (black) to 0.75 (white),"),
        i18n$t("while red and green from 0 to 0.25 (this because NIR reflectance"),
        i18n$t("is usually higher than red and green);</li>"),
        i18n$t("<li><strong>RGBb84B</strong> is a false colour image"),
        i18n$t("commonly used to emphatise vegetated area,"),
        i18n$t("in which SWIR is represented as red, NIR as green and red as blue;"),
        i18n$t("it is computed from BOA, SWIR and NIR band are rescaled from"),
        i18n$t("reflectance 0 (black) to 0.75 (white),"),
        i18n$t("while red one from 0 to 0.25.</li></ul>")
      )),
      p(HTML(
        i18n$t("The button 'Define custom RGB image' can be used to define"),
        i18n$t("different combinations on bands, to compute images from TOA"),
        i18n$t("or to change scale ranges.")
      )),
      p(HTML(
        i18n$t("Notice that, after defining new images, they must be checked"),
        i18n$t("in order to be computed (only checked products are considered,"),
        i18n$t("as for spectral indices).")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

## To be copied in the UI
# mod_rgb_image_ui("rgb_image_ui_1")

## To be copied in the server
# callModule(mod_rgb_image_server, "rgb_image_ui_1")
