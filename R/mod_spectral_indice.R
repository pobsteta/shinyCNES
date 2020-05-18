#' spectral_indice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spectral_indice_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "index",
    h3(i18n$t("Select spectral indices")),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'theia' | input['%s'] == 'peps'", ns("product"), ns("product")),
      fluidRow(
        box(
          title = i18n$t("Spectral indices selection"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 8,
          fluidRow(
            column(
              width = 6,
              radioButtons(
                ns("index_source"),
                label = span(
                  i18n$t("Build indices from:\u2000"),
                  actionLink(ns("help_index_source"), icon("question-circle"))
                ),
                choices = list(
                  "BOA" = "BOA",
                  "TOA" = "TOA"
                ),
                selected = "BOA",
                inline = TRUE
              )
            ), # end column
            column(
              width = 6,
              selectInput(
                ns("index_datatype"),
                label = i18n$t("Data type"),
                choices = list(
                  "Byte" = "Byte",
                  "Integer (16 bits)" = "Int16",
                  "Float (32 bits)" = "Float32",
                  "Float (64 bits)" = "Float64"
                ),
                selected = "Int16"
              )
            ) # end column
          ), # end fluidrow
          hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),
          fluidRow(
            column(
              width = 5,
              textInput(ns("filter_indices"), i18n$t("Filter indices"))
            ),
            column(
              width = 7,
              checkboxInput(
                ns("verified_indices"),
                label = span(
                  i18n$t("Show only verified indices\u2000"),
                  actionLink(ns("note_list_indices"), icon("warning"))
                ),
                value = TRUE
              )
            ) # end column
          ), # end fluidrow
          uiOutput(ns("check_indices"))
        ), # end box
        uiOutput(ns("show_formula"))
      ) # end fluidrow
    ) # end of conditionalpanel on tab_index
  ) # end taglist
}
    
#' spectral_indice Server Function
#' 
#' @importFrom shiny withMathJax
#'
#' @noRd 
mod_spectral_indice_server <- function(input, output, session, rv){
  ns <- session$ns
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  # Reactive variable: TRUE if indices are required, FALSE if not
  indices_req <- reactive({
    # "indices" %in% input$steps_reqout &
    !is.null(input$list_indices)
  })

  # convert in output value to be used in conditionalPanel
  output$indices_req <- renderText(indices_req())

  # options to update these values also if not visible
  outputOptions(output, "indices_req", suspendWhenHidden = FALSE)
  
  # create the list indices
  create_indices_db()
  
  indices_db <- data.table(list_indices(
    c("n_index","name","longname","s2_formula_mathml","link","checked"),
    all = TRUE
  ))
  check_mark <- span(style="color:darkgreen;", icon("check"))
  check_mark <- gsub("\n *","", as.character(check_mark))
  
  indices_db[,extendedname := paste0(
    name,
    " (",longname,")  ",
    ifelse(checked, check_mark, "")
  )]
  setkey(indices_db, "name")
  
  indices_rv <- reactiveValues()
  
  observe({
    indices_db_verified_idx <- if (input$verified_indices==TRUE) {
      indices_db$checked
    } else {
      rep(TRUE, nrow(indices_db))
    }
    indices_rv$matches <- indices_db[
      indices_db_verified_idx &
        grepl(tolower(input$filter_indices),
              tolower(indices_db$extendedname)),
      name
      ]
    indices_rv$filtered <- indices_db[unique(c(indices_rv$checked,indices_rv$matches)),
                                      list(name,extendedname)]
  })
  
  observe({
    indices_rv$checked <- sort(nn(input$list_indices))
  })
  
  output$check_indices <- renderUI({
    checkboxGroupInput(
      ns("list_indices"),
      label = i18n$t("Indices to be exported"),
      choiceNames = lapply(indices_rv$filtered$extendedname, HTML),
      choiceValues = as.list(indices_rv$filtered$name),
      selected = indices_rv$checked
    )
  })
  
  index_details <- function(index) {
    extendedname <- link <- longname <- name <- providers <- s2_formula_mathml <- NULL
    return(box(
      width=12,
      collapsible = TRUE,
      title=indices_db[name==index,name],
      p(em(indices_db[name==index,longname])),
      p(strong(i18n$t("Formula:")),
        br(),
        shiny::withMathJax(indices_db[name==index,
                               HTML(s2_formula_mathml)])),
      p(a(i18n$t("More info"),
          target="_blank",
          href=indices_db[name==index,link]))
    ))
  }
  
  output$show_formula <- renderUI({
    column(
      width=4,
      fluidRow(
        lapply(indices_rv$checked, index_details)
      )
    )
  })
  
  observeEvent(input$help_index_source, {
    showModal(modalDialog(
      title = i18n$t("Build indices from:"),
      p(HTML(
        "<strong>BOA</strong>:",
        i18n$t("Spectral indices are build from surface reflectances"),
        i18n$t(" (Bottom Of Atmosphere)."),
        i18n$t("This is the default option.")
      )),
      p(HTML(
        "<strong>TOA</strong>:",
        i18n$t("Spectral indices are build from Top Of Atmosphere reflectances."),
        i18n$t("It is strongly suggested not to use this option"),
        i18n$t("(use only if you are not interested to the absolute values"),
        i18n$t("of the indices, and if the atmospheric disturbance in your area"),
        i18n$t("of interest is sufficiently uniform).")
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$note_list_indices, {
    showModal(modalDialog(
      title = i18n$t("Spectral indices"),
      HTML(
        "<table style=\"width:100%\">",
        "<tr>",
        "<td style=\"padding-right: 10px;\">",
        as.character(
          a(href="http://www.indexdatabase.de/db/is.php?sensor_id=96",
            target="_blank",
            img(
              src="http://www.indexdatabase.de/daten/grafik/logo.png",
              alt="IDB logo",
              height="70",
              width="125"
            )
          )
        ),
        "</td>",
        "<td>",
        i18n$t("Spectral indices here listed were mostly taken from"),
        "<a href=\"http://www.indexdatabase.de\" target=\"_blank\">Index DataBase</a>.",
        paste0(i18n$t("Indices marked as verified ("),check_mark,i18n$t(") were checked")),
        i18n$t("in order to ensure that the formula used to compute them"),
        i18n$t("is actually the formula used by the authors, and that"),
        i18n$t("Sentinel-2 bands associated to spectral bands are correct."),
        "</td></tr>"
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
 
}
    
## To be copied in the UI
# mod_spectral_indice_ui("spectral_indice_ui_1")
    
## To be copied in the server
# callModule(mod_spectral_indice_server, "spectral_indice_ui_1")
 
