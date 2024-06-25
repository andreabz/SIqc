#' 02_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card page_fillable value_box layout_columns input_task_button
#' @importFrom plotly function
mod_02_repeatability_results_ui <- function(id) {
  ns <- NS(id)
  tagList(div(class = "container", div(
    style = "margin-top: 20px;",
    div(
      style = "display: inline-block;
                     vertical-align: top;
                     width: 300px;
                     padding-left: 10px;",
      shiny::selectInput(
        inputId = ns("method"),
        label = "Metodo",
        choices = sql_get_list(conn, "metodo")
      )
    ),
    div(
      style = "display: inline-block;
                     vertical-align: top;
                     width: 300px;
                     padding-left: 10px;",
      shiny::selectInput(
        inputId = ns("parameter"),
        label = "Analita",
        choices = NULL
      )
    ),
    div(
      style = "display: inline-block;
                     vertical-align: top;
                     width: 250px;
                     margin-top: 32px;
                     padding-left: 25px;",
      bslib::input_task_button(
        id = ns("add_task"),
        label = "Filtra i risultati",
        icon = shiny::icon("filter")
      )
    )
  )),

  bslib::layout_column_wrap(
    width = 1/2,
    height = 300,

    plotly::plotlyOutput(ns("plotbytime")),
    plotly::plotlyOutput(ns("plotbyconc"))

  ),

  DT::DTOutput(ns("reptbl"))

  )
}

#' 02_results Server Functions
#'
#' @noRd
mod_02_repeatability_results_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$method, {
      # get parameters for method id
    })


  })
}

## To be copied in the UI
# mod_02_results_ui("02_results_1")

## To be copied in the server
# mod_02_results_server("02_results_1")
