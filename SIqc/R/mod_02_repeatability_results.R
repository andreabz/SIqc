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
#' @importFrom glue glue
#' @import plotly
mod_02_repeatability_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container", div(
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
          id = ns("filter"),
          label = "Filtra i risultati",
          icon = shiny::icon("filter")
        )
      )
    )),

    bslib::card(
      bslib::card_header(shiny::textOutput(ns("reptxt"))),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1 / 3,
          height = 300,

          plotly::plotlyOutput(ns("plotbytime")),
          plotly::plotlyOutput(ns("plotbyconc")),
          plotly::plotlyOutput(ns("plotbyoperator"))

        ),

        DT::DTOutput(ns("reptbl"))

      )
    )

  )
}

#' 02_results Server Functions
#'
#' @noRd
mod_02_repeatability_results_server <- function(id, r_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r_local <- reactiveValues(
      methodname = NA,
      methodid = NA,
      paramname = NA,
      paramid = NA,
      data = data.table::data.table(),
      title = NA
    )

    observeEvent(input$method, {
      # save the method name and id
      r_local$methodname <- input$method
      r_local$methodid <- sql_get_value(conn, "metodo", "metodo_id", "metodo", input$method)

      # this should be changed to extract only parameters for the given method
      parameters <- sql_get_list(conn, "parametro")
      freezeReactiveValue(input, "parameter")
      updateSelectInput(inputId = "parameter", choices = parameters)
    })

    observeEvent(input$parameter, {
      # save the parameter name and id
      r_local$paramname <- input$parameter
      r_local$paramid <- sql_get_value(conn,
                                       "parametro",
                                       "parametro_id",
                                       "parametro",
                                       input$parameter)
    })

    # store the data in reactive values
    observeEvent(input$filter, {
      r_local$data <- sql_get_repeatability_for_method_parameter(conn, r_local$methodid, r_local$paramid)
      r_local$title <- glue::glue("Parametro {r_local$paramname} per il metodo {r_local$methodname}")
    })

    # card title
    output$reptxt <- renderText({
      req(r_local$title)

      r_local$title
    })

    # repeatability vs time
    output$plotbytime <- plotly::renderPlotly({
      req(nrow(r_local$data) > 0)

      qc_chart_repeatability_time(r_local$data)
    })

    # repeatability vs concentration
    output$plotbyconc <- plotly::renderPlotly({
      req(nrow(r_local$data) > 0)

      qc_chart_repeatability_conc(r_local$data)
    })

    # repeatability vs operator
    output$plotbyoperator <- plotly::renderPlotly({
      req(nrow(r_local$data) > 0)

      qc_chart_repeatability_operator(r_local$data)
    })

    # table of filtered repeatability results
    output$reptbl <- DT::renderDT({
      req(nrow(r_local$data) > 0)

      r_local$data[, .SD, .SDcols = c(
        "campione1",
        "campione2",
        "data_effettiva",
        "operatore_effettivo",
        "valore1",
        "valore2",
        "differenza",
        "requisito",
        "unita_misura",
        "esito"
      )] |>
        repeatability_parameterDT()
    })


  })
}

## To be copied in the UI
# mod_02_results_ui("02_results_1")

## To be copied in the server
# mod_02_results_server("02_results_1")
