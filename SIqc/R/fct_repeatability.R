#' repeatability assessment DT
#'
#' @description A DT table for assessing the repeatability of a measurement.
#' @param data data to be included in the DT.
#'
#' @return A renderDT function
#'
#' @noRd
#' @importFrom DT renderDT
repeatabilityDT <- function(data) {
  stopifnot(is.data.frame(data))

  DT::datatable(
    data,
    filter = "none",
    selection = "none",
    rownames = FALSE,
    editable = list(target = "column", disable = list(columns = c(0, 1, 2, 3, 4, 5))),
    colnames = c(
      "Parametro",
      "Unità di misura",
      "Campione 1",
      "Campione 2",
      "Differenza",
      "Requisito",
      "Esito"
    ),
    options = list(
      columnDefs = list(
        list(className = 'dt-left', targets = 1),
        list(className = 'dt-right', targets = 2),
        list(visible = FALSE, targets = 0) # exclude id column
      ),
      dom = 'tp',
      processing = FALSE,
      language = dt_italian
    )
  )

}

#' modal dialog for repeatablity
#'
#' @description a modal dialog for repeatability calculations and results.
#' @param conn a pool::dbConnect object.
#' @param id session information.
#' @param completed logical. TRUE for completed and FALSE for not completed.
#' @return A dataframe with sample results and repeatability calculations.
#'
#' @noRd
#' @importFrom DT datatable
repeatability_modal <- function(conn, id, completed) {
  ns <- NS(id)

  datalabel <- ifelse(completed, "Visualizza i risultati", "Aggiungi i risultati")

  shiny::modalDialog(
    title = datalabel,
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = ns("sample1"),
          label = "Campione 1",
          choices = sql_get_list(conn, "campione"),
          selected = "",
          multiple = FALSE,
          selectize = FALSE,
          # bug https://github.com/rstudio/shiny/issues/3125
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = ns("sample2"),
          label = "Campione 2",
          choices = sql_get_list(conn, "campione"),
          selected = "",
          multiple = FALSE,
          selectize = FALSE,
          # bug https://github.com/rstudio/shiny/issues/3125
          width = "200px"
        )
      ),
      div(DT::DTOutput(outputId = ns("dt_data")))
    ),
    div(
      class = "d-flex align-content-start justify-content-between",
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = ns("result"),
          label = "Esito",
          choices = sql_get_list(conn, "esito"),
          selected = "",
          multiple = FALSE,
          selectize = FALSE,
          # bug https://github.com/rstudio/shiny/issues/3125
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::textAreaInput(
          inputId = ns("comment"),
          label = "Commento",
          # bug https://github.com/rstudio/shiny/issues/3125
          width = "800px",
          height = "100px"
        )
      )
    ),
    size = "xl",
    easyClose = FALSE,
    footer = tagList(
      div(
        class = "d-flex justify-content-end container",
        shiny::actionButton(
          inputId = ns("save_res"),
          label = "Salva",
          icon = shiny::icon("edit"),
          class = "btn-info"
        ),
        shiny::actionButton(
          inputId = ns("close_btn"),
          label = "Chiudi",
          class = "btn-danger"
        )
      )
    )
  ) |>
    shiny::showModal()

}

#' plotly plot for time vs repeatability results
#'
#' @description a Plotly quality control chart for repeatability results.
#' @param data a dataframe with columns 'data_effettiva',
#' 'differenza_su_requisito and 'operatore_effettivo'
#' @return a plotly plot
#'
#' @noRd
#' @import plotly
qc_chart_repeatability_time <- function(data) {
  stopifnot(c("operatore_effettivo",
              "differenza_su_requisito",
              "data_effettiva") %in% colnames(data))

  plotly::plot_ly(source = "plotlybytime",
                  data = data,
                  x = ~ data_effettiva,
                  y = ~ differenza_su_requisito,
                  text = ~ operatore_effettivo) |>
    plotly::add_markers(hoverinfo = TRUE,
                        showlegend = FALSE,
                        hovertemplate = paste(
                          "Data: %{x: %Y-%b-%d}<br>",
                          "Differenza tra le misure su requisito: %{y:.1f}<br>",
                          "Operatore effettivo: %{text}<br><extra></extra>"
                        )) |>
    plotly::layout(shapes = list(hline(1)),
                   showlegend = FALSE,
                   yaxis = list(title = plotly::TeX("\\dfrac{|x_1 - x_2|}{r}")),
                   xaxis = list(dtick = "M1",
                                tickformat="%b-%Y",
                                title = "")
    ) |>
    plotly::config(mathjax = "cdn",
                   displayModeBar = FALSE,
                   locale = "it")
}

#' plotly plot for concentration vs repeatability results
#'
#' @description a Plotly quality control chart for repeatability results.
#' @param data a dataframe with columns 'valore1',
#' 'differenza_su_requisito, 'operatore_effettivo' and 'unita_misura'
#' @return a plotly plot
#'
#' @noRd
#' @import plotly
qc_chart_repeatability_conc <- function(data) {
  stopifnot(c("operatore_effettivo",
              "differenza_su_requisito",
              "valore1", "unita_misura") %in% colnames(data))

  # just get the first unit, to avoid messy data from lims
  udm <- data$unita_misura[1]

  plotly::plot_ly(source = "plotlybyconc",
                  data = data,
                  x = ~ valore1,
                  y = ~ differenza_su_requisito,
                  text = ~ operatore_effettivo) |>
    plotly::add_markers(hoverinfo = TRUE,
                        showlegend = FALSE,
                        hovertemplate = paste(
                          "Concentrazione: %{x} ", udm, "<br>",
                          "Differenza tra le misure su requisito: %{y:.1f}<br>",
                          "Operatore effettivo: %{text}<br><extra></extra>"
                        )) |>
    plotly::layout(shapes = list(hline(1)),
                   showlegend = FALSE,
                   yaxis = list(title = plotly::TeX("\\dfrac{|x_1 - x_2|}{r}")),
                   xaxis = list(title = paste0("Concentrazione (", udm, ")"))
    ) |>
    plotly::config(mathjax = "cdn",
                   displayModeBar = FALSE,
                   locale = "it")
}

#' plotly plot for operator vs repeatability results
#'
#' @description a Plotly quality control chart for repeatability results.
#' @param data a dataframe with columns 'campione1', 'campione2,
#' 'differenza_su_requisito, and 'operatore_effettivo'.
#' @return a plotly plot
#'
#' @noRd
#' @import plotly
qc_chart_repeatability_operator <- function(data) {
  stopifnot(c("operatore_effettivo",
              "differenza_su_requisito",
              "campione1", "campione2", "unita_misura") %in% colnames(data))

  plotly::plot_ly(source = "plotlybyoperator",
                  data = data,
                  x = ~ operatore_effettivo,
                  y = ~ differenza_su_requisito,
                  text = ~ paste(campione1, campione2, sep = ", ")) |>
    plotly::add_markers(hoverinfo = TRUE,
                        showlegend = FALSE,
                        hovertemplate = paste(
                          "Operatore effettivo: %{x}<br>",
                          "Differenza tra le misure su requisito: %{y:.1f}<br>",
                          "Campioni: %{text}<br><extra></extra>"
                        )) |>
    plotly::layout(shapes = list(hline(1)),
                   showlegend = FALSE,
                   yaxis = list(title = plotly::TeX("\\dfrac{|x_1 - x_2|}{r}")),
                   xaxis = list(title = "Operatore effettivo")
    ) |>
    plotly::config(mathjax = "cdn",
                   displayModeBar = FALSE,
                   locale = "it")
}

#' DT repeatability results by parameter
#'
#' @description A DT table for repeatability results by parameter.
#' @param data data to be included in the DT.
#'
#' @return A renderDT function
#'
#' @noRd
#' @importFrom DT renderDT
repeatability_parameterDT <- function(data) {
  stopifnot(is.data.frame(data))

  DT::datatable(
    data,
    filter = "none",
    selection = "none",
    rownames = FALSE,
    colnames = c(
      "Campione 1",
      "Campione 2",
      "Data effettiva",
      "Operatore effettivo",
      "Valore 1",
      "Valore 2",
      "Differenza",
      "Requisito",
      "Unità di misura",
      "Esito"
    ),
    options = list(
      columnDefs = list(
        list(className = 'dt-left', targets = c(0, 1, 3, 8, 9)),
        list(className = 'dt-right', targets = c(2, 4, 5, 6, 7))
      ),
      dom = 'tp',
      processing = FALSE,
      language = dt_italian
    )
  ) |>
    DT::formatStyle("esito",
                    target = "row",
                    backgroundColor = DT::styleEqual(
                      levels = c("conforme", "non conforme"),
                      value = c(NA, "#ff0039")
                      )
                    )

}
