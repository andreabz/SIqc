#' repeatability assessment DT
#'
#' @description A DT table for assessing the repeatability of a measurement.
#' @param data data to be included in the DT.
#'
#' @return A renderDT function
#'
#' @noRd
#' @importFrom DT renderDT
repeatabilityDT <- function(data){
  stopifnot(is.data.frame(data))
  stopifnot(dim(data)[2] == 8)

  DT::renderDT(
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
#' @param conn a DBI::dbConnect object
#' @param id session information
#' @return A dataframe with sample results and repeatability calculations.
#'
#' @noRd
#' @importFrom DT datatable
repeatability_modal <- function(conn, id) {

  ns <- NS(id)

  shiny::modalDialog(
    title = "Aggiungi i risultati",
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
      div(
        uiOutput(outputId = ns("dt_data"))
      )
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
    easyClose = TRUE,
    footer = div(
      class = "d-flex justify-content-end container",
      div(class = "bd-highlight",
          shiny::actionButton(
            inputId = ns("save_res"),
            label = "Salva",
            icon = shiny::icon("edit"),
            class = "btn-info"
          )
      ),
      div(class = "bd-highlight",
          shiny::actionButton(
            inputId = ns("close_btn"),
            label = "Chiudi",
            class = "btn-danger"
          )
      )
    )
  ) |> shiny::showModal()



}