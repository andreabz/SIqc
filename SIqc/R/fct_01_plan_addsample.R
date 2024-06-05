#' repeatablity sample
#'
#' @description a modal dialog for repeatability calculations and results.
#' @param df a data.frame with columns 'data_effettiva',
#' 'operatore_effettivo', 'matrice,
#' @param edit logical. TRUE for editing and FALSE for adding a new record.
#' @param conn a DBI::dbConnect object
#' @param id session information
#' @return A dataframe with sample results and repeatability calculations.
#'
#' @noRd
#' @importFrom DT datatable
repeatability_modal <- function(edit, conn, id) {
  # stopifnot(is.data.frame(df))
  # stopifnot(
  #   colnames(df) == c(
  #     "metodo",
  #     "attivita",
  #     "anno",
  #     "mese_previsto",
  #     "data_effettiva",
  #     "operatore_previsto",
  #     "operatore_effettivo",
  #     "matrice",
  #     "esito"
  #   )
  # )

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
          choices = c("conforme", "non conforme"),
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
