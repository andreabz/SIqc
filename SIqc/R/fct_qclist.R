#'table_btns
#'
#' @description edit and remove buttons.
#' The function is to be used with lapply functions.
#'
#' @return a character HTML string with edit and remove buttons.
#'
#' @noRd
table_btns <- function(x) {
  paste0(
    '<div class = "btn-group">
      <button class="btn btn-default action-button btn-info action_button" id="edit_',
    x,
    '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
      <button class="btn btn-default action-button btn-danger action_button" id="delete_',
    x,
    '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
  )
}

#'add_btns
#'
#' @description add edit and remove buttons at each rows of a DT table.
#'
#' @param df a dataframe to be visualised as DT table
#' @return a data.frame with a new column 'actions' with HTML code for edit
#'and remove buttons at each row.
#'
#' @noRd
#' @import data.table
add_btns <- function(df) {

  dt <- data.table::data.table(df)
  dt[, actions := table_btns(id)]
}

#' Prepare the dataset
#'
#' @description prepare the dataset of for the tasks summary
#'
#' @param df a dataframe with column names metodo, attivita, mese_previsto,
#' data_effettiva, operatore_previsto, operatore_effettivo, matrice, esito.
#'
#' @return a dataframe with the right class for each column.
#' @import data.table
prepare_tasks_summary <- function(df){
  stopifnot(is.data.frame(df))
  stopifnot(colnames(df) == c("metodo", "attivita", "anno", "mese_previsto",
                              "data_effettiva", "operatore_previsto",
                              "operatore_effettivo", "matrice", "esito"))

  month_lvl <- c("non previsto", format(ISOdate(2000, 1:12, 1), "%B"))

  data <- df
  data$metodo <- as.factor(data$metodo)
  data$attivita <- as.factor(data$attivita)
  data$anno <- as.factor(data$anno)
  data$mese_previsto <- as.factor(data$mese_previsto)
  data$data_effettiva <- as.Date(data$data_effettiva)
  data$matrice <- as.factor(data$matrice)
  data$esito <- ifelse(is.na(data$esito),
                       yes = "incompleto",
                       no = ifelse(data$esito == 1,
                                   yes = "conforme",
                                   no = "non conforme")
  ) |> as.factor()
  data
}

#' modal dialog for editing or adding a table rows
#'
#' @description a modal dialog with input elements for editing or adding a record.
#' @param df a dataframe with column names metodo, attivita, mese_previsto,
#' data_effettiva, operatore_previsto, operatore_effettivo, matrice, esito.
#' @param edit logical. TRUE for editing and FALSE for adding a new record.
#' @param completed logical. TRUE for completed and FALSE for not completed.
#' @param conn a DBI::dbConnect valid connection.
#' @param id id for namespace.
#' @return a single row dataframe.
#' @importFrom shiny NS
modal_dialog <- function(df, edit, completed, conn, id) {
  stopifnot(is.data.frame(df))
  stopifnot(
    colnames(df) == c(
      "metodo",
      "attivita",
      "anno",
      "mese_previsto",
      "tipo_campione",
      "operatore_previsto"
    )
  )
  stopifnot(is.logical(edit))
  ns <- shiny::NS(id)

  datalabel <- ifelse(completed,
                      "Visualizza i risultati",
                      "Aggiungi i risultati")

  if (edit) {
    mylabel <- "Salva"
    mymethod <- df$metodo
    mytask <- df$attivita
    myyear <- df$anno
    mymonth <- df$mese_previsto
    myplanned_operator <- df$operatore_previsto
    mysample_type <- df$tipo_campione
    mydatalabel <- datalabel

  } else {
    mylabel <- "Aggiungi"
    mymethod <- ""
    mytask <- ""
    myyear <- Sys.Date() |> format("%Y") |> as.factor()
    mymonth <- (Sys.Date() + 31) |> format("%B")
    myplanned_operator <- ""
    mysample_type <- ""
    mydatalabel <- datalabel

  }

  shiny::modalDialog(
    title = "Dati di pianificazione",
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = ns("method"),
          label = "Metodo",
          choices = sql_get_list(conn, "metodo"),
          selected = mymethod,
          multiple = FALSE,
          selectize = FALSE,
          # bug https://github.com/rstudio/shiny/issues/3125
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = ns("task"),
          label = "Attvità",
          choices = sql_get_list(conn, "attivita"),
          selected = mytask,
          multiple = FALSE,
          selectize = FALSE,
          # bug https://github.com/rstudio/shiny/issues/3125
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = ns("year"),
          label = "Anno",
          choices = sql_get_list(conn, "anno"),
          selected = myyear,
          multiple = FALSE,
          selectize = FALSE,
          # bug https://github.com/rstudio/shiny/issues/3125
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = ns("month"),
          label = "Mese previsto",
          choices = sql_get_list(conn, "mese"),
          selected = mymonth,
          multiple = FALSE,
          selectize = FALSE,
          # bug https://github.com/rstudio/shiny/issues/3125
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::textInput(
          inputId = ns("planned_operator"),
          label = "Operatore previsto",
          value = myplanned_operator,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        shiny::selectInput(
          inputId = ns("sample_type"),
          label = "Tipologia di campione",
          choices = sql_get_list(conn, "tipo_campione"),
          selected = mysample_type,
          multiple = FALSE,
          selectize = FALSE,
          width = "200px"
        )
      )
    ),
    size = "m",
    easyClose = FALSE,
    footer = div(
      class = "d-flex bd-highlight mb-3 container",
      div(
        class = "me-auto bd-highlight",
        shiny::actionButton(
          inputId = ns("add_data"),
          label = mydatalabel,
          icon = shiny::icon("vials")
        )
      ),
      div(
        class = "bd-highlight",
        shiny::actionButton(
          inputId = ns("final_edit"),
          label = mylabel,
          icon = shiny::icon("edit"),
          class = "btn-info"
        )
      ),
      div(
        class = "bd-highlight",
        shiny::actionButton(
          inputId = ns("dismiss_modal"),
          label = "Chiudi",
          class = "btn-danger"
        )
      )
    )
  ) |> shiny::showModal()
}

#' QC task DT list
#'
#' @description A DT table with a list of QC tasks.
#' @param data data to be included in the DT.
#'
#' @return A renderDT function
#'
#' @noRd
#' @importFrom DT datatable
qclistDT <- function(data) {
  stopifnot(is.data.frame(data))
  stopifnot(dim(data)[2] == 12)

  DT::datatable(
    data,
    filter = list(position = 'top', clear = TRUE),
    colnames = c(
      "id",
      "Metodo",
      "Attività",
      "Anno",
      "Mese previsto",
      "Data effettiva",
      "Operatore previsto",
      "Operatore effettivo",
      "Matrice",
      "Tipo di campione",
      "Esito",
      "Azioni"
    ),
    selection = "none",
    escape = FALSE,
    rownames = FALSE,
    callback = DT::JS(
      '$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tutti" )'
    ),
    options = list(
      processing = FALSE,
      language = dt_italian,
      columnDefs = list(list(
        visible = FALSE, targets = c(0, 9) # exclude id column and sample type
      ))
    )
  )
}
