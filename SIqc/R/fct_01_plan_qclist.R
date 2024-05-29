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
#' @return a data.frame with a new column 'Azioni' with HTML code for edit
#'and remove buttons at each row.
#'
#' @noRd
add_btns <- function(df) {
  stopifnot(is.data.frame(df))

  dfrows <- 1:nrow(df)

  btns <- lapply(dfrows, function(x) table_btns(x)) |> unlist()

  newdf <- cbind(df, btns)
  colnames(newdf) <- c(colnames(df), "Azioni")

  newdf
}

#' Prepare the dataset
#'
#' @description prepare the dataset of for the tasks summary
#'
#' @param df a dataframe with column names metodo, attivita, mese_previsto,
#' data_effettiva, operatore_previsto, operatore_effettivo, matrice, esito.
#'
#' @return a dataframe with the right class for each column.
prepare_tasks_summary <- function(df){
  stopifnot(is.data.frame(df))
  stopifnot(colnames(df) == c("metodo", "attivita", "mese_previsto",
                              "data_effettiva", "operatore_previsto",
                              "operatore_effettivo", "matrice", "esito"))

  month_lvl <- c("non previsto", format(ISOdate(2000, 1:12, 1), "%B"))

  data <- df
  data$metodo <- as.factor(data$metodo)
  data$attivita <- as.factor(data$attivita)
  data$mese_previsto <- as.factor(data$mese_previsto)
  data$data_effettiva <- as.Date(data$data_effettiva)
  data$matrice <- as.factor(data$matrice)
  data$esito <- as.logical(data$esito)

  data
}
