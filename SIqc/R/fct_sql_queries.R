#' SQL query for planned tasks
#'
#' @description get a summary of the planned tasks
#' @param conn a connection to a database obtained by DBI::dbConnect.
#'
#' @return a SQL query performing on a established db connection.
#'
#' @noRd
#' @importFrom DBI dbGetQuery
sql_get_task_summary <- function(conn){
  DBI::dbGetQuery(conn, "SELECT metodo,
                              attivita,
                              anno,
                              mese AS mese_previsto,
                              res.data_effettiva,
                              operatore_previsto,
                              res.operatore_effettivo,
                              matrice,
                              esito FROM plan
                         INNER JOIN metodo ON plan.id_metodo = metodo.id_metodo
                         INNER JOIN attivita ON plan.id_attivita = attivita.id_attivita
                         INNER JOIN anno ON plan.id_anno = anno.id_anno
                         INNER JOIN mese ON plan.id_mese = mese.id_mese
                         INNER JOIN matrice ON plan.id_matrice = matrice.id_matrice
                         LEFT JOIN (
                          SELECT DISTINCT id_plan, data_effettiva, operatore_effettivo FROM risultati
                          ) AS res
                          ON plan.id_plan = res.id_plan;")
}

#' SQL query for unnamed lists
#'
#' @description get a list of elements.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param name name of the table and element of the table to be retrieved:
#' they must be the same and provided as single character value.
#'
#' @return a character vector
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_list <- function(conn, name){
  query <- glue::glue_sql("SELECT {`name`} FROM {`name`};", .con = conn)
  DBI::dbGetQuery(conn, query)|>
    unlist() |>
    unname()
}

#' SQL query for QC sample type list
#'
#' @description get QC sample type list.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#'
#' @return a character vector
#'
#' @noRd
#' @importFrom DBI dbGetQuery
sql_get_list_type <- function(conn){
  DBI::dbGetQuery(conn, "SELECT tipo FROM tipo;") |>
    unlist() |>
    unname()
}

#' SQL query for months
#'
#' @description get months.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#'
#' @return a character vector
#'
#' @noRd
#' @importFrom DBI dbGetQuery
sql_get_list_month <- function(conn){
  DBI::dbGetQuery(conn, "SELECT mese FROM mese;") |>
    unlist() |>
    unname()
}

#' SQL query for year
#'
#' @description get year.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#'
#' @return a SQL query performing on a established db connection.
#'
#' @noRd
#' @importFrom DBI dbGetQuery
sql_get_list_year <- function(conn){
  DBI::dbGetQuery(conn, "SELECT anno FROM anno;")
}

#' SQL query for sample ids
#'
#' @description get a list of sample ids.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#'
#' @return a SQL query performing on a established db connection.
#'
#' @noRd
#' @importFrom DBI dbGetQuery
sql_get_list_sample <- function(conn){
  DBI::dbGetQuery(conn, "SELECT campione FROM campione;")
}

#' SQL query for method ids
#'
#' @description get a list of method ids.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#'
#' @return a SQL query performing on a established db connection.
#'
#' @noRd
#' @importFrom DBI dbGetQuery
sql_get_list_method <- function(conn){
  DBI::dbGetQuery(conn, "SELECT metodo FROM metodo;")
}

#' SQL query for task types
#'
#' @description get a list of method ids.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#'
#' @return a SQL query performing on a established db connection.
#'
#' @noRd
#' @importFrom DBI dbGetQuery
sql_get_list_task <- function(conn){
  DBI::dbGetQuery(conn, "SELECT attivita FROM attivita;")
}
