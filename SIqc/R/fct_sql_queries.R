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

#' SQL query for repeatability sample results
#'
#' @description get a data.frame of sample results for reapeatability.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param sample1 a character string with the name of the sample.
#' @param sample2 a character string with the name of the sample.
#'
#' @return a data.frame
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
#' @import data.table
sql_get_repeatability <- function(conn, sample1, sample2){
  #stopifnot(is.character(sample1))
  #stopifnot(is.character(sample2))

  get_sample_id <- function(name){
    myquery <- glue::glue_sql("SELECT id_campione FROM campione WHERE campione = {name};",
                   .con = conn)

    DBI::dbGetQuery(conn, myquery) |>
      unlist() |>
      unname()
  }

  sample1_id <- get_sample_id(sample1)
  sample2_id <- get_sample_id(sample2)

  query <- glue::glue_sql("SELECT
                            a.parametro,
                            a.udm,
                            a.valore AS campione1,
                            b.valore AS campione2
                           FROM risultati AS a
                          LEFT JOIN (
                            SELECT id_campione, parametro, valore FROM risultati
                          ) AS b
                          ON a.parametro = b.parametro
                          WHERE a.id_campione = {sample1_id} AND b.id_campione = {sample2_id};",
                          .con = conn)

  DBI::dbGetQuery(conn, query) |>
    data.table::data.table()
}

