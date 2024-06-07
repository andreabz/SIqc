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
#' @param mytask id of the task to be evaluated.
#'
#' @return a data.frame
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
#' @import data.table
sql_get_repeatability <- function(conn, sample1, sample2, mytask){
  stopifnot(is.character(sample1))
  stopifnot(is.character(sample2))
  stopifnot(is.integer(mytask))

  sample1_id <- get_sample_id(conn, sample1)
  sample2_id <- get_sample_id(conn, sample2)

  query <- glue::glue_sql("SELECT
                        res1.parametro,
                        res1.udm,
                        res1.valore AS campione1,
                        res2.valore AS campione2,
                        rep.differenza,
                        rep.r,
                        rep.esito
                        FROM plan
                      LEFT JOIN risultati AS res1 ON plan.id_campione1 = res1.id_campione
                      LEFT JOIN risultati AS res2 ON plan.id_campione2 = res2.id_campione
                        AND res2.parametro = res1.parametro
                      LEFT JOIN ripetibilita AS rep ON plan.id_plan = rep.id_plan
                        AND res1.parametro = rep.parametro
                      WHERE (
                        (plan.id_campione1 = {sample1_id} AND plan.id_campione2 = {sample2_id})
                          OR (plan.id_campione1 = {sample2_id} AND plan.id_campione2 = {sample1_id})
                          )
                        AND plan.id_plan = {mytask};",
                          .con = conn)

  results <- DBI::dbGetQuery(conn, query) |>
    data.table::data.table()

  if(results[, .N] == 0) {

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

    results <- DBI::dbGetQuery(conn, query) |>
      data.table::data.table()

    # get the number of decimals
    ndecimal <- lapply(results$campione1, decimalplaces) |> unlist() |> max()
    results[, `:=` (differenza = abs(campione1 - campione2) |> round(ndecimal),
                        r = rep(NA, .N)  |> as.numeric(),
                        esito = rep(NA, .N)  |> as.numeric()) ]
  }

  results
}

#' SQL query for getting a sample id given a sample name
#'
#' @description retrieve the id associated to a sample name.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param sample_name the name of the sample for which the id is to be retrieved.
#' @return an integer
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
get_sample_id <- function(conn, sample_name){
  myquery <- glue::glue_sql("SELECT id_campione FROM campione WHERE campione = {sample_name};",
                            .con = conn)

  DBI::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting a sample name given a sample id
#'
#' @description retrieve the name of a sample associate to an id.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param sample_id the name of the sample for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
get_sample_name <- function(conn, sample_id){
  myquery <- glue::glue_sql("SELECT campione FROM campione WHERE id_campione = {sample_id};",
                            .con = conn)

  DBI::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting the sample ids associated to a given task
#'
#' @description retrieve the sample id associate to a task.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param task the id of the task.
#' @return an integer vector
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
get_sample_id_for_task <- function(conn, task_id){
  myquery <- glue::glue_sql("SELECT id_campione1, id_campione2 FROM plan WHERE id_plan = {task_id};",
                            .con = conn)

  DBI::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

