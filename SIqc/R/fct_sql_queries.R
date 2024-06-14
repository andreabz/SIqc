#' SQL query for planned tasks
#'
#' @description get a summary of the planned tasks
#' @param conn a connection to a database obtained by DBI::dbConnect.
#'
#' @return a SQL query performing on a established db connection.
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @import data.table
sql_get_task_summary <- function(conn){
  DBI::dbGetQuery(conn, "SELECT
                        pz.pianificazione_id AS id,
                        metodo,
                        attivita,
                        anno,
                        mese AS mese_previsto,
                        data_effettiva,
                        operatore_previsto,
                        operatore_effettivo,
                        matrice,
                        tipo_campione,
                        esito,
                        azione AS azioni
                      FROM pianificazione AS pz
                      INNER JOIN metodo
                        ON pz.metodo_id = metodo.metodo_id
                      INNER JOIN attivita
                        ON pz.attivita_id = attivita.attivita_id
                      INNER JOIN anno
                        ON pz.anno_id = anno.anno_id
                      INNER JOIN mese
                        ON pz.mese_id = mese.mese_id
                      LEFT JOIN
                        (SELECT DISTINCT
                          pc.pianificazione_id,
                          risultato.data_effettiva,
                          risultato.operatore_effettivo,
                          risultato.matrice_id
                         FROM risultato
                        INNER JOIN pianificazione_campione AS pc
                          ON risultato.campione_id = pc.campione_id) as eff
                      ON pz.pianificazione_id = eff.pianificazione_id
                      LEFT JOIN matrice
                        ON eff.matrice_id = matrice.matrice_id
                      LEFT JOIN giudizio
                        ON pz.pianificazione_id = giudizio.pianificazione_id
                      LEFT JOIN esito
                        ON giudizio.esito_id = esito.esito_id
                      LEFT JOIN tipo_campione AS tc
                        ON pz.tipo_campione_id = tc.tipo_campione_id
                    ORDER BY pz.ROWID DESC") |>
    data.table::data.table()
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

  sample1_id <- sql_get_id(conn, "campione", sample1)
  sample2_id <- sql_get_id(conn, "campione", sample2)

  query <- glue::glue_sql("SELECT
                  rip.ripetibilita_id,
                  par.parametro,
                  udm.unita_misura,
                  res1.valore AS campione1,
                  res2.valore AS campione2,
                  differenza,
                  requisito,
                  esito
                FROM ripetibilita AS rip
                INNER JOIN esito AS ex
                  ON rip.esito_id = ex.esito_id
                INNER JOIN parametro AS par
                  ON rip.parametro_id = par.parametro_id
                INNER JOIN risultato AS res1
                  ON res1.campione_id = rip.campione1_id
                  AND res1.parametro_id = rip.parametro_id
                INNER JOIN risultato AS res2
                  ON res2.campione_id = rip.campione2_id
                  AND res2.parametro_id = rip.parametro_id
                INNER JOIN unita_misura AS udm
                  ON res1.unita_misura_id = udm.unita_misura_id
                WHERE
                  (
                    (res1.campione_id = {sample1_id} AND res2.campione_id = {sample2_id})
                    OR (res2.campione_id = {sample1_id} AND res1.campione_id = {sample2_id})
                  )
                  AND rip.pianificazione_id = {mytask};",
                          .con = conn)

  results <- DBI::dbGetQuery(conn, query) |>
    data.table::data.table()

  if(results[, .N] == 0) {

     query <- glue::glue_sql("SELECT
                              par.parametro,
                              udm.unita_misura,
                              res1.valore AS campione1,
                              res2.valore AS campione2
                             FROM risultato AS res1
                            LEFT JOIN (
                              SELECT campione_id, parametro_id, valore FROM risultato
                            ) AS res2
                              ON res1.parametro_id = res2.parametro_id
                            INNER JOIN parametro AS par
                              ON res1.parametro_id = par.parametro_id
                            INNER JOIN unita_misura AS udm
                              ON res1.unita_misura_id = udm.unita_misura_id
                            WHERE
                              res1.campione_id = {sample1_id} AND res2.campione_id = {sample2_id};",
                            .con = conn)

     maxid <- DBI::dbGetQuery(conn, "SELECT
                                      MAX(ripetibilita_id) as ripetibilita_id
                                    FROM ripetibilita")

    res_query <- DBI::dbGetQuery(conn, query)

    # adding index
    lengthid <- length(res_query$parametro)
    newids <- seq.int(maxid$ripetibilita_id + 1, length.out = lengthid)
    names(newids) <- "ripetibilita_id"
    results <- data.table::data.table(ripetibilita_id = newids, res_query)

    # get the number of decimals
    ndecimal <- lapply(results$campione1, decimalplaces) |> unlist() |> max()
    results[, `:=` (differenza = abs(campione1 - campione2) |> round(ndecimal),
                        requisito = rep(NA, .N)  |> as.numeric(),
                        esito = rep(NA, .N)  |> as.numeric()) ]
  }

  results
}

#' SQL query for getting an id given an element and table name
#'
#' @description retrieve the id associated to a sample name.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param table the name of the SQL table from which the element id should be retrieved.
#' @param element the name of the element for which the id is to be retrieved.
#' @return an integer
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_id <- function(conn, table, element){

  colid <- glue::glue("{table}_id")
  myquery <- glue::glue_sql("SELECT {`colid`} FROM campione WHERE {`table`} = {element};",
                            .con = conn)

  DBI::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting an element name given a table name and element id
#'
#' @description retrieve the name of a sample associate to an id.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param table the name of the SQL table from which the element name should be retrieved.
#' @param elementid the id of the element for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_name <- function(conn, table, elementid){

  idname <- glue::glue("{table}_id")
  myquery <- glue::glue_sql("SELECT {`table`} FROM {`table`} WHERE {`idname`} = {elementid};",
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
sql_get_sampleid_for_task <- function(conn, task_id){

  myquery <- glue::glue_sql("SELECT campione_id
                              FROM pianificazione_campione
                              WHERE pianificazione_id = {task_id};",
                            .con = conn)

  DBI::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for removing a row given its id.
#'
#' @description remove a row given its id into the plan table into the DB.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param id the id of the task.
#' @return an SQL expression.
#'
#' @noRd
#' @importFrom DBI dbExecute
#' @importFrom glue glue_sql
sql_del_taskid <- function(conn, task_id){
  myquery <- glue::glue_sql("DELETE FROM pianificazione WHERE pianificazione_id = {task_id};",
                            .con = conn)

  DBI::dbExecute(conn, myquery)
}

#' SQL query for modifying a row given its id.
#'
#' @description modify a row given its id into the plan table into the DB.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param newvalue a data.frame with the new values.
#' @param id the id of the task.
#' @return an SQL expression.
#'
#' @noRd
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom glue glue_sql
sql_mod_taskid <- function(conn, newvalue, task_id){

  opprevisto_value <- newvalue$operatore_previsto

  method_value <- DBI::dbGetQuery(conn, "SELECT metodo_id FROM metodo WHERE metodo = ?",
                                  params = newvalue$metodo)
  task_value <- DBI::dbGetQuery(conn, "SELECT attivita_id FROM attivita WHERE attivita = ?",
                                params = newvalue$attivita)
  year_value <- DBI::dbGetQuery(conn, "SELECT anno_id FROM anno WHERE anno = ?",
                                params = newvalue$anno)
  month_value <- DBI::dbGetQuery(conn, "SELECT mese_id FROM mese WHERE mese = ?",
                                 params = newvalue$mese_previsto)
  type_value <- DBI::dbGetQuery(conn, "SELECT tipo_campione_id FROM tipo_campione WHERE tipo_campione = ?",
                                  params = newvalue$tipo_campione)

  myquery <- glue::glue_sql("UPDATE pianificazione
                             SET metodo_id = {method_value},
                                 attivita_id = {task_value},
                                 anno_id = {year_value},
                                 mese_id = {month_value},
                                 tipo_campione_id = {type_value},
                                 operatore_previsto = {opprevisto_value}
                             WHERE pianificazione_id = {task_id};",
                            .con = conn)

  DBI::dbExecute(conn, myquery)
}

#' SQL query for adding a new row.
#'
#' @description modify a row given its id into the plan table into the DB.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param newvalue a data.frame with the new values.
#' @return an SQL expression.
#'
#' @noRd
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom glue glue_sql
sql_add_task <- function(conn, newvalue){

  opprevisto_value <- newvalue$operatore_previsto

  method_value <- DBI::dbGetQuery(conn, "SELECT metodo_id FROM metodo WHERE metodo = ?",
                                  params = newvalue$metodo)
  task_value <- DBI::dbGetQuery(conn, "SELECT attivita_id FROM attivita WHERE attivita = ?",
                                params = newvalue$attivita)
  year_value <- DBI::dbGetQuery(conn, "SELECT anno_id FROM anno WHERE anno = ?",
                                params = newvalue$anno)
  month_value <- DBI::dbGetQuery(conn, "SELECT mese_id FROM mese WHERE mese = ?",
                                 params = newvalue$mese_previsto)
  sampletype_value <- DBI::dbGetQuery(conn, "SELECT tipo_campione_id FROM tipo_campione WHERE tipo_campione = ?",
                                  params = newvalue$tipo_campione)
  max_id <- DBI::dbGetQuery(conn, "SELECT MAX(pianificazione_id) AS id FROM pianificazione") + 1
  actions <- table_btns(max_id)

  myquery <- glue::glue_sql("INSERT INTO pianificazione (
                                               pianificazione_id, metodo_id,
                                               attivita_id, anno_id,
                                               mese_id, tipo_campione_id,
                                               operatore_previsto, azione)
                             VALUES ({max_id}, {method_value},
                                     {task_value}, {year_value},
                                     {month_value}, {sampletype_value},
                                     {opprevisto_value}, {actions});",
                            .con = conn)

  DBI::dbExecute(conn, myquery)
}

#' SQL query for modifying repeatability sample results
#'
#' @description get a data.frame of sample results for reapeatability.
#' @param conn a connection to a database obtained by DBI::dbConnect.
#' @param sample1 a character string with the name of the sample.
#' @param sample2 a character string with the name of the sample.
#' @param mytask id of the task to be evaluated.
#' @param mydata new data to be stored in the repeatability table.
#'
#' @return a data.frame
#'
#' @noRd
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom glue glue_sql
#' @import data.table
sql_mod_repeatability <- function(conn,
                                  sample1,
                                  sample2,
                                  mytask,
                                  mydata){

  stopifnot(c("differenza", "requisito") %in% colnames(mydata))

  mydata[, `:=` (campione1 = rep(sample1, .N),
                 campione2 = rep(sample2, .N),
                 pianificazione_id = rep(mytask, .N),
                 differenza_su_requisito = (differenza / requisito) |> round(2))]
  newdata <- mydata[, .(ripetibilita_id, campione1, campione2,
                        pianificazione_id, parametro, esito,
                        differenza, requisito, differenza_su_requisito)]
  DBI::dbWriteTable(conn, "ripetibilita_tmp", newdata, append = TRUE)

  res <- DBI::dbGetQuery(conn, "SELECT
                          tmp.ripetibilita_id,
                          cmp1.campione_id AS campione1_id,
                          cmp2.campione_id AS campione2_id,
                          tmp.pianificazione_id,
                          par.parametro_id,
                          ex.esito_id,
                          tmp.differenza,
                          tmp.requisito,
                          tmp.differenza_su_requisito
                         FROM ripetibilita_tmp AS tmp
                        --- UPDATE ripetibilita
                        --- SET ripetibilita.ripetibilita_id = tmp.ripetibilita_id,
                        ---     ripetibilita.campione1_id = cmp1.campione_id,
                        ---     ripetibilita.campione2_id = cmp2.campione_id,
                        ---     ripetibilita.pianificazione_id = tmp.pianificazione_id,
                        ---     ripetibilita.parametro_id = par.parametro_id,
                        ---     ripetibilita.esito_id = ex.esito_id,
                        ---     ripetibilita.differenza = tmp.differenza,
                        ---     ripetibilita.requsitio = tmp.requisito,
                        ---     ripetibilita.differenza_su_requisito = tmp.differenza_su_requisito
                        INNER JOIN campione AS cmp1
                          ON tmp.campione1 = cmp1.campione
                        INNER JOIN campione AS cmp2
                          ON tmp.campione2 = cmp2.campione
                        INNER JOIN parametro AS par
                          ON tmp.parametro = par.parametro
                        INNER JOIN esito AS ex
                          ON tmp.esito = ex.esito")

  DBI::dbExecute(conn, "DELETE FROM ripetibilita_tmp")
  res
}
