#' SQL query for planned tasks
#'
#' @description get a summary of the planned tasks
#' @param conn a connection to a database obtained by pool::dbConnect.
#'
#' @return a SQL query performing on a established db connection.
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @import data.table
sql_get_task_summary <- function(conn){
  pool::dbGetQuery(conn, "SELECT
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
#' @param conn a connection to a database obtained by pool::dbConnect.
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
  pool::dbGetQuery(conn, query)|>
    unlist() |>
    unname()
}

#' SQL query for repeatability sample results
#'
#' @description get a data.frame of sample results for reapeatability.
#' @param conn a connection to a database obtained by pool::dbConnect.
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
                  rip.ripetibilita_id AS ripetibilita_id,
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

  results <- pool::dbGetQuery(conn, query) |>
    data.table::data.table()

  # When no results were stored for that samples pair,
  # the function picks only the sample results
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

     maxid <- pool::dbGetQuery(conn, "SELECT
                                      MAX(ripetibilita_id) as ripetibilita_id
                                    FROM ripetibilita") |>
       unlist() |>
       unname()

    res_query <- pool::dbGetQuery(conn, query) |>
      data.table::data.table()

    # adding index
    ids <- pool::dbGetQuery(conn,
      glue::glue_sql("SELECT
                        ripetibilita_id
                        FROM ripetibilita
                      WHERE pianificazione_id = {mytask}", .con = conn) |>
        unlist() |>
        unname()
    )

    ## for new results
    if(nrow(ids) == 0) {
      lengthid <- length(res_query$parametro)
      newids <- data.frame(ripetibilita_id = seq.int(maxid + 1, length.out = lengthid))
    } else {
    ## for updated results
      newids <- ids
      names(newids) <- "ripetibilita_id"
    }

    results <- data.table::data.table(newids, res_query)

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
#' @param conn a connection to a database obtained by pool::dbConnect.
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

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting an element name given a table name and element id
#'
#' @description retrieve the name of a sample associate to an id.
#' @param conn a connection to a database obtained by pool::dbConnect.
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

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting the sample ids associated to a given task
#'
#' @description retrieve the sample id associate to a task.
#' @param conn a connection to a database obtained by pool::dbConnect.
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

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for removing a row given its id.
#'
#' @description remove a row given its id into the plan table into the DB.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param table the name of the table from which the row should be removed.
#' @param column the name of the column with the id information.
#' @param id the id of the task.
#' @return an SQL expression.
#'
#' @noRd
#' @importFrom DBI dbExecute
#' @importFrom glue glue_sql
sql_del_taskid <- function(conn, table, column, task_id){
  myquery <- glue::glue_sql("DELETE FROM {`table`} WHERE {`column`} = {task_id};",
                            .con = conn)

  pool::dbExecute(conn, myquery)
}

#' SQL query for modifying a row given its id.
#'
#' @description modify a row given its id into the plan table into the DB.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param newvalue a data.frame with the new values.
#' @param id the id of the task.
#' @return an SQL expression.
#'
#' @noRd
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom glue glue_sql
sql_mod_taskid <- function(conn, newvalue, task_id){

  opprevisto_value <- newvalue$operatore_previsto

  method_value <- pool::dbGetQuery(conn, "SELECT metodo_id FROM metodo WHERE metodo = ?",
                                  params = newvalue$metodo)
  task_value <- pool::dbGetQuery(conn, "SELECT attivita_id FROM attivita WHERE attivita = ?",
                                params = newvalue$attivita)
  year_value <- pool::dbGetQuery(conn, "SELECT anno_id FROM anno WHERE anno = ?",
                                params = newvalue$anno)
  month_value <- pool::dbGetQuery(conn, "SELECT mese_id FROM mese WHERE mese = ?",
                                 params = newvalue$mese_previsto)
  type_value <- pool::dbGetQuery(conn, "SELECT tipo_campione_id FROM tipo_campione WHERE tipo_campione = ?",
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

  pool::dbExecute(conn, myquery)
}

#' SQL query for adding a new row.
#'
#' @description modify a row given its id into the plan table into the DB.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param newvalue a data.frame with the new values.
#' @return an SQL expression.
#'
#' @noRd
#' @importFrom DBI dbExecute dbGetQuery
#' @importFrom glue glue_sql
sql_add_task <- function(conn, newvalue){

  opprevisto_value <- newvalue$operatore_previsto

  method_value <- pool::dbGetQuery(conn, "SELECT metodo_id FROM metodo WHERE metodo = ?",
                                  params = newvalue$metodo)
  task_value <- pool::dbGetQuery(conn, "SELECT attivita_id FROM attivita WHERE attivita = ?",
                                params = newvalue$attivita)
  year_value <- pool::dbGetQuery(conn, "SELECT anno_id FROM anno WHERE anno = ?",
                                params = newvalue$anno)
  month_value <- pool::dbGetQuery(conn, "SELECT mese_id FROM mese WHERE mese = ?",
                                 params = newvalue$mese_previsto)
  sampletype_value <- pool::dbGetQuery(conn, "SELECT tipo_campione_id FROM tipo_campione WHERE tipo_campione = ?",
                                  params = newvalue$tipo_campione)
  max_id <- pool::dbGetQuery(conn, "SELECT MAX(pianificazione_id) AS id FROM pianificazione") + 1
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

  pool::dbExecute(conn, myquery)
}

#' SQL query for modifying repeatability sample results
#'
#' @description get a data.frame of sample results for reapeatability.
#' @param conn a connection to a database obtained by pool::dbConnect.
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

  # write the new data to a tmp SQL table
  pool::dbWriteTable(conn, "ripetibilita_tmp", newdata, append = TRUE)

  # normalise the new data
  res <- pool::dbGetQuery(conn, "SELECT
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
                        INNER JOIN campione AS cmp1
                          ON tmp.campione1 = cmp1.campione
                        INNER JOIN campione AS cmp2
                          ON tmp.campione2 = cmp2.campione
                        INNER JOIN parametro AS par
                          ON tmp.parametro = par.parametro
                        INNER JOIN esito AS ex
                          ON tmp.esito = ex.esito")

  # remove the tmp SQL table
  pool::dbExecute(conn, "DELETE FROM ripetibilita_tmp")

  # get the max id for the results table
  maxid <- pool::dbGetQuery(conn, "SELECT MAX(ripetibilita_id) FROM ripetibilita") |>
    unlist() |>
    unname()

  # for new data to be added to the results table
  if(min(res$ripetibilita_id) > maxid){

    # adding the new results
    tblname <- "ripetibilita"
    mycols <- pool::dbListFields(conn, tblname)

    lapply(seq_len(nrow(res)), function(x) {
      myvals <- res[x, ]

      sql_insert(
        conn = conn,
        tbl = tblname,
        cols = mycols,
        vals = myvals
      )
    })

    # adding the samples to the new plan id
    sample1_id <- sql_get_id(conn, "campione", sample1)
    sample2_id <- sql_get_id(conn, "campione", sample2)
    sample_id <- c(sample1_id, sample2_id)
    tblname <- "pianificazione_campione"
    mycols <- c("pianificazione_id", "campione_id")

    lapply(seq_len(length(sample_id)), function(x) {
      myvals <- c(mytask, sample_id[x])

      sql_insert(
        conn = conn,
        tbl = tblname,
        cols = mycols,
        vals = myvals
      )
    })

  # for updating existing columns
  } else {

    tblname <- "ripetibilita"
    mycols <- pool::dbListFields(conn, tblname)[-1]

    lapply(seq_len(nrow(res)), function(x){
      myvals <- res[x, ]
      mycondval <- res[x, 1]

      sql_update(
        conn = conn,
        tbl = tblname,
        cols = mycols,
        vals = myvals,
        condcol = "ripetibilita_id",
        condval = mycondval
      )
    })

    # updating samples for the modified plan id
    sample1_id <- sql_get_id(conn, "campione", sample1)
    sample2_id <- sql_get_id(conn, "campione", sample2)
    sample_id <- c(sample1_id, sample2_id)
    tblname <- "pianificazione_campione"
    mycols <- c("pianificazione_id", "campione_id")
    vals <- data.frame(pianificazione_id = rep(mytask, 2),
                       campione_id = sample_id)
    pianificazione_campione_id <- pool::dbGetQuery(conn, "SELECT pianificazione_campione_id
                                                  FROM pianificazione_campione
                                                  WHERE pianificazione_id = ?;",
                                                  params = mytask) |>
      unlist() |>
      unname()

    lapply(seq_len(length(sample_id)), function(x){
      myvals <- vals[x, ]
      mycondval <- pianificazione_campione_id[x]

      sql_update(
        conn = conn,
        tbl = tblname,
        cols = mycols,
        vals = myvals,
        condcol = "pianificazione_campione_id",
        condval = mycondval
      )
    })

  }

  res
}

#' SQL code for adding new data
#'
#' @description The function add new data to a database using SQL code.
#' @param conn connection object.
#' @param tbl the name of the table in which new data is to be added.
#' @param cols the name of the fields for which new data has been provided.
#' @param vals the new values to be added to the dataset.
#'
#' @return no output
#'
#' @noRd
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom glue glue_sql glue_sql_collapse
sql_insert <- function(conn, tbl, cols, vals){

  # for properly quoting only character arguments
  myvals <- lapply(vals, function(x) {
    if(is.character(x)){
      glue::glue_sql("{as.character(x)}", .con = conn)
    } else {
      glue::glue_sql("{x}", .con = conn)
    }
  }) |>
    unlist() |>
    glue::glue_sql_collapse(sep = ", ")

  sql_cols <- paste0("(",
                     glue::glue_sql_collapse({`cols`}, sep = ","),
                     ")") |> glue::glue_sql()
  sql_vals <- glue::glue_sql("({myvals})")
  sql_query <- glue::glue_sql(.con = conn,
                              "INSERT INTO {`tbl`}
                               {sql_cols}
                               VALUES {sql_vals};")

  pool::dbExecute(conn, sql_query)
}

#' SQL code for updating the data
#'
#' @description The function updates the database using SQL code.
#' @param conn connection object.
#' @param tbl the name of the table in which the data is to be updated.
#' @param cols the name of the fields for which the data to be updated has been provided.
#' @param vals the new values to update the dataset.
#' @param condcol the column name on which the WHERE condition is set.
#' @param condval the value on which the WHERE condition is set.
#'
#' @return no output
#'
#' @noRd
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom glue glue_sql glue_sql_collapse
sql_update <- function(conn, tbl, cols, vals, condcol, condval){

  sql_cols <- lapply(cols, function(x, data = vals) {
    val <- data[[x]]

    if(is.character(val)){
      glue::glue_sql("{`x`} = {as.character(val)}", .con = conn)
    }

    glue::glue_sql("{`x`} = {val}", .con = conn)
  }) |> glue::glue_sql_collapse(sep = ", ")

  if(is.character(condval)){
    condval <- as.character(condval)
  }

  sql_query <- glue::glue_sql(.con = conn,
                              "UPDATE {`tbl`}
                               SET {sql_cols}
                               WHERE {`condcol`} = {condval};")

  pool::dbExecute(conn, sql_query)
}

#' SQL query for modifying task activity result
#'
#' @description get a data.frame of sample results for reapeatability.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param result a character string with the result of the task.
#' @param comment a character string with the comment of the task.
#' @param mytask id of the task to be evaluated.
#'
#' @return a data.frame
#'
#' @noRd
#' @importFrom DBI dbGetQuery dbExecute
#' @importFrom glue glue_sql
#' @import data.table
sql_mod_result <- function(conn,
                           result,
                           comment,
                           mytask){

  id <- pool::dbGetQuery(conn,
                        "SELECT giudizio_id FROM giudizio WHERE pianificazione_id = ?",
                        params = mytask)
  maxid <- pool::dbGetQuery(conn,
                           "SELECT MAX(giudizio_id) FROM giudizio")

  if(nrow(id) == 0) {
    id <- 1 + maxid |>
      unlist() |>
      unname()
  }

  newdata <- data.frame(giudizio_id = id,
                        pianificazione_id = mytask,
                        esito = result,
                        commento = comment)

  # write the new data to a tmp SQL table
  pool::dbWriteTable(conn, "giudizio_tmp", newdata, append = TRUE)

  # normalise the new data
  res <- pool::dbGetQuery(conn,
                        "SELECT
                          tmp.giudizio_id,
                          tmp.pianificazione_id,
                          ex.esito_id,
                          tmp.commento
                          FROM giudizio_tmp AS tmp
                         INNER JOIN esito AS ex
                          ON tmp.esito = ex.esito")

  # remove the tmp SQL table
  pool::dbExecute(conn, "DELETE FROM giudizio_tmp")

    # for new data to be added to the results table
  if(id > maxid){

    # adding the new results
    tblname <- "giudizio"
    mycols <- pool::dbListFields(conn, tblname)

    lapply(seq_len(nrow(res)), function(x) {
      myvals <- res[x, ]

      sql_insert(
        conn = conn,
        tbl = tblname,
        cols = mycols,
        vals = myvals
      )
    })

    # for updating existing columns
  } else {

    tblname <- "giudizio"
    mycols <- pool::dbListFields(conn, tblname)[-1]

    lapply(seq_len(nrow(res)), function(x){
      myvals <- res[x, ]
      mycondval <- res[x, 1]

      sql_update(
        conn = conn,
        tbl = tblname,
        cols = mycols,
        vals = myvals,
        condcol = "giudizio_id",
        condval = mycondval
      )
    })

  }

  res
}

#' SQL query for getting the result associated to a task id
#'
#' @description retrieve the result associated to a task id.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param taskid the id of the element for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_result_for_task <- function(conn, taskid){

  myquery <- glue::glue_sql("SELECT esito
                             FROM esito
                             JOIN giudizio
                             ON esito.esito_id = giudizio.esito_id
                             WHERE pianificazione_id = {taskid};",
                            .con = conn)

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting the comment associated to a task id
#'
#' @description retrieve the result associated to a task id.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param taskid the id of the element for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_comment_for_task <- function(conn, taskid){

  myquery <- glue::glue_sql("SELECT commento
                             FROM giudizio
                             WHERE pianificazione_id = {taskid};",
                            .con = conn)

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting the activity type associated to a task id
#'
#' @description retrieve the result associated to a task id.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param taskid the id of the element for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_activity_for_task <- function(conn, taskid){

  myquery <- glue::glue_sql("SELECT attivita
                             FROM pianificazione AS pl
                             INNER JOIN attivita AS ac
                             ON pl.attivita_id = ac.attivita_id
                             WHERE pianificazione_id = {taskid};",
                            .con = conn)

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for completed and not completed task
#'
#' @description retrieve the result associated to a task id.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param taskid the id of the element for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_is_task_completed <- function(conn, taskid){

  myquery <- glue::glue_sql("SELECT esito_id
                             FROM giudizio
                             WHERE pianificazione_id = {taskid};",
                            .con = conn)

  esito_id <- pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()

  ifelse(length(esito_id) == 0, FALSE, TRUE)
}

#' SQL query for getting the method associated to a task
#'
#' @description retrieve the method associated to a task id.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param taskid the id of the element for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_method_for_task <- function(conn, taskid){

  myquery <- glue::glue_sql("SELECT metodo
                             FROM pianificazione AS pl
                             INNER JOIN metodo AS mt
                             ON pl.metodo_id = mt.metodo_id
                             WHERE pianificazione_id = {taskid};",
                            .con = conn)

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting the planned operator associated to a task
#'
#' @description retrieve the planned operator associated to a task id.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param taskid the id of the element for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_ploperator_for_task <- function(conn, taskid){

  myquery <- glue::glue_sql("SELECT operatore_previsto
                             FROM pianificazione
                             WHERE pianificazione_id = {taskid};",
                            .con = conn)

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}

#' SQL query for getting the actual operator associated to a task
#'
#' @description retrieve the actual operator associated to a task id.
#' @param conn a connection to a database obtained by pool::dbConnect.
#' @param taskid the id of the element for which the id is to be retrieved.
#' @return a character
#'
#' @noRd
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
sql_get_acoperator_for_task <- function(conn, taskid){

  myquery <- glue::glue_sql("SELECT DISTINCT
                              operatore_effettivo
                             FROM pianificazione AS pl
                             INNER JOIN pianificazione_campione AS pc
                              ON pl.pianificazione_id = pc.pianificazione_id
                             INNER JOIN risultato AS rs
                              ON pc.campione_id = rs.campione_id
                             WHERE pl.pianificazione_id = {taskid};",
                            .con = conn)

  pool::dbGetQuery(conn, myquery) |>
    unlist() |>
    unname()
}
