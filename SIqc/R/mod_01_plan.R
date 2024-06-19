#' 01_plan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib input_task_button
#' @importFrom DT DTOutput
#' @importFrom here here
mod_01_plan_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(
      class = "container",
      div(
        style = "margin-top: 20px;",
        bslib::input_task_button(id = ns("add_task"),
                                 label = "Aggiungi attività",
                                 icon = shiny::icon("plus")
                                 )
        )
      ),

    div(
      class = "container",
        style = "margin-top: 20px;",
    DT::DTOutput(outputId = ns("dt_table"), width = "100%")
      )

  )
}

#' 01_plan Server Functions
#'
#' @noRd
#' @import shiny
#' @importFrom DT renderDT dataTableProxy replaceData JS
#' @importFrom stringi stri_detect_regex
mod_01_plan_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # a reactive trigger for importing data from db
    dbtrigger <- makereactivetrigger()

    conn <- isolate(r_global$conn)

    input_df <- reactive({
      dbtrigger$depend()
      sql_get_task_summary(conn)
    })

    r_local <- shiny::reactiveValues(
      df = NULL,
      edited_row = NULL,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = NULL,
      sample_results = data.table::data.table()
    )

    ##### add the action buttons ----
    observeEvent(input_df(), {
      r_local$df <- input_df()
      r_local$keep_track_id <- max(r_local$df$id)
    })

    ###### tasks table -----
    output$dt_table <- renderDT({
      qclistDT(r_local$df)
      })
    proxy <- DT::dataTableProxy("dt_table")
    shiny::observeEvent(r_local$df, {
      DT::replaceData(proxy, r_local$df, resetPaging = FALSE, rownames = FALSE)
    })

    ##### delete a row ----
    shiny::observeEvent(input$current_id, {
      req(!is.null(input$current_id))
      req(grepl("delete", input$current_id))

      # stringi functions are much faster than grepl
      r_local$dt_row <- r_local$df[which(stringi::stri_detect_regex(r_local$df$azioni,
                                                         paste0("\\b", input$current_id, "\\b"))), id]

      sql_del_taskid(conn, r_local$dt_row)
      # update data from db
      dbtrigger$trigger()
    })

    ##### edit a row ----
    shiny::observeEvent(input$current_id, {
      req(!is.null(input$current_id))
      req(grepl("edit", input$current_id))

      # stringi functions are much faster than grepl
      r_local$dt_row <- r_local$df[which(stringi::stri_detect_regex(r_local$df$azioni,
                                                                    paste0("\\b", input$current_id, "\\b"))), id]
      r_local$edited_row <- r_local$df[id == r_local$dt_row,
                                       .SD,
                                       .SDcols = c("metodo",
                                                   "attivita",
                                                   "anno",
                                                   "mese_previsto",
                                                   "tipo_campione",
                                                   "operatore_previsto")]

      modal_dialog(r_local$edited_row, edit = TRUE, conn = conn, id = id)
      r_local$add_or_edit <- NULL
    })

    ###### close modal ----
    shiny::observeEvent(input$dismiss_modal, {
      shiny::removeModal()
    })

    ###### save modal ----
    shiny::observeEvent(input$final_edit, {
      shiny::req(!is.null(input$current_id) &
                  grepl("edit", input$current_id) &
                  is.null(r_local$add_or_edit))

      r_local$edited_row <- list(
        id = r_local$dt_row,
        metodo = input$method,
        attivita = input$task,
        anno = input$year |> as.integer(),
        mese_previsto = input$month,
        operatore_previsto = input$planned_operator,
        tipo_campione = input$sample_type,
        azioni = r_local$df[id == r_local$dt_row, azioni]
      )

      sql_mod_taskid(conn, r_local$edited_row, r_local$dt_row)
      # update data from db
      dbtrigger$trigger()
      shiny::removeModal()
    })

    ##### add samples ----
    shiny::observeEvent(input$add_task, {
      empty_row <- data.frame(
        metodo = NA,
        attivita = NA,
        anno = NA,
        mese_previsto = NA,
        tipo_campione = NA,
        operatore_previsto = NA
      )

      modal_dialog(empty_row, edit = FALSE, conn = conn, id = id)
      r_local$add_or_edit <- 1
    })

    ##### save modal for new row ----
    shiny::observeEvent(input$final_edit, {
      shiny::req(r_local$add_or_edit == 1)

      new_rowid <- r_local$keep_track_id + 1

      add_row <- list(
        id = new_rowid,
        metodo = input$method,
        attivita = input$task,
        anno = input$year |> as.integer(),
        mese_previsto = input$month,
        tipo_campione = input$sample_type,
        operatore_previsto = input$planned_operator,
        azione = table_btns(new_rowid)
      )

      sql_add_task(conn, add_row)
      dbtrigger$trigger()

      shiny::removeModal()
    })

    ##### add data ----
    shiny::observeEvent(input$add_data, {
      sample_ids <- sql_get_sampleid_for_task(conn, r_local$dt_row)

      if(sum(!is.na(sample_ids)) == 2){
        updateSelectInput(session, "sample1", selected = sql_get_name(conn, "campione", sample_ids[1]))
        updateSelectInput(session, "sample2", selected = sql_get_name(conn, "campione", sample_ids[2]))
      }

      updateSelectInput(session, "result",
                        selected = sql_get_result_for_task(conn, taskid = r_local$dt_row))
      updateSelectInput(session, "comment",
                        selected = sql_get_comment_for_task(conn, taskid = r_local$dt_row))

      repeatability_modal(conn = conn, id)
      shiny::removeModal()
    })

    observeEvent(c(input$sample1, input$sample2), {
      r_local$sample_results <- sql_get_repeatability(conn,
                                                      input$sample1,
                                                      input$sample2,
                                                      mytask = r_local$dt_row)
    })

    output$dt_data <- renderUI({

      output$dt_data_tbl <- repeatabilityDT(r_local$sample_results)
      DT::DTOutput(ns("dt_data_tbl"))
    })

    # when the table is edited compliance is assessed
    observeEvent(input$dt_data_tbl_cell_edit, {

      r_local$sample_results <- DT::editData(r_local$sample_results,
                                             input$dt_data_tbl_cell_edit,
                                             "dt_data_tbl", rownames = FALSE)
      r_local$sample_results$esito <- ifelse(r_local$sample_results$differenza <= r_local$sample_results$requisito,
                                             "conforme",
                                             "non conforme")
    })

    ###### close modal ----
    shiny::observeEvent(input$close_btn, {
      shiny::removeModal()
    })

    ###### save modal ----
    shiny::observeEvent(input$save_res, {
      shiny::req(!is.null(input$current_id) &
                   grepl("edit", input$current_id) &
                   is.null(r_local$add_or_edit))

     sql_mod_repeatability(conn,
                           sample1 = input$sample1,
                           sample2 = input$sample2,
                           mytask = r_local$dt_row,
                           mydata = r_local$sample_results)

     sql_mod_result(conn,
                    result = input$result,
                    comment = input$comment,
                    mytask = r_local$dt_row)


      dbtrigger$trigger()
      shiny::removeModal()
    })


  })
}

## To be copied in the UI
# mod_01_plan_ui("01_plan_1")

## To be copied in the server
# mod_01_plan_server("01_plan_1")
