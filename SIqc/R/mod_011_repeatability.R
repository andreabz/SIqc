#' 011_repeatability Server Functions
#'
#' @noRd
mod_011_repeatability_server <- function(id, r_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conn <- r_global$conn
    dbtrigger <- makereactivetrigger()

    r_local <- reactiveValues(sample_results = data.table::data.table())

    shiny::observeEvent(r_global$taskid, {
      req(!is.na(r_global$taskid))

      sample_ids <- sql_get_sampleid_for_task(conn, r_global$taskid)

      # update the inputs only when sample information has been saved
      if(length(sample_ids) == 2){
      updateSelectInput(session, "sample1", selected = sql_get_name(conn, "campione", sample_ids[1]))
      updateSelectInput(session, "sample2", selected = sql_get_name(conn, "campione", sample_ids[2]))
      }

      updateSelectInput(session, "result", selected = sql_get_result_for_task(conn, taskid = r_global$taskid))
      updateSelectInput(session, "comment", selected = sql_get_comment_for_task(conn, taskid = r_global$taskid))

      repeatability_modal(conn = conn, id, completed = r_global$completed)
      shinyjs::hide("save_res")
      shiny::removeModal()
    })

      shiny::observeEvent(c(input$sample1, input$sample2), {
        req(!is.na(r_global$taskid))

        r_local$sample_results <- sql_get_repeatability(conn,
                                                        input$sample1,
                                                        input$sample2,
                                                        mytask = r_global$taskid)
      })

      output$dt_data <- renderUI({
        req(!is.na(r_global$taskid))

        repeatabilityDT(r_local$sample_results)
      })

      # when the table is edited compliance is assessed
      observeEvent(input$dt_data_tbl_cell_edit, {
        r_local$sample_results <- DT::editData(
          r_local$sample_results,
          input$dt_data_tbl_cell_edit,
          "dt_data_tbl",
          rownames = FALSE
        )
        r_local$sample_results$esito <- ifelse(
          r_local$sample_results$differenza <= r_local$sample_results$requisito,
          "conforme",
          "non conforme"
        )

      })

      ###### close modal ----
      shiny::observeEvent(input$close_btn, {
        r_global$taskid <- NA
        shiny::removeModal()
      })

      # enable save button only when limit and final result are reported
      observeEvent(c(input$dt_data_tbl_cell_edit,
                     input$result,
                     input$comment), {

        req_completed <- sum(!is.na(as.numeric(r_local$sample_results$requisito)))
        n_parameter <- r_local$sample_results$parametro |> length()

        if(req_completed == n_parameter && !is.null(input$result)){
          shinyjs::show("save_res")
        }

      })

      ###### save modal ----
      shiny::observeEvent(input$save_res, {

        sql_mod_repeatability(
          conn,
          sample1 = input$sample1,
          sample2 = input$sample2,
          mytask = r_global$taskid,
          mydata = r_local$sample_results
        )

        sql_mod_result(
          conn,
          result = input$result,
          comment = input$comment,
          mytask = r_global$taskid
        )

        r_global$dbtrigger <- dbtrigger$trigger()
        r_global$taskid <- NA
        shiny::removeModal()
      })

  })
}

## To be copied in the UI
# mod_011_repeatability_ui("011_repeatability_1")

## To be copied in the server
# mod_011_repeatability_server("011_repeatability_1")
