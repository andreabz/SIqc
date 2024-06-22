#' 011_repeatability Server Functions
#'
#' @noRd
mod_011_repeatability_server <- function(id, r_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    conn <- isolate(r_global$conn)
    dbtrigger <- makereactivetrigger()

    r_local <- reactiveValues(sample_results = data.table::data.table(),
                              sample_ids = NA,
                              sample_name1 = NA,
                              sample_name2 = NA,
                              task_result = NA,
                              task_comment = NA)

    shiny::observeEvent(r_global$taskid, {

      r_local$sample_ids <- sql_get_sampleid_for_task(conn, r_global$taskid)
      r_local$sample_name1 <- sql_get_name(conn, "campione", r_local$sample_ids[1])
      r_local$sample_name2 <- sql_get_name(conn, "campione", r_local$sample_ids[2])
      r_local$task_result <- sql_get_result_for_task(conn, taskid = r_global$taskid)
      r_local$task_comment <- sql_get_comment_for_task(conn, taskid = r_global$taskid)

      # update the inputs only when sample information has been saved
      if(length(r_local$sample_ids) == 2){
        freezeReactiveValue(input, "sample1")
        updateSelectInput(session, "sample1", selected = r_local$sample_name1)
        freezeReactiveValue(input, "sample2")
        updateSelectInput(session, "sample2", selected = r_local$sample_name2)
      }

      freezeReactiveValue(input, "result")
      updateSelectInput(session, "result", selected = r_local$task_result)
      freezeReactiveValue(input, "comment")
      updateSelectInput(session, "comment", selected = r_local$task_comment)

    })

    shiny::observeEvent(r_global$edit_results, {
      req(r_global$activity == "ripetibilità")

      repeatability_modal(conn = conn, id, completed = r_global$completed)
      shinyjs::hide("save_res")
    })

    shiny::observeEvent(c(input$sample1, input$sample2), {

      r_local$sample_results <- sql_get_repeatability(conn,
                                                      input$sample1,
                                                      input$sample2,
                                                      mytask = r_global$taskid)
    })

    result_tbl <- reactive({repeatabilityDT(r_local$sample_results)})
    output$dt_data <- DT::renderDT(result_tbl())

      # when the table is edited compliance is assessed
      observeEvent(input$dt_data_cell_edit, {
        r_local$sample_results <- DT::editData(
          r_local$sample_results,
          input$dt_data_cell_edit,
          "dt_data",
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
        shiny::removeModal()
      })

      # enable save button only when limit and final result are reported
      observeEvent(c(input$dt_data_cell_edit,
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
        shiny::removeModal()
      })

  })
}

## To be copied in the UI
# mod_011_repeatability_ui("011_repeatability_1")

## To be copied in the server
# mod_011_repeatability_server("011_repeatability_1")
