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
mod_01_plan_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "container",
      div(
        style = "margin-top: 20px;",
        bslib::input_task_button(
          id = ns("add_task"),
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
mod_01_plan_server <- function(id, r_global) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # a reactive trigger for importing data from db
    dbtrigger <- makereactivetrigger()

    conn <- isolate(r_global$conn)

    input_df <- eventReactive(r_global$dbtrigger, sql_get_task_summary(conn))

    r_local <- shiny::reactiveValues(
      df = data.table::data.table(),
      edited_row = NA,
      add_or_edit = NA,
      edit_button = NA,
      keep_track_id = NA
    )

    ##### store the dataset and keep track of the max id value ----
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
      DT::replaceData(proxy,
                      r_local$df,
                      resetPaging = FALSE,
                      rownames = FALSE)
    })

    ##### delete a row ----
    shiny::observeEvent(input$current_id, {
      req(!is.null(input$current_id))
      req(grepl("delete", input$current_id))

      # stringi functions are much faster than grepl
      r_global$taskid <- r_local$df[which(stringi::stri_detect_regex(
        r_local$df$azioni,
        paste0("\\b", input$current_id, "\\b")
      )), id]

      sql_del_taskid(conn, "pianificazione", "pianificazione_id", r_global$taskid)
      sql_del_taskid(conn, "pianificazione_campione", "pianificazione_id", r_global$taskid)
      sql_del_taskid(conn, "ripetibilita", "pianificazione_id", r_global$taskid)
      # update data from db
      r_global$dbtrigger <- dbtrigger$trigger()
    })

    ##### edit a row ----
    shiny::observeEvent(input$current_id, {
      req(!is.null(input$current_id))
      req(grepl("edit", input$current_id))

      # stringi functions are much faster than grepl
      r_global$taskid <- r_local$df[which(stringi::stri_detect_regex(
        r_local$df$azioni,
        paste0("\\b", input$current_id, "\\b")
      )), id]
      r_local$edited_row <- r_local$df[id == r_global$taskid, .SD, .SDcols = c(
        "metodo",
        "attivita",
        "anno",
        "mese_previsto",
        "tipo_campione",
        "operatore_previsto"
      )]

      # completed or not completed flag and activity type
      r_global$completed <- sql_is_task_completed(conn, r_global$taskid)
      r_global$activity <- sql_get_activity_for_task(conn, r_global$taskid)

      modal_dialog(
        r_local$edited_row,
        edit = TRUE,
        completed = r_global$completed,
        conn = conn,
        id = id
      )
      r_local$add_or_edit <- NULL
    })

    ###### close modal ----
    shiny::observeEvent(input$dismiss_modal, {
      shiny::removeModal()
    })

    ###### save modal ----
    shiny::observeEvent(input$final_edit, {
      shiny::req(
        !is.null(input$current_id) &
          grepl("edit", input$current_id) &
          is.null(r_local$add_or_edit)
      )

      r_local$edited_row <- list(
        id = r_global$taskid,
        metodo = input$method,
        attivita = input$task,
        anno = input$year |> as.integer(),
        mese_previsto = input$month,
        operatore_previsto = input$planned_operator,
        tipo_campione = input$sample_type,
        azioni = r_local$df[id == r_global$taskid, azioni]
      )

      sql_mod_taskid(conn, r_local$edited_row, r_global$taskid)
      # update data from db
      r_global$dbtrigger <- dbtrigger$trigger()
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

      modal_dialog(
        empty_row,
        edit = FALSE,
        completed = FALSE,
        conn = conn,
        id = id
      )
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
      r_global$dbtrigger <- dbtrigger$trigger()

      shiny::removeModal()
    })

    ##### add data ----
    shiny::observeEvent(input$add_data, {
      r_global$edit_results <- isolate(r_global$edit_results + 1)
      shiny::removeModal()
    })

  })
}

## To be copied in the UI
# mod_01_plan_ui("01_plan_1")

## To be copied in the server
# mod_01_plan_server("01_plan_1")
