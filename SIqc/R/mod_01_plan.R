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

    conn <- isolate(r_global$conn)
    input_df <- sql_get_task_summary(conn)

    ##### add the action buttons ----
    df <- add_btns(input_df)

    r_local <- shiny::reactiveValues(
      df = df,
      edited_row = NULL,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(df) + 1,
      sample_results = data.table::data.table()
    )

    ###### tasks table -----
    output$dt_table <- DT::renderDT(
      r_local$df,
      filter = list(position = 'top', clear = TRUE),
      colnames = c("Metodo", "Attività", "Anno", "Mese previsto", "Data effettiva",
                   "Operatore previsto", "Operatore effettivo", "Matrice",
                   "Esito", "Azioni"),
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      callback = DT::JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tutti" )'),
      options = list(processing = FALSE,
                     language = dt_italian
      )
    )

    proxy <- DT::dataTableProxy("dt_table")
    shiny::observeEvent(r_local$df, {
      DT::replaceData(proxy, r_local$df, resetPaging = FALSE, rownames = FALSE)
    })

    ##### delete a row ----
    shiny::observeEvent(input$current_id, {
      req(!is.null(input$current_id))
      req(grepl("delete", input$current_id))

      # stringi functions are much faster than grepl
      r_local$dt_row <- which(stringi::stri_detect_regex(r_local$df$actions,
                                                         paste0("\\b", input$current_id, "\\b")))
      r_local$df <- r_local$df[-r_local$dt_row, ]
    })

    ##### edit a row ----
    shiny::observeEvent(input$current_id, {
      req(!is.null(input$current_id))
      req(grepl("edit", input$current_id))

      # stringi functions are much faster than grepl
      r_local$dt_row <- which(stringi::stri_detect_regex(r_local$df$actions,
                                                         paste0("\\b", input$current_id, "\\b")))

      r_local$edited_row <- r_local$df[r_local$dt_row, .SD, .SDcols = !c("actions")]
      modal_dialog(r_local$edited_row, edit = TRUE, conn = conn, id = id)
      r_local$add_or_edit <- NULL
    })

    ##### close modal ----
    shiny::observeEvent(input$dismiss_modal, {
      shiny::removeModal()
    })

    ##### save modal ----
    shiny::observeEvent(input$final_edit, {
      shiny::req(!is.null(input$current_id) &
                  grepl("edit", input$current_id) &
                  is.null(r_local$add_or_edit))

      r_local$edited_row <- list(
        metodo = input$method,
        attivita = input$task,
        anno = input$year |> as.integer(),
        mese_previsto = input$month,
        data_effettiva = input$date,
        operatore_previsto = input$planned_operator,
        operatore_effettivo = input$actual_operator,
        matrice = input$matrix,
        esito = NA,
        actions = r_local$df$actions[r_local$dt_row]
      )

      r_local$df[r_local$dt_row,] <- r_local$edited_row
      shiny::removeModal()
    })

    ##### add samples ----
    shiny::observeEvent(input$add_task, {
      empty_row <- data.frame(
        metodo = NA,
        attivita = NA,
        anno = NA,
        mese_previsto = NA,
        data_effettiva = NA,
        operatore_previsto = NA,
        operatore_effettivo = NA,
        matrice = NA,
        esito = NA
      )

      modal_dialog(empty_row, edit = FALSE, conn = conn, id = id)
      r_local$add_or_edit <- 1
    })

    ##### save modal for new row ----
    shiny::observeEvent(input$final_edit, {
      shiny::req(r_local$add_or_edit == 1)

      add_row <- list(
        metodo = input$method,
        attivita = input$task,
        anno = input$year |> as.integer(),
        mese_previsto = input$month,
        data_effettiva = input$date,
        operatore_previsto = input$planned_operator,
        operatore_effettivo = input$actual_operator,
        matrice = input$matrix,
        esito = NA,
        actions = table_btns(r_local$keep_track_id)
      )

      r_local$df <- list(r_local$df, add_row) |> data.table::rbindlist()
      shiny::removeModal()
    })

    ##### add data ----
    shiny::observeEvent(input$add_data, {

      repeatability_modal(edit = TRUE, conn = conn, id)
      shiny::removeModal()
    })

    observeEvent(c(input$sample1, input$sample2), {
      sample_data <- sql_get_repeatability(conn, input$sample1, input$sample2)
      # get the number of decimals
      ndecimal <- lapply(sample_data$campione1, decimalplaces) |> unlist() |> max()
      sample_data[, `:=` (differenza = abs(campione1 - campione2) |> round(ndecimal),
                          r = rep(NA, .N)  |> as.numeric(),
                          esito = rep(NA, .N)  |> as.numeric()) ]

      r_local$sample_results <- sample_data
    })

    output$dt_data <- renderUI({

      output$dt_data_tbl <- DT::renderDT(
        r_local$sample_results,
        filter = "none",
        selection = "none",
        rownames = FALSE,
        editable = list(target = "column", disable = list(columns = c(0, 1, 2, 3, 4, 6))),
        colnames = c(
          "Parametro",
          "Unità di misura",
          "Campione 1",
          "Campione 2",
          "Differenza",
          "r",
          "esito"
        ),
        options = list(
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-right', targets = 1)
          ),
          dom = 'tp',
          processing = FALSE,
          language = dt_italian
        )
    )
      DT::DTOutput(ns("dt_data_tbl"))
    })

    # when the table is edited compliance is assessed
    observeEvent(input$dt_data_tbl_cell_edit, {

      r_local$sample_results <- DT::editData(r_local$sample_results,
                                             input$dt_data_tbl_cell_edit,
                                             "dt_data_tbl", rownames = FALSE)
      r_local$sample_results$esito <- ifelse(r_local$sample_results$differenza <= r_local$sample_results$r,
                                             "conforme",
                                             "non conforme")
    })


  })
}

## To be copied in the UI
# mod_01_plan_ui("01_plan_1")

## To be copied in the server
# mod_01_plan_server("01_plan_1")
