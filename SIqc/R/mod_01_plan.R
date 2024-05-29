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
        style = "margin-top: 50px;",
        bslib::input_task_button(id = ns("add_task"),
                                 label = "Aggiungi attività",
                                 icon = shiny::icon("plus")
                                 )
        )
      ),

    div(
      class = "container",
        style = "margin-top: 50px;",
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
mod_01_plan_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    input_df <- read.csv2(here::here("data-raw/registro.csv")) |> prepare_tasks_summary()
    ##### add the action buttons ----
    df <- add_btns(input_df)

    rv <- shiny::reactiveValues(
      df = df,
      dt_row = NULL,
      add_or_edit = NULL,
      edit_button = NULL,
      keep_track_id = nrow(mtcars) + 1
    )

    ###### tasks table -----
    output$dt_table <- DT::renderDT(
      rv$df,
      filter = list(position = 'top', clear = TRUE),
      colnames = c("Metodo", "Attività", "Mese previsto", "Data effettiva",
                   "Operatore previsto", "Operatore effettivo", "Matrice",
                   "Esito", "Azioni"),
      selection = "none",
      escape = FALSE,
      rownames = FALSE,
      callback = DT::JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "Tutti" )'),
      options = list(processing = FALSE,
                     language = list(
                       length = "Mostra",
                       emptyTable = "Nessun dato presente nella tabella",
                       info = "Vista da _START_ a _END_ di _TOTAL_ elementi",
                       infoEmpty ="Vista da 0 a 0 di 0 elementi",
                       infoFiltered = "(filtrati da _MAX_ elementi totali)",
                       infoPostfix = "",
                       infoThousands = ".",
                       lengthMenu = "Visualizza _MENU_ elementi",
                       loadingRecords = "Caricamento...",
                       processing = "Elaborazione...",
                       search = "Cerca:",
                       zeroRecords = "La ricerca non ha portato alcun risultato.",
                       paginate = list(
                         first = "Inizio",
                         previous = "Precedente",
                         `next` = "Successivo",
                         last = "Fine"
                       ),
                       aria = list(
                         sortAscending = ": attiva per ordinare la colonna in ordine crescente",
                         sortDescending = ": attiva per ordinare la colonna in ordine decrescente"
                       )
                     )
      )
    )

    proxy <- DT::dataTableProxy("dt_table")
    shiny::observe({
      DT::replaceData(proxy, rv$df, resetPaging = FALSE, rownames = FALSE)
    })

    ##### delete a row ----
    shiny::observeEvent(input$current_id, {
      req(!is.null(input$current_id))
      req(grepl("delete", input$current_id))

      # stringi functions are much faster than grepl
      rv$dt_row <- which(stringi::stri_detect_regex(rv$df$Azioni, paste0("\\b", input$current_id, "\\b")))
      rv$df <- rv$df[-rv$dt_row, ]
      print(str(rv$df))
    })



  })
}

## To be copied in the UI
# mod_01_plan_ui("01_plan_1")

## To be copied in the server
# mod_01_plan_server("01_plan_1")
