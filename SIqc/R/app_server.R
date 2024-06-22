#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom  pool dbPool poolClose
#' @importFrom RSQLite SQLite
#' @noRd
app_server <- function(input, output, session) {
  conn <- pool::dbPool(
    drv = RSQLite::SQLite(),
    dbname = "./data/sqlite_test.db",
    extended_types = TRUE
  )
  onStop(function() {
    pool::poolClose(conn)
  })

  r_global <- reactiveValues(
    conn = conn,
    taskid = NA,
    activity = NA,
    completed = NA,
    edit_results = 0,
    dbtrigger = NA
  )

  mod_01_plan_server("tasks", r_global)

  observeEvent(r_global$edit_results,
               {
                 req(!is.na(r_global$activity))

                 switch (
                   r_global$activity,
                   "ripetibilità" = mod_011_repeatability_server("rep", r_global),
                   "giustezza" = NA,
                   "proficency test" = NA
                 )

               },
               # very important to avoid the bug of stacked modal dialog
               once = TRUE,
               ignoreInit = TRUE)

}
