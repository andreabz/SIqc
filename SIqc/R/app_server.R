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
    conn = conn,           # db connection shared across modules
    taskid = NA,           # id of the task to be visualised, modified or removed
    activity = NA,         # activity type
    completed = NA,        # is the activity completed or not
    edit_results = 0,      # are results to be modified?
    dbtrigger = 0          # trigger to refresh db connection
  )

  mod_01_plan_server("tasks", r_global)

  observeEvent(r_global$edit_results,
               # very important to avoid the bug of stacked modal dialog
               once = TRUE,
               ignoreInit = TRUE,
               {
                 req(!is.na(r_global$activity))

                 switch (
                   r_global$activity,
                   "ripetibilità" = mod_011_repeatability_server("rep", r_global),
                   "giustezza" = NA,
                   "proficency test" = NA
                 )

               })

  mod_02_repeatability_results_server("repdata", r_global)

}
