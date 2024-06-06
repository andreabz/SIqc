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

  r_global <- reactiveValues(conn = conn)

  mod_01_plan_server("tasks", r_global)

}
