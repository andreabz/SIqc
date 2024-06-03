#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r_global <- reactiveValues()

  mod_01_plan_server("tasks", r_global)

  on.exit(DBI::dbDisconnect(conn))
}
