#' 02_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_02_results_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 02_results Server Functions
#'
#' @noRd 
mod_02_results_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_02_results_ui("02_results_1")
    
## To be copied in the server
# mod_02_results_server("02_results_1")
