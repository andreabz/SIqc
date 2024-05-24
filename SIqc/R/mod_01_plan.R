#' 01_plan UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_01_plan_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 01_plan Server Functions
#'
#' @noRd 
mod_01_plan_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_01_plan_ui("01_plan_1")
    
## To be copied in the server
# mod_01_plan_server("01_plan_1")
