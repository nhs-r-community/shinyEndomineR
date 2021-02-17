#' custom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_custom_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    DT::dataTableOutput(ns("CustomTable"))
  )
}

#' custom Server Functions
#'
#' @noRd 
mod_custom_server <- function(id, merge_data, map_terms){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$CustomTable = DT::renderDT({
      
      DT::datatable(merge_data(), 
                    escape = FALSE, 
                    extensions = c("Select","Buttons"), 
                    selection = "none", 
                    callback = DT::JS(readLines("inst/app/www/custom_dt.js")),
                    options = list(
                      scrollX = TRUE,
                      scrollY = TRUE,
                      pageLength = 200,
                      select = "api",
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))
      )
    }, server = FALSE)
  })
}
