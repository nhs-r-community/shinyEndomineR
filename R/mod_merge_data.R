#' merge_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_merge_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # render controls for both datasets in one renderUI()
      uiOutput(ns("varSelectors")),
      actionButton(ns("mergeData"), "Merge data")
  )
}
    
#' merge_data Server Functions
#'
#' @noRd 
mod_merge_data_server <- function(id, endo_data, path_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$varSelectors <- renderUI({
      
      fluidRow(
        # endo controls
        column(6, 
               selectInput("endoDate", "Endoscopy date", 
                           choices = names(endo_data())),
               selectInput("endoNumber", "Endoscopy hospital number",
                           choices = names(endo_data()))
               ),
        # pathology controls
        column(6,
               selectInput("pathDate", "Pathology date",
                           choices = names(path_data())),
               selectInput("pathNumber", "Pathology hospital number",
                           choices = names(path_data()))
               )
      )
    })
    
    observeEvent("mergeData", {
      
      merged_data <- reactiveValues({
        data = EndoMineR::Endomerge2(endo_data(),
                                     input$endoData,
                                     input$endoNumber,
                                     path_data(),
                                     input$pathDate,
                                     input$pathNumber)
      })
    })
 
  })
}
