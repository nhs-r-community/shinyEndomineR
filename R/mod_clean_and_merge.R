#' clean_and_merge UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_clean_and_merge_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # dataset 1
    
    fluidRow(
      
      fileInput(ns("endoscopyFile"), "Load data file"),
      tags$div(id = ns("placeholder1"))
      ),
    
    fluidRow(
      
      DT::dataTableOutput(ns("endotable"))
    )
  )
}

#' clean_and_merge Server Function
#'
#' @noRd 
mod_clean_and_merge_server <- function(input, output, session){
  ns <- session$ns
  
  # load the data
  
  endoscopyData <- reactive({
    
    req(input$endoscopyFile)
    
    load_endoscopy(input$endoscopyFile$datapath)
  })
  
  # show the raw data
  
  output$endotable <- DT::renderDT({
    
    endoscopyData()
  })
  
  # produce UI elements for each heading
  
  textbox_number <- reactiveValues()
  
  observe({
    
    possible_headings <- unlist(strsplit(as.character(endoscopyData()[, 1]), "\n"))
    
    insertUI(selector = paste0("#", ns("placeholder1")), where = "afterEnd", 
             ui = textInput(session$ns("testID"), "Insert text")
    )
  })
  
}

