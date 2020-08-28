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
      uiOutput(ns("textInputsUI"))
    ),
    
    fluidRow(
      
      # textOutput(ns("testInputs")),
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
    
    if(isTruthy(endoData())){
      
      endoData()
    } else {
      
      endoscopyData()
    }
  })
  
  # produce UI elements for each heading
  
  output$textInputsUI <- renderUI({
    
    possible_vars <- unlist(strsplit(as.character(endoscopyData()[1, 1]), "\n"))
    
    possible_vars <- substr(possible_vars, 1, 30)
    
    do.call(flowLayout,
            lapply(1 : length(possible_vars), function(x){
              textInput(session$ns(paste0("heading_id_", x)), "Insert text",
                        value = possible_vars[x])
            })
    )
  })
  
  output$testInputs <- renderText({
    
    list_of_headings <- sapply(
      grep(pattern = "heading_id_", 
           x = stringr::str_sort(names(input), numeric = TRUE), value = TRUE), 
      function(x) input[[x]])
    
    list_of_headings
  })
  
  endoData <- reactive({
    
    mywordsOGD <- sapply(
      grep(pattern = "heading_id_", 
           x = stringr::str_sort(names(input), numeric = TRUE), value = TRUE), 
      function(x) input[[x]])
    
    mywordsOGD <- stringi::stri_remove_empty(trimws(mywordsOGD))
    
    endo_object <- withProgress(message = 'Splitting the data...
        spell checking... 
        term mapping against lexicons...
        cleaning columns...
        formatting columns...',
      
      EndoMineR::textPrep(endoscopyData()[, 1], mywordsOGD)
    )
  })
}
