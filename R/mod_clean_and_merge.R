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
    
    fluidPage(
      fluidRow(
        
        fileInput(ns("selectFile"), "Load data file"),
        fileInput(ns("loadHeaders"), "Load headers from a previous run"),
        downloadButton(ns("saveHeaders"), "Save headers"),
        uiOutput(ns("textInputsUI")),
        actionButton(ns("add"), "Add UI")
      ),
      
      fluidRow(
        
        column(width = 12,
               DT::dataTableOutput(ns("endotable")),
               style = "overflow-x: scroll;"
        )
      )
    )
  )
}

#' clean_and_merge Server Function
#'
#' @noRd 
mod_clean_and_merge_server <- function(id, header_filename){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # load the data
    
    returnData <- reactive({
      
      req(input$selectFile)
      
      load_data(input$selectFile$datapath)
    })
    
    # show the raw data
    
    output$endotable <- DT::renderDT({
      
      if(isTruthy(endoData())){
        
        endoData()
      } else {
        
        returnData()
      }
    })
    
    # produce UI elements for each heading
    
    output$textInputsUI <- renderUI({
      
      possible_vars <- unlist(strsplit(as.character(returnData()[1, 1]), "\n"))
      
      possible_vars <- substr(possible_vars, 1, 30)
      
      do.call(flowLayout,
              lapply(1 : length(possible_vars), function(x){
                textInput(session$ns(paste0("heading_id_", x)), "Add delimiter",
                          value = possible_vars[x])
              })
      )
    })
    
    # insert new inputs
    
    observeEvent(input$add, {
      insertUI(
        selector = paste0('#', session$ns('add')),
        where = "beforeBegin",
        ui = textInput(session$ns(paste0("heading_id_", input$add + 100)),
                       "Add delimiter")
      )
    })
    
    # define a reactive for the headers
    
    spreadsheetHeaders <- reactive({
      
      if(isTruthy(input$loadHeaders$datapath)){
        
        return(readRDS(input$loadHeaders$datapath))
      }
      
      mywordsOGD <- sapply(
        grep(pattern = "heading_id_", 
             x = stringr::str_sort(names(input), numeric = TRUE), value = TRUE), 
        function(x) input[[x]])
      
      return(stringi::stri_remove_empty(trimws(mywordsOGD)))
    })
    
    # debounce the reactive
    
    spreadsheet_d <- spreadsheetHeaders %>% 
      debounce(5000)
    
    endoData <- reactive({
      
      req(isTruthy(spreadsheet_d()))

      endo_object <- withProgress(message = 'Splitting the data...
        spell checking... 
        term mapping against lexicons...
        cleaning columns...
        formatting columns...',
        
        EndoMineR::textPrep(returnData()[, 1], spreadsheet_d())
      )
    })
    
    # handle saving the headers
    
    output$saveHeaders <- downloadHandler(
      
      filename = header_filename,
      
      content = function(file){
        
        saveRDS(spreadsheet_d, file = file)
      }
    )
    
    # return the data
    
    reactive(
      
      endoData()
    )
  })
}
