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
    uiOutput(ns("varSelectors"))
    
    # now let's render controls for the names of the merged dataset
  )
}

#' merge_data Server Functions
#'
#' @noRd 
mod_merge_data_server <- function(id, load_prev, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$varSelectors <- renderUI({
      
      fluidRow(
        # endo controls
        column(6, 
               selectInput(session$ns("endoDate"), "Endoscopy date", 
                           choices = names(r$endo_data)),
               selectInput(session$ns("endoNumber"), "Endoscopy hospital number",
                           choices = names(r$endo_data))
        ),
        # pathology controls
        column(6,
               selectInput(session$ns("pathDate"), "Pathology date",
                           choices = names(r$path_data)),
               selectInput(session$ns("pathNumber"), "Pathology hospital number",
                           choices = names(r$path_data))
        )
      )
    })
    
    return_merge <- reactive({
      
      if(!is.null(load_prev())){
        
        return(load_prev()$merge_data)
      }
      
      req(r$endo_data,
          input$endoDate,
          input$endoNumber,
          r$path_data,
          input$pathDate,
          input$pathNumber,
          input$pathNumber != "Start")
      
      cat(str(r$endo_data))
      cat(str(input$endoDate))
      cat(str(input$endoNumber))
      cat(str(r$path_data))
      cat(str(input$pathDate))
      cat(str(input$pathNumber))
      
      the_data <- EndoMineR::Endomerge2(r$endo_data,
                                        input$endoDate,
                                        input$endoNumber,
                                        r$path_data,
                                        input$pathDate,
                                        input$pathNumber)
      if(!("Date" %in% colnames(the_data))){
        colnames(the_data)[colnames(the_data) == input$endoDate] <- "Date"
      }
      
      #To Make sure no date clash issues
      if("Date.x" %in% colnames(the_data)){
        colnames(the_data)[colnames(the_data) == "Date.x"] <- "Date"
      }
      
      if(!("HospitalNum" %in% colnames(the_data))){
        colnames(the_data)[colnames(the_data) == "eHospitalNum"] <- "HospitalNum"
      }
      
      
      #Remove duplicates here
      the_data <- the_data[!duplicated(the_data), ]
      
      return(the_data)
    })
    
    observe({
      
      r$merge_data <- return_merge()
    })
  })
}
