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
mod_merge_data_server <- function(id, endo_data, path_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$varSelectors <- renderUI({
      
      fluidRow(
        # endo controls
        column(6, 
               selectInput(session$ns("endoDate"), "Endoscopy date", 
                           choices = names(endo_data())),
               selectInput(session$ns("endoNumber"), "Endoscopy hospital number",
                           choices = names(endo_data()))
        ),
        # pathology controls
        column(6,
               selectInput(session$ns("pathDate"), "Pathology date",
                           choices = names(path_data())),
               selectInput(session$ns("pathNumber"), "Pathology hospital number",
                           choices = names(path_data()))
        )
      )
    })
    
    return_merge <- reactive({
      
      the_data <- EndoMineR::Endomerge2(endo_data(),
                                   input$endoDate,
                                   input$endoNumber,
                                   path_data(),
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
      the_data <-  the_data[!duplicated(the_data), ]
      
      return(the_data)
    })
    
    reactive({
      return_merge()
    })
  })
}
