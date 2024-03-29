#' map_terms UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_terms_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    splitLayout(
      fileInput(ns("loadHeaders"), "Load headers from a previous run"),
      actionButton(ns("click_map"), "Map data")
    ),
    
    uiOutput(ns("termMappingUI")),
    
    downloadButton(ns("saveHeaders"), "Save headers")
  )
}

#' map_terms Server Functions
#'
#' @noRd 
mod_map_terms_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    fieldsMapping <- c("Map_HospitalNumberIn", "Map_EndoscopistIn", 
                       "Map_ProcedurePerformedIn", "Map_EndoscopyDateIn", 
                       "Map_FindingsIn", "Map_Findings2In",
                       "Map_EventsIn", "Map_MacroscopicTextIn", 
                       "Map_MicroscopicTextIn", 
                       "Map_MedicationsIn", "Map_MacroscopicTextDelimIn", 
                       "Map_InstrumentIn", "Map_IndicationsIn")
    
    output$termMappingUI <- renderUI({
      
      tagList(
        fluidRow(
          column(4, 
                 selectizeInput(session$ns("Map_HospitalNumberIn"),
                                "Select the Hospital Number column", 
                                choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_EndoscopistIn"),
                                "Select the endoscopist column", 
                                choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_MedicationsIn"),
                                "Select the Medication column", 
                                choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_IndicationsIn"),
                                "Select the Indications column", 
                                choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_ProcedurePerformedIn"), 
                                "Select the procedure performed description column", 
                                choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_EndoscopyDateIn"), 
                                "Select the procedure performed date", 
                                choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_InstrumentIn"),
                                "Select the Instrument column", 
                                choices = colnames(r$merge_data))
          ),
          column(4, 
                 selectizeInput(session$ns("Map_FindingsIn"),
                                "Select the Endoscopic Findings Column", 
                                choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_Findings2In"),
                                "Select the second findings column (if present)", 
                                choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_EventsIn"),
                                "Select the column which states what events occurred 
                              (eg clips/ dilatation etc.)", 
                              choices = colnames(r$merge_data)),
                 
                 selectizeInput(session$ns("Map_MacroscopicTextIn"),
                                "Select the macroscopic histology column", 
                                choices = colnames(r$merge_data)),
                 
                 textInput(session$ns("Map_MacroscopicTextDelimIn"),
                           "Write term that delimits the biopsies"),
                 
                 selectizeInput(session$ns("Map_MicroscopicTextIn"),
                                "Select the microscopic histology description column", 
                                choices = colnames(r$merge_data))
                 
          ))
      )
    })
    
    # handle saving the headers
    
    output$saveHeaders <- downloadHandler(
      
      filename = "map_terms.rds",
      
      content = function(file){
        
        map_terms <- purrr::map_chr(fieldsMapping, function(x) {
          
          input[[x]]
        })
        
        names(map_terms) <- fieldsMapping
        
        saveRDS(map_terms, file = file)
      }
    )
    
    observe({
      
      req(input$loadHeaders)
      
      load_mapped_terms <- readRDS(input$loadHeaders$datapath)
      
      purrr::walk(fieldsMapping[!fieldsMapping == "Map_MacroscopicTextDelimIn"], 
                  function(x) {
                    
                    updateSelectInput(session, x, 
                                      selected = load_mapped_terms[[x]])
                  })
      
      updateTextInput(session, "Map_MacroscopicTextDelimIn",
                      value = load_mapped_terms[["Map_MacroscopicTextDelimIn"]])
      
    })
    
    observe({
      
      req(input$click_map)
      
      # input mapping defined for a lot of functions above
      
      map_to_df <- fieldsMapping
      
      names(map_to_df) <- fieldsMapping
      
      r$map_terms <- purrr::map_df(map_to_df, function(x) input[[x]])
      
      showModal(modalDialog(
        title = "Successful mapping!",
        "Click anywhere to dismiss this message", 
        easyClose = TRUE
      ))
    })
  })
}
