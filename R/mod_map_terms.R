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
    
    uiOutput(ns("termMappingUI"))
  )
}

#' map_terms Server Functions
#'
#' @noRd 
mod_map_terms_server <- function(id, merge_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$termMappingUI <- renderUI({
      
      tagList(
        fluidRow(
          column(4, 
                 selectizeInput(session$ns("Map_HospitalNumberIn"),
                                "Select the Hospital Number column", 
                                choices = colnames(merge_data()),
                                selected = "HospitalNum"),
                 
                 selectizeInput(session$ns("Map_EndoscopistIn"),
                                "Select the endoscopist column", 
                                choices = colnames(merge_data()),
                                selected = "endoscopist"),
                 
                 selectizeInput(session$ns("Map_MedicationsIn"),
                                "Select the Medication column", 
                                choices = colnames(merge_data()),
                                selected = "medications"),
                 
                 selectizeInput(session$ns("Map_IndicationsIn"),
                                "Select the Indications column", 
                                choices = colnames(merge_data()),
                                selected = "indications"),
                 
                 selectizeInput(session$ns("Map_ProcedurePerformedIn"), 
                                "Select the procedure performed description column", 
                                choices = colnames(merge_data()),
                                selected = "procedureperformed"),
                 
                 selectizeInput(session$ns("Map_EndoscopyDateIn"), 
                                "Select the procedure performed date", 
                                choices = colnames(merge_data()),
                                selected = "Date"),
                 
                 selectizeInput(session$ns("Map_InstrumentIn"),
                                "Select the Instrument column", 
                                choices = colnames(merge_data()),
                                selected = "instrument")
          ),
          column(4, 
                 selectizeInput(session$ns("Map_FindingsIn"),
                                "Select the Endoscopic Findings Column", 
                                choices = colnames(merge_data()),
                                selected = "findings"),
                 
                 selectizeInput(session$ns("Map_Findings2In"),
                                "Select the second findings column (if present)", 
                                choices = colnames(merge_data()),
                                selected = "diagnosis"),
                 
                 selectizeInput(session$ns("Map_EventsIn"),
                                "Select the column which states what events occurred 
                              (eg clips/ dilatation etc.)", 
                              choices = colnames(merge_data()),
                              selected = "findings"),
                 
                 selectizeInput(session$ns("Map_MacroscopicTextIn"),
                                "Select the macroscopic histology column", 
                                choices = colnames(merge_data()),
                                selected = "macroscopicdescription"),
                 
                 selectizeInput(session$ns("Map_MacroscopicTextDelimIn"),
                                "Select the term that delimits the biopsies", 
                                choices = colnames(merge_data())),
                 
                 selectizeInput(session$ns("Map_MicroscopicTextIn"),
                                "Select the microscopic histology description column", 
                                choices = colnames(merge_data()),
                                selected = "natureofspecimen")
                 
          ))
      )
    })
    
    reactive({
      
      fieldsMapping <- c("Map_HospitalNumberIn", "Map_EndoscopistIn", 
                         "Map_ProcedurePerformedIn", "Map_EndoscopyDateIn", 
                         "Map_FindingsIn", "Map_Findings2In",
                         "Map_EventsIn", "Map_MacroscopicTextIn", 
                         "Map_MicroscopicTextIn", "ImageMerge_DelimTextPickersIn", 
                         "Map_MedicationsIn", "Map_MacroscopicTextDelimIn", 
                         "Map_InstrumentIn", "Map_IndicationsIn")
      
      sapply(fieldsMapping, function(x) input[[x]])
    })
  })
}
