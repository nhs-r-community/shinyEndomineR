#' custom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import esquisse
#' @importFrom shiny NS tagList 
mod_custom_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    esquisserUI(
      id = ns("esquisseCustom"), 
      header = FALSE, # dont display gadget title
      choose_data = FALSE # dont display button to change data
    ),

    rpivotTable::rpivotTableOutput(ns("OverallPivot")),
    
    DT::dataTableOutput(ns("CustomTable"))
  )
}

#' custom Server Functions
#'
#' @noRd 
mod_custom_server <- function(id, merge_data, map_terms){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    data_r <- reactiveValues(data = data.frame(), name = "custom")
    
    observe({
      
      req(map_terms()$Map_HospitalNumberIn)
      
      data_r$data <- merge_data() %>% 
        dplyr::select(-DayDiff)
    })
    
    custom_trim <- reactive({
      
      req(input$CustomTable_columns_selected)

      merge_data() %>%
        dplyr::slice(input$CustomTable_rows_all) %>%
        dplyr::select(input$CustomTable_columns_selected)
    })
    
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
                      buttons = c('copy', 'csv', 'excel', 'pdf', 
                                  'print','colvis'))
      )
    }, server = TRUE)
    
    result <- callModule(
      module = esquisserServer,
      id = "esquisseCustom",
      data = data_r
    )
    
    output$OverallPivot <- rpivotTable::renderRpivotTable({
      
      rpivotTable::rpivotTable(custom_trim())
    })
  })
}
