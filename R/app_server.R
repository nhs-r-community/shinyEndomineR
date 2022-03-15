#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # initialise petit r
  
  r <- reactiveValues()
  
  load_prev <- reactive({
    
    if(!is.null(input$loadData)){
      return(readRDS(input$loadData$datapath))
    } else {
      
      return(NULL)
    }
  })
  
  output$downloadData <- downloadHandler(
    
    filename = "load_prev.rds",
    
    content = function(file){
      
      saveRDS(list("merge_data" = merge_data(), 
                   "map_terms" = map_terms()), 
              file = file)
    }
  )
  
  # List the first level callModules here
  
  mod_clean_and_merge_server("clean_and_merge_ui_1", 
                             header_filename = "endo.rda",
                             r = r)
  
  mod_clean_and_merge_server("clean_and_merge_ui_2", 
                             header_filename = "path.rda",
                             r = r)
  
  mod_merge_data_server("merge_data_ui_1", 
                        load_prev = load_prev,
                        r = r)
  
  mod_map_terms_server("map_terms_ui_1", r = r)
  
  barretts_data <- mod_barretts_server("barretts_ui_1")

  polyp_data <- mod_polyps_server("polyps_ui_1")
  
  mod_per_endoscopist_server("per_endoscopist_ui_1", barretts_data = barretts_data,
                             polyp_data = polyp_data)
  
  mod_custom_server("custom_ui_1")
}
