#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
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
      
      saveRDS(merge_data(), file = file)
    }
  )
  
  endo_data <- mod_clean_and_merge_server("clean_and_merge_ui_1", 
                                          header_filename = "endo.rda")
  
  path_data <- mod_clean_and_merge_server("clean_and_merge_ui_2", 
                                          header_filename = "path.rda")
  
  merge_data <- mod_merge_data_server("merge_data_ui_1", endo_data = endo_data, 
                                      path_data = path_data, load_prev = load_prev)
  
  map_terms <- mod_map_terms_server("map_terms_ui_1", merge_data = merge_data)
  
  mod_barretts_server("barretts_ui_1", merge_data = merge_data, 
                      map_terms = map_terms)
  
  mod_polyps_server("polyps_ui_1", merge_data = merge_data, 
                    map_terms = map_terms)
  
  mod_per_endoscopist_server("per_endoscopist_ui_1", merge_data = merge_data, 
                             map_terms = map_terms)
  
  mod_custom_server("custom_ui_1", merge_data = merge_data, 
                    map_terms = map_terms)
}
