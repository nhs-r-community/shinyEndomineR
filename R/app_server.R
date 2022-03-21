#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  # initialise petit r
  
  r <- reactiveValues()
  
  observe({
    
    if(!is.null(input$loadData)){
      
      list_output <- readRDS(input$loadData$datapath)
      
      r$merge_data <- list_output$merge_data
      r$map_terms <- list_output$map_terms
      
    } else {
      
      # nothing!
    }
  })
  
  output$downloadData <- downloadHandler(
    
    filename = "load_prev.rds",
    
    content = function(file){
      
      saveRDS(list("merge_data" = r$merge_data, 
                   "map_terms" = r$map_terms), 
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
  
  # barretts_data <- mod_barretts_server("barretts_ui_1", r = r)
  # 
  # polyp_data <- mod_polyps_server("polyps_ui_1", r = r)
  # 
  # mod_per_endoscopist_server("per_endoscopist_ui_1", barretts_data = barretts_data,
  #                            polyp_data = polyp_data, r = r)
  # 
  # mod_custom_server("custom_ui_1", r = r)
}
