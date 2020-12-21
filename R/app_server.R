#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  endo_data <- mod_clean_and_merge_server("clean_and_merge_ui_1", header_filename = "endo.rda")
  
  path_data <- mod_clean_and_merge_server("clean_and_merge_ui_2", header_filename = "path.rda")
  
  merge_data <- mod_merge_data_server("merge_data_ui_1", endo_data = endo_data, path_data = path_data)
}
