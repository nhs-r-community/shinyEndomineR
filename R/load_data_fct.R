#' load datafile
#' 
#' Load a dataframe from an excel sheet and call EndoPaste on it
#' 
#' @param filepath filepath
#' @return dataframe
load_endoscopy <- function(filepath){
  
  data_file <- readxl::read_excel(filepath)
  return(data.frame(EndoMineR::EndoPaste(data_file)[1], stringsAsFactors = FALSE))
  
  # return(as.character(EndoMineR::EndoPaste(data_file)[[1]]))
}
