
buttonHTML <- function(i){
  as.character(
    actionButton(
      paste0("button_", i), label = "Report", 
      onclick = sprintf("Shiny.setInputValue('button', %d);", i)           
    )
  )
}
