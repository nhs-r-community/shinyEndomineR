#' plots 
#'
#' @description Plot the occurrence of endoscopies over time
#' 
#' @param data dataframe \\TODO
#' @param endoscopy_date string the name of the endoscopy date 
#' variable in the merged dataset
#'
#' @return a plot suitable for renderPlot()
#'
#' @noRd

calendar_heatmap <- function(data, endoscopy_date){
  
  dtData <- data |> 
    dplyr::group_by(!!rlang::sym(endoscopy_date)) %>%
    dplyr::summarise(n = dplyr::n())
  
  # Get rid of NA's as they mess things up.
  dtData <- na.omit(data.table::as.data.table(dtData))
  
  p1 = ggTimeSeries::ggplot_calendar_heatmap(
    dtData,
    endoscopy_date,
    'n'
  )
  
  p1 +
    ggplot2::xlab('') +
    ggplot2::ylab('') +
    ggplot2::scale_fill_continuous(low = 'green', high = 'red') +
    ggplot2::facet_wrap(~ Year, ncol = 1)
  
}
