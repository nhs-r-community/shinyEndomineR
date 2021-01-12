#' barretts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_barretts_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # DT::DTOutput(ns("test")),
    
    plotly::plotlyOutput(ns("plotBarrQM")),
    
    plotly::plotlyOutput(ns("plotBarrEQ")),
    
    plotly::plotlyOutput(ns("endoscopyUse_EndoscopyUseBarr")),
    
    plotly::plotlyOutput(ns("plotBarrTSA"))
  )
}

#' barretts Server Functions
#'
#' @noRd 
mod_barretts_server <- function(id, merge_data, map_terms){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    barretts_data <- reactive({
      
      barretts_data <- merge_data()[Reduce(`|`, lapply(merge_data(), 
                                                       grepl, 
                                                       pattern = "columnar.*?lined.*?\\.|barrett")), ]
      
      barretts_data <- EndoMineR::Barretts_PragueScore(barretts_data, 
                                                       map_terms()$Map_FindingsIn, 
                                                       map_terms()$Map_Findings2In)
      
      barretts_data2 <- EndoMineR::Barretts_PragueScore(barretts_data, 
                                                       "findings", 
                                                       NULL)
      barretts_data$mytext <- NULL
      barretts_data$MStage <- as.numeric(barretts_data$MStage)
      barretts_data$CStage <- as.numeric(barretts_data$CStage)
      barretts_data$IMorNoIM <- EndoMineR::Barretts_PathStage(barretts_data, 
                                                              map_terms()$Map_MicroscopicTextIn)
      # note that the strings in the following line are not names of the merged dataset, they are fixed
      
      barretts_data$FU_Type <- EndoMineR::Barretts_FUType(barretts_data, 
                                                          "CStage", "MStage", "IMorNoIM")
      
      barretts_data <- EndoMineR::SurveilTimeByRow(barretts_data, 
                                                   map_terms()$Map_HospitalNumberIn,
                                                   map_terms()$Map_EndoscopyDateIn)
    })
    
    output$plotBarrQM <- plotly::renderPlotly({
      
      plotly::ggplotly(
        ggplot2::ggplot(barretts_data(), 
                        ggplot2::aes_string(x = "endoscopist", fill="IMorNoIM")) + 
          ggplot2::geom_histogram(stat = "count") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))
      )
    })
    
    output$test <- DT::renderDT({
      
      merge_data()
    })
    
  })
}
