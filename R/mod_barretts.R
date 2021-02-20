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
    
    fluidRow(
      column(6, 
             plotly::plotlyOutput(ns("plotBarrQM"))),
      column(6, plotly::plotlyOutput(ns("plotBarrEQ")))
    ),
    
    fluidRow(
      column(6, 
             plotly::plotlyOutput(ns("endoscopyUse_EndoscopyUseBarr"))),
      column(6, plotly::plotlyOutput(ns("plotBarrTSA"))
      )
    )
  )
}

#' barretts Server Functions
#'
#' @noRd 
mod_barretts_server <- function(id, merge_data, map_terms){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    barretts_data <- reactive({
      
      barretts_data <- 
        merge_data()[Reduce(`|`, 
                            lapply(merge_data(), 
                                   grepl, 
                                   pattern = "columnar.*?lined.*?\\.|barrett")), ]
      
      barretts_data <- EndoMineR::Barretts_PragueScore(barretts_data, 
                                                       map_terms()$Map_FindingsIn, 
                                                       map_terms()$Map_Findings2In)
      
      barretts_data$mytext <- NULL
      barretts_data$MStage <- as.numeric(barretts_data$MStage)
      barretts_data$CStage <- as.numeric(barretts_data$CStage)
      barretts_data$IMorNoIM <- 
        EndoMineR::Barretts_PathStage(barretts_data, 
                                      map_terms()$Map_MicroscopicTextIn)
      # note that the strings in the following line are not names of the merged dataset, 
      # they are fixed
      
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
    
    output$plotBarrEQ <- plotly::renderPlotly({
      
      Hiatus <- merge_data() %>% 
        dplyr::group_by(!! rlang::sym(map_terms()$Map_EndoscopistIn)) %>% 
        dplyr::summarise(Hiatus = (sum(grepl("[Hh]iatus|[Ii]sland", 
                                             !!rlang::sym(map_terms()$Map_FindingsIn))) / dplyr::n()) * 100)
      Island <- merge_data() %>% 
        dplyr::group_by(!! rlang::sym(map_terms()$Map_EndoscopistIn)) %>% 
        dplyr::summarise(Island = (sum(grepl("[Ii]sland", 
                                             !!rlang::sym(map_terms()$Map_FindingsIn))) / dplyr::n()) * 100)
      Pinch <- merge_data() %>% 
        dplyr::group_by(!! rlang::sym(map_terms()$Map_EndoscopistIn)) %>% 
        dplyr::summarise(Pinch = (sum(grepl("[Pp]inch", 
                                            !!rlang::sym(map_terms()$Map_FindingsIn))) / dplyr::n()) * 100)
      Lesion <- merge_data() %>% 
        dplyr::group_by(!! rlang::sym(map_terms()$Map_EndoscopistIn)) %>% 
        dplyr::summarise(Lesion = (sum(grepl("esion|odule|lcer", 
                                             !!rlang::sym(map_terms()$Map_FindingsIn))) / dplyr::n()) * 100)
      
      FinalTable <- dplyr::full_join(Hiatus, Island, by = map_terms()$Map_EndoscopistIn)
      FinalTable <- dplyr::full_join(FinalTable, Pinch, by = map_terms()$Map_EndoscopistIn)
      FinalTable <- dplyr::full_join(FinalTable, Lesion, by = map_terms()$Map_EndoscopistIn)
      
      FinalTable <- data.frame(FinalTable)
      
      #Need to gather the table to make tidy for ggplot
      
      FinalTable <- tidyr::gather(FinalTable,
                                  key = "DocumentedElement",
                                  value = "PercentDocs",
                                  -!!rlang::sym(map_terms()$Map_EndoscopistIn))
      
      key <- map_terms()$Map_EndoscopistIn
      
      p <- ggplot2::ggplot(FinalTable, 
                           ggplot2::aes_string(x = key, y = "PercentDocs", fill = "DocumentedElement")) + 
        ggplot2::geom_bar(stat = "identity") + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))
      
      plotly::ggplotly(p, source = "subset", key = key) %>% 
        plotly::layout(dragmode = "select")
    })
    
    output$test <- DT::renderDT({
      
      merge_data()
    })
    
  })
}
