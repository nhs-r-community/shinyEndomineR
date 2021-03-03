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
    
    hr(),
    
    fluidRow(
      column(6, 
             plotly::plotlyOutput(ns("endoscopyUse_EndoscopyUseBarr"))),
      column(6, plotly::plotlyOutput(ns("plotBarrTSA"))
      )
    ),
    fluidRow(
      column(6, 
             DT::DTOutput(ns("BarrDDR_Table"))
      ),
      column(6, 
             DT::DTOutput(ns("drilldownBarr")))
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
                           ggplot2::aes_string(x = key, y = "PercentDocs", 
                                               fill = "DocumentedElement")) + 
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))
      
      plotly::ggplotly(p, source = "subset", key = key) %>% 
        plotly::layout(dragmode = "select")
    })
    
    output$endoscopyUse_EndoscopyUseBarr <- plotly::renderPlotly({
      
      # Create the grouped table here of the number of endoscopies done by day
      # Then perform as per below
      
      dtData <- barretts_data() %>% 
        dplyr::group_by(!!rlang::sym(map_terms()$Map_EndoscopyDateIn)) %>% 
        dplyr::summarise(n = dplyr::n())
      
      # Get rid of NA's as they mess things up.
      dtData <- na.omit(data.table::as.data.table(dtData))
      
      p1 = ggTimeSeries::ggplot_calendar_heatmap(
        dtData,
        map_terms()$Map_EndoscopyDateIn,
        'n'
      )
      
      # adding some formatting
      p1 + 
        ggplot2::xlab('') + 
        ggplot2::ylab('') + 
        ggplot2::scale_fill_continuous(low = 'green', high = 'red') + 
        ggplot2::facet_wrap(~ Year, ncol = 1)
    })
    
    output$plotBarrTSA <- plotly::renderPlotly({
      
      cat(str(map_terms()$Map_EndoscopyDateIn))
      
      Endo_ResultPerformeda <- rlang::sym(map_terms()$Map_EndoscopyDateIn)
      
      TestNumbers <- barretts_data() %>% 
        dplyr::group_by(!!rlang::sym(map_terms()$Map_EventsIn)) %>% 
        dplyr::arrange(as.Date(!!Endo_ResultPerformeda)) %>% 
        dplyr::group_by(
          week = lubridate::week(as.Date(!!Endo_ResultPerformeda)),
          month = lubridate::month(as.Date(!!Endo_ResultPerformeda)),
          year = lubridate::year(as.Date(!!Endo_ResultPerformeda))
        ) %>%
        dplyr::summarise(Number = dplyr::n())
      
      names(TestNumbers) <- c("week", "month", "year", "freq")
      
      TestNumbers$DayMonth <- paste("01_", 
                                    TestNumbers$month, "_", 
                                    TestNumbers$year, sep = "")
      
      TestNumbers$DayMonth <- lubridate::dmy(TestNumbers$DayMonth)
      
      ggplot2::ggplot(data = TestNumbers, ggplot2::aes(x = week, y = freq)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_smooth(method = "loess") 
    })
    
    Barr_DDR_data <- reactive({
      
      DDRTable <- barretts_data() %>%
        dplyr::group_by(!!rlang::sym(map_terms()$Map_EndoscopistIn),
                        barretts_data()$IMorNoIM) %>%
        dplyr::summarise(n = dplyr::n())
    })
    
    output$BarrDDR_Table = DT::renderDT({
      
      Barr_DDR_data() %>%
        tidyr::spread(2, n)
    },
    
    filter = 'top', 
    selection = list(target = 'row'),
    extensions = 'Buttons', 
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      pageLength = 50,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis'))
    )
    
    drilldataBarrd <- reactive({
      
      shiny::validate(
        need(length(input$BarrDDR_Table_rows_selected) > 0, "Select rows to drill down!")
      )
      
      selected_species <- Barr_DDR_data()[input$BarrDDR_Table_rows_selected, ]
      variables <- c(t(selected_species[, 1]))
      mycolname <- colnames(selected_species)[1]
      df <- barretts_data()[barretts_data()[, mycolname] %in% variables, ]
      
      df %>%
        dplyr::select(map_terms()$Map_HospitalNumberIn, map_terms()$Map_EndoscopyDateIn, 
                      map_terms()$Map_FindingsIn, map_terms()$Map_MicroscopicTextIn, 
                      CStage, MStage, IMorNoIM, FU_Type, TimeToNext, 
                      contains("url"))
    })
    
    output$drilldownBarr <- DT::renderDT({
      
      drilldataBarrdf <- drilldataBarrd()
      
      drilldataBarrdf$Actions <- sapply(1 : nrow(drilldataBarrdf), buttonHTML)
      
      drilldataBarrdf
    })
  })
}