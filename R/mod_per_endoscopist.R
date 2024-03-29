#' per_endoscopist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_per_endoscopist_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      uiOutput(ns("endoscopistUI"))
    ),
    fluidRow(
      splitLayout(
        plotly::plotlyOutput(ns("IndicsVsBiopsies")),
        DT::dataTableOutput(ns("performanceTable"))
      )
    ),
    fluidRow(
      uiOutput(ns("endoscopistUI_2"))
    ),
    fluidRow(
      splitLayout(
        plotly::plotlyOutput(ns("GRS_perEndoscopistPlot")),
        plotly::plotlyOutput(ns("plotBarrQM_Perform"))
      )
    )
  )
}

#' per_endoscopist Server Functions
#'
#' @noRd 
mod_per_endoscopist_server <- function(id, barretts_data, polyp_data, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    biopsy_data <- reactive({
      
      biopsy_data <- r$merge_data
      
      biopsy_data$indicationsforexamination <- 
        EndoMineR::ColumnCleanUp(biopsy_data[, r$map_terms$Map_IndicationsIn])
      
      myBx_df <- tidyr::separate_rows(biopsy_data, r$map_terms$Map_IndicationsIn, 
                                      sep = ",", convert = FALSE)
      
      myBx_df$indicationsforexamination <- gsub("^\\.", "", myBx_df$indicationsforexamination)
      
      # Then get average per indication for that endoscopist
      # ATTN this function returns all 0s
      
      myBx_df$NumBx <- EndoMineR::HistolNumbOfBx(
        myBx_df[[r$map_terms$Map_MacroscopicTextIn]], 
        r$map_terms$Map_MacroscopicTextDelimIn)
      
      myBx_df
      
    })
    
    output$endoscopistUI <- renderUI({
      
      endo_choices <- biopsy_data() |> 
        dplyr::filter(!is.na(NumBx),
                      NumBx > 0) |> 
        dplyr::pull(!!rlang::sym(r$map_terms$Map_EndoscopistIn)) |> 
        unique()
      
      selectInput(session$ns("EndoscopistChooserIn"), 
                  label = h4("Choose the endscopist to show the results for"),
                  choices = endo_choices,
                  selected = 1)
    })
    
    output$endoscopistUI_2 <- renderUI({
      
      endo_choices <- polyp_data() |> 
        dplyr::pull(!!rlang::sym(r$map_terms$Map_EndoscopistIn)) |> 
        unique()
      
      selectInput(session$ns("EndoscopistChooserIn_2"), 
                  label = h4("Choose the endscopist to show the results for"),
                  choices = endo_choices,
                  selected = 1)
    })
    
    performance_data <- reactive({
      
      req(r$map_terms$Map_HospitalNumberIn)
      
      perf_data <- data.frame(r$merge_data[, r$map_terms$Map_HospitalNumberIn],
                              r$merge_data[, r$map_terms$Map_EndoscopistIn],
                              r$merge_data[, r$map_terms$Map_FindingsIn],
                              r$merge_data[, r$map_terms$Map_MicroscopicTextIn])
      
      names(perf_data) <- c(r$map_terms$Map_HospitalNumberIn, 
                            r$map_terms$Map_EndoscopistIn,
                            r$map_terms$Map_FindingsIn,
                            r$map_terms$Map_MicroscopicTextIn)
      
      if(!is.null(input$EndoscopistChooserIn)){
        
        perf_data <- perf_data %>%
          dplyr::filter(.data[[r$map_terms$Map_EndoscopistIn]] == input$EndoscopistChooserIn) %>%
          dplyr::select(r$map_terms$Map_HospitalNumberIn,
                        r$map_terms$Map_FindingsIn,
                        r$map_terms$Map_MicroscopicTextIn)
      }
      return(perf_data)
    })
    
    output$performanceTable = DT::renderDT({
      
      DT::datatable(
        performance_data(),
        escape = F, 
        extensions = c("Select","Buttons"), 
        selection = "none",
        callback = DT::JS(readLines("inst/app/www/custom_dt.js")),
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          pageLength = 5,
          select = "api",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis'))
      )
    })
    
    output$IndicsVsBiopsies <- plotly::renderPlotly({
      
      cc <- biopsy_data() %>% 
        dplyr::filter(.data[[r$map_terms$Map_EndoscopistIn]] == input$EndoscopistChooserIn) %>%
        dplyr::group_by(!!rlang::sym(r$map_terms$Map_IndicationsIn)) %>%
        dplyr::summarise(endoscopist_Mean = mean(NumBx, na.rm = TRUE))%>%
        dplyr::filter(endoscopist_Mean > 0, 
                      !is.na(!!rlang::sym(r$map_terms$Map_IndicationsIn)))
      
      names(cc) <- c("Indications", "endoscopist_Mean")
      #Now need to get the average for all the endoscopists
      
      cd <- biopsy_data() %>% 
        dplyr::group_by(!!rlang::sym(r$map_terms$Map_IndicationsIn)) %>%
        dplyr::summarise(all_Mean = mean(NumBx, na.rm = T))%>%
        dplyr::filter(all_Mean > 0, !is.na(!!rlang::sym(r$map_terms$Map_IndicationsIn)))
      
      names(cd) <- c("Indications", "all_Mean")
      
      #Now merge 
      biopsies <- merge(cd, cc, by = 1)
      
      IndicBiopsy <- ggplot2::ggplot(biopsies, 
                                     ggplot2::aes(x = Indications, y = endoscopist_Mean)) +
        ggplot2::geom_bar(stat = "identity")+
        ggplot2::coord_flip()
      
      plotly::ggplotly(IndicBiopsy, source = "subset", key = key) %>% 
        plotly::layout(dragmode = "select")
    })
    
    output$plotBarrQM_Perform <- plotly::renderPlotly({
      
      key <- r$map_terms$Map_EndoscopistIn
      
      p <- barretts_data() %>% 
        dplyr::filter(base::get(r$map_terms$Map_EndoscopistIn) == input$EndoscopistChooserIn)
      
      q <- p %>%
        ggplot2::ggplot() + 
        ggplot2::aes_string(x = "IMorNoIM", fill = r$map_terms$Map_EndoscopistIn) + 
        ggplot2::geom_histogram(stat = "count")
      
      plotly::ggplotly(q, source = "subset", key = key) %>% 
        plotly::layout(dragmode = "select")
    })
    
    GRS_perEndoscopist_TablePrep <- reactive({
      
      save_data <- polyp_data()
      
      save(save_data, file = "test.rda")
      
      polyp_data() %>% 
        dplyr::filter(.data[[r$map_terms$Map_EndoscopistIn]] == input$EndoscopistChooserIn_2)
    })
    
    output$GRS_perEndoscopistPlot = plotly::renderPlotly({
      
      # ATTN following df is empty after filtering above
      # not sure if problem with code or data
      
      MyPolypTable <- tidyr::gather(GRS_perEndoscopist_TablePrep(),
                                    key = "DocumentedElement",
                                    value = "percentage",
                                    -!!rlang::sym(r$map_terms$Map_EndoscopistIn))
      
      key <- r$map_terms$Map_EndoscopistIn
      
      lk <- ggplot2::ggplot(MyPolypTable, 
                            ggplot2::aes_string(x = "DocumentedElement",  y = "percentage")) + 
        ggplot2::geom_bar(stat = "identity", position = "dodge") + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45))
      
      plotly::ggplotly(lk, source = "subset", key = key)
    })
  })
}
