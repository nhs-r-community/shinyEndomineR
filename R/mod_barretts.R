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
    
    tabsetPanel(
      tabPanel("Plots",
               splitLayout(
                 cellArgs = list(style = "padding: 6px"),
                 plotly::plotlyOutput(ns("plotBarrQM")),
                 plotly::plotlyOutput(ns("plotBarrEQ"))
               )
      ),
      tabPanel("Time series",
               splitLayout(
                 cellArgs = list(style = "padding: 6px"),
                 plotOutput(ns("endoscopyUse_EndoscopyUseBarr")),
                 plotly::plotlyOutput(ns("plotBarrTSA"))
               )
      ),
      tabPanel("Tables",
               fluidRow(
                 splitLayout(
                   cellArgs = list(style = "padding: 6px"),
                   DT::DTOutput(ns("BarrDDR_Table")),
                   DT::DTOutput(ns("drilldownBarr"))
                 )
               )
      ),
      tabPanel("Visualise",
               fluidRow(
                 tags$div(
                   style = "height: 700px;", # needs to be in fixed height container
                   esquisserUI(
                     id = ns("esquisseBarr"),
                     header = FALSE, 
                     choose_data = FALSE 
                   )
                 )
               ),
               fluidRow(
                 rpivotTable::rpivotTableOutput(ns("BarrPivot"))
               )
      ),
      tabPanel("Theograph",
               plotOutput(ns("plotBarrPT"))
      )
    )
  )
}

#' barretts Server Functions
#'
#' @noRd 
mod_barretts_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    barretts_data <- reactive({
      
      req(r$map_terms$Map_MacroscopicTextDelimIn)
      
      barretts_data <- 
        r$merge_data[Reduce(`|`, 
                            lapply(r$merge_data, 
                                   grepl, 
                                   pattern = "columnar.*?lined.*?\\.|barrett")), ]
      
      barretts_data <- EndoMineR::Barretts_PragueScore(barretts_data, 
                                                       r$map_terms$Map_FindingsIn, 
                                                       r$map_terms$Map_Findings2In)
      
      barretts_data$mytext <- NULL
      barretts_data$MStage <- as.numeric(barretts_data$MStage)
      barretts_data$CStage <- as.numeric(barretts_data$CStage)
      barretts_data$IMorNoIM <- 
        EndoMineR::Barretts_PathStage(barretts_data, 
                                      r$map_terms$Map_MicroscopicTextIn)
      # note that the strings in the following line are not names of the merged dataset, 
      # they are fixed
      
      barretts_data$FU_Type <- EndoMineR::Barretts_FUType(barretts_data, 
                                                          "CStage", "MStage", "IMorNoIM")
      
      EndoMineR::SurveilTimeByRow(barretts_data, 
                                  r$map_terms$Map_HospitalNumberIn,
                                  r$map_terms$Map_EndoscopyDateIn)
      
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
      
      Hiatus <- r$merge_data %>%
        dplyr::group_by(!! rlang::sym(r$map_terms$Map_EndoscopistIn)) %>%
        dplyr::summarise(Hiatus = (sum(
          grepl("[Hh]iatus|[Ii]sland",
                !!rlang::sym(r$map_terms$Map_FindingsIn))) / dplyr::n()) * 100)
      Island <- r$merge_data %>%
        dplyr::group_by(!! rlang::sym(r$map_terms$Map_EndoscopistIn)) %>%
        dplyr::summarise(Island = (sum(
          grepl("[Ii]sland",
                !!rlang::sym(r$map_terms$Map_FindingsIn))) / dplyr::n()) * 100)
      Pinch <- r$merge_data %>%
        dplyr::group_by(!! rlang::sym(r$map_terms$Map_EndoscopistIn)) %>%
        dplyr::summarise(Pinch = (sum(
          grepl("[Pp]inch",
                !!rlang::sym(r$map_terms$Map_FindingsIn))) / dplyr::n()) * 100)
      Lesion <- r$merge_data %>%
        dplyr::group_by(!! rlang::sym(r$map_terms$Map_EndoscopistIn)) %>%
        dplyr::summarise(Lesion = (sum(
          grepl("esion|odule|lcer",
                !!rlang::sym(r$map_terms$Map_FindingsIn))) / dplyr::n()) * 100)
      
      FinalTable <- dplyr::full_join(Hiatus, Island, by = r$map_terms$Map_EndoscopistIn)
      FinalTable <- dplyr::full_join(FinalTable, Pinch, by = r$map_terms$Map_EndoscopistIn)
      FinalTable <- dplyr::full_join(FinalTable, Lesion, by = r$map_terms$Map_EndoscopistIn)
      
      FinalTable <- data.frame(FinalTable)
      
      #Need to gather the table to make tidy for ggplot
      
      FinalTable <- tidyr::gather(FinalTable,
                                  key = "DocumentedElement",
                                  value = "PercentDocs",
                                  -!!rlang::sym(r$map_terms$Map_EndoscopistIn))
      
      key <- r$map_terms$Map_EndoscopistIn
      
      p <- ggplot2::ggplot(FinalTable,
                           ggplot2::aes_string(x = key, y = "PercentDocs",
                                               fill = "DocumentedElement")) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))
      
      plotly::ggplotly(p, source = "subset", key = key) %>%
        plotly::layout(dragmode = "select")
    })
    
    output$endoscopyUse_EndoscopyUseBarr <- renderPlot({
      
      # Create the grouped table here of the number of endoscopies done by day
      # Then perform as per below
      
      barretts_data() |> 
        calendar_heatmap(r$map_terms$Map_EndoscopyDateIn)
      
    })
    
    output$plotBarrTSA <- plotly::renderPlotly({
      
      Endo_ResultPerformeda <- rlang::sym(r$map_terms$Map_EndoscopyDateIn)
      
      TestNumbers <- barretts_data() %>%
        dplyr::group_by(!!rlang::sym(r$map_terms$Map_EventsIn)) %>%
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
      
      ggplot2::ggplot(data = TestNumbers, 
                      ggplot2::aes(x = DayMonth, y = freq)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::geom_smooth(method = "loess")
    })
    
    Barr_DDR_data <- reactive({
      
      DDRTable <- barretts_data() %>%
        dplyr::group_by(!!rlang::sym(r$map_terms$Map_EndoscopistIn),
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
        need(length(input$BarrDDR_Table_rows_selected) > 0, 
             "Select rows to drill down!")
      )
      
      selected_species <- Barr_DDR_data()[input$BarrDDR_Table_rows_selected, ]
      variables <- c(t(selected_species[, 1]))
      mycolname <- colnames(selected_species)[1]
      df <- barretts_data()[barretts_data()[, mycolname] %in% variables, ]
      
      df %>%
        dplyr::select(r$map_terms$Map_HospitalNumberIn, 
                      r$map_terms$Map_EndoscopyDateIn,
                      r$map_terms$Map_FindingsIn, 
                      r$map_terms$Map_MicroscopicTextIn,
                      CStage, MStage, IMorNoIM, FU_Type, TimeToNext,
                      contains("url"))
    })
    
    output$drilldownBarr <- DT::renderDT({
      
      drilldataBarrdf <- drilldataBarrd()
      
      drilldataBarrdf$Actions <- sapply(1 : nrow(drilldataBarrdf), buttonHTML)
      
      drilldataBarrdf
    })
    
    output$BarrettsTable = DT::renderDT({
      
      DT::datatable(
        barretts_data(),
        escape = FALSE,
        extensions = c("Select","Buttons"),
        selection = "none",
        callback = DT::JS(readLines("inst/app/www/custom_dt.js")),
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          pageLength = 200,
          select = "api",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))
      )
    })
    
    barr_trim <- reactive({
      
      barretts_data()[input$BarrettsTable_rows_all, input$BarrettsTable_columns_selected]
    })
    
    output$BarrPivot <- rpivotTable::renderRpivotTable({
      
      validate(
        need(is.data.frame(barr_trim()), "Select two columns")
      )
      
      rpivotTable::rpivotTable(barr_trim())
    })
    
    data_r <- reactiveValues(data = data.frame(), name = "barretts")
    
    observe({
      
      req(r$map_terms$Map_HospitalNumberIn)
      req(is.data.frame(barr_trim()))
      
      data_r$data <- barr_trim()
    })
    
    callModule(module = esquisserServer, id = "esquisseBarr", data = data_r)
    
    output$plotBarrPT <- renderPlot({
      
      # ATTN this output does not work
      
      # Create a column with factors for the worst grade
      
      df <- barretts_data()
      
      df$RecodedColumn <- as.integer(
        factor(df$IMorNoIM,
               c("No_IM","IM","LGD","HGD","T1a","IGD","SM1","SM2"),
               ordered = TRUE)
      )
      
      # Only select patients where there is more than one endoscopy:
      bb <- df %>%
        dplyr::filter(!is.na(RecodedColumn)) |> 
        dplyr::group_by(!!rlang::sym(r$map_terms$Map_HospitalNumberIn)) %>%
        dplyr::filter(dplyr::n() > 1)
      
      # Now develop the patient specific journey with faceted plot in ggplot2
      ggplot2::ggplot(bb) +
        ggplot2::geom_line(ggplot2::aes(r$map_terms$Map_EndoscopyDateIn, RecodedColumn),
                           shape = 11, size = 1) +
        ggplot2::geom_point(ggplot2::aes(r$map_terms$Map_EndoscopyDateIn, RecodedColumn),
                            shape = 11, colour = "red", size = 1) +
        ggplot2::xlab("Date") +
        ggplot2::ylab("Histopathological State") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
        ggplot2::facet_grid(r$map_terms$Map_HospitalNumberIn)
    })
    
    # return barrett's data to send to per_endoscopist mod
    
    reactive({
      barretts_data()
    })
  })
}
