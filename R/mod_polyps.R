#' polyps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_polyps_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(5,
             plotly::plotlyOutput(ns("plotPolypEQ")),
             plotly::plotlyOutput(ns("endoscopyUse_EndoscopyUsePolyp"))
      ),
      column(7,
             DT::DTOutput(ns("grs_table")),
             DT::DTOutput(ns("drilldown"))
      )
    )
  )
}

#' polyps Server Functions
#'
#' @noRd 
mod_polyps_server <- function(id, merge_data, map_terms){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    polyp_data <- reactive({
      
      dataset <- merge_data()
      
      dataset[, map_terms()$Map_EndoscopistIn] <- EndoMineR::EndoscEndoscopist(
        dataset[, map_terms()$Map_EndoscopistIn])
      
      #Polyp Processing:
      ForGRS <- dataset[grepl("colonoscopy", 
                              dataset[, map_terms()$Map_ProcedurePerformedIn]), ]
      
      #Need to get rid of duplicate entries because of reporting colons and OGDs
      # on the same report:
      
      if("Select" %in% colnames(ForGRS)){
        # Get rid of the Select and Actions columns which create a unique row unnecessarily:
        
        ForGRS <- ForGRS %>%
          select(-Select,-Actions)
        
        ForGRS <- ForGRS %>%
          select(-contains(".y"))
        
        ForGRS <- unique(ForGRS)
      }
      
      ForGRS <- EndoMineR::GRS_Type_Assess_By_Unit(ForGRS, 
                                                   map_terms()$Map_ProcedurePerformedIn,
                                                   map_terms()$Map_EndoscopistIn,
                                                   map_terms()$Map_MacroscopicTextIn,
                                                   map_terms()$Map_MicroscopicTextIn)
      
      ForGRS
    })
    
    reduce_polyp <- reactive({
      
      polypdata <- merge_data()[Reduce(`|`, lapply(merge_data(), grepl, pattern = "polyp")),]
      polypdata <- polypdata[Reduce(`|`, lapply(polypdata, grepl, pattern = "colonoscopy")),]
    })
    
    output$plotPolypEQ <- plotly::renderPlotly({
      
      MyPolypTable <- tidyr::gather(
        polyp_data(),
        key = "DocumentedElement",
        value = "percentage",
        -!!rlang::sym(map_terms()$Map_EndoscopistIn))
      
      #Get rid of the overall number figure (=n)
      MyPolypTable <- MyPolypTable %>%
        dplyr::filter(!grepl("^n$", DocumentedElement))
      
      key <- map_terms()$Map_EndoscopistIn
      
      p <- ggplot2::ggplot(MyPolypTable, 
                           ggplot2::aes_string(x = key, y = "percentage", 
                                               fill = "DocumentedElement")) + 
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))
      
      plotly::ggplotly(p, source = "subset", key = key) %>% 
        plotly::layout(dragmode = "select")
    })
    
    output$grs_table <- DT::renderDT({
      
      polyp_data()
    }, filter = 'top', 
    selection = list(target = 'row'), extensions = 'Buttons', 
    options = list(
      scrollX = TRUE,
      scrollY = TRUE,
      pageLength = 50,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))
    )
    
    output$endoscopyUse_EndoscopyUsePolyp <- plotly::renderPlotly({
      
      dtData <- reduce_polyp() %>% 
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
    
    drilldataPolyp <- reactive({
      shiny::validate(
        need(length(input$grs_table_rows_selected) > 0, "Select rows to drill down!")
      )
      
      selected_species <- polyp_data()[input$grs_table_rows_selected, ]
      variables <- c(t(as.character(selected_species[, 1])))
      mycolname <- colnames(selected_species)[1]
      df <- reduce_polyp()[reduce_polyp()[, mycolname] %in% variables, ]
      
      df %>%
        dplyr::select(map_terms()$Map_HospitalNumberIn, 
               map_terms()$Map_EndoscopyDateIn,
               map_terms()$Map_FindingsIn, 
               map_terms()$Map_MicroscopicTextIn, 
               dplyr::contains("url"))
    })

    output$drilldown <- DT::renderDT({
      
      DT::datatable(
        drilldataPolyp(),
        escape = F, 
        extensions = c("Select", "Buttons"), 
        selection = "none",
        callback = DT::JS(readLines("inst/app/www/custom_dt.js")),
        options = list(
          columnDefs = list(
            list(targets = as.numeric(
              which(names(drilldataPolyp()) == names(drilldataPolyp()[map_terms()$Map_EndoscopyDateIn]))
            ), 
            visible = TRUE)),
          fixedHeader = TRUE,
          scrollX = TRUE,
          scrollY = TRUE,
          pageLength = 5,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print','colvis'))
      )
    }, server = FALSE)
  })
}
