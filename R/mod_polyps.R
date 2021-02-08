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
             plotly::plotlyOutput(ns("plotPolypEQ"))
      ),
      column(7,
             DT::DTOutput(ns("grs_table")),
             plotly::plotlyOutput(ns("endoscopyUse_EndoscopyUsePolyp"))
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
      ForGRS <- dataset[grepl("colonoscopy", dataset[, map_terms()$Map_ProcedurePerformedIn]), ]
      
      #Need to get rid of duplicate entries because of reporting colons and OGDs on the same report:
      if("Select" %in% colnames(ForGRS)){
        #Get rid of the Select and Actions columns which create a unique row unnecessarily:
        ForGRS <- ForGRS %>%
          select(-Select,-Actions)
        
        ForGRS <- ForGRS %>%
          select(-contains(".y"))
        
        # Get rid of '.y' columns which are named this way to prevent duplicate 
        # named columns when endoscopy and pathology are merged-
        # May need to deal with the column cleaning within EndoMineR itself but 
        # for now keep in the Shiny package.
        
        ForGRS <- unique(ForGRS)
      }
      
      ForGRS <- EndoMineR::GRS_Type_Assess_By_Unit(ForGRS, map_terms()$Map_ProcedurePerformedIn,
                                                   map_terms()$Map_EndoscopistIn,
                                                   map_terms()$Map_MacroscopicTextIn,
                                                   map_terms()$Map_MicroscopicTextIn)
      
      ForGRS
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
      
      p <- ggplot2::ggplot(MyPolypTable, ggplot2::aes_string(x = key, 
                                                             y = "percentage", fill = "DocumentedElement")) + 
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
  })
}
