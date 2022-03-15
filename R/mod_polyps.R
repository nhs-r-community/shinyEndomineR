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
    
    tabsetPanel(
      tabPanel("Plots",
               splitLayout(
                 cellArgs = list(style = "padding: 6px"),
                 plotly::plotlyOutput(ns("plotPolypEQ")),
                 plotly::plotlyOutput(ns("endoscopyUse_EndoscopyUsePolyp"))
               )
      ),
      tabPanel("Tables",
               fluidRow(
                 splitLayout(
                   cellArgs = list(style = "padding: 6px"),
                   DT::DTOutput(ns("grs_table")),
                   DT::DTOutput(ns("drilldown"))
                 )
               ),
               fluidRow(
                 
               )
      ),
      tabPanel("Visualise",
        fluidRow(
          tags$div(
            style = "height: 700px;", # needs to be in fixed height container
            esquisserUI(
              id = ns("esquissePolyp"),
              header = FALSE, 
              choose_data = FALSE 
            )
          )),
        fluidRow(
          splitLayout(
            DT::dataTableOutput(ns("polypTable")),
            rpivotTable::rpivotTableOutput(ns("OverallPivotPolyp"))
          )
        )
      ),
      tabPanel("Theograph", 
               # ATTN no code for this?
               plotOutput(ns("plotPolypPF"))
      )
    )
  )
}

#' polyps Server Functions
#'
#' @noRd 

mod_polyps_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    polyp_data <- reactive({
      
      dataset <- merge_data()
      
      dataset[, r$map_terms$Map_EndoscopistIn] <- EndoMineR::EndoscEndoscopist(
        dataset[, r$map_terms$Map_EndoscopistIn])
      
      #Polyp Processing:
      ForGRS <- dataset[grepl("colonoscopy", 
                              dataset[, r$map_terms$Map_ProcedurePerformedIn]), ]
      
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
      
      ForGRS <- EndoMineR::GRS_Type_Assess_By_Unit(
        ForGRS, 
        r$map_terms$Map_ProcedurePerformedIn,
        r$map_terms$Map_EndoscopistIn,
        r$map_terms$Map_MacroscopicTextIn,
        r$map_terms$Map_MicroscopicTextIn
      )
      
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
        -!!rlang::sym(r$map_terms$Map_EndoscopistIn))
      
      # Get rid of the overall number figure (=n)
      MyPolypTable <- MyPolypTable %>%
        dplyr::filter(!grepl("^n$", DocumentedElement))
      
      key <- r$map_terms$Map_EndoscopistIn
      
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
        dplyr::group_by(!!rlang::sym(r$map_terms$Map_EndoscopyDateIn)) %>% 
        dplyr::summarise(n = dplyr::n())
      
      # Get rid of NA's as they mess things up.
      
      dtData <- na.omit(data.table::as.data.table(dtData))
      
      p1 = ggTimeSeries::ggplot_calendar_heatmap(
        dtData,
        r$map_terms$Map_EndoscopyDateIn,
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
        dplyr::select(r$map_terms$Map_HospitalNumberIn, 
                      r$map_terms$Map_EndoscopyDateIn,
                      r$map_terms$Map_FindingsIn, 
                      r$map_terms$Map_MicroscopicTextIn, 
                      dplyr::contains("url"))
    })
    
    output$drilldown <- DT::renderDT({
      
      DT::datatable(
        drilldataPolyp(),
        escape = FALSE, 
        extensions = c("Select", "Buttons"), 
        selection = "none",
        callback = DT::JS(readLines("inst/app/www/custom_dt.js")),
        options = list(
          columnDefs = list(
            list(targets = as.numeric(
              which(names(drilldataPolyp()) == 
                      names(drilldataPolyp()[r$map_terms$Map_EndoscopyDateIn]))
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
    
    output$polypTable = DT::renderDT({
      
      # Create a copy that can be independently edited for the polyp table
      
      DT::datatable(
        reduce_polyp(),
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
    
    polyp_trim <- reactive({
      
      reduce_polyp()[input$polypTable_rows_all, input$polypTable_columns_selected]
    })
    
    output$OverallPivotPolyp <- rpivotTable::renderRpivotTable({
      
      validate(
        need(is.data.frame(polyp_trim()), "Select two columns")
      )
      
      rpivotTable::rpivotTable(polyp_trim())
    })
    
    data_r <- reactiveValues(data = data.frame(), name = "polyp")
    
    observe({
      
      req(r$map_terms$Map_HospitalNumberIn)
      req(is.data.frame(polyp_trim()))
      
      data_r$data <- polyp_trim()
    })
    
    callModule(module = esquisserServer, 
               id = "esquissePolyp", data = data_r)
    
    # return polyp data for the per endoscopist view
    
    reactive({ 
      
      polyp_data()
    })
  })
}
