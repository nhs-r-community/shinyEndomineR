#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  
  header <- dashboardHeader(title = "Shiny Endominer")
  
  sidebar <- dashboardSidebar(
    uiOutput("interfaceUI")
  )
  
  body <- dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "endoData",
              mod_clean_and_merge_ui("clean_and_merge_ui_1")
      ),
      
      # Second tab content
      tabItem(tabName = "pathData",
              mod_clean_and_merge_ui("clean_and_merge_ui_2")
      ),
      
      tabItem(tabName = "mergeData",
              mod_merge_data_ui("merge_data_ui_1")
      ),
      
      tabItem(tabName = "mapTerms",
              mod_map_terms_ui("map_terms_ui_1")
      ),
      
      tabItem(tabName = "barretts",
              mod_barretts_ui("barretts_ui_1")
      ),
      tabItem(tabName = "polyps",
              mod_polyps_ui("polyps_ui_1")
      ),
      tabItem(tabName = "endoscopist",
              mod_per_endoscopist_ui("per_endoscopist_ui_1")
      ),
      tabItem(tabName = "custom",
              mod_custom_ui("custom_ui_1")
      )
    )
  )
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    dashboardPage(header, sidebar, body)
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinyEndomineR'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

