#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = "Shiny Endominer"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Endoscopy data", tabName = "endoData", icon = icon("user-md")),
          menuItem("Pathology data", tabName = "pathData", icon = icon("microscope")),
          menuItem("Merge data", tabName = "mergeData", icon = icon("object-group")),
          menuItem("Map terms", tabName = "mapTerms", icon = icon("map")),
          menuItem("Barrett's", tabName = "barretts", icon = icon("chart-bar"))
        )
      ),
      dashboardBody(
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
          )
        )
      )
    )
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

