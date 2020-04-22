widget.cbsa <- function(demand) {

  shinyWidgets::pickerInput(
    inputId = "CBSA",
    label = "Area", 
    choices = c("All", demand %>% 
                  dplyr::distinct(CBSA) %>%
                  dplyr::pull(CBSA)),
    multiple = FALSE,
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3",
      `live-search` = TRUE
    )
  )
}

widget.employment_sector <- function(demand) {
  
  radioButtons(inputId = "employment_sector",
               label = "Employment sector",
               choices = demand %>% 
                     dplyr::distinct(employment_sector) %>%
                     dplyr::pull(employment_sector), 
               selected = "All Industries",
               inline = FALSE, width = NULL)
}

widget.other_child_care_provider <- function(demand) {

  radioButtons(inputId = "other_child_care_provider",
               label = "Other Child Care provider",
               choices = demand %>% 
                 dplyr::distinct(other_child_care_provider) %>%
                 dplyr::pull(other_child_care_provider), 
               selected = "All",
               inline = FALSE, width = NULL)
}

demand.ui <- function(demand) {

  shiny::fluidRow(
    shiny::column(width = 2,
                  widget.cbsa(demand),
                  widget.employment_sector(demand),
                  widget.other_child_care_provider(demand)
    ),
    shiny::column(width = 7
                  ,ggiraph::girafeOutput("demand_map")
    ),
    shiny::column(width = 3)
  )
}

demand.server <- function(input, output, session) {

  demand_sub <- shiny::reactive({

    demand %>%
      dplyr::filter(employment_sector %in% input$employment_sector) %>%
      dplyr::filter(other_child_care_provider %in% input$other_child_care_provider)
  })

  tx_counties_df <- shiny::reactive({
 
    df <- tx_counties %>% 
      dplyr::left_join(demand_sub()) %>%
      dplyr::mutate(demand = all_children)
    
    if(input$CBSA != "All") {
      df %>% 
        dplyr::filter(CBSA %in% input$CBSA)
    } else {
      df
    }
  })

  output$demand_map <- ggiraph::renderGirafe({

    map_cbsa(df = tx_counties_df())
  })

}