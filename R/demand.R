widget.wfb <- function(wfb) {

  all <- wfb %>% 
    dplyr::distinct(workforce_board) %>% 
    dplyr::pull(workforce_board)
  
  shinyWidgets::pickerInput(
    inputId = "wfbPicker",
    label = "Workforce Board", 
    choices = all,
    multiple = TRUE,
    selected = all,
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3",
      `live-search` = TRUE
    )
  )
}

widget.demand <- function(estimates) {

  all <- estimates %>% 
    dplyr::distinct(demand) %>% 
    dplyr::pull(demand)
  
  shiny::radioButtons("demandRadio",
                      "Demand Setting",
                      choices = all,
                      selected = all[1])
}

widget.supply <- function(estimates) {

  all <- estimates %>% 
    dplyr::distinct(supply) %>% 
    dplyr::pull(supply)

  shiny::radioButtons("supplyRadio",
                      "Supply Setting",
                      choices = all,
                      selected = all[1])
}

demand.ui <- function(wfb, estimates) {

  shiny::fluidRow(
    shiny::fluidRow(
      shiny::column(width = 2,
                    widget.demand(estimates),
                    widget.supply(estimates),
                    widget.wfb(wfb)
                    ),
      shiny::column(width = 8
                    ,ggiraph::girafeOutput("demand_map")
      ),
      shiny::column(width =2,
      shiny::fluidRow(
                   shinydashboard::valueBox(value = "", subtitle = "High estimate", color = "red", width = 12)
                  ,shinydashboard::valueBox(value = "", subtitle = "Medium estimate", color = "red", width = 12)
                  ,shinydashboard::valueBox(value = "", subtitle = "Conservative estimate", color = "red", width = 12)
      )
      )
    ),
  
    shiny::fluidRow(
      shiny::textOutput("low_supply")
    )
  )
}

demand.server <- function(input, output, session) {

  tx_counties_df <- shiny::reactive({

    tx_counties %>%
      dplyr::filter(workforce_board %in% input$wfbPicker) %>% 
      dplyr::left_join(estimates %>% 
                         dplyr::mutate(county = gsub(" County", "", county)) %>% 
                         dplyr::filter(demand %in% input$demandRadio) %>% 
                         dplyr::filter(supply %in% input$supplyRadio))
  })

  est <- shiny::reactive({
    sum_est <- estimates %>%
      dplyr::group_by(county, fill) %>% 
      dplyr::summarize(n = dplyr::n()) %>% 
      dplyr::filter(fill == "< 15" & n == 6)
  })
  
  output$demand_map <- ggiraph::renderGirafe({
    map_cbsa(df = tx_counties_df())
  })

}
