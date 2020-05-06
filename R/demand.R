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

widget.demand <- function(est_ccs) {

  all <- est_ccs %>% 
    dplyr::distinct(demand) %>% 
    dplyr::pull(demand)
  
  shiny::radioButtons("demandRadio",
                      label = NULL,
                      choices = all,
                      selected = all[1])
}

widget.supply <- function(est_ccs) {

  all <- est_ccs %>% 
    dplyr::distinct(supply) %>% 
    dplyr::pull(supply)

  shiny::radioButtons("supplyRadio",
                      label = NULL,
                      choices = all,
                      selected = all[1])
}

widget.overlay_covid <- function() {
  shinyWidgets::switchInput(
    inputId = "show_covid"
  )
}

widget.covid_metric <- function() {
  shiny::radioButtons("covid_metric",
                      label = NULL,
                      choices = c("Confirmed cases", "Deaths"),
                      selected = "Confirmed cases")
}

demand.ui <- function(wfb, est_css) {

  shiny::fluidRow(
    shiny::fluidRow(
      shiny::column(width = 3,
                    widget.wfb(wfb),
                    shiny::tags$div(
                      class = "est-container",
                      shiny::h3("Childcare"),
                      shiny::h4("Demand estimated by"),
                      widget.demand(est_ccs),
                      shiny::h4("Supply scenarios"),
                      widget.supply(est_ccs)
                    ),
                    shiny::tags$div(
                      class = "covid-container",
                      shiny::h3("Overlay COVID-19 Metrics"),
                      shiny::h4("On/Off"),
                      widget.overlay_covid(),
                      shiny::h4("Metric"),
                      widget.covid_metric()
                    )
                    ),
      shiny::column(width = 9
                    ,fluidRow(ggiraph::girafeOutput("demand_map"))
                    ,fluidRow(DT::dataTableOutput("estimate_table", width = "85%"))

      )
    )
  )
}

demand.server <- function(input, output, session) {

  est_supply <- shiny::reactive({
    est_s %>% 
      dplyr::filter(variable %in% input$supplyRadio) %>% 
      dplyr::filter(workforce_board %in% input$wfbPicker) %>% 
      dplyr::rename(Supply = value)
  })
  
  est_demand <- shiny::reactive({
    est_d %>% 
      dplyr::filter(variable %in% input$demandRadio) %>% 
      dplyr::filter(workforce_board %in% input$wfbPicker) %>% 
      dplyr::rename(Demand = value)
  })
  
  covid_df <- shiny::reactive({
    covid %>% 
      dplyr::filter(variable %in% input$covid_metric) %>% 
      dplyr::filter(workforce_board %in% input$wfbPicker) %>% 
      dplyr::rename(`Total #` = value,
                    county = County) %>% 
      dplyr::left_join(tx_counties %>% 
                         dplyr::group_by(county) %>% 
                         dplyr::summarise(long = mean(long, na.rm = TRUE),
                                          lat = mean(lat, na.rm = TRUE),
                                          group = mean(group),
                                          subregion = unique(subregion)))
  })
  
  est_ccs_df <- shiny::reactive({
    est_ccs %>% 
      dplyr::filter(demand %in% input$demandRadio) %>% 
      dplyr::filter(supply %in% input$supplyRadio) %>% 
      dplyr::filter(workforce_board %in% input$wfbPicker)
  })

  tx_counties_df <- shiny::reactive({
    tx_counties %>%
      dplyr::filter(workforce_board %in% input$wfbPicker) %>% 
      dplyr::left_join(est_ccs_df() %>% 
                         dplyr::mutate(county = gsub(" County", "", county))
                       )

    })
  
  output$demand_map <- ggiraph::renderGirafe({

    map_cbsa(df = tx_counties_df(),
             covid_df = covid_df(),
             show_covid = input$show_covid)
  })

  table <- shiny::reactive({

    est_ccs_df() %>%
      dplyr::left_join(est_demand(), by = c("county", "workforce_board")) %>% 
      dplyr::left_join(est_supply(), by = c("county", "workforce_board")) %>% 
      dplyr::mutate(Demand = format(round(Demand, 0), nsmall = 0),
                    Supply = format(round(Supply, 0), nsmall = 0),
                    value = format(round(value, 0), nsmall = 0)) %>% 
      dplyr::select(county, Demand, Supply, value) %>% 
      dplyr::arrange(value) %>% 
      dplyr::rename(County = county,
                    `Seats per 100 children` = value)
    
  })
  
  pageLength <- shiny::reactive({
    if (length(input$wfbPicker) == 1) {
      return(nrow(table()))
    } else {
      return(10)
    }
  })
  
  paging <- shiny::reactive({
    if (length(input$wfbPicker) == 1){
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  output$estimate_table <- DT::renderDataTable(

    DT::datatable(table(), 
                  rownames= FALSE,
                  options = list(searching = FALSE,
                                 pageLength = pageLength(),
                                 paging = paging()
                                )
                  )
  )

}
