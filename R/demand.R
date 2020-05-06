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
                      "Demand Setting",
                      choices = all,
                      selected = all[1])
}

widget.supply <- function(est_ccs) {

  all <- est_ccs %>% 
    dplyr::distinct(supply) %>% 
    dplyr::pull(supply)

  shiny::radioButtons("supplyRadio",
                      "Supply Setting",
                      choices = all,
                      selected = all[1])
}

demand.ui <- function(wfb, est_css) {

  shiny::fluidRow(
    shiny::fluidRow(
      shiny::column(width = 2,
                    widget.demand(est_ccs),
                    widget.supply(est_ccs),
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
      shiny::column(width = 12, 
                    DT::dataTableOutput("estimate_table", width = "90%"))
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
    map_cbsa(df = tx_counties_df())
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
