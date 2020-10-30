widget.wfb <- function(wfb) {
  
  all <- sapply(wfb$wfb_name, function(x, wfb) wfb %>% 
                  dplyr::filter(wfb_name == x) %>%
                  dplyr::pull(wfb_id), 
                simplify = FALSE, 
                USE.NAMES = TRUE, wfb = wfb)

  shinyWidgets::pickerInput(
    inputId = "wfbPicker",
    label = "Workforce Board", 
    choices = all,
    multiple = FALSE,
    selected = all$All[1],
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3",
      `live-search` = TRUE
    )
  )
}

widget.demand_type <- function(est_ccs) {

  all <- list("Occupation (lower bound)" = "Occupation",
              "Industry (upper bound)" = "Industry")

  shiny::radioButtons("demandRadio",
                      label = NULL,
                      choices = all,
                      selected = all$`Occupation (lower bound)`[1])
}

widget.supply_type <- function(est_ccs) {

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

widget.covid_metric <- function(covid) {

  all <- covid %>% 
    dplyr::distinct(covid_metric) %>% 
    dplyr::pull(covid_metric)

  shiny::radioButtons("covid_metric",
                      label = NULL,
                      choices = all,
                      selected = all[1])
}

filter.wfb <- function(df, input) {
  df %>%
    dplyr::filter(wfb_id %in% input$wfbPicker)
}

filter.supply_type <- function(df, input) {
  df %>%
    dplyr::filter(supply %in% input$supplyRadio)
}

filter.demand_type <- function(df, input) {
  df %>%
    dplyr::filter(demand %in% input$demandRadio)
}

filter.covid_metric <- function(df, input) {
  df %>% 
    dplyr::filter(covid_metric %in% input$covid_metric)
}

demand.ui <- function(wfb, est_ccs, covid) {

  shiny::fluidRow(
    shiny::fluidRow(
      shiny::column(width = 2,
                    widget.wfb(wfb),
                    shiny::tags$div(
                      class = "est-container",
                      shiny::h3("Childcare"),
                      shiny::h4("Demand estimated by"),
                      widget.demand_type(est_ccs),
                      shiny::h4("Supply scenarios"),
                      widget.supply_type(est_ccs)
                    ),
                    shiny::tags$div(
                      class = "covid-container",
                      shiny::h3("Overlay DSHS COVID-19 Metrics"),
                      shiny::h4("On/Off"),
                      widget.overlay_covid(),
                      shiny::h4("Metric"),
                      widget.covid_metric(covid)
                    )
                    ),
      shiny::column(width = 10
                    ,fluidRow(ggiraph::girafeOutput("demand_map"))
                    ,fluidRow(DT::dataTableOutput("estimate_table", width = "85%"))

      )
    )
  )
}

demand.server <- function(input, output, session) {

  ccs_map_data <- shiny::reactive({

    df <- filter.supply_type(df = est_ccs, input = input)
    df <- filter.demand_type(df = df, input = input)

    df <- map_tx_counties %>% 
      dplyr::left_join(df) %>% 
      dplyr::left_join(covid %>%
                         tidyr::spread(covid_metric, `Total # (COVID metrics)`) %>% 
                         dplyr::rename(Cases = `Confirmed cases`))

    if(input$wfbPicker != "0") {
      df <- filter.wfb(df = df, input = input)  
    } else {
      df <- df
    }

  })

  covid_map_data <- shiny::reactive({

    df <- filter.covid_metric(df = covid, input = input)
    if(input$wfbPicker != "0") {
      df <- filter.wfb(df = df, input = input)  
    }
    df <- df %>% 
          dplyr::left_join(map_tx_counties %>%
                             dplyr::group_by(county) %>%
                             dplyr::summarise(long = mean(long),
                                              lat = mean(lat),
                                              group = mean(group),
                                              subregion = unique(subregion)))
  })

  table <- shiny::reactive({

    s <- filter.supply_type(df = est_s, input = input)
    d <- filter.demand_type(df = est_d, input = input)
    ccs <- filter.supply_type(df = est_ccs, input = input) 
    ccs <- filter.demand_type(df = ccs, input = input)
    c19 <- covid %>%
      tidyr::spread(covid_metric, `Total # (COVID metrics)`)
  
    df <- s %>%
      dplyr::left_join(d) %>%
      dplyr::left_join(ccs) %>%
      dplyr::left_join(c19)

    if(input$wfbPicker != "0") {
      df <- filter.wfb(df = df, input = input)  
    }

    df <- df %>%
      dplyr::select(county, est_supply, est_demand, est_ccs, `Confirmed cases`, Deaths) %>%
      dplyr::rename(County = county,
                    Supply = est_supply,
                    Demand = est_demand,
                    `Seats per 100 children` = est_ccs) %>% 
      dplyr::mutate(Demand = as.numeric(format(round(Demand, 0), nsmall = 0)),
                    Supply = as.numeric(format(round(Supply, 0), nsmall = 0)),
                    `Seats per 100 children` = as.numeric(format(round(`Seats per 100 children`, 0), nsmall = 0)),
                    `Confirmed cases` = as.numeric(`Confirmed cases`),
                    Deaths = as.numeric(Deaths))
  })

  pageLength <- shiny::reactive({
    if (length(input$wfbPicker) == 1) {
      return(10)
    } else {
      return(10)
    }
  })

  output$demand_map <- ggiraph::renderGirafe({

    map_cbsa(ccs_map_data = ccs_map_data(),
             covid_map_data = covid_map_data(),
             show_covid = input$show_covid)
  })

  output$estimate_table <- DT::renderDataTable(
  
    DT::datatable(table(),
                  rownames= FALSE,
                  options = list(searching = FALSE,
                                 pageLength = 10,
                                 lengthMenu = list(c(10, 25, 50, 100, -1),
                                                   c('10', '25', '50', '100', 'All')),
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '75px', targets = "_all"))
                                )
                  )
  )

}
