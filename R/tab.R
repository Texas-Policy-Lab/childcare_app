#' @title Tab 1 user interface element
#' @description Creates tab 1 user interface element
ui_element <- function(tab, world, state, county, ...) UseMethod("ui_element")

ui_element.default <- function(tab, world, state, county, ...) {
  return(NULL)
}

# This tab corresponds to tab 1 in the `mainDashboard.yaml` file
ui_element.tab1 <- function(tab, demand, ...) {
  demand.ui(demand)
}

ui_element.tab2 <- function(tab, ...) {
  # shiny::includeHTML(
  #   "./README.html"
  # )
}
