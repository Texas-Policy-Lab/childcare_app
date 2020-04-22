#' @title Tab 1 user interface element
#' @description Creates tab 1 user interface element
ui_element <- function(tab, world, state, county, ...) UseMethod("ui_element")

ui_element.default <- function(tab, world, state, county, ...) {
  return(NULL)
}

# This tab corresponds to tab 1 in the `mainDashboard.yaml` file
ui_element.tab1 <- function(tab, world, ...) {
  return(NULL)
}
