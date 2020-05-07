#' @title Tab 1 user interface element
#' @description Creates tab 1 user interface element
ui_element <- function(tab, ...) UseMethod("ui_element")

ui_element.default <- function(tab, ...) {
  return(NULL)
}

# This tab corresponds to tab 1 in the `mainDashboard.yaml` file
ui_element.tab1 <- function(tab, wfb, est_ccs, covid = covid, ...) {
  demand.ui(wfb = wfb, est_ccs = est_ccs, covid = covid)
}
