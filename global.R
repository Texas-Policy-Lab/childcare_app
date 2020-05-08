library(magrittr)
library(ggplot2)
library(scales)
library(plyr)
library(tidyverse)
library(ggiraph)
library(maps)
library(mapproj)

rm(list = ls())
options(encoding = "UTF-8")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./mainDashboard.yaml")

wfb <- dm.workforce_board(pth = config$data$wfb)

tx_counties <- dm.tx_counties(pth = config$data$tx_counties)

wfb <- wfb %>% 
  dplyr::distinct(wfb_id, wfb_name)

assertthat::assert_that(sum(is.na(tx_counties$wfb_id)) == 0)
assertthat::assert_that(sum(is.na(tx_counties$wfb_name)) == 0)

est <- dm.estimates(pth = config$data$estimates)

est_ccs <- dm.estimates_ccs(est = est,
                            tx_counties = tx_counties)

est_d <- dm.estimates_demand(est = est,
                            tx_counties = tx_counties)

est_s <- dm.estimates_supply(est = est,
                             tx_counties = tx_counties)

cases <- dm.cases(pth = config$dshs$cases)

deaths <- dm.deaths(pth = config$dshs$deaths)

covid <- dm.covid(cases = cases,
                  deaths = deaths,
                  tx_counties = tx_counties)

map_tx_counties <- dm.tx_counties_map(tx_counties)

