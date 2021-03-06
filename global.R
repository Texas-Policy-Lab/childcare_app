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

httr::set_config(httr::config(ssl_verifypeer = 0L))

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./dashboard.yaml")

## GLOBALS

CAPTION <- config$caption
TPL_URL <- config$tpl_url

wfb <- dm.workforce_board(pth = config$data$wfb)

tx_counties <- dm.tx_counties(pth = config$data$tx_counties)

row <- data.frame(wfb_id = 0, wfb_name = "All",
                  stringsAsFactors = FALSE)

wfb <- wfb %>% 
  dplyr::distinct(wfb_id, wfb_name) %>%  
  dplyr::bind_rows(row) %>%
  dplyr::arrange(wfb_id)

assertthat::assert_that(sum(is.na(tx_counties$wfb_id)) == 0)
assertthat::assert_that(sum(is.na(tx_counties$wfb_name)) == 0)

est <- dm.estimates(pth = config$data$estimates)

est_ccs <- dm.estimates_ccs(est = est,
                            tx_counties = tx_counties)

est_d <- dm.estimates_demand(est = est,
                             tx_counties = tx_counties)

est_s <- dm.estimates_supply(est = est,
                             tx_counties = tx_counties)

cases <- dm.cases(pth = config$dshs$cases,
                  tx_counties = tx_counties)

deaths <- dm.deaths(pth = config$dshs$deaths,
                    tx_counties = tx_counties)

covid <- dm.covid(cases = cases,
                  deaths = deaths,
                  tx_counties = tx_counties)

map_tx_counties <- dm.tx_counties_map(tx_counties)
