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

config <- yaml::read_yaml("./mainDashboard.yaml")

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

pop_data <- dm.pop(pth = "./data/Texas Population Data (2).xlsx")

occ_data <- dm.occ_summary(pth = "./data/occupations.xlsx", pop_data = pop_data)

ind_data <- dm.ind_summary(pth = "./data/occupations.xlsx", pop_data = pop_data)

df <- occ_data %>% 
  dplyr::select(Geography, n_kid_needcare_phase2) %>% 
  dplyr::rename(n_kid_needcare_phase2_occ = n_kid_needcare_phase2) %>% 
  dplyr::left_join(ind_data %>% 
                     dplyr::select(Geography, n_kid_needcare_phase2) %>% 
                     dplyr::rename(n_kid_needcare_phase2_ind = n_kid_needcare_phase2))

