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

cases <- dm.cases(pth = config$dshs$cases)

deaths <- dm.deaths(pth = config$dshs$deaths)

covid <- dm.covid(cases = cases,
                  deaths = deaths,
                  tx_counties = tx_counties)

map_tx_counties <- dm.tx_counties_map(tx_counties)

# pop_data <- pop(pth = "./data/Texas Population Data.xlsx") %>%
#   dplyr::select(`County Name`, `Total Population`, `Total Households`,
#                 `Number People Under 18`, `Households with one or more under 18`) %>% 
#   dplyr::rename(n_pop = `Total Population`,
#                 n_hhld = `Total Households`,
#                 n_kid_under18 = `Number People Under 18`,
#                 n_hhld_multiple_kid_under18 = `Households with one or more under 18`) %>% 
#   dplyr::mutate(n_kid_under12 = (n_kid_under18/18)*13,
#                 n_ppl_hhld = n_pop/n_hhld,
#                 pct_kid_under12 = (n_kid_under12/n_pop)*100,
#                 n_kid_under12_per_100hhld = (n_kid_under12/n_hhld)*100
#                 )
# 
# occ_data <- occupations(pth = "./data/occupations.xlsx") %>% 
#   dplyr::left_join(pop_data) %>% 
#   dplyr::mutate(n_frontline_hhld = phase1/n_ppl_hhld)
# 
# occ_data %>% dplyr::filter(`County Name` == "Harris County, Texas") %>% as.data.frame()
# 
# ind_data <- industry(pth = "./data/occupations.xlsx")
