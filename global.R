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

pattern <- paste(paste("Workforce Solutions",
                       c("", "of", "for", "for the", "of the")),
                 collapse = "|")

wfb <- readr::read_csv(here::here("./data/workforce_board_county.csv")) %>% 
  dplyr::rename(wfb_name = workforce_board) %>% 
  dplyr::mutate(wfb_name = stringr::str_replace_all(wfb_name, "[^[:alnum:] ]" , " "),
                wfb_name = tools::toTitleCase(tolower(wfb_name)),
                wfb_name = stringr::str_trim(gsub(pattern, "", wfb_name), "both"),
                wfb_id = as.numeric(factor(wfb_name, 
                                  levels=unique(wfb_name)))
                )

tx_counties <- readr::read_csv(here::here("./data/tx_counties.csv")) %>% 
  dplyr::mutate(county_name = paste(county, "County")) %>% 
  dplyr::left_join(wfb)

row <- data.frame(wfb_id = 0, wfb_name = "All",
                  stringsAsFactors = FALSE)

wfb <- wfb %>% 
  dplyr::distinct(wfb_id, wfb_name) %>%  
  dplyr::bind_rows(row) %>%
  dplyr::arrange(wfb_id)

assertthat::assert_that(sum(is.na(tx_counties$wfb_id)) == 0)
assertthat::assert_that(sum(is.na(tx_counties$wfb_name)) == 0)

est <- readr::read_csv("./data/Potential_supply_demand_county_5520.csv") %>% 
  dplyr::mutate(county = gsub(" County", "", county)) 

est_ccs <- est %>% 
  dplyr::select(county, dplyr::starts_with("ccs_per_100")) %>% 
  tidyr::gather(variable, est_ccs , -c(county)) %>% 
  dplyr::mutate(label = ifelse(est_ccs > 35, "> 35",
                              ifelse(est_ccs <= 35 & est_ccs > 15, "> 15 & <= 35", "< 15")),
                demand = ifelse(grepl("occ", variable), "Occupation", "Industry"),
                supply = ifelse(grepl("low", variable), "Low",
                                ifelse(grepl("med", variable), "Medium", "High"))) %>% 
  dplyr::select(-variable) %>% 
  dplyr::left_join(tx_counties)

assertthat::assert_that(sum(is.na(est_ccs$county_fips)) == 0)
assertthat::assert_that(sum(is.na(est_ccs$county_name)) == 0)
assertthat::assert_that(sum(is.na(est_ccs$wfb_name)) == 0)

est_d <- est %>% 
  dplyr::select(county, dplyr::contains("Demand")) %>%
  tidyr::gather(demand, est_demand , -c(county)) %>%  
  dplyr::mutate(demand = ifelse(grepl("Occupation", demand), "Occupation", "Industry")) %>% 
  dplyr::left_join(tx_counties)

assertthat::assert_that(sum(is.na(est_d$county_fips)) == 0)
assertthat::assert_that(sum(is.na(est_d$county_name)) == 0)
assertthat::assert_that(sum(is.na(est_d$wfb_name)) == 0)

est_s <- est %>%
  dplyr::select(county, dplyr::contains("supply")) %>% 
  tidyr::gather(supply, est_supply, -c(county)) %>% 
  dplyr::mutate(supply = gsub(" supply scenario", "", supply)) %>% 
  dplyr::left_join(tx_counties)

assertthat::assert_that(sum(is.na(est_s$county_fips)) == 0)
assertthat::assert_that(sum(is.na(est_s$county_name)) == 0)
assertthat::assert_that(sum(is.na(est_s$wfb_name)) == 0)

cases <- readr::read_csv(here::here('./data/covid/confirmed.csv')) %>% 
  dplyr::select(-Population) %>% 
  dplyr::rename(county = County) %>% 
  tidyr::gather(variable, `Confirmed cases`, -c(county)) %>% 
  dplyr::mutate(variable = gsub("Cases", "", variable),
                variable = gsub("\n", "", variable),
                variable = paste0(variable, "-2020"),
                date = lubridate::mdy(variable)) %>% 
  dplyr::select(-variable) %>% 
  dplyr::arrange(county, desc(date)) %>% 
  dplyr::group_by(county) %>% 
  dplyr::slice(1)

deaths <- readr::read_csv(here::here("./data/covid/deaths.csv")) %>% 
  dplyr::select(-Population) %>% 
  dplyr::rename(county = County) %>% 
  tidyr::gather(variable, `Deaths`, -c(county)) %>%
  dplyr::mutate(variable = gsub("Fatalities", "", variable),
                variable = gsub("\n", "", variable),
                variable = paste0(variable, "-2020"),
                date = lubridate::mdy(variable)) %>% 
  dplyr::select(-variable) %>% 
  dplyr::arrange(county, desc(date)) %>% 
  dplyr::group_by(county) %>% 
  dplyr::slice(1)

covid <- cases %>% 
  dplyr::left_join(deaths) %>% 
  dplyr::ungroup() %>% 
  tidyr::gather(covid_metric, `Total # (COVID metrics)`, -c(county, date)) %>% 
  dplyr::mutate(county = gsub("\n", " ", county)) %>% 
  dplyr::select(-date) %>% 
  dplyr::left_join(tx_counties)

assertthat::assert_that(sum(is.na(covid$county_fips)) == 0)
assertthat::assert_that(sum(is.na(covid$county_name)) == 0)
assertthat::assert_that(sum(is.na(covid$wfb_name)) == 0)

tx_counties <- tx_counties %>% 
  dplyr::mutate(subregion = tolower(county),
                subregion = stringr::str_trim(subregion, "both"),
                subregion = gsub(" ", "", subregion)) %>% 
  dplyr::right_join(ggplot2::map_data("county", region = "texas") %>% 
                     dplyr::mutate(subregion = stringr::str_trim(subregion, "both"),
                                   subregion = gsub(" ", "", subregion))) 

assertthat::assert_that(sum(is.na(tx_counties$county_fips)) == 0)
assertthat::assert_that(sum(is.na(tx_counties$county_name)) == 0)
assertthat::assert_that(sum(is.na(tx_counties$wfb_name)) == 0)
assertthat::assert_that(sum(is.na(tx_counties$subregion)) == 0)

