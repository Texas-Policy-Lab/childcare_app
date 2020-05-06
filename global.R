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
# tpltheme::set_tpl_theme(style = "print", font = "lato")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./mainDashboard.yaml")

pattern <- paste(paste("Workforce Solutions",
                       c("", "of", "for", "for the", "of the")),
                 collapse = "|")

wfb <- readr::read_csv(here::here("./data/workforce_board_county.csv")) %>% 
  dplyr::mutate(workforce_board = stringr::str_replace_all(workforce_board, "[^[:alnum:] ]" , " "),
                workforce_board = tools::toTitleCase(tolower(workforce_board)),
                workforce_board = stringr::str_trim(gsub(pattern, "", workforce_board), "both")
                )

est <- readr::read_csv("./data/Potential_supply_demand_county_5520.csv") %>% 
  dplyr::left_join(wfb %>% 
                     dplyr::mutate(county = paste(county, "County", sep = " ")))

est_ccs <- est %>% 
  dplyr::select(county, workforce_board, dplyr::starts_with("ccs_per_100")) %>% 
  tidyr::gather(variable, value , -c(county, workforce_board)) %>% 
  dplyr::mutate(est = ifelse(value > 35, "> 35",
                              ifelse(value <= 35 & value > 15, "> 15 & <= 35", "< 15")),
                demand = ifelse(grepl("occ", variable), "Occupation", "Industry"),
                supply = ifelse(grepl("low", variable), "Low",
                                ifelse(grepl("med", variable), "Medium", "High")))

est_d <- est %>% 
  dplyr::select(county, workforce_board, dplyr::starts_with("Demand")) %>% 
  tidyr::gather(variable, value , -c(county, workforce_board)) %>% 
  dplyr::mutate(variable = ifelse(grepl("Occupation", variable), "Occupation", "Industry"))

est_s <- est %>% 
  dplyr::select(county, workforce_board, dplyr::contains("supply")) %>% 
  tidyr::gather(variable, value , -c(county, workforce_board)) %>% 
  dplyr::mutate(variable = gsub(" supply scenario", "", variable))

cases <- readr::read_csv(here::here('./data/covid/confirmed.csv')) %>% 
  dplyr::select(-Population) %>% 
  tidyr::gather(variable, `Confirmed cases`, -c(County)) %>% 
  dplyr::mutate(variable = gsub("Cases", "", variable),
                variable = gsub("\n", "", variable),
                variable = paste0(variable, "-2020"),
                date = lubridate::mdy(variable)) %>% 
  dplyr::select(-variable) %>% 
  dplyr::arrange(County, desc(date)) %>% 
  dplyr::group_by(County) %>% 
  dplyr::slice(1)

deaths <- readr::read_csv(here::here("./data/covid/deaths.csv")) %>% 
  dplyr::select(-Population) %>% 
  tidyr::gather(variable, Deaths, -c(County)) %>% 
  dplyr::mutate(variable = gsub("Fatalities", "", variable),
                variable = gsub("\n", "", variable),
                variable = paste0(variable, "-2020"),
                date = lubridate::mdy(variable)) %>% 
  dplyr::select(-variable) %>% 
  dplyr::arrange(County, desc(date)) %>% 
  dplyr::group_by(County) %>% 
  dplyr::slice(1)

covid <- cases %>% 
  dplyr::left_join(deaths) %>% 
  dplyr::ungroup() %>% 
  tidyr::gather(variable, value, -c(County, date)) %>% 
  dplyr::mutate(County = gsub("\n", " ", County)) %>% 
  dplyr::select(-date) %>% 
  dplyr::left_join(wfb %>% 
                     dplyr::rename(County = county))

tx_counties <- ggplot2::map_data("county", region = "texas") %>% 
  dplyr::mutate(subregion = stringr::str_trim(subregion, "both"),
                subregion = gsub(" ", "", subregion)) %>% 
  dplyr::left_join(wfb %>% 
                     dplyr::mutate(subregion = tolower(county),
                                   subregion = stringr::str_trim(subregion, "both"),
                                   subregion = gsub(" ", "", subregion)
                                   )
                   )
