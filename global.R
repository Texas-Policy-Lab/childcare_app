library(magrittr)
library(ggplot2)
library(scales)
library(plyr)
library(tigris)
library(sf)
library(tidyverse)
library(ggiraph)
library(maps)
library(mapdata)
library(maptools)
library(viridis)

rm(list = ls())
options(encoding = "UTF-8")
tpltheme::set_tpl_theme(style = "print", font = "lato")

sapply(list.files("R", full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./mainDashboard.yaml")

cbsa <- readr::read_csv(here::here("./data/cbsa/cbsa_2013.csv")) %>% 
  dplyr::rename(cbsa_cd = `CBSA Code`
               ,cbsa_title = `CBSA Title`
               ,county = `County/County Equivalent`
               ,state = `State Name`
               ,state_fips = `FIPS State Code`
               ,county_fips = `FIPS County Code`) %>% 
  dplyr::select(cbsa_cd, cbsa_title, county, state, state_fips, county_fips) %>% 
  dplyr::filter(state_fips == "48")

cbsa_cd <- cbsa %>% 
  dplyr::distinct(cbsa_cd, cbsa_title)

cbsa_county <- cbsa %>% 
  dplyr::distinct(cbsa_cd, cbsa_title, county, county_fips)

demand <- readr::read_csv(here::here("./data/Potential_Demand.csv")) %>% 
  dplyr::rename(CBSA = MSA
                ,employment_sector = `Parent Employment Sector`
                ,other_child_care_provider = `Other Child Care Provider in the home`
                ,all_children = `All Children (0-11)`
                ,infant_toddler = `Infants and Toddlers (0-2)`
                ,pre_school = `Pre-Schoolers (3-5)`
                ,school_age = `School Age (6-11)`) %>% 
  dplyr::filter(state == "Texas") %>% 
  dplyr::left_join(cbsa_cd)

assertthat::assert_that(demand %>%
                          dplyr::filter(is.na(cbsa_cd)) %>%
                          dplyr::distinct(cbsa_title) %>%
                          nrow() == 0)

tx_counties <- ggplot2::map_data("county", region = "texas") %>% 
  dplyr::left_join(cbsa_county %>% 
                     dplyr::mutate(county = tolower(gsub(" County", "", county))) %>% 
                     dplyr::rename(subregion = county) %>% 
                     dplyr::filter(cbsa_cd %in% demand$cbsa_cd))
