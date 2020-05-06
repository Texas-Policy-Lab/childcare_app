library(magrittr)
library(ggplot2)
library(scales)
library(plyr)
library(tidyverse)
library(ggiraph)
library(maps)
library(mapproj)
library(plyr)

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
  dplyr::mutate(fill = ifelse(value > 35, "> 35",
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

tx_counties <- ggplot2::map_data("county", region = "texas") %>% 
  dplyr::mutate(subregion = stringr::str_trim(subregion, "both"),
                subregion = gsub(" ", "", subregion)) %>% 
  dplyr::left_join(wfb %>% 
                     dplyr::mutate(subregion = tolower(county),
                                   subregion = stringr::str_trim(subregion, "both"),
                                   subregion = gsub(" ", "", subregion)
                                   )
                   )

# cbsa <- readr::read_csv(here::here("./data/cbsa/cbsa_2013.csv")) %>% 
#   dplyr::rename(cbsa_cd = `CBSA Code`
#                ,cbsa_title = `CBSA Title`
#                ,county = `County/County Equivalent`
#                ,state = `State Name`
#                ,state_fips = `FIPS State Code`
#                ,county_fips = `FIPS County Code`) %>% 
#   dplyr::select(cbsa_cd, cbsa_title, county, state, state_fips, county_fips) %>% 
#   dplyr::filter(state_fips == "48")
# 
# cbsa_cd <- cbsa %>% 
#   dplyr::distinct(cbsa_cd, cbsa_title)
# 
# cbsa_county <- cbsa %>% 
#   dplyr::distinct(cbsa_cd, cbsa_title, county, county_fips) %>% 
#   dplyr::mutate(cbsa = ifelse(is.na(cbsa_cd), 0, 1))
# 
# # pop <- readr::read_csv("./data/pop_by_county.csv") %>% 
# #   dplyr::left_join(cbsa_county) %>% 
# #   dplyr::mutate(cbsa = ifelse(is.na(cbsa_cd), 0, 1)) %>% 
# #   dplyr::group_by(cbsa) %>% 
# #   dplyr::summarise(pop = sum(population)) %>% 
# #   dplyr::ungroup() %>% 
# #   dplyr::mutate(total_pop = sum(pop),
# #                  pct = (pop/total_pop)*100)
# 
# txt <- tabulizer::extract_tables("./data/twc_workforce_board.pdf") %>% 
#   
# l <- lapply(txt, function(x) {
#                               x <- as.data.frame(x, stringsAsFactors = FALSE)
#                               names(x) <- x[1,]
#                               x <- x[-1,]
#                               })
# 
# df <- do.call("rbind.fill", l)
# names(df)[1:2] <- c("workforce_board", "county")
# 
# df <- df %>% 
#   dplyr::select(workforce_board, county)
# 
# cntys <- stringr::str_trim(unlist(str_split(df$county, ",")), "both")
# 
# cntys2 <- sapply(cntys, function(cnty) {
#   
#   ifelse(grepl(cnty, df$county), 1, 0)
#   
# }, USE.NAMES = TRUE, simplify = FALSE)
# 
# cntys3 <- do.call("cbind", cntys2) %>% 
#   as.data.frame()
# 
# df <- df %>% 
#   dplyr::bind_cols(cntys3) %>% 
#   dplyr::select(-county) %>% 
#   tidyr::gather(county, value, -workforce_board) %>% 
#   dplyr::filter(value == 1) %>% 
#   dplyr::select(-value)
#   
# df <- readr::read_csv("workforce_board_county.csv") %>% 
#   dplyr::left_join(cbsa_county %>% 
#                      dplyr::mutate(county = gsub(" County", "", county)) %>% 
#                      dplyr::filter(cbsa_cd %in% unique(demand$cbsa_cd)))
# 
# df_sum <- df %>% 
#   dplyr::group_by(workforce_board) %>% 
#   dplyr::summarise(n = sum(cbsa, na.rm = T))
# 
# 
# demand <- readr::read_csv(here::here("./data/Potential_Demand.csv")) %>% 
#   dplyr::rename(CBSA = MSA
#                 ,employment_sector = `Parent Employment Sector`
#                 ,other_child_care_provider = `Other Child Care Provider in the home`
#                 ,all_children = `All Children (0-11)`
#                 ,infant_toddler = `Infants and Toddlers (0-2)`
#                 ,pre_school = `Pre-Schoolers (3-5)`
#                 ,school_age = `School Age (6-11)`) %>% 
#   dplyr::filter(state == "Texas") %>% 
#   dplyr::left_join(cbsa_cd)
# 
# assertthat::assert_that(demand %>%
#                           dplyr::filter(is.na(cbsa_cd)) %>%
#                           dplyr::distinct(cbsa_title) %>%
#                           nrow() == 0)
# 
# tx_counties <- ggplot2::map_data("county", region = "texas") %>%
#   dplyr::left_join(cbsa_county %>%
#                      dplyr::mutate(subregion = tolower(gsub(" County", "", county))) %>%
#                      dplyr::select(subregion, cbsa_cd, cbsa_title) %>%
#                      dplyr::filter(cbsa_cd %in% demand$cbsa_cd)) %>%
#   dplyr::left_join(readr::read_csv(here::here("./data/tx_counties.csv")) %>%
#                      dplyr::mutate(subregion = tolower(gsub(" County", "", county)))
#   )
