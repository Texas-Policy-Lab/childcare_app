#' @title Data management Workforce board data
#' @param pth the path to the data
#' @export
dm.workforce_board <- function(pth) {
  pattern <- paste(paste("Workforce Solutions",
                         c("", "of", "for", "for the", "of the")),
                   collapse = "|")

  wfb <- readr::read_csv(here::here(pth)) %>% 
    dplyr::rename(wfb_name = workforce_board) %>% 
    dplyr::mutate(wfb_name = stringr::str_replace_all(wfb_name, "[^[:alnum:] ]" , " "),
                  wfb_name = tools::toTitleCase(tolower(wfb_name)),
                  wfb_name = stringr::str_trim(gsub(pattern, "", wfb_name), "both"),
                  wfb_id = as.numeric(factor(wfb_name, 
                                             levels=unique(wfb_name)))
    )

  return(wfb)
}

#' @title Data management Texas Counties
#' @inheritParams dm.workforce_board
#' @export
dm.tx_counties <- function(pth) {
  tx_counties <- readr::read_csv(here::here(pth)) %>% 
    dplyr::mutate(county_name = paste(county, "County")) %>% 
    dplyr::left_join(wfb)
}

#' @title Data management estimates
#' @inheritParams dm.workforce_board
#' @export
dm.estimates <- function(pth) {
  est <- readr::read_csv(pth) %>% 
    dplyr::mutate(county = gsub(" County", "", county)) 
}

#' @title Data management Child care seats estimates
#' @param est dataframe. Estimates data frame.
#' @param tx_counties. Texas counties data frame.
#' @export
dm.estimates_ccs <- function(est, tx_counties) {

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

  return(est_ccs)
}

#'@title Data management child care demand
#'@inheritParams dm.estimates_ccs
#'@export
dm.estimates_demand <- function(est, tx_counties) {

  est_d <- est %>% 
    dplyr::select(county, dplyr::contains("Demand")) %>%
    tidyr::gather(demand, est_demand , -c(county)) %>%  
    dplyr::mutate(demand = ifelse(grepl("Occupation", demand), "Occupation", "Industry")) %>% 
    dplyr::left_join(tx_counties)
  
  assertthat::assert_that(sum(is.na(est_d$county_fips)) == 0)
  assertthat::assert_that(sum(is.na(est_d$county_name)) == 0)
  assertthat::assert_that(sum(is.na(est_d$wfb_name)) == 0)

  return(est_d)
}

#'@title Data management child care supply
#'@inheritParams dm.estimates_ccs
#'@export
dm.estimates_supply <- function(est, tx_counties) {

  est_s <- est %>%
    dplyr::select(county, dplyr::contains("supply")) %>% 
    tidyr::gather(supply, est_supply, -c(county)) %>% 
    dplyr::mutate(supply = gsub(" supply scenario", "", supply)) %>% 
    dplyr::left_join(tx_counties)

  assertthat::assert_that(sum(is.na(est_s$county_fips)) == 0)
  assertthat::assert_that(sum(is.na(est_s$county_name)) == 0)
  assertthat::assert_that(sum(is.na(est_s$wfb_name)) == 0)
 
  return(est_s)
}

#' @title Read DSHS
#' @inheritParams dm.workforce_board
#' @export
read_dshs <- function(pth, tx_counties) {

  httr::GET(pth, httr::write_disk(temp <- tempfile(fileext = ".xlsx")))
  
  if (file.exists(temp)) {
    df <- readxl::read_excel(temp, sheet = 1, skip = 2)
  }

  names(df)[1] <- stringr::str_replace_all(names(df)[1], "[\r\n]" , " ")
  names(df)[1] <- stringr::str_replace_all(names(df)[1], "  ", " ")
  
  assertthat::assert_that(names(df)[1] == "County Name")
  
  df <- df %>% 
    dplyr::rename(county = 1) %>% 
    dplyr::right_join(tx_counties %>% 
                       dplyr::select(county)) %>% 
    dplyr::select(-Population) %>% 
    tidyr::gather(variable, value, -c(county)) %>% 
    dplyr::mutate(variable = gsub("Cases|Fatalities", "", variable),
                  variable = stringr::str_replace_all(variable, "[\r\n]" , ""),
                  variable = paste0(variable, "-2020"),
                  date = lubridate::mdy(variable),
                  county = gsub("\r\n", " ", county),
                  value = as.numeric(value)) %>% 
    dplyr::select(-variable) %>% 
    dplyr::arrange(county, desc(date)) %>% 
    dplyr::group_by(county) %>% 
    dplyr::slice(1)
}

#' @title Data management cases
#' @inheritParams dm.workforce_board 
#' @export
dm.cases <- function(pth, tx_counties) {
  read_dshs(pth, tx_counties) %>% 
    dplyr::rename(`Confirmed cases` = value)
}

#' @title Data management deaths
#' @inheritParams dm.deaths
#' @export
dm.deaths <- function(pth, tx_counties) {
  read_dshs(pth, tx_counties) %>% 
    dplyr::rename(Deaths = value)}

#' @title Data management covid data
#' @param cases data.frame.
#' @param deaths data.frame.
#' @inheritParams dm.estimates_ccs
dm.covid <- function(cases, deaths, tx_counties) {

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

  return(covid)
}

#' @title Data management Texas Counties Map
#' @inheritParams dm.estimates_ccs
#' @export
dm.tx_counties_map <- function(tx_counties) {

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

  return(tx_counties)  
}

#' @title Data management for population data
#' @inheritParams dm.workforce_board
#' @export
dm.pop <- function(pth) {
  
  df <- readxl::read_xlsx(pth, sheet = 1) %>% 
    dplyr::mutate(Geography = gsub("Texas", "TX", `County Name`)) %>%
    dplyr::select(Geography, `County Name`, `Total Population`, `Total Households`,
                  `Number People Under 18`) %>%
    dplyr::rename(n_pop = `Total Population`,
                  n_hhld = `Total Households`,
                  n_kid_under18 = `Number People Under 18`
                  # n_hhld_multiple_kid_under18 = `Households with one or more under 18`
                  ) %>%
    dplyr::mutate(n_kid_under12 = (n_kid_under18/18)*13,
                  n_ppl_hhld = n_pop/n_hhld,
                  pct_kid_under12 = (n_kid_under12/n_pop)*100,
                  n_kid_under12_per_100hhld = (n_kid_under12/n_hhld)*100)
  
  return(df)
}

#' @title Data management for child demand data
#' @param pop_data dataframe. Dataframe containing the population data at the county level.
#' @inheritParams dm.occupations 
#' @export
dm.childdemand <- function(df, pop_data) {
  
  df <- df %>%
    dplyr:: left_join(pop_data) %>%
    dplyr:: mutate(n_hhld_w_ess = phase1/n_ppl_hhld, 
                   n_kid_needcare = .75*(n_kid_under12_per_100hhld*n_hhld_w_ess)/100)
  
  return(df)
  
}

#' @title Read occupation data
#' @param pth string. The path to the excel file.
#' @export
dm.occ_read_data <- function(pth) {
  
  # Read in data and change names
  df <- readxl::read_excel(pth, sheet = 2) %>%
    dplyr::rename(ind_occ_id = `ID Occupation`,
                  ind_occ = Occupation,
                  workforce = `Workforce by Occupation and Gender`)
  
  return(df)
}

#' @title Read industry data
#' @param pth string. The path to the excel file.
#' @export
dm.ind_read_data <- function(pth) {
  
  # Read in data and change names
  df <- readxl::read_excel(pth, sheet = 5) %>% 
    dplyr::rename(ind_occ_id = `ID Industry`,
                  ind_occ = Industry,
                  workforce = `Workforce by Industry and Gender`)
  
  return(df)
}

#' @title Data management to create the number of essential workers
#' @param df dataframe. The data frame to apply data management steps to.
#' @param essential_id vector. Vector of numeric ids indicating industries/occupations deemed essential in Texas.
#' @param phase1_id vector. Vector of numeric ides indicating industries/occupations deemed essential in Texas.
#' @export
dm.essential_workforce <- function(df, essential_id, phase1_id) {
  
  # Select most recent years for each Geography and industry or occupation
  
  df <- df %>% 
    dplyr::select(Geography, ind_occ_id, ind_occ, Year, workforce) %>%
    dplyr::filter(ind_occ_id %in% c(essential_id, phase1_id)) %>%
    dplyr::arrange(desc(Year)) %>%
    dplyr::group_by(Geography, ind_occ_id) %>%
    dplyr::slice(1) %>% 
    dplyr::ungroup()

  assertthat::assert_that(all(df$Year %in% c(2017, 2018)))

  df <- df %>% 
    dplyr::select(-Year) %>% 
    dplyr::mutate(essential = dplyr::if_else(ind_occ_id %in% essential_id, 1, 0),
                  phase1 = dplyr::if_else(ind_occ_id %in% c(essential_id, phase1_id), 1, 0),
                  workforce = ifelse(ind_occ_id %in% phase1_id, .7*workforce, workforce)) %>% 
    tidyr::gather(variable, value, -c(Geography, ind_occ_id, ind_occ, workforce))
  
  assertthat::assert_that(all(df%>% dplyr::filter(variable == "phase1")  %>% dplyr::select(value) == 1))
  
  df <- df %>% 
    dplyr::filter(value == 1) %>% 
    dplyr::select(-c(value))

  x <- df %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(n = sum(workforce, na.rm = T))
  
  assertthat::assert_that(x %>%
                            dplyr::filter(variable == "essential") %>%
                            dplyr::select(n) %>%
                            dplyr::pull(n) <
                            x %>%
                            dplyr::filter(variable == "phase1") %>%
                            dplyr::select(n) %>%
                            dplyr::pull(n))

  return(df)
}

#' @title Data management essential occupations
#' @description Data management to create the number of essential workers for occupations
#' @inheritParams dm.read_occ_data
#' @export
dm.occ_essential_workforce <- function(pth, 
                                       essential_id = c(23, 21, 19, 7, 13, 12, 9, 16, 10, 15, 11),
                                       phase1_id = c(14, 17, 8)) {

  df <- dm.occ_read_data(pth = pth)
  df <- dm.essential_workforce(df = df, 
                               essential_id = essential_id,
                               phase1_id = phase1_id)

  return(df)
}

#' @title Data management essential industries
#' @description Data management to create the number of essential works for industries
#' @inheritParams dm.read_ind_data
#' @export
dm.ind_essential_workforce <- function(pth,
                                       essential_id = c(15, 3, 14, 11, 6, 13, 4, 19, 8, 7),
                                       phase1_id = c(17, 16, 5)) {

  df <- dm.ind_read_data(pth = pth)
  df <- dm.essential_workforce(df = df, 
                               essential_id = essential_id, 
                               phase1_id = phase1_id)
  return(df)
}

#' @title Data management summarize industry or occupation
#' @description Creates a dataframe which is unique at the county level and summarizes the total number of essential workers
dm.summarize_essential_workforce <- function(df) {

  df <- df %>%
    dplyr::group_by(Geography, variable) %>%
    dplyr::summarise(workforce = sum(workforce))%>%
    tidyr::spread(variable, workforce) %>%
    dplyr::mutate(`County Name` = gsub("TX", "Texas", Geography)) %>%
    dplyr::ungroup()

  assertthat::assert_that(nrow(df) == 254)

  return(df)
}

#' @title Data management for occupation data at the county-level
#' @inheritParams dm.workforce_board
#' @inheritParams dm.demand
#' @inheritParams dm.childdemand
#' @export
dm.occ_summary <- function(pth,
                           pop_data) {

  # Read in occupation data and create new variables
  df <- dm.occ_essential_workforce(pth = pth)

  # Summarize occupation data at the county level
  df <- dm.summarize_essential_workforce(df)

  # Merge childcare data
  df <- dm.childdemand(df, pop_data)

  return(df)
}

#' @title Data management for industry data at the county-level
#' @inheritParams dm.workforce_board
#' @inheritParams dm.demand
#' @inheritParams dm.childdemand
#' @export
dm.ind_summary <- function(pth,
                           pop_data) {

  # Read in industry data and create new variables
  df <- dm.ind_essential_workforce(pth = pth)

  # Summarize occupation data at the county level
  df <- dm.summarize_essential_workforce(df)

  # Merge childcare data
  df <- dm.childdemand(df, pop_data)  

  return(df)
}

#' @title Data management for occupation breakdowns
#' @inheritParams 
#' @export
dm.occ_breakdown <- function(pth) {

  # Read in industry data and create new variables
  df <- dm.occ_essential_workforce(pth = pth)

  df <- df %>% 
    tidyr::spread(variable, workforce) %>% 
    dplyr::mutate(`County Name` = gsub("TX", "Texas", Geography))

  assertthat::assert_that(nrow(df) == 14*254)

  return(df)
}

#' @title Data management for industry breakdowns
#' @inheritParams 
#' @export
dm.ind_breakdown <- function(pth) {

  # Read in industry data and create new variables
  df <- dm.ind_essential_workforce(pth = pth)

  df <- df %>% 
    tidyr::spread(variable, workforce) %>% 
    dplyr::mutate(`County Name` = gsub("TX", "Texas", Geography))

  assertthat::assert_that(nrow(df) == 20*254)

  return(df)
}
