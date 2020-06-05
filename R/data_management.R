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
    dplyr::rename(county_merge = COUNTY) %>% 
    dplyr::mutate(county_merge = tolower(county_merge))
}

#' @title Data management Child care seats estimates
#' @param est dataframe. Estimates data frame.
#' @param tx_counties. Texas counties data frame.
#' @export
dm.estimates_ccs <- function(est, tx_counties) {

  est_ccs <- est %>% 
    dplyr::select(county_merge, LsupplyUpper, LsupplyLower, MsupplyUpper, MsupplyLower, HsupplyUpper, HsupplyLower) %>% 
    tidyr::gather(variable, est_ccs , -c(county_merge)) %>% 
    dplyr::mutate(est_ccs = ifelse(est_ccs == -9999, NA, est_ccs),
                  label = ifelse(est_ccs > 35, "> 35",
                                 ifelse(est_ccs <= 35 & est_ccs > 15, "> 15 & <= 35", "< 15")),
                  # label = factor(label),
                  demand = ifelse(grepl("Lower", variable), "Occupation", "Industry"),
                  supply = ifelse(grepl("Lsupply", variable), "Low",
                                  ifelse(grepl("Msupply", variable), "Medium", "High"))) %>% 
    dplyr::select(-variable) %>%
    dplyr::left_join(tx_counties %>% 
                       dplyr::mutate(county_merge = tolower(county))) %>% 
    dplyr::select(-county_merge)
  
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
    dplyr::select(county_merge, LowerRangeOccupation, UpperRangeIndustry) %>%
    tidyr::gather(demand, est_demand , -c(county_merge)) %>%  
    dplyr::mutate(demand = ifelse(grepl("Occupation", demand), "Occupation", "Industry")) %>% 
    dplyr::left_join(tx_counties %>% 
                       dplyr::mutate(county_merge = tolower(county))) %>% 
    dplyr::select(-county_merge)
  
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
    dplyr::select(county_merge, low_supply, med_supply, hi_supply) %>% 
    tidyr::gather(supply, est_supply, -c(county_merge)) %>% 
    dplyr::mutate(supply = dplyr::case_when(supply == "hi_supply" ~ "High",
                                             supply == "med_supply" ~ "Medium",
                                             supply == "low_supply" ~ "Low")) %>% 
    dplyr::left_join(tx_counties %>% 
                       dplyr::mutate(county_merge = tolower(county))) %>% 
    dplyr::select(-county_merge)

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

  names(df) <- stringr::str_replace_all(names(df), "[\r|\n]" , " ")

  assertthat::assert_that(grepl("county", tolower(names(df)[1])))
  
  df <- df %>% 
    dplyr::rename(county = 1) %>% 
    dplyr::right_join(tx_counties %>% 
                       dplyr::select(county)) %>% 
    dplyr::select(-Population) %>% 
    tidyr::gather(variable, value, -c(county)) %>% 
    dplyr::mutate(variable = gsub("Cases|Fatalities", "", variable),
                  variable = stringr::str_trim(variable, "both"),
                  value = as.numeric(value))

  if(any(is.na(as.numeric(df$variable)))) {

    df <- df %>%
      dplyr::mutate(variable = paste0(variable, "-2020"),
                    date = lubridate::mdy(variable))

  } else {
    df <- df %>% 
      dplyr::mutate(date = as.Date(as.numeric(variable), origin = "1899-12-30"))
  }
  
  df <- df %>%
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
    dplyr::left_join(pop_data) %>%
    dplyr::mutate(n_hhld_w_ess = workforce/n_ppl_hhld,
                  n_kid_needcare = .75*(n_kid_under12_per_100hhld*n_hhld_w_ess)/100) %>%
    dplyr::select(Geography, reopening, workforce) %>%
    tidyr::spread(reopening, workforce)

  assertthat::assert_that(all(df$essential <= df$phase1))
  assertthat::assert_that(all(df$phase1 <= df$phase2))
  assertthat::assert_that(all(df$phase2 <= df$phase3))

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
#' @export
dm.essential_workforce <- function(df,
                                   reopen_pct_df) {

  # Select most recent years for each Geography and industry or occupation
  df <- df %>% 
    dplyr::select(Geography, ind_occ_id, ind_occ, Year, workforce) %>%
    dplyr::arrange(desc(Year)) %>%
    dplyr::group_by(Geography, ind_occ_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  assertthat::assert_that(all(df$Year %in% c(2017, 2018)))
  assertthat::assert_that(nrow(df) == length(unique(df$ind_occ_id))*254)

  df <- df %>%
    dplyr::left_join(reopen_pct_df %>%
                       dplyr::select(-ind_occ) %>% 
                       tidyr::gather(reopening, value, -ind_occ_id)) %>% 
    dplyr::mutate(value = value/100,
                  workforce = workforce*value) %>% 
    dplyr::select(-c(value, Year))
browser()
  return(df)
}

#' @title Data management summarize industry or occupation
#' @description Creates a dataframe which is unique at the county level and summarizes the total number of essential workers
dm.summarize_essential_workforce <- function(df) {

  df <- df %>%
    dplyr::group_by(Geography, reopening) %>%
    dplyr::summarise(workforce = sum(workforce))%>%
    dplyr::mutate(`County Name` = gsub("TX", "Texas", Geography)) %>%
    dplyr::ungroup()

  assertthat::assert_that(nrow(df) == 254*length(unique(df$reopening)))

  return(df)
}

#' @title Data management for occupation data at the county-level
#' @inheritParams dm.workforce_board
#' @inheritParams dm.demand
#' @inheritParams dm.childdemand
#' @export
dm.occ_summary <- function(pth,
                           reopen_pct_pth,
                           pop_data) {

  # Read in occupation data and create new variables
  df <- dm.ind_read_data(pth = pth)
  
  reopen_pct_df <- readr::read_csv(reopen_pct_pth)
  
  # Compute the essential workforce for reopening in different phases
  df <- dm.essential_workforce(df = df,
                               reopen_pct_df)
  
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
                           reopen_pct_pth,
                           pop_data) {

  # Read in occupation data and create new variables
  df <- dm.ind_read_data(pth = pth)

  reopen_pct_df <- readr::read_csv(reopen_pct_pth)

  # Compute the essential workforce for reopening in different phases
  df <- dm.essential_workforce(df = df,
                               reopen_pct_df)

  # Summarize occupation data at the county level
  df <- dm.summarize_essential_workforce(df)

  # Merge childcare data
  df <- dm.childdemand(df, pop_data)  

  return(df)
}
