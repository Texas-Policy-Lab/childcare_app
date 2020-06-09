library(magrittr)

source("R/data_management.R")

config <- yaml::read_yaml("./mainDashboard.yaml")

pop_data <- dm.pop(pth = config$data$population)

occ_data <- dm.occ_summary(pth = config$data$occ_ind, 
                           reopen_pct_pth = config$data$occ_reopen,
                           pop_data = pop_data)

ind_data <- dm.ind_summary(pth = config$data$occ_ind,
                           reopen_pct_pth = config$data$ind_reopen,
                           pop_data = pop_data)

df <- occ_data %>% 
  dplyr::select(Geography, phase3) %>% 
  dplyr::rename(`Lower Range (Occupation)` = phase3) %>% 
  dplyr::left_join(
    ind_data %>% 
      dplyr::select(Geography, phase3) %>% 
      dplyr::rename(`Upper Range (Industry)` = phase3)
  )

write.csv(df, "phase3_reopening.csv", row.names = FALSE)  
