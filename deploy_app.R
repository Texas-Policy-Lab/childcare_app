sapply(list.files("R",
                  full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

config <- yaml::read_yaml("./mainDashboard.yaml")

deploy_main(deploy = config$deploy)
