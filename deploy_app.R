config <- yaml::read_yaml("./mainDashboard.yaml")

tplshinyserver::deploy_main(deploy = config$deploy)
