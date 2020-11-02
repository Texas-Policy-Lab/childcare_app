config <- yaml::read_yaml("./deploy_app.yaml")

tplshinyserver::deploy_main(deploy = config$deploy)
