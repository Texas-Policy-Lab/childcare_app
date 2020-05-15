# Childcare supply and demand application

Childcare supply and demand application during COVID-19. 

## Data Sources

* COVID-19 data are from DSHS
* Demand data are from the American Community Survey 5-year estimates

## Run app locally

1. In the Console run `shiny::runApp()` to run the application locally.

## Update app

To deploy the app to the linux server after changes have been made.

1. Log onto the [linux server](http://10.128.93.29:8787/)
2. Pull changes from github `git pull origin master`
3. Update configuration file to indicate whether to deploy application to development or to the production.
  - To deploy to development the configuration file should look like

```
deploy:
  prod:
    url: "childcare-supply-demand"
    run: FALSE
  dev:
    url: "childcare-supply-demand-staging"
    run: TRUE
```
  - to deploy to production the configuration file should look like

```
deploy:
  prod:
    url: "childcare-supply-demand"
    run: TRUE
  dev:
    url: "childcare-supply-demand-staging"
    run: FALSE
```

4. In the Console run `./deploy_app.R`
