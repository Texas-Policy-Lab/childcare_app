# childcare_app

Childcare supply and demand application

## Update app

my_modules <- list.files("~/childcare-supply-demand/", recursive = TRUE)
lapply(my_modules, function(filename) {
  file.copy(from = paste0("~/childcare-supply-demand/", filename),
            to = paste0("/srv/shiny-server/childcare-supply-demand/",
            filename), 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
})

