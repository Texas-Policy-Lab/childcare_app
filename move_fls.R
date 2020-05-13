my_modules <- list.files("~/childcare_app",
                         recursive = TRUE)
lapply(my_modules, function(filename) {

  from <- file.path("~/childcare_app", filename)

  assertthat::assert_that(file.exists(from))
  assertthat::assert_that(dir.exists("/srv/shiny-server/"))
  assertthat::assert_that(dir.exists("/srv/shiny-server/childcare-supply-demand-staging"))

  to <- file.path("/srv/shiny-server/childcare-supply-demand-staging",
                  filename)

  file.copy(from = from,
            to = to, 
            overwrite = TRUE,
            recursive = FALSE, 
            copy.mode = TRUE)
})
