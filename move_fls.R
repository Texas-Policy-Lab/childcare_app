my_modules <- list.files("~/childcare-supply-demand",
                         recursive = TRUE)
lapply(my_modules, function(filename) {

  from <- file.path("~/childcare-supply-demand", filename)

  assertthat::assert_that(file.exists(from))
  assertthat::assert_that(dir.exists("/srv/shiny-server/"))
  assertthat::assert_that(dir.exists("/srv/shiny-server/childcare-supply-demand"))

  to <- file.path("/srv/shiny-server/childcare-supply-demand",
                 filename)

  file.copy(from = from,
            to = to, 
            overwrite = TRUE,
            recursive = FALSE, 
            copy.mode = TRUE)
})
