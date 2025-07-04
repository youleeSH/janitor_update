run_beautiful_dashboard <- function() {
  app_file <- system.file("shiny.R", package = "janitor_update")
  if (app_file == "") stop("Could not find shiny.R file. Please reinstall or fix the package.")
  source(app_file, local = TRUE)
}
