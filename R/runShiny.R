#' Run the Beautiful Shiny Dashboard
#'
#' Launches the integrated data cleaning & visualization dashboard.
#' @export
run_beautiful_dashboard <- function() {
  app_file <- system.file("shiny", "app.R", package = "mypackage") # mypackage는 패키지명
  if (app_file == "") stop("Could not find Shiny app file. Please reinstall the package.")
  shiny::runApp(app_file, display.mode = "normal")
}
