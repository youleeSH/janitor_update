#' Run the Beautiful Shiny Dashboard
#'
#' Launches the integrated data cleaning & visualization dashboard.
#'
#' Note:
#' - The shiny.R file should be located under the inst/ directory and will be copied to the package root after installation.
#' - The function searches for shiny.R in the installed package (see DESCRIPTION and inst/ directory structure).
#'
#' Implementation details:
#' - ui and server objects must be defined in shiny.R.
#' - Loads ui and server into a new environment, then launches shinyApp.
#'
#' @export
run_beautiful_dashboard <- function() {
  # Locate shiny.R in the installed package
  app_file <- system.file("shiny.R", package = "janitor")  # Or use janitor_update if applicable, based on DESCRIPTION
  
  if (app_file == "") stop("shiny.R not found in the installed package. Please check that inst/shiny.R exists and reinstall.")
  
  # Load ui/server/shinyApp definitions into a new environment
  env <- new.env()
  sys.source(app_file, envir = env)
  
  # Ensure both ui and server are defined
  if (!exists("ui", envir = env) || !exists("server", envir = env)) {
    stop("shiny.R must define both `ui` and `server` objects.")
  }
  
  # Launch the shinyApp
  shiny::shinyApp(ui = env$ui, server = env$server)
}
