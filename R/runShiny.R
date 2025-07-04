#' Run the Beautiful Shiny Dashboard
#'
#' Launches the integrated data cleaning & visualization dashboard.
#' @export
run_beautiful_dashboard <- function() {
  # shiny.R는 inst/ 아래에 위치해야 하며 설치 후에는 패키지 루트로 복사됨
  app_file <- system.file("shiny.R", package = "janitor")  # 또는 janitor_update, DESCRIPTION 기준
  if (app_file == "") stop("shiny.R not found in installed package. Please check inst/shiny.R and reinstall.")
  
  # 새로운 환경에서 ui/server/shinyApp 호출
  env <- new.env()
  sys.source(app_file, envir = env)
  
  # ui/server가 존재해야 함
  if (!exists("ui", envir = env) || !exists("server", envir = env)) {
    stop("shiny.R must define both `ui` and `server` objects.")
  }
  
  # shinyApp 실행
  shiny::shinyApp(ui = env$ui, server = env$server)
}
