#' Run the Beautiful Shiny Dashboard
#'
#' Launches the integrated data cleaning & visualization dashboard.
#' @export
run_beautiful_dashboard <- function() {
  # 'shiny.R' 파일이 패키지 루트 또는 inst 아래 존재하는 경우 자동 탐색
  app_file <- system.file("shiny.R", package = "janitor")  # ← 패키지명 주의
  if (app_file == "") stop("Could not find shiny.R file. Please reinstall or fix the package.")
  
  source(app_file, local = TRUE)  # shinyApp()가 정의되어 있다고 가정
}
