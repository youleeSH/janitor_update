###################################################################3
# 권장 패키지 자동 설치
req_pkgs <- c("shiny", "shinydashboard", "DT", "janitor", "ggplot2", "readxl", "dplyr", "lubridate")
for(pkg in req_pkgs) if(!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)

library(shiny)
library(shinydashboard)
library(DT)
library(janitor)
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = "Beautiful Shiny Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("데이터 업로드/정제", tabName = "data", icon = icon("table")),
      menuItem("시각화", tabName = "viz", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              fluidRow(
                box(title = "데이터 업로드",
                    width = 12,
                    fileInput("file", "csv/xlsx 데이터 업로드", accept = c(".csv", ".xls", ".xlsx")),
                    numericInput("header_row", "헤더로 사용할 행 번호(1부터)", value = 1, min = 1),
                    checkboxInput("apply_clean_names", "컬럼명 일괄정제(clean_names)", TRUE)
                )
              ),
              fluidRow(
                box(title = "정제 옵션",
                    width = 12,
                    checkboxInput("apply_remove_empty", "빈 행/열 삭제(remove_empty)", TRUE),
                    checkboxInput("apply_remove_constant", "상수 컬럼 삭제(remove_constant)", TRUE),
                    # 중복 탐색 옵션 추가
                    uiOutput("dupe_cols_select"),
                    actionButton("find_dupes", "중복 탐색(get_dupes)"),
                    checkboxInput("apply_remove_dupes", "중복 제거(첫 행만 남김)", FALSE),
                    checkboxInput("apply_date_convert", "날짜 컬럼 변환(convert_to_date)", FALSE),
                    uiOutput("date_col_select"),
                    checkboxInput("apply_coalesce", "여러 컬럼 합치기(coalesce)", FALSE),
                    uiOutput("coalesce_col_select"),
                    textInput("coalesce_new_col", "합친 컬럼명(예: cert)", "cert"),
                    checkboxInput("remove_coalesced", "합친 뒤 기존 컬럼 삭제", TRUE),
                    checkboxInput("show_cleaned", "정제 데이터 미리보기", TRUE)
                )
              ),
              fluidRow(
                box(title = "데이터 테이블", width = 12, DTOutput("data_table"))
              ),
              fluidRow(
                box(title = "중복 행 미리보기", width = 12, DTOutput("dupes_table"))
              )
      ),
      tabItem(tabName = "viz",
              fluidRow(
                box(title = "시각화 옵션", width = 4,
                    uiOutput("var_select"),
                    selectInput("plot_type", "시각화 유형", c("히스토그램", "박스플롯", "산점도")),
                    actionButton("draw", "시각화 실행")
                ),
                box(title = "그래프", width = 8, plotOutput("plot", height = 350))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ## 1. 원본 데이터 불러오기 + 헤더 지정 + clean_names
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (tolower(ext) %in% c("xls", "xlsx")) {
      df <- readxl::read_excel(input$file$datapath, .name_repair = "minimal", col_names = FALSE)
    } else if (tolower(ext) == "csv") {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE, header = FALSE)
    } else {
      showNotification("지원하지 않는 파일 형식입니다.", type = "error")
      return(NULL)
    }
    # row_to_names로 원하는 행을 헤더로 사용
    header_row <- input$header_row
    if (!is.null(header_row) && header_row > 0) {
      df <- janitor::row_to_names(df, row_number = header_row)
    }
    # clean_names 적용
    if (isTRUE(input$apply_clean_names)) {
      df <- janitor::clean_names(df)
    }
    df <- df[, !is.na(names(df)) & names(df) != ""]
    df
  })
  
  ## 2. 정제 데이터 (remove_empty, remove_constant 등 단계별 적용)
  cleaned_data <- reactive({
    req(raw_data())
    df <- raw_data()
    # 빈 행/열 제거
    if (isTRUE(input$apply_remove_empty)) {
      df <- janitor::remove_empty(df, c("rows", "cols"))
    }
    # 상수 컬럼 제거
    if (isTRUE(input$apply_remove_constant)) {
      df <- janitor::remove_constant(df, na.rm = TRUE)
    }
    # 날짜 컬럼 변환
    if (isTRUE(input$apply_date_convert) && !is.null(input$date_col) && input$date_col %in% names(df)) {
      df[[input$date_col]] <- janitor::convert_to_date(df[[input$date_col]], character_fun = lubridate::mdy)
    }
    # 여러 컬럼 합치기(coalesce)
    if (isTRUE(input$apply_coalesce) && !is.null(input$coalesce_cols) &&
        length(input$coalesce_cols) > 1 && nzchar(input$coalesce_new_col)) {
      df[[input$coalesce_new_col]] <- dplyr::coalesce(!!!df[input$coalesce_cols])
      if (isTRUE(input$remove_coalesced)) {
        df <- df[, !(names(df) %in% input$coalesce_cols)]
      }
    }
    # 중복 제거
    if (isTRUE(input$apply_remove_dupes) && !is.null(input$dupe_cols) && length(input$dupe_cols) > 0) {
      df <- df[!duplicated(df[input$dupe_cols]), ]
    }
    df
  })
  
  ## 3. 데이터 테이블 미리보기
  output$data_table <- renderDT({
    df <- if (isTRUE(input$show_cleaned)) cleaned_data() else raw_data()
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  ## 4. 날짜 변환 및 coalesce 컬럼 선택 UI
  output$date_col_select <- renderUI({
    req(raw_data())
    selectInput("date_col", "날짜 변환 컬럼 선택", choices = names(raw_data()), selected = NULL)
  })
  output$coalesce_col_select <- renderUI({
    req(raw_data())
    selectizeInput("coalesce_cols", "합칠 컬럼 선택(2개이상)", choices = names(raw_data()), multiple = TRUE)
  })
  
  ## 5. 중복 탐색 옵션
  output$dupe_cols_select <- renderUI({
    req(raw_data())
    selectizeInput("dupe_cols", "중복 체크 기준 컬럼(1개 이상 선택)", choices = names(raw_data()),
                   multiple = TRUE, selected = NULL)
  })
  observeEvent(input$find_dupes, {
    req(input$dupe_cols, length(input$dupe_cols) >= 1)
    df <- cleaned_data()
    dupe_tbl <- janitor::get_dupes(df, !!!rlang::syms(input$dupe_cols))
    output$dupes_table <- renderDT({
      if (nrow(dupe_tbl) > 0) datatable(dupe_tbl) else datatable(data.frame(메시지 = "중복 없음"))
    })
  })
  
  ## 6. 시각화 변수 선택 UI
  output$var_select <- renderUI({
    req(cleaned_data())
    df <- cleaned_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    tagList(
      selectInput("xvar", "X축 변수", choices = num_vars, selected = num_vars[1]),
      selectInput("yvar", "Y축 변수 (산점도/박스플롯)", choices = num_vars, selected = if(length(num_vars) > 1) num_vars[2] else num_vars[1])
    )
  })
  
  ## 7. 시각화
  plot_data <- eventReactive(input$draw, { req(cleaned_data()); cleaned_data() })
  output$plot <- renderPlot({
    req(plot_data())
    df <- plot_data()
    xvar <- input$xvar; yvar <- input$yvar; plot_type <- input$plot_type
    if (plot_type == "히스토그램" && !is.null(xvar)) {
      ggplot(df, aes_string(x = xvar)) + geom_histogram(fill = "#69b3a2", color = "white") + theme_minimal()
    } else if (plot_type == "박스플롯" && !is.null(xvar) && !is.null(yvar)) {
      ggplot(df, aes_string(x = xvar, y = yvar)) + geom_boxplot(fill = "#e07a5f") + theme_minimal()
    } else if (plot_type == "산점도" && !is.null(xvar) && !is.null(yvar)) {
      ggplot(df, aes_string(x = xvar, y = yvar)) + geom_point(size = 2, color = "#577590") + theme_minimal()
    }
  })
}

shinyApp(ui, server)

###############################################################

