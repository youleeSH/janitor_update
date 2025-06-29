library(shiny)
library(shinydashboard)
library(DT)
library(janitor)
library(ggplot2)
library(readxl)   # dirty_data.xlsx를 읽으려면...

ui <- dashboardPage(
  dashboardHeader(title = "Beautiful Shiny Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("데이터프레임", tabName = "data", icon = icon("table")),
      menuItem("시각화", tabName = "viz", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # 데이터프레임 탭
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "데이터 업로드 및 보기",
                  width = 12,
                  fileInput("file", "csv/xlsx 데이터 업로드", accept = c(".csv", ".xls", ".xlsx")),
                  checkboxInput("apply_clean_names", "컬럼명 일괄정제(clean_names)", TRUE),
                  checkboxInput("apply_na_rm", "결측치(NA) 포함 행 삭제", FALSE),
                  checkboxInput("apply_outlier_rm", "이상치 행 삭제(수치형만)", FALSE),
                  uiOutput("rename_vars_ui"),
                  DTOutput("data_table")
                )
              )
      ),
      # 시각화 탭
      tabItem(tabName = "viz",
              fluidRow(
                box(
                  title = "시각화 옵션",
                  width = 4,
                  uiOutput("var_select"),
                  selectInput("plot_type", "시각화 유형",
                              choices = c("히스토그램", "박스플롯", "산점도")),
                  actionButton("draw", "시각화 실행")
                ),
                box(
                  title = "그래프",
                  width = 8,
                  plotOutput("plot", height = 350)
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ##변수명 변경에 대한 수정 코드
  rv_varnames <- reactiveVal(NULL)
  observeEvent(raw_data(), {
    df <- raw_data()
    rv_varnames(names(df))
  })
  observeEvent(input$rename_var_btn, {
    req(rv_varnames())
    varnames <- rv_varnames()
    idx <- which(varnames == input$selected_var_to_rename)
    if (length(idx) == 1 && nzchar(input$new_var_name)) {
      varnames[idx] <- input$new_var_name
      rv_varnames(varnames)
    }
  })
  user_data <- reactive({
    req(raw_data())
    df <- raw_data()
    # ... (기존 janitor 등 전처리)
    new_names <- rv_varnames()
    if (!is.null(new_names) && length(new_names) == ncol(df) && all(nzchar(new_names))) {
      names(df) <- new_names
    }
    df
  })
  ####
  
  
  # 원본 데이터 reactive

  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (tolower(ext) %in% c("xls", "xlsx")) {
      readxl::read_excel(input$file$datapath)
    } else if (tolower(ext) == "csv") {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else {
      showNotification("지원하지 않는 파일 형식입니다.", type = "error")
      NULL
    }
  })
  
  
  # 변수명 수동 입력값 모음
  rename_vars <- reactive({
    req(raw_data())
    df <- raw_data()
    sapply(seq_along(df), function(i) {
      input[[paste0("rename_var_", i)]]
    })
  })
  
  # 전처리된 데이터
  user_data <- reactive({
    req(raw_data())
    df <- raw_data()
    # 1. clean_names()
    if (isTRUE(input$apply_clean_names)) {
      df <- janitor::clean_names(df)
    }
    # 2. NA 포함 행 삭제
    if (isTRUE(input$apply_na_rm)) {
      df <- df[complete.cases(df), ]
    }
    # 3. 이상치 행 삭제 (각 수치형 변수별 IQR 밖 값 삭제)
    if (isTRUE(input$apply_outlier_rm)) {
      num_cols <- sapply(df, is.numeric)
      if (any(num_cols)) {
        for (col in names(df)[num_cols]) {
          q <- quantile(df[[col]], probs = c(0.25, 0.75), na.rm = TRUE)
          iqr <- q[2] - q[1]
          mask <- (df[[col]] >= (q[1] - 1.5 * iqr)) & (df[[col]] <= (q[2] + 1.5 * iqr))
          df <- df[mask | is.na(df[[col]]), ]
        }
      }
    }
    # 4. 변수명 수동 변경: 반드시 rv_varnames()로만 적용
    new_names <- rv_varnames()
    if (!is.null(new_names) && length(new_names) == ncol(df) && all(nzchar(new_names))) {
      names(df) <- new_names
    }
    df
  })
  
  # 데이터테이블 출력
  output$data_table <- renderDT({
    req(user_data())
    datatable(user_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # 변수명 수동변경 UI 생성
  output$rename_vars_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    tagList(
      fluidRow(
        column(6,
               selectInput("selected_var_to_rename",
                           "변경할 변수 선택",
                           choices = names(df),
                           selected = names(df)[1])
        ),
        column(4,
               textInput("new_var_name", "새 변수명 입력", value = "")
        ),
        column(2,
               actionButton("rename_var_btn", "변경")
        )
      )
    )
  })
  
  
  # 시각화 변수 선택 UI
  output$var_select <- renderUI({
    req(user_data())
    df <- user_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    tagList(
      selectInput("xvar", "X축 변수", choices = num_vars, selected = num_vars[1]),
      selectInput("yvar", "Y축 변수 (산점도/박스플롯)", choices = num_vars, selected = if(length(num_vars) > 1) num_vars[2] else num_vars[1])
    )
  })
  
  # 시각화
  plot_data <- eventReactive(input$draw, {
    req(user_data())
    user_data()
  })
  
  output$plot <- renderPlot({
    req(plot_data())
    df <- plot_data()
    xvar <- input$xvar
    yvar <- input$yvar
    plot_type <- input$plot_type
    
    if (plot_type == "히스토그램" && !is.null(xvar)) {
      ggplot(df, aes_string(x = xvar)) +
        geom_histogram(fill = "#69b3a2", color = "white") +
        theme_minimal()
    } else if (plot_type == "박스플롯" && !is.null(xvar) && !is.null(yvar)) {
      ggplot(df, aes_string(x = xvar, y = yvar)) +
        geom_boxplot(fill = "#e07a5f") +
        theme_minimal()
    } else if (plot_type == "산점도" && !is.null(xvar) && !is.null(yvar)) {
      ggplot(df, aes_string(x = xvar, y = yvar)) +
        geom_point(size = 2, color = "#577590") +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)

