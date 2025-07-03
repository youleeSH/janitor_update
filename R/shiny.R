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

###-
# ============ 패키지 자동 설치 및 로딩 ================
required_packages <- c("shiny", "shinydashboard", "DT", "janitor", "ggplot2", "readxl")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# ============ 내부 함수 파일 불러오기 ==================
# source("tabyl.R")
# source("tabyl_nway.R")
# source("outlier.R")
# source("inspect_missing.R")
# # 필요시 clean_names.R, remove_empties.R, 등도 source("clean_names.R")

# ============ UI 구성 ===================
ui <- dashboardPage(
  dashboardHeader(title = "Beautiful Shiny Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("데이터셋 정제", tabName = "data", icon = icon("table")),
      menuItem("교차표/탐색", tabName = "tabyl", icon = icon("th")),
      menuItem("이상치 탐색", tabName = "outlier", icon = icon("exclamation-triangle")),
      menuItem("시각화", tabName = "viz", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # 데이터셋 정제 탭
      tabItem(tabName = "data",
              fluidRow(
                box(title = "데이터 업로드 및 정제", width = 12,
                    fileInput("file", "csv/xlsx 데이터 업로드", accept = c(".csv", ".xls", ".xlsx")),
                    numericInput("header_row", "헤더로 사용할 행 번호 (자동 감지: 0)", value = 0, min = 0),
                    checkboxInput("apply_clean_names", "컬럼명 일괄정제(clean_names)", TRUE),
                    checkboxInput("apply_remove_empty", "빈 행/열 제거(remove_empty)", TRUE),
                    checkboxInput("apply_na_rm", "결측치 포함 행 제거(complete.cases)", FALSE),
                    uiOutput("rename_vars_ui"),
                    DTOutput("data_table")
                )
              )
      ),
      
      # 교차표(탐색) 탭
      tabItem(tabName = "tabyl",
              fluidRow(
                box(title = "교차표 옵션", width = 4,
                    helpText("1~3개는 tabyl, 4개 이상은 tabyl_nway로 분석"),
                    uiOutput("cross_var_select"),
                    actionButton("run_tabyl", "교차표 실행")
                ),
                box(title = "교차표 결과", width = 8,
                    DTOutput("tabyl_table"),
                    plotOutput("tabyl_plot")
                )
              )
      ),
      
      # 이상치 탐색 탭
      tabItem(tabName = "outlier",
              fluidRow(
                box(title = "이상치 탐색 옵션", width = 4,
                    selectInput("outlier_method", "이상치 탐색 방법", 
                                choices = c("iqr", "zscore", "percentile")),
                    uiOutput("outlier_var_select"),
                    actionButton("run_outlier", "이상치 탐색 실행")
                ),
                box(title = "이상치 결과", width = 8,
                    DTOutput("outlier_table"),
                    plotOutput("outlier_boxplot")
                )
              )
      ),
      
      # 시각화 탭
      tabItem(tabName = "viz",
              fluidRow(
                box(title = "시각화 옵션", width = 4,
                    checkboxInput("use_cleaned_for_viz", "정제된 데이터로 시각화", TRUE),
                    uiOutput("var_select"),
                    selectInput("plot_type", "시각화 유형", choices = c("히스토그램", "박스플롯", "산점도")),
                    actionButton("draw", "시각화 실행")
                ),
                box(title = "그래프", width = 8, plotOutput("plot", height = 350))
              ),
              fluidRow(
                box(title = "결측치 비율 시각화", width = 12, plotOutput("missing_plot"))
              )
      )
    )
  )
)

# =========== SERVER 구성 ==================
server <- function(input, output, session) {
  
  ## 1. 데이터 불러오기 및 헤더 처리
  raw_data <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$datapath))
    df <- if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(input$file$datapath, col_names = FALSE)
    } else if (ext == "csv") {
      read.csv(input$file$datapath, stringsAsFactors = FALSE, header = FALSE)
    } else {
      showNotification("지원하지 않는 파일 형식입니다.", type = "error")
      return(NULL)
    }
    # 헤더 지정 (0: 자동 감지, 1이상: 해당 행을 header로)
    hdr <- input$header_row
    if (!is.null(hdr) && hdr > 0 && hdr <= nrow(df)) {
      # row_to_names는 janitor 함수, 아니면 base로 처리
      names(df) <- as.character(unlist(df[hdr,]))
      df <- df[-(1:hdr),, drop=FALSE]
      rownames(df) <- NULL
    }
    df
  })
  
  ## 2. 정제된 데이터 만들기 (사용자 옵션 반영)
  cleaned_data <- reactive({
    req(raw_data())
    df <- as.data.frame(raw_data())
    # 컬럼명 정제
    if (isTRUE(input$apply_clean_names)) {
      df <- janitor::clean_names(df)
    }
    # 빈 행/열 제거
    if (isTRUE(input$apply_remove_empty)) {
      df <- janitor::remove_empty(df, which = c("rows", "cols"))
    }
    # 결측치 포함 행 제거
    if (isTRUE(input$apply_na_rm)) {
      df <- df[complete.cases(df),]
    }
    # 변수명 수동 변경
    new_names <- rv_varnames()
    if (!is.null(new_names) && length(new_names) == ncol(df) && all(nzchar(new_names))) {
      names(df) <- new_names
    }
    df
  })
  
  ## 3. 변수명 수동 변경 UI
  rv_varnames <- reactiveVal(NULL)
  observeEvent(cleaned_data(), {
    df <- cleaned_data()
    rv_varnames(names(df))
  })
  observeEvent(input$rename_var_btn, {
    varnames <- rv_varnames()
    idx <- which(varnames == input$selected_var_to_rename)
    if (length(idx) == 1 && nzchar(input$new_var_name)) {
      varnames[idx] <- input$new_var_name
      rv_varnames(varnames)
    }
  })
  output$rename_vars_ui <- renderUI({
    req(cleaned_data())
    df <- cleaned_data()
    tagList(
      fluidRow(
        column(6, selectInput("selected_var_to_rename", "변경할 변수 선택", choices = names(df))),
        column(4, textInput("new_var_name", "새 변수명 입력", value = "")),
        column(2, actionButton("rename_var_btn", "변경"))
      )
    )
  })
  
  ## 4. 데이터테이블 출력
  output$data_table <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ------------------------- 교차표/탐색 -----------------------
  output$cross_var_select <- renderUI({
    req(cleaned_data())
    df <- cleaned_data()
    selectInput("cross_vars", "교차분석 변수 (여러 개 선택 가능)", choices = names(df),
                multiple = TRUE, selectize = TRUE)
  })
  
  observeEvent(input$run_tabyl, {
    output$tabyl_table <- renderDT({
      req(input$cross_vars)
      df <- cleaned_data()
      vars <- input$cross_vars
      if (length(vars) == 1) {
        tb <- tabyl(df[[vars[1]]])
        datatable(tb)
      } else if (length(vars) == 2) {
        tb <- tabyl(df, vars[1], vars[2])
        datatable(tb)
      } else if (length(vars) == 3) {
        tb <- tabyl(df, vars[1], vars[2], vars[3])
        # 3-way는 리스트로 반환, 첫번째만 예시
        first_tb <- tb[[1]]
        datatable(first_tb)
      } else if (length(vars) >= 4) {
        tb <- tabyl_nway(df, !!!rlang::syms(vars))
        datatable(tb)
      } else {
        datatable(data.frame())
      }
    })
    output$tabyl_plot <- renderPlot({
      req(input$cross_vars)
      df <- cleaned_data()
      vars <- input$cross_vars
      # 1개 → 막대그래프, 2개 이상 → ggplot2로 시각화
      if (length(vars) == 1) {
        ggplot(df, aes_string(x = vars[1])) +
          geom_bar(fill = "#6497b1") + theme_minimal()
      } else if (length(vars) == 2) {
        ggplot(df, aes_string(x = vars[1], fill = vars[2])) +
          geom_bar(position = "dodge") + theme_minimal()
      } else if (length(vars) == 3) {
        ggplot(df, aes_string(x = vars[1], fill = vars[2])) +
          geom_bar(position = "dodge") + facet_wrap(vars[3]) +
          theme_minimal()
      } else {
        NULL
      }
    })
  })
  
  # ---------------------- 이상치 탐색 -------------------------
  output$outlier_var_select <- renderUI({
    req(cleaned_data())
    df <- cleaned_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("outlier_vars", "이상치 탐색 변수 (여러 개 선택)", choices = num_vars, multiple = TRUE)
  })
  
  observeEvent(input$run_outlier, {
    output$outlier_table <- renderDT({
      req(input$outlier_vars)
      df <- cleaned_data()
      method <- input$outlier_method
      tb <- identify_outliers(df, method = method, columns = input$outlier_vars)
      datatable(tb)
    })
    output$outlier_boxplot <- renderPlot({
      req(input$outlier_vars)
      df <- cleaned_data()
      method <- input$outlier_method
      vars <- input$outlier_vars
      if (length(vars) == 1) {
        ggplot(df, aes_string(y = vars[1])) +
          geom_boxplot(fill = "#e07a5f") + theme_minimal()
      } else if (length(vars) > 1) {
        # melt해서 여러 변수 박스플롯
        df_melt <- reshape2::melt(df[,vars,drop=FALSE])
        ggplot(df_melt, aes(x = variable, y = value, fill = variable)) +
          geom_boxplot() + theme_minimal()
      }
    })
  })
  
  # ---------------------- 시각화 -----------------------------
  output$var_select <- renderUI({
    df <- if (input$use_cleaned_for_viz) cleaned_data() else raw_data()
    num_vars <- names(df)[sapply(df, is.numeric)]
    tagList(
      selectInput("xvar", "X축 변수", choices = num_vars, selected = num_vars[1]),
      selectInput("yvar", "Y축 변수 (산점도/박스플롯)", choices = num_vars, selected = if(length(num_vars) > 1) num_vars[2] else num_vars[1])
    )
  })
  
  plot_data <- eventReactive(input$draw, {
    if (input$use_cleaned_for_viz) cleaned_data() else raw_data()
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
        geom_boxplot(fill = "#e07a5f") + theme_minimal()
    } else if (plot_type == "산점도" && !is.null(xvar) && !is.null(yvar)) {
      ggplot(df, aes_string(x = xvar, y = yvar)) +
        geom_point(size = 2, color = "#577590") + theme_minimal()
    }
  })
  
  output$missing_plot <- renderPlot({
    req(cleaned_data())
    inspect_missing(cleaned_data())
  })
}

# =========== 앱 실행 ===================
shinyApp(ui, server)
