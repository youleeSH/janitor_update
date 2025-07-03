##########################################################
# 권장 패키지 자동설치
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

# 자동 타입 변환 (초기값으로만 사용)
auto_convert_types <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      suppressWarnings({
        numval <- as.numeric(df[[col]])
        if (all(is.na(df[[col]]) | !is.na(numval))) {
          if (all(numval == floor(numval), na.rm = TRUE)) {
            df[[col]] <- as.integer(numval)
          } else {
            df[[col]] <- numval
          }
        } else if (all(tolower(df[[col]]) %in% c("true","false","t","f","yes","no","y","n",NA))) {
          df[[col]] <- as.logical(df[[col]])
        } else if (length(unique(df[[col]])) <= 10) {
          df[[col]] <- as.factor(df[[col]])
        }
      })
    }
  }
  df
}

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
                box(title = "데이터 업로드", width = 12,
                    fileInput("file", "csv/xlsx 데이터 업로드", accept = c(".csv", ".xls", ".xlsx")),
                    numericInput("header_row", "헤더로 사용할 행 번호(1부터)", value = 1, min = 1),
                    checkboxInput("apply_clean_names", "컬럼명 일괄정제(clean_names)", TRUE)
                )
              ),
              fluidRow(
                box(title = "정제 옵션", width = 12,
                    checkboxInput("apply_remove_empty", "빈 행/열 삭제(remove_empty)", TRUE),
                    checkboxInput("apply_remove_constant", "상수 컬럼 삭제(remove_constant)", TRUE),
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
                box(title = "변수 타입 직접 변경", width = 12,
                    uiOutput("type_change_ui"),
                    actionButton("apply_types", "타입 적용하기")
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
                    radioButtons("n_var", "시각화 변수 개수", choices = c("1개", "2개", "3개"), selected = "1개", inline = TRUE),
                    uiOutput("var_select_1"),
                    uiOutput("type_show_1"),
                    uiOutput("slider_var1"),
                    uiOutput("var_select_2"),
                    uiOutput("type_show_2"),
                    uiOutput("slider_var2"),
                    uiOutput("var_select_3"),
                    uiOutput("type_show_3"),
                    selectInput("plot_type", "시각화 유형", 
                                choices = c("자동", "히스토그램", "박스플롯", "산점도", "막대그래프", "비율그래프")),
                    checkboxInput("show_cleaned_plot", "정제 데이터로 시각화", TRUE),
                    actionButton("draw", "시각화 실행")
                ),
                box(title = "그래프", width = 8, plotOutput("plot", height = 400))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # 1. 데이터 불러오기 및 자동 타입 추론
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (tolower(ext) %in% c("xls", "xlsx")) {
      df <- readxl::read_excel(input$file$datapath, .name_repair = "minimal", col_names = FALSE)
    } else if (tolower(ext) == "csv") {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE, header = FALSE, na.strings = c("", "NA"))
    } else {
      showNotification("지원하지 않는 파일 형식입니다.", type = "error")
      return(NULL)
    }
    header_row <- input$header_row
    if (!is.null(header_row) && header_row > 0) {
      df <- janitor::row_to_names(df, row_number = header_row)
    }
    if (isTRUE(input$apply_clean_names)) {
      df <- janitor::clean_names(df)
    }
    df <- df[, !is.na(names(df)) & names(df) != ""]
    df <- auto_convert_types(df)
    df
  })
  
  # 2. 타입 선택 위젯과 상태 저장
  user_types <- reactiveValues(list = NULL)
  observeEvent(raw_data(), {
    # 새 데이터 업로드시 타입 초기화
    df <- raw_data()
    if (!is.null(df)) {
      user_types$list <- sapply(df, function(x) class(x)[1])
    }
  })
  output$type_change_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    types <- user_types$list
    if (is.null(types) || !identical(names(types), names(df))) {
      types <- sapply(df, function(x) class(x)[1])
    }
    user_types$list <- types
    lapply(names(df), function(v) {
      selectInput(
        paste0("type_", v),
        paste0(v, " (", class(df[[v]])[1], ") →"),
        choices = c("numeric", "factor", "character", "logical", "Date"),
        selected = types[v], width = "250px"
      )
    })
  })
  observeEvent(input$apply_types, {
    req(raw_data())
    df <- raw_data()
    # 사용자가 selectInput에서 지정한 타입을 저장
    sel_types <- sapply(names(df), function(v) input[[paste0("type_", v)]])
    user_types$list <- sel_types
  })
  
  # 3. 정제 데이터(타입 적용, 옵션 적용)
  cleaned_data <- reactive({
    req(raw_data())
    df <- raw_data()
    types <- user_types$list
    # 타입 적용
    if (!is.null(types) && identical(names(df), names(types))) {
      for (v in names(df)) {
        tgt <- types[v]
        suppressWarnings({
          if (tgt == "numeric") df[[v]] <- as.numeric(df[[v]])
          else if (tgt == "factor") df[[v]] <- as.factor(df[[v]])
          else if (tgt == "character") df[[v]] <- as.character(df[[v]])
          else if (tgt == "logical") df[[v]] <- as.logical(df[[v]])
          else if (tgt == "Date") df[[v]] <- as.Date(df[[v]])
        })
      }
    }
    if (isTRUE(input$apply_remove_empty)) df <- janitor::remove_empty(df, c("rows", "cols"))
    if (isTRUE(input$apply_remove_constant)) df <- janitor::remove_constant(df, na.rm = TRUE)
    if (isTRUE(input$apply_date_convert) && !is.null(input$date_col) && input$date_col %in% names(df)) {
      df[[input$date_col]] <- janitor::convert_to_date(df[[input$date_col]], character_fun = lubridate::mdy)
    }
    if (isTRUE(input$apply_coalesce) && !is.null(input$coalesce_cols) &&
        length(input$coalesce_cols) > 1 && nzchar(input$coalesce_new_col)) {
      df[[input$coalesce_new_col]] <- dplyr::coalesce(!!!df[input$coalesce_cols])
      if (isTRUE(input$remove_coalesced)) {
        df <- df[, !(names(df) %in% input$coalesce_cols)]
      }
    }
    if (isTRUE(input$apply_remove_dupes) && !is.null(input$dupe_cols) && length(input$dupe_cols) > 0) {
      df <- df[!duplicated(df[input$dupe_cols]), ]
    }
    df
  })
  
  # 4. 데이터 테이블
  output$data_table <- renderDT({
    df <- if (isTRUE(input$show_cleaned)) cleaned_data() else raw_data()
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$date_col_select <- renderUI({
    req(raw_data())
    selectInput("date_col", "날짜 변환 컬럼 선택", choices = names(raw_data()), selected = NULL)
  })
  output$coalesce_col_select <- renderUI({
    req(raw_data())
    selectizeInput("coalesce_cols", "합칠 컬럼 선택(2개이상)", choices = names(raw_data()), multiple = TRUE)
  })
  output$dupe_cols_select <- renderUI({
    req(raw_data())
    selectizeInput("dupe_cols", "중복 체크 기준 컬럼(1개 이상 선택)", choices = names(raw_data()), multiple = TRUE)
  })
  observeEvent(input$find_dupes, {
    req(input$dupe_cols, length(input$dupe_cols) >= 1)
    df <- cleaned_data()
    dupe_tbl <- janitor::get_dupes(df, !!!rlang::syms(input$dupe_cols))
    output$dupes_table <- renderDT({
      if (nrow(dupe_tbl) > 0) datatable(dupe_tbl) else datatable(data.frame(메시지 = "중복 없음"))
    })
  })
  
  # 5. 시각화 변수 선택 옵션
  get_data_for_plot <- reactive({
    if (isTRUE(input$show_cleaned_plot)) cleaned_data() else raw_data()
  })
  
  make_var_select <- function(varid, label) {
    renderUI({
      req(get_data_for_plot())
      selectInput(varid, label, choices = names(get_data_for_plot()), selected = names(get_data_for_plot())[1])
    })
  }
  make_type_show <- function(varid) {
    renderUI({
      req(get_data_for_plot(), input[[varid]])
      type <- class(get_data_for_plot()[[input[[varid]]]])[1]
      HTML(paste0("<b>", input[[varid]], "</b> 타입: <span style='color:#0072B2'>", type, "</span>"))
    })
  }
  output$var_select_1 <- make_var_select("var1", "변수 1 (필수)")
  output$type_show_1 <- make_type_show("var1")
  output$var_select_2 <- renderUI({
    req(get_data_for_plot())
    if (input$n_var %in% c("2개", "3개")) {
      selectInput("var2", "변수 2", choices = names(get_data_for_plot()), selected = names(get_data_for_plot())[2])
    }
  })
  output$type_show_2 <- renderUI({
    req(get_data_for_plot(), input$n_var %in% c("2개", "3개"), input$var2)
    type <- class(get_data_for_plot()[[input$var2]])[1]
    HTML(paste0("<b>", input$var2, "</b> 타입: <span style='color:#0072B2'>", type, "</span>"))
  })
  output$var_select_3 <- renderUI({
    req(get_data_for_plot())
    if (input$n_var == "3개") {
      selectInput("var3", "변수 3", choices = names(get_data_for_plot()), selected = names(get_data_for_plot())[3])
    }
  })
  output$type_show_3 <- renderUI({
    req(get_data_for_plot(), input$n_var == "3개", input$var3)
    type <- class(get_data_for_plot()[[input$var3]])[1]
    HTML(paste0("<b>", input$var3, "</b> 타입: <span style='color:#0072B2'>", type, "</span>"))
  })
  
  # 슬라이더도 그대로 추가 (생략 가능, 이전 코드 참고)
  output$slider_var1 <- renderUI({
    req(input$var1, get_data_for_plot())
    x <- get_data_for_plot()[[input$var1]]
    if (is.numeric(x) && length(unique(x[!is.na(x)])) > 10) {
      rng <- range(x, na.rm = TRUE)
      sliderInput("xrange", "X축 값 범위", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
    }
  })
  output$slider_var2 <- renderUI({
    req(input$n_var %in% c("2개", "3개"), input$var2, get_data_for_plot())
    x <- get_data_for_plot()[[input$var2]]
    if (is.numeric(x) && length(unique(x[!is.na(x)])) > 10) {
      rng <- range(x, na.rm = TRUE)
      sliderInput("yrange", "Y축 값 범위", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
    }
  })
  
  plot_data <- eventReactive(input$draw, { req(get_data_for_plot()); get_data_for_plot() })
  
  output$plot <- renderPlot({
    req(plot_data(), input$var1)
    df <- plot_data()
    nvar <- input$n_var
    v1 <- input$var1
    v2 <- input$var2
    v3 <- input$var3
    plt_type <- input$plot_type
    if (!is.null(input$xrange) && is.numeric(df[[v1]])) df <- df[df[[v1]] >= input$xrange[1] & df[[v1]] <= input$xrange[2], , drop=FALSE]
    if (!is.null(input$yrange) && !is.null(v2) && is.numeric(df[[v2]])) df <- df[df[[v2]] >= input$yrange[1] & df[[v2]] <= input$yrange[2], , drop=FALSE]
    # 시각화 로직은 이전과 동일하게!
    if (nvar == "1개") {
      v <- v1
      if (plt_type == "자동") {
        if (is.numeric(df[[v]])) {
          ggplot(df, aes_string(x = v)) + geom_histogram(fill = "#69b3a2", color = "white") + theme_minimal()
        } else {
          ggplot(df, aes_string(x = v)) + geom_bar(fill = "#457b9d") + theme_minimal()
        }
      } else if (plt_type == "히스토그램" && is.numeric(df[[v]])) {
        ggplot(df, aes_string(x = v)) + geom_histogram(fill = "#69b3a2", color = "white") + theme_minimal()
      } else if (plt_type == "막대그래프" && (is.factor(df[[v]]) || is.character(df[[v]]))) {
        ggplot(df, aes_string(x = v)) + geom_bar(fill = "#457b9d") + theme_minimal()
      } else if (plt_type == "비율그래프" && (is.factor(df[[v]]) || is.character(df[[v]]))) {
        tb <- as.data.frame(table(df[[v]]))
        colnames(tb) <- c("group", "n")
        tb$prop <- tb$n / sum(tb$n)
        ggplot(tb, aes(x = group, y = prop, fill = group)) + geom_bar(stat = "identity") +
          ylab("비율") + theme_minimal() + guides(fill = FALSE)
      } else {
        plot.new(); text(0.5, 0.5, "만들 수 없는 플롯입니다", cex=2, col="red")
      }
    }
    else if (nvar == "2개" && !is.null(v2)) {
      if (plt_type == "자동") {
        if (is.numeric(df[[v1]]) && is.numeric(df[[v2]])) {
          ggplot(df, aes_string(x = v1, y = v2)) + geom_point(color = "#2a9d8f") + theme_minimal()
        } else if ((is.factor(df[[v1]]) || is.character(df[[v1]])) && is.numeric(df[[v2]])) {
          ggplot(df, aes_string(x = v1, y = v2)) + geom_boxplot(fill = "#e07a5f") + theme_minimal()
        } else if ((is.factor(df[[v2]]) || is.character(df[[v2]])) && is.numeric(df[[v1]])) {
          ggplot(df, aes_string(x = v2, y = v1)) + geom_boxplot(fill = "#e07a5f") + theme_minimal()
        } else {
          plot.new(); text(0.5, 0.5, "만들 수 없는 플롯입니다", cex=2, col="red")
        }
      } else if (plt_type == "박스플롯" && is.numeric(df[[v2]]) && (is.factor(df[[v1]]) || is.character(df[[v1]]))) {
        ggplot(df, aes_string(x = v1, y = v2)) + geom_boxplot(fill = "#e07a5f") + theme_minimal()
      } else if (plt_type == "산점도" && is.numeric(df[[v1]]) && is.numeric(df[[v2]])) {
        ggplot(df, aes_string(x = v1, y = v2)) + geom_point(color = "#2a9d8f") + theme_minimal()
      } else {
        plot.new(); text(0.5, 0.5, "만들 수 없는 플롯입니다", cex=2, col="red")
      }
    }
    else if (nvar == "3개" && !is.null(v2) && !is.null(v3)) {
      if (is.numeric(df[[v1]]) && is.numeric(df[[v2]]) && (is.factor(df[[v3]]) || is.character(df[[v3]]))) {
        ggplot(df, aes_string(x = v1, y = v2, color = v3)) +
          geom_point(size = 2) + theme_minimal()
      } else if (is.numeric(df[[v1]]) && is.numeric(df[[v2]]) && is.numeric(df[[v3]])) {
        ggplot(df, aes_string(x = v1, y = v2, size = v3)) +
          geom_point(alpha = 0.7, color = "#2196f3") + theme_minimal()
      } else if ((is.numeric(df[[v1]]) && is.numeric(df[[v2]])) && (is.factor(df[[v3]]) || is.character(df[[v3]]))) {
        ggplot(df, aes_string(x = v1, y = v2)) +
          geom_point() + facet_wrap(as.formula(paste("~", v3))) + theme_minimal()
      } else if ((is.factor(df[[v1]]) || is.character(df[[v1]])) &&
                 (is.factor(df[[v2]]) || is.character(df[[v2]])) &&
                 is.numeric(df[[v3]])) {
        ggplot(df, aes_string(x = v1, y = v3, fill = v2)) +
          geom_boxplot(position = position_dodge(width = 0.8)) + theme_minimal()
      } else {
        plot.new(); text(0.5, 0.5, "만들 수 없는 플롯입니다", cex=2, col="red")
      }
    } else {
      plot.new(); text(0.5, 0.5, "만들 수 없는 플롯입니다", cex=2, col="red")
    }
  })
}

shinyApp(ui, server)

#####################################################3
##########################################################

