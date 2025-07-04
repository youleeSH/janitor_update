identify_outliers <- function(df, 
                              method = c("iqr", "zscore", "percentile"), 
                              column, 
                              z_thresh = 3, 
                              lower_percentile = 0.01, 
                              upper_percentile = 0.99,
                              visualize = TRUE) {

  required_packages <- c("ggplot2", "dplyr", "patchwork")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
  
  if (!is.data.frame(df)) stop("입력은 data.frame 또는 tibble 이어야 합니다.")
  
  cat("📌 데이터프레임 변수들의 타입:\n")
  print(sapply(df, class))
  
  if (missing(column)) stop("❗ 'column' 인자를 지정해야 합니다.")
  if (any(!column %in% names(df))) {
    stop("❗ 지정한 변수들 중 데이터프레임에 존재하지 않는 변수가 있습니다.")
  }
  
  method <- match.arg(method)
  outlier_results <- list()
  plots <- list()
  
  for (col in column) {
    cat(paste0("\n🔍 변수 처리 중: ", col, "\n"))
    
    vec <- df[[col]]
    if (!is.numeric(vec)) {
      warning(paste0("⚠️ '", col, "' 변수는 수치형이 아니므로 건너뜁니다."))
      next
    }
    
    is_outlier <- rep(FALSE, length(vec))
    
    if (method == "iqr") {
      Q1 <- quantile(vec, 0.25, na.rm = TRUE)
      Q3 <- quantile(vec, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      is_outlier <- vec < lower | vec > upper
      
    } else if (method == "zscore") {
      z <- scale(vec)
      is_outlier <- abs(z) > z_thresh
      
    } else if (method == "percentile") {
      lower <- quantile(vec, lower_percentile, na.rm = TRUE)
      upper <- quantile(vec, upper_percentile, na.rm = TRUE)
      is_outlier <- vec < lower | vec > upper
    }
    
    outlier_idx <- which(is_outlier %in% TRUE)
    outlier_values <- data.frame(
      row = outlier_idx,
      value = vec[outlier_idx]
    )
    
    cat(paste0("✅ '", col, "' 변수에서 ", nrow(outlier_values), "개의 이상치가 탐지되었습니다:\n"))
    print(outlier_values)
    
    outlier_results[[col]] <- outlier_values
    
    # ✅ 박스플롯 객체만 만들어 저장
    if (visualize) {
      p <- ggplot(df, aes_string(y = col)) +
        geom_boxplot(outlier.colour = "red", fill = "skyblue", alpha = 0.6, na.rm = TRUE) +
        labs(title = paste("Boxplot of", col), y = col) +
        theme_minimal()
      plots[[col]] <- p
    }
  }
  
  # ✅ 화면에 한 번에 여러 박스플롯 출력
  if (visualize && length(plots) > 0) {
    combined_plot <- Reduce(`|`, plots)  # 좌우로 배치
    print(combined_plot)
  }
  
  return(invisible(list(
    outlier_rows = outlier_results,
    method_used = method
  )))
}
