#' Identify and visualize outliers in numerical variables
#'
#' @description
#' Detect outliers in one or more numeric columns of a data frame using the IQR, z-score,
#' or percentile method. Optionally, visualize the results with boxplots.
#'
#' If `visualize = TRUE`, boxplots are shown using `ggplot2`, arranged horizontally using `patchwork`.
#' Outlier detection methods supported:
#' - `"iqr"`: Outside 1.5 * IQR
#' - `"zscore"`: Absolute z-score > threshold (default = 3)
#' - `"percentile"`: Outside given percentiles
#'
#' @param df A data.frame or tibble containing numeric columns to analyze.
#' @param method Outlier detection method: "iqr", "zscore", or "percentile".
#' @param column A character vector of column names (must be numeric).
#' @param z_thresh Threshold for the z-score method (default: 3).
#' @param lower_percentile Lower bound (e.g., 0.01) for percentile method.
#' @param upper_percentile Upper bound (e.g., 0.99) for percentile method.
#' @param visualize If TRUE (default), display boxplots for each selected column.
#'
#' @return An invisible list with:
#' \describe{
#'   \item{outlier_rows}{List of data.frames showing outlier values per column.}
#'   \item{method_used}{The method used for outlier detection.}
#' }
#' @examples
#' identify_outliers(airquality, method = "iqr", column = c("Wind", "Temp"))
#'
#' @export

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
  
  if (!is.data.frame(df)) stop("Input must be a data.frame or tibble.")
  
  cat("📌 Data types of the dataframe columns:\n")
  print(sapply(df, class))
  
  if (missing(column)) stop("❗ Argument 'column' must be specified.")
  if (any(!column %in% names(df))) {
    stop("❗ One or more specified columns do not exist in the dataframe.")
  }
  
  method <- match.arg(method)
  outlier_results <- list()
  plots <- list()
  
  for (col in column) {
    cat(paste0("\n🔍 Processing column: ", col, "\n"))
    
    vec <- df[[col]]
    if (!is.numeric(vec)) {
      warning(paste0("⚠️ Column '", col, "' is not numeric and will be skipped."))
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
    
    cat(paste0("✅ Found ", nrow(outlier_values), " outliers in column '", col, "':\n"))
    print(outlier_values)
    
    outlier_results[[col]] <- outlier_values
    
    if (visualize) {
      p <- ggplot(df, aes_string(y = col)) +
        geom_boxplot(outlier.colour = "red", fill = "skyblue", alpha = 0.6, na.rm = TRUE) +
        labs(title = paste("Boxplot of", col), y = col) +
        theme_minimal()
      plots[[col]] <- p
    }
  }
  
  if (visualize && length(plots) > 0) {
    combined_plot <- Reduce(`|`, plots)
    print(combined_plot)
  }
  
  return(invisible(list(
    outlier_rows = outlier_results,
    method_used = method
  )))
}