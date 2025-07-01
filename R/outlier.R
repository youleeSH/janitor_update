#' Identify outliers in numeric columns of a data frame.
#'
#' This function detects outliers in the specified columns of a data frame using one of three methods:
#' Interquartile Range (IQR), Z-score, or percentile-based thresholds. The result is a logical data.frame
#' indicating outlier status for each cell and summary columns indicating total outliers per row.
#'
#' @param df A data.frame or tibble containing the data to be analyzed.
#' @param method The method to use for outlier detection. Options are:
#'   - `"iqr"`: Values beyond 1.5 * IQR from Q1 and Q3.
#'   - `"zscore"`: Absolute Z-score greater than `z_thresh`.
#'   - `"percentile"`: Values below or above the given lower and upper percentiles.
#' @param columns Optional character vector specifying which numeric columns to analyze.
#'   If NULL (default), all numeric columns are used.
#' @param z_thresh Threshold for Z-score method (default is 3).
#' @param lower_percentile Lower bound percentile for the percentile method (default is 0.01).
#' @param upper_percentile Upper bound percentile for the percentile method (default is 0.99).
#'
#' @return A data.frame with:
#'   - One logical column per target variable indicating outlier status (`TRUE`/`FALSE`)
#'   - A `total_outliers` column summing how many variables are outliers for each row
#'   - An `outlier_row` column marking rows that contain at least one outlier
#'
#' @examples
#' df <- data.frame(x = c(1, 2, 3, 100), y = c(5, 6, 7, 200))
#' identify_outliers(df, method = "iqr")
#'
#' @export
#' 


identify_outliers <- function(df, method = c("iqr", "zscore", "percentile"), 
                              columns = NULL, z_thresh = 3, 
                              lower_percentile = 0.01, upper_percentile = 0.99) {
  method <- match.arg(method)
  
  if (!is.data.frame(df)) stop("df must be a data.frame or tibble.")
  if (is.null(columns)) {
    numeric_cols <- sapply(df, is.numeric)
    columns <- names(df)[numeric_cols]
  }
  
  outlier_flags <- data.frame(row = 1:nrow(df))
  
  for (col in columns) {
    if (!col%in% names(df)){
      stop(paste0("Column '",col,"' does not exist in the data frame."))
    }
    vec <- df[[col]]
    
    if (method == "iqr") {
      Q1 <- quantile(vec, 0.25, na.rm = TRUE)
      Q3 <- quantile(vec, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      outlier_flags[[col]] <- vec < lower | vec > upper
      
    } else if (method == "zscore") {
      z <- scale(vec)
      outlier_flags[[col]] <- abs(z) > z_thresh
      
    } else if (method == "percentile") {
      lower <- quantile(vec, lower_percentile, na.rm = TRUE)
      upper <- quantile(vec, upper_percentile, na.rm = TRUE)
      outlier_flags[[col]] <- vec < lower | vec > upper
    }
  }
  
  outlier_flags$total_outliers <- rowSums(outlier_flags[ , -1,drop=FALSE], na.rm = TRUE)
  outlier_flags$outlier_row <- outlier_flags$total_outliers > 0
  
  return(outlier_flags)
}
