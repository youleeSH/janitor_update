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
