inspect_missing <- function(df, sort = TRUE, top_n = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install ggplot2 to use this function.")
  }
  
  miss_pct <- sapply(df, function(x) mean(is.na(x)))
  miss_df <- data.frame(
    variable = names(miss_pct),
    missing_pct = miss_pct,
    stringsAsFactors = FALSE
  )
  
  if (sort) {
    miss_df <- miss_df[order(-miss_df$missing_pct), ]
  }
  
  if (!is.null(top_n)) {
    miss_df <- head(miss_df, top_n)
  }
  
  ggplot2::ggplot(miss_df, ggplot2::aes(x = reorder(variable, -missing_pct), y = missing_pct)) +
    ggplot2::geom_bar(stat = "identity", fill = "tomato") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Missing Value Percentage by Variable",
                  x = "Variable", y = "Missing %") +
    ggplot2::theme_minimal()
}


