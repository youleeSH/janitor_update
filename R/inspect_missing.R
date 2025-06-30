#' Visualize the percentage of missing values for each variable
#'
#' This function calculates the proportion of missing (NA) values for each column
#' in a data frame and visualizes the results as a horizontal bar plot using ggplot2.
#' Useful for quickly identifying variables with a high proportion of missing data.
#'
#' @param df A `data.frame` (or tibble) whose missing values you want to visualize.
#' @param sort Logical. If TRUE (default), variables are sorted in descending order of missingness.
#' @param top_n Optional. If provided, only the top_n variables with the highest percentage of missing values are shown.
#'
#' @return A `ggplot` object showing the percentage of missing values per variable.
#' @export
#'
#' @examples
#' # Example with base data
#' df <- data.frame(
#'   A = c(1, NA, 3),
#'   B = c(NA, NA, 6),
#'   C = c(7, 8, 9),
#'   stringsAsFactors = FALSE
#' )
#' inspect_missing(df)
#'
#' # Example with mtcars (with simulated missing values)
#' df2 <- mtcars
#' df2[c(1, 3), "mpg"] <- NA
#' inspect_missing(df2)
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


