df_test <- data.frame(
  A = c(1, NA, 3),
  B = c(NA, NA, 6),
  C = c(7, 8, 9),
  stringsAsFactors = FALSE
)

test_that("inspect_missing returns a ggplot object without error", {
  plot <- inspect_missing(df_test)
  expect_true(inherits(plot, "ggplot"))
})

test_that("inspect_missing sorts and filters top_n correctly", {
  plot_top2 <- inspect_missing(df_test, sort = TRUE, top_n = 2)
  vars <- as.character(plot_top2$data$variable)
  expect_equal(vars, c("B", "A"))
  expect_equal(nrow(plot_top2$data), 2)
})

test_that("inspect_missing without sorting and top_n", {
  plot_no_sort <- inspect_missing(df_test, sort = FALSE, top_n = NULL)
  expect_equal(as.character(plot_no_sort$data$variable), c("A", "B", "C"))
  expect_equal(nrow(plot_no_sort$data), 3)
})


library(ggplot2)

df <- mtcars
df[c(1, 3), "mpg"] <- NA
df[5:10, "hp"] <- NA
df[15, "drat"] <- NA

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
plot <- inspect_missing(df)
print(plot)