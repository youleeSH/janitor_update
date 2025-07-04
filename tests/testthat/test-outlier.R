# test-identify_outliers.R

# 필수 패키지 로드
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
library(testthat)

# 함수 불러오기 전제: source("identify_outliers.R") 또는 동일 환경 내 정의되어 있어야 함

test_that("identify_outliers works with IQR method", {
  df <- data.frame(x = c(1:10, 100), y = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 200))
  result <- identify_outliers(df, method = "iqr", column = c("x", "y"), visualize = FALSE)
  
  expect_type(result, "list")
  expect_true("x" %in% names(result$outlier_rows))
  expect_true("y" %in% names(result$outlier_rows))
  expect_true(result$method_used == "iqr")
  expect_true(nrow(result$outlier_rows$x) >= 1)
  expect_true(nrow(result$outlier_rows$y) >= 1)
})

test_that("identify_outliers works with z-score method", {
  set.seed(123)
  df <- data.frame(a = rnorm(100), b = c(rnorm(99), 10))
  result <- identify_outliers(df, method = "zscore", column = c("a", "b"), z_thresh = 3, visualize = FALSE)
  
  expect_type(result, "list")
  expect_true("b" %in% names(result$outlier_rows))
  expect_true(result$method_used == "zscore")
  expect_true(nrow(result$outlier_rows$b) >= 1)
})

test_that("identify_outliers works with percentile method", {
  df <- data.frame(x = c(1:100, 1000))
  result <- identify_outliers(df, method = "percentile", column = "x", lower_percentile = 0.01, upper_percentile = 0.99, visualize = FALSE)
  
  expect_type(result, "list")
  expect_true("x" %in% names(result$outlier_rows))
  expect_true(result$method_used == "percentile")
  expect_true(nrow(result$outlier_rows$x) >= 1)
})

test_that("identify_outliers handles non-numeric columns gracefully", {
  df <- data.frame(a = 1:10, b = letters[1:10])
  result <- identify_outliers(df, method = "iqr", column = c("a", "b"), visualize = FALSE)
  
  expect_true("a" %in% names(result$outlier_rows))
  expect_false("b" %in% names(result$outlier_rows))
})


test_that("identify_outliers respects specified columns", {
  df <- data.frame(x = c(1:10, 100), y = c(1:11))
  result <- identify_outliers(df, method = "iqr", column = "x", visualize = FALSE)
  
  expect_true("x" %in% names(result$outlier_rows))
  expect_false("y" %in% names(result$outlier_rows))
})

test_that("identify_outliers errors on invalid method", {
  df <- data.frame(a = 1:10)
  expect_error(identify_outliers(df, method = "invalid", column = "a", visualize = FALSE), 
               regexp = "must be one of")
})

test_that("identify_outliers errors when df is not a data.frame", {
  expect_error(identify_outliers(1:10), 
               regexp = "must be a data.frame")
})

test_that("identify_outliers errors with non-existent column", {
  df <- data.frame(a = 1:10)
  expect_error(identify_outliers(df, column = "b"), 
               regexp = "does not exist")
})

test_that("identify_outliers handles case with no outliers gracefully", {
  df <- data.frame(a = rep(5, 10))
  result <- identify_outliers(df, method = "iqr", column = "a", visualize = FALSE)
  
  expect_equal(nrow(result$outlier_rows$a), 0)
})

test_that("identify_outliers handles NA values", {
  df <- data.frame(a = c(1:5, NA, 100))
  result <- identify_outliers(df, method = "iqr", column = "a", visualize = FALSE)
  
  expect_type(result$outlier_rows$a, "list")
  expect_true("row" %in% names(result$outlier_rows$a))
  expect_true("value" %in% names(result$outlier_rows$a))
})
