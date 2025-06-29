install.packages("testthat")
library(testthat)

test_that("identify_outliers works with IQR method", {
  df <- data.frame(x = c(1:10, 100), y = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 200))
  result <- identify_outliers(df, method = "iqr")
  
  expect_true("x" %in% colnames(result))
  expect_true("y" %in% colnames(result))
  expect_true("outlier_row" %in% colnames(result))
  expect_equal(nrow(result), nrow(df))
  expect_true(any(result$outlier_row))
}) #iqr 정상작동여부

test_that("identify_outliers works with z-score method", {
  set.seed(123)
  df <- data.frame(a = rnorm(100), b = c(rnorm(99), 10))
  result <- identify_outliers(df, method = "zscore", z_thresh = 3)
  
  expect_equal(nrow(result), nrow(df))
  expect_true("b" %in% colnames(result))
  expect_true(any(result$outlier_row))
}) #z-score 정상작동여부

test_that("identify_outliers works with percentile method", {
  df <- data.frame(x = c(1:100, 1000))
  result <- identify_outliers(df, method = "percentile", lower_percentile = 0.01, upper_percentile = 0.99)
  
  expect_equal(nrow(result), nrow(df))
  expect_true(any(result$outlier_row))
}) #백분위수 정상작동여부

test_that("identify_outliers handles non-numeric columns gracefully", {
  df <- data.frame(a = 1:10, b = letters[1:10])
  result <- identify_outliers(df, method = "iqr")
  
  expect_true("a" %in% colnames(result))
  expect_false("b" %in% colnames(result))
}) #비수치형 열 무시

test_that("identify_outliers respects specified columns", {
  df <- data.frame(x = c(1:10, 100), y = c(1:11))
  result <- identify_outliers(df, method = "iqr", columns = "x")
  
  expect_true("x" %in% colnames(result))
  expect_false("y" %in% colnames(result))
  expect_equal(nrow(result), 11)
}) #특정 열만 지정

test_that("identify_outliers errors on invalid method", {
  df <- data.frame(a = 1:10)
  expect_error(identify_outliers(df, method = "invalid"), 
               regexp = "should be one of")
}) #잘못된 method 입력

test_that("identify_outliers errors when df is not a data.frame", {
  expect_error(identify_outliers(1:10), 
               regexp = "df must be a data.frame")
}) #df가 데이터 프레임이 아닐때

test_that("identify_outliers errors with non-existent column", {
  df <- data.frame(a = 1:10)
  expect_error(identify_outliers(df, columns = "b"), 
               regexp = "Column 'b' does not exist")
}) #존재하지 않는 열 이름 입력

test_that("identify_outliers handles no outliers gracefully", {
  df <- data.frame(a = rep(5, 10))
  result <- identify_outliers(df, method = "iqr")
  
  expect_equal(sum(result$outlier_row), 0)
}) #이상치가 없는 경우

test_that("identify_outliers handles NA values", {
  df <- data.frame(a = c(1:5, NA, 100))
  result <- identify_outliers(df, method = "iqr")
  
  expect_equal(nrow(result), nrow(df))
  expect_true("outlier_row" %in% names(result))
}) #NA가 포함된 열 처리
