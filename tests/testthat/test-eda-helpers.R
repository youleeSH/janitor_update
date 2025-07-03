#test clean levels
test_that("clean_levels collapses rare levels correctly", {
  cleaned <- clean_levels(titanic, Embarked, min_freq = 100, other_label = "Other")
  
  expect_s3_class(cleaned$Embarked, "factor")
  expect_true("Other" %in% levels(cleaned$Embarked))
  expect_equal(nrow(cleaned), nrow(titanic))
})

test_that("clean_levels errors on non-character/factor column", {
  expect_error(
    clean_levels(titanic, Age),
    regexp = "`col` must be a factor or character variable\\."  # 정규표현식 escape
  )
})


#test summarize factors
test_that("summarize_factors returns list with top_n summaries", {
  summaries <- summarize_factors(titanic, top_n = 2)
  
  expect_type(summaries, "list")
  expect_true("Sex" %in% names(summaries))
  
  summary_tbl <- summaries$Sex$summary
  expect_s3_class(summary_tbl, "data.frame")
  expect_lte(nrow(summary_tbl), 2)
  expect_true(all(c("vars", "n", "percent") %in% names(summary_tbl)))
})


#test identiry duplicates
test_that("identify_duplicates returns proper summary output", {
  expect_output(
    identify_duplicates(titanic, by = "Ticket", return = "summary"),
    regexp = "총 중복 행: [0-9]+ \\([0-9\\.]+%\\)"
  )
})

test_that("identify_duplicates returns data.frame of duplicates", {
  dup_rows <- identify_duplicates(titanic, by = "Ticket", return = "data")
  
  expect_s3_class(dup_rows, "data.frame")
  expect_true(nrow(dup_rows) > 0)
})

test_that("identify_duplicates returns grouped duplicates when specified", {
  dup_grouped <- identify_duplicates(titanic, by = "Ticket", return = "grouped")
  
  expect_s3_class(dup_grouped, "data.frame")
  expect_gt(nrow(dup_grouped), 0)
  expect_true("Ticket" %in% names(dup_grouped))
})


#test summarize numeric
test_that("summarize_numeric outputs correct numeric summary", {
  result <- summarize_numeric(titanic)
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("variable", "mean", "sd", "min", "max", "na_pct") %in% names(result)))
  expect_true(nrow(result) >= 1)
})

test_that("summarize_numeric errors when no numeric columns present", {
  expect_error(
    summarize_numeric(data.frame(name = c("a", "b"))),
    regexp = "No numeric columns found"
  )
})

