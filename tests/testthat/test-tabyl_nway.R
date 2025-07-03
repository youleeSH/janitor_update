# Test: tabyl_nway works correctly with 4+ variables
test_that("tabyl_nway works with 4+ variables (without filling missing)", {
  df <- data.frame(
    v1 = c("A", "A", "B", "B"),
    v2 = c("X", "X", "Y", "Y"),
    v3 = c("I", "J", "I", "J"),
    v4 = c(1, 1, 2, 2)
  )
  
  result <- tabyl_nway(df, v1, v2, v3, v4, show_missing_levels = FALSE)
  
  expect_s3_class(result, "tabyl_nway")
  expect_equal(nrow(result), 4)
  expect_equal(colnames(result), c("v1", "v2", "v3", "v4", "n"))
})


# Test: show_na = FALSE excludes rows with NA in any input column
test_that("tabyl_nway respects show_na = FALSE", {
  df <- data.frame(
    a = c("x", "x", NA),
    b = c("y", NA, "y"),
    c = c("z", "z", "z"),
    d = c("k", "k", "k")
  )
  
  result <- tabyl_nway(df, a, b, c, d, show_na = FALSE)
  
  expect_true(!any(is.na(result)))
  expect_equal(nrow(result), 1)
})

# Test: show_missing_levels = TRUE includes zero-count combinations
test_that("tabyl_nway includes zero-count combinations with show_missing_levels = TRUE", {
  df <- data.frame(
    x = factor(c("A", "B")),
    y = factor(c("M", "M")),
    z = factor(c("Q", "Q")),
    w = factor(c("T", "S"))
  )
  
  # Missing combination: B-M-Q-S
  result <- tabyl_nway(df, x, y, z, w, show_missing_levels = TRUE)
  
  expect_true("B" %in% result$x)
  expect_true(any(result$n == 0))  # zero-count row included
  expect_equal(ncol(result), 5)
})

