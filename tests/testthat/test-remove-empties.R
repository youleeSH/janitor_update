dat <- data.frame(
  A = c(NA, NA, NA, NA, 1),
  B = c(1, 2, 3, 4, 5),
  C = c(NA, NA, 3, 4, 5),
  D = c(NA, 4, 6, 3, 1),
  E = c(NA, NA, 1, 2, 3)
)

test_that("empty rows are removed", {
  expect_equal(remove_empty(dat, "rows", cutoff = 0.8), dat[2:5, ])
})

test_that("empty cols are removed", {
  expect_equal(remove_empty(dat, "cols", cutoff = 0.8), dat[, c("B", "C", "D", "E")])
})

test_that("bad argument to which throws error", {
  expect_error(
    mtcars %>%
      remove_empty("blargh"),
    paste0('"which" must be one of "rows", "cols", or c("rows", "cols")'),
    fixed = TRUE
  )
})

test_that("missing argument to which defaults to both, printing a message", {
  expect_message(
    result <- dat %>% remove_empty(),
    "value for \"which\" not specified, defaulting to c(\"rows\", \"cols\")",
    fixed = TRUE
  )
  expect_equal(
    result,
    dat %>% remove_empty(c("rows", "cols"))
  )
})

test_that("missing data.frame input throws its error before messages about 'which' arg", {
  expect_error(remove_empty(),
               "argument \"dat\" is missing, with no default",
               fixed = TRUE
  )
})


test_that("remove_empty leaves matrices as matrices", {
  mat <- matrix(c(NA, NA, NA, rep(0, 3)), ncol = 2, byrow = TRUE)
  expect_message(
    expect_equal(
      remove_empty(mat), matrix(c(NA, rep(0, 3)), ncol = 2),
      info = "remove_empty with a matrix returns a matrix"
    ),
    regexp = 'value for "which" not specified, defaulting to c("rows", "cols")',
    fixed = TRUE
  )
})

test_that("remove_empty leaves single-column results as the original class", {
  mat <- matrix(c(NA, NA, NA, 0), ncol = 2, byrow = FALSE)
  expect_equal(
    remove_empty(mat, which = c("rows", "cols")),
    matrix(0, ncol = 1),
    info = "remove_empty with a matrix that should return a single row and column still returns a matrix"
  )
  df <- data.frame(A = NA, B = c(NA, 0))
  expect_equal(
    remove_empty(df, which = c("rows", "cols")),
    data.frame(B = 0, row.names = 2L),
    info = "remove_empty with a data.frame that should return a single row and column still returns a data.frame"
  )
})

test_that("remove_empty single-column input results as the original class", {
  mat <- matrix(c(NA, NA, NA, 0), ncol = 1, byrow = FALSE)
  expect_equal(
    remove_empty(mat, which = c("rows", "cols")),
    matrix(0, ncol = 1),
    info = "remove_empty with a matrix that should return a single row and column still returns a matrix"
  )
  df <- data.frame(B = c(NA, 0))
  expect_equal(
    remove_empty(df, which = c("rows", "cols")),
    data.frame(B = 0, row.names = 2L),
    info = "remove_empty with a data.frame that should return a single row and column still returns a data.frame"
  )
})

test_that("Messages are accurate with remove_empty and remove_constant", {
  expect_message(
    remove_empty(data.frame(A = NA, B = 1), which = "cols", quiet = FALSE),
    regexp = "Removing 1 empty or high-NA columns of 2 columns total (Removed: A).",
    fixed = TRUE
  )
  expect_message(
    remove_empty(data.frame(A = NA, B = 1, C = NA), which = "cols", quiet = FALSE),
    regexp = "Removing 2 empty or high-NA columns of 3 columns total (Removed: A, C).",
    fixed = TRUE
  )
  expect_message(
    remove_empty(data.frame(A = NA, B = c(1, NA)), which = "rows", quiet = FALSE),
    regexp = "Removing 1 empty or high-NA rows of 2 rows total (50%).",
    fixed = TRUE
  )
  expect_message(
    remove_empty(matrix(c(NA, NA, 1, NA), nrow = 2), which = "cols", quiet = FALSE),
    regexp = "Removing 1 empty or high-NA columns of 2 columns total (50%).",
    fixed = TRUE
  )
  expect_message(
    remove_constant(matrix(c(NA, NA, 1, NA), nrow = 2), quiet = FALSE),
    regexp = "Removing 1 constant columns of 2 columns total (50%).",
    fixed = TRUE,
    info = "Unnamed, constant columns"
  )
  expect_silent(
    remove_empty(data.frame(A = NA, B = 1), which = "cols", quiet = TRUE)
  )
  expect_silent(
    remove_empty(data.frame(A = NA, B = c(1, NA)), which = "rows", quiet = TRUE)
  )
  expect_message(
    remove_constant(mtcars, quiet = FALSE),
    regexp = "No constant columns to remove.",
    fixed = TRUE,
    info = "No constant columns to remove"
  )
  expect_message(
    expect_message(
      remove_empty(mtcars, quiet = FALSE, which = c("rows", "cols")),
      regexp = "No empty or high-NA columns to remove.",
      fixed = TRUE
    ),
    regexp = "No empty or high-NA rows to remove.",
    fixed = TRUE
  )
})

test_that("remove_empty cutoff tests", {
  dat <-
    data.frame(
      A = rep(NA, 10),
      B = c(1, 1, rep(NA, 8)),
      C = c(rep(1, 8), NA, NA),
      D = c(rep(1, 9), NA),
      E = 1
    )
  expect_equal(
    remove_empty(dat, which = c("rows", "cols")),
    remove_empty(dat, cutoff = 1, which = c("rows", "cols"))
  )
  expect_equal(
    remove_empty(dat, cutoff = 1, which = "rows"),
    dat
  )
  expect_equal(
    remove_empty(dat, cutoff = 0.8, which = "rows"),
    dat[1:9, ]
  )
  expect_equal(
    remove_empty(dat, cutoff = 0.6, which = "rows"),
    dat[1:8, ]
  )
  expect_equal(
    remove_empty(dat, cutoff = 0.2, which = "rows"),
    dat[c(), ]
  )
  expect_equal(
    remove_empty(dat, cutoff = 1, which = "cols"),
    dat[, c("B", "C", "D", "E")]
  )
  expect_equal(
    remove_empty(dat, cutoff = 0.8, which = "cols"),
    dat[, c("C", "D", "E"), drop = FALSE]
  )
  expect_equal(
    remove_empty(dat, cutoff = 0.2, which = "cols"),
    dat[, c("D", "E"), drop = FALSE]
  )
})

test_that("remove_empty cutoff errors", {
  expect_error(
    remove_empty(cutoff = c(0.1, 0.2)),
    regexp = "cutoff must be a single numeric value"
  )
  expect_error(
    remove_empty(cutoff = "A"),
    regexp = "cutoff must be a single numeric value"
  )
  expect_error(
    remove_empty(cutoff = 0),
    regexp = "cutoff must be >0 and <= 1"
  )
  expect_error(
    remove_empty(cutoff = 1.1),
    regexp = "cutoff must be >0 and <= 1"
  )
  expect_error(
    remove_empty(cutoff = 0.9),
    regexp = "cutoff must be used with only one of which = 'rows' or 'cols', not both"
  )
  expect_error(
    remove_empty(cutoff = 0.9, which = c("rows", "cols")),
    regexp = "cutoff must be used with only one of which = 'rows' or 'cols', not both"
  )
})
