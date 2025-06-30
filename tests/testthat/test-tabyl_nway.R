# 4개 이상의 변수를 사용하는 tabyl_nway 함수가 제대로 작동하는지 테스트
test_that("tabyl_nway works with 4+ variables", {
  df <- data.frame(
    v1 = c("A", "A", "B", "B"),
    v2 = c("X", "X", "Y", "Y"),
    v3 = c("I", "J", "I", "J"),
    v4 = c(1, 1, 2, 2)
  )
  
  result <- tabyl_nway(df, v1, v2, v3, v4)
  
  expect_s3_class(result, "tabyl_nway")
  expect_equal(nrow(result), 4)
  expect_equal(colnames(result), c("v1", "v2", "v3", "v4", "n"))
})

# show_na = FALSE 옵션이 tabyl_nway에 잘 적용되어 NA가 결과에 포함되지 않는지 테스트
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
