context("test-col_by")

test_that("factor_to_matrix_by works", {
  x <- factor(c("a", "b", "a", "a", "b"))
  expect_equal(
    factor_to_matrix_by(x),
    data.frame(
      a = c(TRUE, FALSE, TRUE, TRUE, FALSE), 
      b = c(FALSE, TRUE, FALSE, FALSE, TRUE)
    )
  )
})


test_that("add_total_by works", {
  x <- factor(c("a", "b", "a", "a", "b"))
  expect_equal(
    add_total_by(factor_to_matrix_by(x), label = "tot"),
    data.frame(
      a = c(TRUE, FALSE, TRUE, TRUE, FALSE), 
      b = c(FALSE, TRUE, FALSE, FALSE, TRUE), 
      tot = c(TRUE, TRUE, TRUE, TRUE, TRUE)
    )
  )
})