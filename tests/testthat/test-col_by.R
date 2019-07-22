context("test-col_by")

test_that("by_factor_to_matrix works", {
  x <- factor(c("a", "b", "a", "a", "b"))
  expect_equal(
    by_factor_to_matrix(x),
    data.frame(
      a = c(TRUE, FALSE, TRUE, TRUE, FALSE), 
      b = c(FALSE, TRUE, FALSE, FALSE, TRUE)
    )
  )
})

test_that("by_add_total works", {
  x <- factor(c("a", "b", "a", "a", "b"))
  expect_equal(
    by_add_total(by_factor_to_matrix(x), label = "tot"),
    data.frame(
      a = c(TRUE, FALSE, TRUE, TRUE, FALSE), 
      b = c(FALSE, TRUE, FALSE, FALSE, TRUE), 
      tot = c(TRUE, TRUE, TRUE, TRUE, TRUE)
    )
  )
})

test_that("col_by_to_matrix works", {
  expect_error(
    col_by_to_matrix(c("a", "b", "a", "a", "b")),
    "unknown class"
  )
  x <- factor(c("a", "b", "a", "a", "b"))
  expect_equal(
    col_by_to_matrix(x),
    col_by_to_matrix(col_by_to_matrix(x))
  )
  expect_error(col_by_to_matrix(by_all("my_total")), "is.null")
  expect_equal(
    col_by_to_matrix(by_all("my_total"), x = 1:3),
    data.frame(my_total = c(TRUE, TRUE, TRUE))
  )
})
