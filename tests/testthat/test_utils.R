context("Checking utility functions")

test_that("func_takes works with different inputs", {
    func_takes <- rtables:::func_takes
  f1 <- function() {}
  f2 <- function(df = "a") {}
  f3 <- function(x = "df") {}
  f4 <- function(x, df) {}
  f5 <- function(Df, df = "df") {}
  f6 <- function(df1, df = "df") {}
  f_l <- list(f1, f2, f3, f4, f5, f6)

  expect_silent(res <- unlist(func_takes(f_l, "df", is_first = TRUE)))
  expect_identical(res, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))

  expect_silent(res <- func_takes(f_l, "df", is_first = FALSE))
  expect_identical(
    unlist(res, use.names = FALSE),
    c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
  )

  expect_true(all(sapply(res, names) == "df"))

  expect_error(
    func_takes(f_l, c("df", "l"), is_first = TRUE),
    "is_first works only with one parameters."
  )
})
