test_that("analyze's var_labels accepts a vector of length 1 consisting of a single NA", {
  testthat::expect_no_error(
    basic_table() %>%
      split_cols_by("ARM") %>%
      analyze("AGE", var_labels = c(AGE = NA))
  )
})
