context("Summarizing table structure")

test_that("path summaries", {

  lyt <- make_big_lyt()

  tbl <- build_table(lyt, rawdat)

  capture.output(cpathsum <- col_paths_summary(tbl))

  arm1tmp <- c("ARM", "ARM1")
  arm2tmp <- c("ARM", "ARM2")
  expect_identical(cpathsum,
    data.frame(label = c("ARM1", "Male", "Female",
      "ARM2", "Male", "Female"),
    path = I(list(arm1tmp,
      c(arm1tmp, c("SEX", "M")),
      c(arm1tmp, c("SEX", "F")),
      arm2tmp,
      c(arm2tmp, c("SEX", "M")),
      c(arm2tmp, c("SEX", "F")))),
    stringsAsFactors = FALSE))

  cpval <- col_paths(tbl)
  ## cpval doesn't contain the non-leaf paths
  expect_identical(cpval, cpathsum$path[-c(1, 4)])

  capture.output(rpathsum <- row_paths_summary(tbl))

  ## defined in setup-fakedata.R
  expect_identical(complx_lyt_rnames,
    rpathsum$label)

  expect_identical(row_paths(tbl),
    rpathsum$path)

})

test_that("vars_in_layout works", {
  lyt <- make_big_lyt()
  vars <- vars_in_layout(lyt)
  expect_identical(vars,
    c("ARM", # split_cols_by
      "SEX", # split_cols_by
      "gend_label", # split_cols_by labels_var
      "RACE", # split_rows_by
      "ethn_label", # split_rows_by labels_var
      "FACTOR2", # split_rows_by
      "fac2_label", # split_rows_by labels_var
      "AGE", # analyze
      "VAR3") #analyze
  )

  expect_identical(vars_in_layout(ManualSplit(c("A", "B"), label = "stuff")),
    character())
})
