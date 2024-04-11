context("matrix_form works")

test_that("matrix_form works with indent_rownames = FALSE", {
  # Discovered thanks to more stringent indentation checks. See #844 for when it
  # surfaced. Cause: make_row_df does not update mf_rinfo indentation if indent_rownames = FALSE
  lyt <- basic_table() %>%
    split_rows_by("ARM") %>%
    analyze("AGE")
  tbl <- build_table(lyt, ex_adsl)
  mf <- matrix_form(tbl, indent_rownames = FALSE)
  expect_equal(mf_rinfo(mf)$indent, rep(0, nrow(tbl)))
  expect_silent(out <- toString(mf))
})
