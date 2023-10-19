context("Result Data Frames")


test_that("Result Data Frame generation works v0", {

  ## change here (only) when v0 is crystalized (no longer experimental)
  spec_version <- "v0_experimental"
  lyt <- make_big_lyt()

  tbl <- build_table(lyt, rawdat)

  result_df <- as_result_df(tbl, spec_version)
  expect_identical(result_df[2, "ARM1.M"][[1]],
    c(37, 37/256))

  expect_identical(nrow(tbl) - 8L,
    nrow(result_df))

  expect_identical(names(result_df)[1:5],
    c("spl_var_1", "spl_value_1", "spl_var_2", "spl_value_2", "avar_name"))

  ## handle multiple analyses
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("STRATA1") %>%
    analyze(c("AGE", "BMRKR2"))

  tbl2 <- build_table(lyt, ex_adsl)
  result_df2 <- as_result_df(tbl2, spec_version )

  ## regression test
  expect_false(any(is.na(result_df2$spl_var_1)))

  ## test colvar analysis and bug regarding 1 row multi column tables
  test <- data.frame(
    a = c(1, 2),
    b = c(1, NA)
  )

  lyt3 <- basic_table() %>%
    split_cols_by_multivar(c("a", "b")) %>%
    analyze_colvars(afun = length, inclNAs = TRUE)

  tbl3 <- build_table(lyt3, test)
  result_df3 <- as_result_df(tbl3, spec_version)

  expect_identical(nrow(result_df3), 1L)

})
