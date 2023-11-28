context("Result Data Frames")

test_that("Result Data Frame generation works v0", {
  ## change here (only) when v0 is crystalized (no longer experimental)
  spec_version <- "v0_experimental"
  lyt <- make_big_lyt()

  tbl <- build_table(lyt, rawdat)

  result_df <- as_result_df(tbl, spec_version)
  expect_identical(
    result_df[2, "ARM1.M"][[1]],
    c(37, 37 / 256)
  )

  expect_identical(
    nrow(tbl) - 8L,
    nrow(result_df)
  )

  expect_identical(
    names(result_df)[1:5],
    c("spl_var_1", "spl_value_1", "spl_var_2", "spl_value_2", "avar_name")
  )

  ## handle multiple analyses
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("STRATA1") %>%
    analyze(c("AGE", "BMRKR2"))

  tbl2 <- build_table(lyt, ex_adsl)
  result_df2 <- as_result_df(tbl2, spec_version)

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

  ## test labels when no row splits
  lyt4 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "SEX"))

  tbl4 <- build_table(lyt4, DM)
  result_df4 <- as_result_df(tbl4)

  expect_identical(
    names(result_df4),
    c(
      "avar_name", "row_name", "row_num", "is_group_summary",
      "node_class", "A: Drug X", "B: Placebo", "C: Combination"
    )
  )
})

test_that("as_result_df works with visual output (as_viewer)", {
  lyt <- make_big_lyt()
  tbl <- build_table(lyt, rawdat)

  res <- expect_silent(as_result_df(tbl, simplify = TRUE, as_viewer = TRUE))
  expect_equal(res$ARM1.M[[1]], c(116.0, 45.3))

  res <- expect_silent(as_result_df(tbl, simplify = TRUE, as_viewer = TRUE, as_strings = TRUE))
  expect_equal(res$ARM1.M[[1]], "116 (45.3%)")

  mf <- matrix_form(tbl)
  string_tbl <- mf_strings(mf)[-seq_len(mf_nlheader(mf)), ]
  string_tbl <- string_tbl[nzchar(string_tbl[, 2]), ]
  colnames(string_tbl) <- colnames(res)
  expect_equal(res, data.frame(string_tbl))

  res <- expect_silent(as_result_df(tbl, simplify = TRUE, as_strings = TRUE, expand_colnames = TRUE))
  string_tbl <- mf_strings(mf)
  string_tbl <- data.frame(string_tbl[nzchar(string_tbl[, 2]), ])
  colnames(string_tbl) <- colnames(res)
  string_tbl$row_name[seq_len(mf_nlheader(mf))] <- res$row_name[seq_len(mf_nlheader(mf))]
  expect_equal(res, string_tbl)

  expect_silent(basic_table() %>% build_table(DM) %>% as_result_df())

  tbl <- basic_table(show_colcounts = TRUE) %>%
    analyze("BMRKR1") %>%
    build_table(DM)
  expect_equal(as_result_df(tbl)$`all obs`, 5.851948, tolerance = 1e-6)
  expect_equal(
    as_result_df(tbl, as_viewer = TRUE)$`all obs`,
    as.numeric(as_result_df(tbl, as_strings = TRUE)$`all obs`)
  )
  expect_equal(as_result_df(tbl, expand_colnames = TRUE)$`all obs`[2], "356")
  expect_equal(as_result_df(tbl, expand_colnames = TRUE, as_strings = TRUE)$`all obs`[2], "(N=356)")
})
