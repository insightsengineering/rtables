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
      "avar_name", "row_name", "label_name", "row_num", "is_group_summary",
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
  
  
  # Test for integer extraction and ranges
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("STRATA1") %>%
    analyze("AGE", afun = function(x) list(a = mean(x), b = range(x)))
  
  tbl <- build_table(lyt, ex_adsl)
  expect_equal(as_result_df(tbl, simplify = TRUE, as_viewer = TRUE)[2, 2][[1]], c(24, 46))
  
  # Test for tables with less than 3 rows
  tbl <- rtable(
    header = rheader(rrow("", "c1", "c2")),
    rrow("row 1", 1, c(.8, 1.2))
  )
  expect_equal(
    as_result_df(tbl)[, 1:6], 
    data.frame(
      "avar_name" = "row 1", 
      "row_name" = "row 1", 
      "label_name" = "row 1", 
      "row_num" = 1, 
      "is_group_summary" = FALSE, 
      "node_class" = "DataRow"
    )
  )
})

test_that("as_result_df works fine also with multiple rbind_root", {
  # regression test for rtables#815
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("STRATA1") %>%
    analyze(c("AGE", "BMRKR2"))
  
  tbl <- build_table(lyt, ex_adsl)
  
  mega_rbind_tbl <- rbind(tbl, rbind(tbl, tbl, rbind(tbl, tbl)))
  
  out <- expect_silent(as_result_df(mega_rbind_tbl))
  
  expect_true(all(out[,1] == "STRATA1"))
})

test_that("as_result_df keeps label rows", {
  # regression test for rtables#815
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA2") %>%
    split_rows_by("STRATA1") %>%
    analyze(c("AGE", "BMRKR2"))
  
  tbl <- build_table(lyt, ex_adsl)
  
  rd1 <- as_result_df(tbl, keep_label_rows = TRUE)
  rd2 <- as_result_df(tbl, keep_label_rows = TRUE, expand_colnames = TRUE)
  rd3 <- as_result_df(tbl, keep_label_rows = TRUE, expand_colnames = TRUE, as_strings = TRUE)
  rd4 <- as_result_df(tbl, keep_label_rows = TRUE, expand_colnames = TRUE, as_viewer = TRUE)
  
  expect_equal(nrow(rd1), nrow(rd2) - 2)
  expect_equal(nrow(rd1), nrow(rd3) - 2)
  expect_equal(nrow(rd1), nrow(rd4) - 2)
  expect_identical(ncol(rd1), ncol(rd2))
  expect_identical(ncol(rd1), ncol(rd3))
  expect_identical(ncol(rd1), ncol(rd4))
  
  expect_identical(as.character(rd1[3, ]), as.character(rd2[5, ]))
  expect_identical(rd2[is.na(rd2[, ncol(rd2)]), ], rd4[is.na(rd4[, ncol(rd4)]), ])
  
  # More challenging labels
  lyt <- make_big_lyt()
  tbl <- build_table(lyt, rawdat)
  
  ard_out <- as_result_df(tbl, keep_label_rows = TRUE)
  mf_tbl <- matrix_form(tbl)
  
  # Label works
  expect_identical(
    ard_out$label_name,
    mf_strings(mf_tbl)[-seq_len(mf_nrheader(mf_tbl)), 1]
  )
  
  # Row names respects path
  pths <- make_row_df(tbl)$path
  expect_identical(
    ard_out$row_name,
    sapply(pths, tail, 1)
  )
})

test_that("as_result_df as_is is producing a data.frame that is compatible with df_to_tt", {
  # More challenging labels
  lyt <- make_big_lyt()
  tbl <- build_table(lyt, rawdat)
  
  ard_out <- as_result_df(tbl, as_is = TRUE)
  mf_tbl <- matrix_form(tbl)
  
  # Label works
  expect_identical(
    ard_out$label_name,
    mf_strings(mf_tbl)[-seq_len(mf_nrheader(mf_tbl)), 1]
  )
  
  expect_identical(
    ard_out$label_name,
    df_to_tt(ard_out) %>% row.names()
  )
  
  init_tbl <- df_to_tt(mtcars) 
  end_tbl <- init_tbl %>% as_result_df(as_is = TRUE) %>% df_to_tt()
  
  expect_equal(
    matrix_form(init_tbl)$strings, 
    matrix_form(end_tbl)$strings
  )
})
