context("Result Data Frames")

test_that("Result Data Frame generation works v0", {
  lyt <- make_big_lyt()

  tbl <- build_table(lyt, rawdat)

  result_df <- as_result_df(tbl)
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
    c("group1", "group1_level", "group2", "group2_level", "avar_name")
  )

  ## handle multiple analyses
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("STRATA1") %>%
    analyze(c("AGE", "BMRKR2"))

  tbl2 <- build_table(lyt, ex_adsl)
  result_df2 <- as_result_df(tbl2)

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
  result_df3 <- as_result_df(tbl3)

  expect_identical(nrow(result_df3), 1L)

  ## test labels when no row splits
  lyt4 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "SEX"))

  tbl4 <- build_table(lyt4, DM)
  result_df4 <- as_result_df(tbl4, add_tbl_name_split = TRUE)

  expect_identical(
    names(result_df4),
    c(
      "group1", "group1_level",
      "avar_name", "row_name", "label_name", "row_num", "is_group_summary",
      "node_class", "A: Drug X", "B: Placebo", "C: Combination"
    )
  )
})

test_that("as_result_df works with visual output (data_format as numeric)", {
  lyt <- make_big_lyt()
  tbl <- build_table(lyt, rawdat)

  res <- expect_silent(as_result_df(tbl, simplify = TRUE, data_format = "numeric"))
  expect_equal(res$ARM1.M[[1]], c(116.0, 45.3))

  res <- expect_silent(as_result_df(tbl, data_format = "strings", simplify = TRUE))
  expect_equal(res$ARM1.M[[1]], "116 (45.3%)")

  mf <- matrix_form(tbl)
  string_tbl <- mf_strings(mf)[-seq_len(mf_nlheader(mf)), ]
  string_tbl <- as.data.frame(string_tbl[nzchar(string_tbl[, 2]), ])
  colnames(string_tbl) <- colnames(res)
  rownames(string_tbl) <- NULL
  expect_equal(res, string_tbl)

  res <- expect_silent(as_result_df(tbl, simplify = TRUE, data_format = "strings", expand_colnames = TRUE))
  string_tbl <- mf_strings(mf)
  string_tbl <- data.frame(string_tbl[nzchar(string_tbl[, 2]), ])
  colnames(string_tbl) <- colnames(res)
  string_tbl$label_name[seq_len(mf_nlheader(mf))] <- res$label_name[seq_len(mf_nlheader(mf))]
  rownames(string_tbl) <- NULL
  expect_equal(res, string_tbl)

  expect_silent(basic_table() %>% build_table(DM) %>% as_result_df())

  tbl <- basic_table(show_colcounts = TRUE) %>%
    analyze("BMRKR1") %>%
    build_table(DM)
  expect_equal(as_result_df(tbl)$`all obs`, 5.851948, tolerance = 1e-6)
  expect_equal(
    as_result_df(tbl, data_format = "numeric")$`all obs`[[1]],
    as.numeric(as_result_df(tbl, data_format = "strings")$`all obs`)
  )
  expect_equal(as_result_df(tbl, expand_colnames = TRUE)$`all obs`[2], "356")
  expect_equal(as_result_df(tbl, expand_colnames = TRUE, data_format = "strings")$`all obs`[2], "(N=356)")


  # Test for integer extraction and ranges
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("STRATA1") %>%
    analyze("AGE", afun = function(x) list(a = mean(x), b = range(x)))

  tbl <- build_table(lyt, ex_adsl)
  expect_equal(as_result_df(tbl, simplify = TRUE, data_format = "numeric")[2, 2][[1]], c(24, 46))

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

  expect_true(all(out[, 1] == "STRATA1"))
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
  rd3 <- as_result_df(tbl, keep_label_rows = TRUE, expand_colnames = TRUE, data_format = "strings")
  rd4 <- as_result_df(tbl, keep_label_rows = TRUE, expand_colnames = TRUE, data_format = "numeric")

  expect_equal(nrow(rd1), nrow(rd2) - 2)
  expect_equal(nrow(rd1), nrow(rd3) - 2)
  expect_equal(nrow(rd1), nrow(rd4) - 2)
  expect_identical(ncol(rd1), ncol(rd2))
  expect_identical(ncol(rd1), ncol(rd3))
  expect_identical(ncol(rd1), ncol(rd4))

  expect_identical(as.character(rd1[3, ]), as.character(rd2[5, ]))
  expect_identical(rd3[["A: Drug X.S1"]], rd4[["A: Drug X.S1"]] %>% unlist())

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

test_that("as_result_df simplify is producing a data.frame that is compatible with df_to_tt", {
  # More challenging labels
  lyt <- make_big_lyt()
  tbl <- build_table(lyt, rawdat)

  ard_out <- as_result_df(tbl, simplify = TRUE, keep_label_rows = TRUE)
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
  end_tbl <- init_tbl %>%
    as_result_df(simplify = TRUE) %>%
    df_to_tt()

  expect_equal(
    matrix_form(init_tbl)$strings,
    matrix_form(end_tbl)$strings
  )
})

test_that("as_result_df works fine with empty tables and no character(0) is allowed", {
  tbl <- basic_table() %>%
    build_table(mtcars)

  expect_silent(as_result_df(tbl))

  expect_equal(
    .remove_root_elems_from_path(
      c("a", "b", "c"),
      which_root_name = c("a", "b", "c"),
      all = TRUE
    ),
    "a"
  )
})

test_that("as_result_df works with analyze-only tables (odd num of path elements)", {
  tbl <- basic_table() %>%
    analyze("cyl", table_names = "a") %>%
    analyze("mpg") %>%
    build_table(mtcars)

  expect_equal(as_result_df(tbl, add_tbl_name_split = TRUE)$group1[[1]], "<analysis_spl_tbl_name>")
  expect_equal(as_result_df(tbl, make_ard = TRUE, add_tbl_name_split = TRUE)$group1[[1]], "<analysis_spl_tbl_name>")
})

test_that("make_ard produces realistic ARD output with as_result_df", {
  # Testing fundamental getters/setters
  rc <- rcell(c(1, 2), stat_names = c("Rand1", "Rand2"))
  expect_equal(obj_stat_names(rc), c("Rand1", "Rand2"))

  rc_row <- in_rows(
    .list = list(a = c(NA, 1), b = c(1, NA)),
    .formats = c("xx - xx", "xx.x - xx.x"),
    .format_na_strs = list(c("asda", "lkjklj")),
    .stat_names = list(c("A", "B"), c("B", "C")) # if c("A", "B") one each row, if single list duplicated
  )

  expect_equal(
    list("a" = c("A", "B"), "b" = c("B", "C")), # now it is named
    lapply(rc_row, obj_stat_names)
  )

  # Lets make a custom function and check ARDs
  mean_sd_custom <- function(x) {
    mean <- mean(x, na.rm = FALSE)
    sd <- sd(x, na.rm = FALSE)

    rcell(c(mean, sd),
      label = "Mean (SD)", format = "xx.x (xx.x)",
      stat_names = c("Mean", "SD")
    )
  }
  counts_percentage_custom <- function(x) {
    cnts <- table(x)
    out <- lapply(cnts, function(x) {
      perc <- x / sum(cnts)
      rcell(c(x, perc), format = "xx. (xx.%)")
    })
    in_rows(
      .list = as.list(out), .labels = names(cnts),
      .stat_names = list(c("Count", "Percentage"))
    )
  }

  lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") %>%
    split_cols_by("ARM", split_fun = keep_split_levels(c("A: Drug X", "B: Placebo"))) %>%
    analyze(vars = "AGE", afun = mean_sd_custom) %>%
    analyze(vars = "SEX", afun = counts_percentage_custom)

  tbl <- build_table(lyt, ex_adsl)
  ard_out <- as_result_df(tbl, make_ard = TRUE, add_tbl_name_split = TRUE)

  # Numeric output
  expect_equal(
    ard_out[2, , drop = TRUE],
    list(
      group1 = "<analysis_spl_tbl_name>",
      group1_level = "ma_AGE_SEX",
      group2 = "ARM",
      group2_level = "A: Drug X",
      variable = "AGE",
      variable_level = "Mean (SD)",
      variable_label = "Mean (SD)",
      stat_name = "SD",
      stat = 6.553326,
      stat_string = "6.6"
    ),
    tolerance = 10e-6
  )

  # Percentage output
  expect_equal(
    ard_out[14, , drop = TRUE],
    list(
      group1 = "<analysis_spl_tbl_name>",
      group1_level = "ma_AGE_SEX",
      group2 = "ARM",
      group2_level = "B: Placebo",
      variable = "SEX",
      variable_level = "F",
      variable_label = "F",
      stat_name = "Percentage",
      stat = 0.5746269,
      stat_string = "57"
    ),
    tolerance = 10e-6
  )

  # Default values
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "SEX"))

  tbl <- build_table(lyt, ex_adsl)
  ard_out <- as_result_df(tbl, make_ard = TRUE)

  expect_equal(unique(ard_out$stat_name), c("mean", "n"))
})

test_that("make_ard works with multiple row levels", {
  lyt <- basic_table() %>%
    split_rows_by("STRATA1") %>%
    split_rows_by("STRATA2") %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "SEX"))

  tbl <- build_table(lyt, ex_adsl)
  ard_out <- as_result_df(tbl, make_ard = TRUE)

  expect_equal(unique(ard_out$stat_name), c("mean", "n"))
  expect_contains(colnames(ard_out), c("group2", "group2_level"))

  # Count output
  expect_equal(
    ard_out[90, , drop = TRUE],
    list(
      group1 = "STRATA1",
      group1_level = "C",
      group2 = "STRATA2",
      group2_level = "S2",
      group3 = "ARM",
      group3_level = "C: Combination",
      variable = "SEX",
      variable_level = "UNDIFFERENTIATED",
      variable_label = "UNDIFFERENTIATED",
      stat_name = "n",
      stat = 0,
      stat_string = "0"
    ),
    tolerance = 10e-6
  )
})

test_that("make_ard works with multiple column levels", {
  lyt <- basic_table() %>%
    split_rows_by("STRATA1") %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA2") %>%
    analyze(c("AGE", "SEX"))

  tbl <- build_table(lyt, ex_adsl)
  ard_out <- as_result_df(tbl, make_ard = TRUE)

  expect_equal(unique(ard_out$stat_name), c("mean", "n"))
  expect_contains(colnames(ard_out), c("group1", "group1_level"))
  expect_contains(colnames(ard_out), c("group2", "group2_level"))

  # Count output
  expect_equal(
    ard_out[16, , drop = TRUE],
    list(
      group1 = "STRATA1",
      group1_level = "A",
      group2 = "ARM",
      group2_level = "A: Drug X",
      group3 = "STRATA2",
      group3_level = "S2",
      variable = "AGE",
      variable_level = "Mean",
      variable_label = "Mean",
      stat_name = "mean",
      stat = 34.4,
      stat_string = "34.4"
    ),
    tolerance = 10e-6
  )
})

test_that("make_ard works with summarize_row_groups", {
  lyt <- basic_table() %>%
    split_rows_by("STRATA2") %>%
    summarize_row_groups() %>%
    split_rows_by("ARM") %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA1") %>%
    analyze(c("AGE", "SEX"))

  tbl <- build_table(lyt, ex_adsl)
  ard_out <- as_result_df(tbl, make_ard = TRUE)

  expect_contains(unique(ard_out$stat_name), c("mean", "n", "p"))
  expect_contains(colnames(ard_out), c("group1", "group1_level"))
  expect_contains(colnames(ard_out), c("group2", "group2_level"))

  # label row output
  expect_equal(
    ard_out[1, , drop = TRUE],
    list(
      group1 = "STRATA2",
      group1_level = "S1",
      group2 = NA_character_,
      group2_level = NA_character_,
      group3 = "ARM",
      group3_level = "A: Drug X",
      group4 = "STRATA1",
      group4_level = "A",
      variable = "STRATA2",
      variable_level = "S1",
      variable_label = "S1",
      stat_name = "n",
      stat = 18,
      stat_string = "18"
    ),
    tolerance = 10e-6
  )

  # Testing different placements of summarize_row_groups
  lyt <- basic_table() %>%
    split_rows_by("STRATA2") %>%
    split_rows_by("ARM") %>%
    summarize_row_groups() %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA1") %>%
    analyze(c("AGE", "SEX"))

  tbl <- build_table(lyt, ex_adsl)
  ard_out <- as_result_df(tbl, make_ard = TRUE)

  # label row output
  expect_equal(
    ard_out[1, , drop = TRUE],
    list(
      group1 = "STRATA2",
      group1_level = "S1",
      group2 = "ARM",
      group2_level = "A: Drug X",
      group3 = "ARM",
      group3_level = "A: Drug X",
      group4 = "STRATA1",
      group4_level = "A",
      variable = "ARM",
      variable_level = "A: Drug X",
      variable_label = "A: Drug X",
      stat_name = "n",
      stat = 18,
      stat_string = "18"
    ),
    tolerance = 10e-6
  )
})

test_that("make_ard works if there are no stat_names", {
  mean_sd_custom <- function(x) {
    mean <- mean(x, na.rm = FALSE)
    sd <- sd(x, na.rm = FALSE)

    rcell(c(mean, sd),
      label = "Mean (SD)", format = "xx.x (xx.x)"
    )
  }

  lyt <- basic_table(show_colcounts = TRUE, colcount_format = "N=xx") %>%
    split_cols_by("ARM", split_fun = keep_split_levels(c("A: Drug X", "B: Placebo"))) %>%
    analyze(vars = "AGE", afun = mean_sd_custom)

  tbl <- build_table(lyt, ex_adsl)

  expect_equal(as_result_df(tbl, make_ard = TRUE)$stat_name, rep(NA_character_, 4))
})

test_that("make_ard works if string precision is needed", {
  lyt <- basic_table() %>%
    split_rows_by("ARM") %>%
    summarize_row_groups() %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA1") %>%
    analyze(c("AGE", "SEX"))

  tbl <- build_table(lyt, ex_adsl)

  # Some edge cases
  expect_equal(
    as_result_df(tbl[, 1], make_ard = TRUE) %>% dim(),
    c(21, 12)
  )
  expect_equal(
    as_result_df(tbl[1, ], make_ard = TRUE) %>% dim(),
    c(18, 12)
  )

  # One result
  test_out <- as_result_df(tbl[, 1][1, ], make_ard = TRUE)
  expect_equal(test_out$stat_name, c("n", "p"))
  expect_equal(test_out$stat, c(38, 1))
  expect_equal(test_out$stat_string, c("38", "100"))
})

test_that("make_ard works with split_cols_by_multivar", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  # Regression test #970
  n <- 400

  df <- tibble(
    arm = factor(sample(c("Arm A", "Arm B"), n, replace = TRUE), levels = c("Arm A", "Arm B")),
    country = factor(sample(c("CAN", "USA"), n, replace = TRUE, prob = c(.55, .45)), levels = c("CAN", "USA")),
    gender = factor(sample(c("Female", "Male"), n, replace = TRUE), levels = c("Female", "Male")),
    handed = factor(sample(c("Left", "Right"), n, prob = c(.6, .4), replace = TRUE), levels = c("Left", "Right")),
    age = rchisq(n, 30) + 10
  ) %>% mutate(
    weight = 35 * rnorm(n, sd = .5) + ifelse(gender == "Female", 140, 180)
  )

  colfuns <- list(
    function(x) in_rows(mean = mean(x), .formats = "xx.x"),
    function(x) in_rows("# x > 5" = sum(x > .5), .formats = "xx")
  )

  lyt <- basic_table() %>%
    split_cols_by("arm") %>%
    split_cols_by_multivar(c("age", "weight")) %>%
    split_rows_by("country") %>%
    summarize_row_groups() %>%
    analyze_colvars(afun = colfuns)

  tbl <- build_table(lyt, df)

  expect_silent(out <- as_result_df(tbl, make_ard = TRUE))
  expect_true(all(out$group3 == "multivar_split1"))
})

test_that("make_ard works when printed format differs from cell values", {
  # Also regression test for #1001
  mean_sd_custom <- function(x, .spl_context, ...) {
    if (.spl_context$value[2] == "B: Placebo" || .spl_context$cur_col_id[2] == "B: Placebo") {
      return(NULL)
    }
    rcell(c(1, 2),
      label = "Mean (SD)", format = function(xf, ...) {
        return(as.character(xf[1]))
      }
    )
  }

  test_out <- basic_table() %>%
    split_rows_by("ARM") %>%
    split_cols_by("ARM") %>%
    analyze(vars = "AGE", afun = mean_sd_custom) %>%
    build_table(DM)

  expect_warning(
    out <- as_result_df(test_out, make_ard = TRUE, verbose = TRUE),
    paste0(
      "Found 1 cell values that differ from printed values. ",
      "This is possibly related to conditional formatting. ",
      "\\\nThe following row names were modified: Mean \\(SD\\), Mean \\(SD\\)\\\n"
    )
  )
  expect_equal(
    as.character(out$stat),
    out$stat_string
  )
})
