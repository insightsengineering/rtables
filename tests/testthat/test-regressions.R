context("regression tests")


test_that("unlisting rtables has no effect on them", {
  t1 <- rtable(header = c("A", "B"), format = "xx", rrow("row 1", 1, 2))

  expect_identical(t1, unlist(t1))
})


test_that(
  "manually created label l rows are always visible",
  expect_true(rtables:::labelrow_visible(rrow("")))
)


## was error before rtables 0.3.2.16
test_that("printing table with 0 rows works", {
  norows <- rtable(c("hi", "lo"))
  mf_rinfo(matrix_form(norows))
  capture.output(prout <- print(norows))
  expect_identical(prout, norows)
})



test_that("inclNAs argument works as expected", {
  tinydat <- data.frame(
    RSP = c(TRUE, FALSE, NA, TRUE),
    ARM = factor(c("A", "A", "B", "B"))
  )
  tbl1 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(vars = "RSP", inclNAs = FALSE) %>%
    build_table(df = tinydat)

  expect_equal(tbl1[1, 2, drop = TRUE], 1)
  tbl2 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(vars = "RSP", inclNAs = TRUE) %>%
    build_table(df = tinydat)

  expect_true(is.na(tbl2[1, 2, drop = TRUE]))
})

test_that("head/tail work", {
  tbl <- rtable(
    c("hi", "lo"),
    rrow("rn", 5, 5)
  )
  expect_false(is.null(head(tbl)))
  expect_false(is.null(tail(tbl)))
  tbl_none <- tbl
  top_left(tbl) <- "hiya"
  main_title(tbl) <- "title"
  subtitles(tbl) <- c("subt 1", "subt2")
  main_footer(tbl) <- "footer"
  prov_footer(tbl) <- "prov"
  ## 335
  .check_em <- function(t, tbl) {
    expect_identical(top_left(t), top_left(tbl))
    expect_identical(main_title(t), main_title(tbl))
    expect_identical(subtitles(t), subtitles(tbl))
    expect_identical(top_left(t), top_left(tbl))
    expect_identical(top_left(t), top_left(tbl))
    expect_identical(top_left(t), top_left(tbl))
    TRUE
  }
  t_h <- head(tbl)
  .check_em(head(tbl), tbl)
  .check_em(tail(tbl), tbl)
  .check_em(
    head(tbl, keep_topleft = FALSE, keep_titles = FALSE),
    tbl_none
  )
  .check_em(
    tail(tbl, keep_topleft = FALSE, keep_titles = FALSE),
    tbl_none
  )
})

test_that("sort does not clobber top-level siblings", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze("AGE") %>%
    split_rows_by("SEX") %>%
    analyze("AGE", function(x) in_rows(mean = mean(x), "mean+5" = mean(x) + 5))

  tbl <- build_table(lyt, rawdat)

  stbl <- sort_at_path(tbl, c("SEX", "*", "AGE"), function(tt) sum(unlist(row_values((tt)))), decreasing = TRUE)

  expnms <- c("Mean", "M", "mean+5", "mean", "F", "mean+5", "mean")
  expect_identical(row.names(stbl), expnms)
})


test_that("repeated multi-var analyzes work as expected", {
  works <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("SEX", "RACE", "STRATA1"), afun = list_wrap_x(table)) %>%
    analyze("COUNTRY", afun = list_wrap_x(table)) %>%
    build_table(DM)

  fails <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("SEX", "RACE"), afun = list_wrap_x(table)) %>%
    analyze(c("STRATA1", "COUNTRY"), afun = list_wrap_x(table)) %>%
    build_table(DM)

  expect_identical(works, fails)
})


test_that("summarize_row_groups after analyze call(s) work", {
  lyt1 <- basic_table() %>%
    analyze("SEX") %>%
    split_rows_by("SEX") %>%
    analyze("SEX")
  tbl1 <- build_table(lyt1, DM)
  expect_equal(dim(tbl1), c(24, 1))

  ## further regression when we have multiple analyze calls
  lyt2 <- basic_table() %>%
    analyze("SEX") %>%
    analyze("STRATA1") %>%
    split_rows_by("SEX") %>%
    analyze("SEX")
  tbl2 <- build_table(lyt2, DM)
  expect_equal(dim(tbl2), c(29, 1))
})


test_that("summarize_row_groups at top level works", {
  lyt <- basic_table() %>%
    summarize_row_groups("SEX")

  tbl <- build_table(lyt, DM)
  expect_equal(length(tree_children(tbl)), 0)
  expect_equal(dim(tbl), c(1, 1))
})

test_that("CellValue on something with object labels", {
  expect_identical(
    obj_label(CellValue(with_label(5, "hi"))),
    "hi"
  )

  expect_identical(
    obj_label(CellValue(with_label(5, "hi"),
      label = ""
    )),
    ""
  )

  expect_identical(
    obj_label(CellValue(with_label(5, "hi"),
      label = NULL
    )),
    "hi"
  )
})


test_that("rcell on CellValue overrides attrs as necessary", {
  val <- CellValue(c(100, .5),
    format = "xx (xx.x%)", label = "oldlabel",
    colspan = 2L,
    indent_mod = 2L
  )
  val2 <- CellValue(c(100, .5),
    format = "xx (xx.xx%)", label = "new label",
    colspan = 3L,
    indent_mod = 3L
  )
  expect_identical(
    rcell(val,
      format = "xx (xx.xx%)", label = "new label",
      colspan = 3L, indent_mod = 3L
    ),
    val2
  )
})


test_that("cell-level formats are retained when column subsetting", {
  tbl <- rtable(
    header = c("Treatement\nN=100", "Comparison\nN=300"),
    format = "xx (xx.xx%)",
    rrow("A", c(104, .2), c(100, .4)),
    rrow("B", c(23, .4), c(43, .5)),
    rrow(""),
    rrow("this is a very long section header"),
    rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
    rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
  )

  ## this tests for no warnings, because testthat is terribly designed
  expect_warning(toString(tbl), regexp = NA)
  subset <- tbl[, 1]
  expect_identical(
    matrix_form(subset)$strings,
    matrix_form(tbl)$strings[, -3]
  )
})

test_that("row subsetting works on table with only content rows", {
  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    summarize_row_groups()
  tab <- build_table(l, DM)
  rw <- tab[1, ]
  expect_identical(
    cell_values(rw),
    cell_values(tab)[[1]]
  )
  expect_identical(
    unname(tab[1, 1, drop = TRUE]),
    79 * c(1, 1 / sum(DM$ARM == "A: Drug X"))
  )
})


test_that("calls to make_afun within loop work correctly", {
  dummy_stats_function <- function(x) {
    list("s_mean" = mean(x))
  }

  dummy_layout <- function(lyt, vv) {
    for (i in seq_along(vv)) {
      afun <- make_afun(
        dummy_stats_function,
        .stats = "s_mean",
        .labels = c(s_mean = vv[i]), # set labels here to match variable name
        .formats = c(s_mean = "xx.x")
      )

      lyt <- analyze(
        lyt,
        vars = vv[i],
        afun = afun,
        show_labels = "visible"
      )
    }

    lyt
  }

  tab <- basic_table() %>%
    split_cols_by("ARM") %>%
    dummy_layout(vv = c("BMRKR1", "AGE")) %>%
    build_table(DM)

  expect_identical(
    row.names(tab),
    c("BMRKR1", "BMRKR1", "AGE", "AGE")
  )
})


test_that("keeping non-existent levels doesn't break internal machinery", {
  ANL <- DM
  ANL$COUNTRY <- as.character(ANL$COUNTRY)

  sfun <- keep_split_levels("ABC")


  lyt <- basic_table() %>%
    analyze("AGE") %>%
    split_rows_by("COUNTRY", split_fun = sfun) %>%
    summarize_row_groups() %>%
    analyze("AGE")

  result <- build_table(lyt, df = ANL)
  expect_identical(dim(result), c(3L, 1L))
  expect_identical(row.names(result), c("Mean", "ABC", "Mean"))
  cbres <- cbind_rtables(result, result)
  expect_identical(dim(cbres), c(3L, 2L))
  expect_identical(row.names(cbres), c("Mean", "ABC", "Mean"))

  ## because its a factor and "ABC" isn't a real level
  expect_error(build_table(lyt, DM))

  expect_error(cbind_rtables(result[-1, ], result[-3, ]), "Mismatching, non-empty row names")
})

test_that("add_overall_col with no col splits works", {
  lyt <- basic_table() %>%
    add_overall_col("whaaat") %>%
    analyze("AGE", mean)
  tab <- build_table(lyt, DM) ## previously error
  expect_identical(names(tab), "whaaat")
})


test_that("cell_values works when you path all the way to the row", {
  tbl <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("SEX", "AGE")) %>%
    build_table(ex_adsl)
  res <- cell_values(tbl, c("AGE", "Mean"), c("ARM", "B: Placebo"))
  expect_identical(res[[1]], mean(subset(ex_adsl, ARM == "B: Placebo")$AGE))
})


test_that("(xx,xx) format works correctly", {
  expect_identical(
    "(2, 5)",
    format_rcell(rcell(c(2, 5), format = "(xx, xx)"))
  )
})

test_that("inclNAs with empty factor levels behaves", {
  ## no NAs in DM$RACE so following 2 tables should be fully identical

  ## NO TIBBLES!!!!!!!!!!!!!!!!!!!
  dfdm <- as.data.frame(DM)
  tbl <- basic_table() %>%
    split_rows_by("RACE") %>%
    analyze("COUNTRY", function(x) in_rows(nobs = length(x)), inclNAs = TRUE) %>%
    build_table(dfdm)

  tbl2 <- basic_table() %>%
    split_rows_by("RACE") %>%
    analyze("COUNTRY", function(x) in_rows(nobs = length(x)), inclNAs = FALSE) %>%
    build_table(dfdm)
  expect_identical(tbl, tbl2)
})


## #173
test_that("column labeling works correctly when value label var is a factor", {
  ex_adsl$ARMLAB <- factor(ex_adsl$ARM,
    labels = c("Drug X", "Placebo", "Combination")
  )
  lyt_orig <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "BMRKR2"))
  tbl_orig <- build_table(lyt_orig, ex_adsl)

  lyt_lab <- basic_table() %>%
    split_cols_by("ARM", labels_var = "ARMLAB") %>%
    analyze(c("AGE", "BMRKR2"))
  tbl_lab <- build_table(lyt_lab, ex_adsl)

  tbl_orig
  tbl_lab # wrong labeling here
  expect_identical(
    names(tbl_lab),
    names(tbl_orig)
  )
  str <- matrix_form(tbl_lab)$strings
  expect_identical(
    as.vector(str[1, ]),
    c("", "Drug X", "Placebo", "Combination")
  )
})


## pathing regression tests
test_that("pathing works", {
  ## issue https://github.com/insightsengineering/rtables/issues/172
  result_overall <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM") %>%
    add_overall_col("overall") %>%
    analyze(c("AGE", "SEX")) %>%
    build_table(ex_adsl)

  va <- value_at(result_overall, c("AGE", "Mean"), c("ARM", "C: Combination"))
  expect_identical(va, result_overall[2, 3, drop = TRUE])

  ## issue https://github.com/insightsengineering/rtables/issues/178
  t2 <- basic_table() %>%
    split_cols_by("ARMCD") %>%
    split_rows_by("COUNTRY", split_fun = keep_split_levels("CHN")) %>%
    analyze("SEX") %>%
    analyze("AGE", nested = FALSE) %>%
    analyze("BMRKR1") %>%
    build_table(ex_adsl)

  ## this may get changed, but for now enforce it
  expect_error(cell_values(t2, "AGE"))
  expect_identical(
    cell_values(t2, c("ma_AGE_BMRKR1", "AGE")),
    cell_values(t2, c("ma_AGE_BMRKR1", "AGE", "Mean"))
  )
  expect_identical(
    cell_values(t2, c("ma_AGE_BMRKR1", "AGE")),
    lapply(split(ex_adsl$AGE, ex_adsl$ARMCD), mean)
  )
})

## issue https://github.com/insightsengineering/rtables/issues/175
test_that("pagination works on tables with only 1 row", {
  tt <- rtable(header = " ", rrow("", "NUll report"))
  expect_identical(nrow(tt), 1L)
  expect_identical(pag_tt_indices(tt), list(1L))
})

test_that("in_rows doesn't clobber cell format when only 1 row", {
  afun <- function(x) {
    in_rows("name" = rcell(123.31241231, format = "xx.xx"))
  }
  lyt <- basic_table() %>%
    analyze("AGE", afun = afun)
  tbl <- build_table(lyt, DM)
  mf <- matrix_form(tbl)
  expect_identical(mf$strings[2, 2, drop = TRUE], "123.31")
})


## newlabels works in reorder_split_levels (https://github.com/insightsengineering/rtables/issues/191)

test_that("newlabels works in reorder_split_levels", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by(
      "COUNTRY",
      split_fun = reorder_split_levels(
        neworder = c("CAN", "PAK", "BRA"),
        newlabels = c(CAN = "Canada", PAK = "Pakistan", BRA = "Brazil")
      )
    ) %>%
    analyze("AGE")
  tab <- build_table(lyt, ex_adsl)
  expect_identical(
    c("Canada", "Mean", "Pakistan", "Mean", "Brazil", "Mean"),
    row.names(tab)
  )
})



## https://github.com/insightsengineering/rtables/issues/198
test_that("no extraneous footnote attribute", {
  r1 <- in_rows(
    .list = list(
      ncols = rcell(5L, "xx", label = "ncol")
    )
  )
  expect_false("footnote" %in% names(attributes(r1$ncols)))

  r2 <- in_rows(
    .list = list(
      ncols = rcell(5L, "xx", label = "ncol"),
      nrows = rcell(10L, "xx", label = "nrow")
    )
  )
  expect_false("footnote" %in% names(attributes(r2$ncols)))
})


## https://github.com/insightsengineering/rtables/issues/200
# nolint start
test_that("no max is -Inf warnings from make_row_df when content rows exist in places that don't have any child rows in the subsequent split", {
  # nolint end
  dat2 <- data.frame(
    l1 = factor(c("A", "B")),
    l2 = factor(c("aa1", "bb1")),
    l3 = c("aaa1", "bbb1"),
    stringsAsFactors = FALSE
  )

  lyt <- basic_table() %>%
    split_rows_by("l1") %>%
    summarize_row_groups() %>%
    split_rows_by("l2") %>%
    summarize_row_groups() %>%
    split_rows_by("l3") %>%
    summarize_row_groups()
  tbl <- build_table(lyt, dat2)
  ## again, regexp of NA tests for ***no warnings***
  ## I know, I know, but I didn't design testthat!
  expect_warning(make_row_df(tbl), regexp = NA)
})

## discovered while preparing response for https://github.com/insightsengineering/rtables/issues/307
test_that("specifying function format with no cfun in summarize_row_groups works", {
  formfun <- function(x, output) if (x[1] == 0) "0" else format_value(x, "xx (xx.x%)", output = output)

  lyt <- basic_table() %>%
    split_cols_by("SEX", split_fun = keep_split_levels(c("F", "M"))) %>%
    split_rows_by("RACE",
      split_label = "Ethnicity", # 5
      label_pos = "topleft",
      split_fun = keep_split_levels(c("ASIAN", "WHITE"))
    ) %>%
    summarize_row_groups(format = formfun) %>% # 4
    analyze("AGE", afun = mean, format = "xx.x")

  tbl <- build_table(lyt, DM[1:15, ]) # WHITE-F is 0  in the first 15 rows...
  mat <- matrix_form(tbl)
  expect_identical(mat$strings[4, 2, drop = TRUE], "0")
})

## https://github.com/insightsengineering/rtables/issues/314
test_that("child_label = hidden does not affect tree structure/pathing", {
  df <- expand.grid(
    ARM = factor(paste("ARM", c("A", "B"))),
    FCT = factor(c("f1", "f2"))
  )
  df <- cbind(df, val = seq_len(NROW(df)))

  df

  s_test <- function(df, ...) in_rows(mn = 1, sd = 2)

  lyt <- basic_table() %>%
    split_cols_by("ARM", ref_group = "ARM A") %>%
    split_rows_by("FCT", child_labels = "hidden") %>%
    analyze("val", afun = s_test)

  tbl <- build_table(lyt, df)

  lyt2 <- basic_table() %>%
    split_cols_by("ARM", ref_group = "ARM A") %>%
    split_rows_by("FCT") %>%
    analyze("val", afun = s_test)

  tbl2 <- build_table(lyt2, df)

  rdf1 <- make_row_df(tbl)
  rdf2 <- make_row_df(tbl2)
  expect_identical(
    row_paths(tbl),
    row_paths(tbl2)[-c(1, 4)]
  )

  expect_identical(
    make_row_df(tbl, visible_only = FALSE)$path,
    make_row_df(tbl2, visible_only = FALSE)$path[-c(2, 7)]
  )

  expect_identical(
    value_at(tbl, c("FCT", "f1", "val", "mn"), c("ARM", "ARM A")),
    1
  )
  expect_identical(
    value_at(tbl2, c("FCT", "f1", "val", "mn"), c("ARM", "ARM A")),
    1
  )
})


## ensure nested = FALSE not needed after analyze

test_that("nested = FALSE not needed after analyze", {
  lyt1 <- basic_table() %>%
    analyze("AGE") %>%
    split_rows_by("STRATA1") %>%
    analyze("AGE")

  lyt2 <- basic_table() %>%
    analyze("AGE") %>%
    split_rows_by("STRATA1", nested = FALSE) %>%
    analyze("AGE")

  expect_identical(lyt1, lyt2)
})



test_that("indent mod preserved when paginating between multi-analyses", {
  adsl2 <- ex_adsl
  adsl2$smoker <- factor(NA, levels = c("10 cigarettes", ">10 cigarettes"))
  adsl2$age_grp <- cut(adsl2$AGE, c(18, 65, 75, 1000), labels = c(
    "18 <= to < 65",
    "65 <= to < 75",
    "Elderly >= 75"
  ))

  ## make one of the factor levels of SEX variable empty
  adsl2 <- subset(adsl2, SEX != "UNDIFFERENTIATED")

  ## helper that omits the pct entirely if the count is 0
  count_pct <- function(x, .N_col, ...) {
    if (x == 0) {
      rcell(0, format = "xx")
    } else {
      rcell(c(x, x / .N_col), format = "xx (xx.x%)")
    }
  }

  ## analysis function: table factor then apply above to get our cell values
  tab_w_pct <- function(x, .N_col, ...) {
    tab <- as.list(table(x))
    lapply(tab, count_pct, .N_col = .N_col)
  }

  lyt3 <- basic_table() %>%
    split_cols_by("ARM") %>%
    summarize_row_groups("USUBJID", label_fstr = "Number of Patients", format = "xx") %>%
    analyze("SEX", tab_w_pct, var_labels = "Gender", indent_mod = -1) %>%
    analyze("smoker", tab_w_pct, indent_mod = -1) %>%
    analyze("age_grp", tab_w_pct, indent_mod = -1)

  tab <- build_table(lyt3, adsl2)

  res <- paginate_table(tab, lpp = 10, verbose = TRUE)

  rdf <- make_row_df(res[[2]])
  expect_equal(
    rdf$indent[2], # smoker row
    0
  )
})


## https://github.com/insightsengineering/rtables/issues/634
## problem was actually in formatters fixed there in PR #152
test_that("export_as_txt works when there are newlines in column labels (naturally or after wrapping", {
  tbl <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ACTARM") %>%
    split_rows_by(
      "PARAMCD",
      labels_var = "PARAM",
      split_fun = drop_split_levels
    ) %>%
    split_rows_by(
      "AVISIT",
      split_fun = drop_split_levels,
      label_pos = "hidden"
    ) %>%
    split_cols_by_multivar(
      vars = c("AVAL", "CHG"),
      varlabels = c("Analysis Value", "Change from\nBaseline")
    ) %>%
    analyze_colvars(afun = mean) %>%
    build_table(formatters::ex_adlb)

  expect_silent(tmp <- export_as_txt(tbl, lpp = 20))
})

## overridden colcounts via build_table are used correctly
## during tabulation

test_that("overridden colcounts via build_table are used during tabulation correctly", {
  afun <- function(x, .N_col) {
    .N_col
  }

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA1") %>%
    analyze("AGE", afun = afun)

  tbl <- build_table(lyt, ex_adsl, col_counts = 1:9)
  mpf <- matrix_form(tbl)
  strs <- mf_strings(mpf)
  expect_identical(strs[3, -1],
                   paste0("(N=", strs[4, -1], ")")) 
})
