context("Custom Split Functions")

# Custom functions works -------------------------------------------------------
test_that("Custom functions in multivar splits work", {
  uneven_splfun <- function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
    ret <- do_base_split(spl, df, vals, labels, trim)
    if (NROW(df) == 0) {
      ret <- lapply(ret, function(x) x[1])
    }
    ret
  }

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by_multivar(c("USUBJID", "AESEQ", "BMRKR1"),
      varlabels = c("N", "E", "BMR1"),
      split_fun = uneven_splfun
    ) %>%
    analyze_colvars(list(
      USUBJID = function(x, ...) length(unique(x)),
      AESEQ = max,
      BMRKR1 = mean
    ))

  tab <- build_table(lyt, subset(ex_adae, as.numeric(ARM) <= 2))

  expect_equal(ncol(tab), 7)

  uneven_row_splfun <- function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
    ret <- do_base_split(spl, df, vals, labels, trim)
    if (NROW(df) < 125) ret <- lapply(ret, function(x) x[1])
    ret
  }

  lyt <- basic_table() %>%
    split_rows_by("ARM") %>%
    split_rows_by_multivar(c("SEX", "STRATA1"), split_fun = uneven_row_splfun) %>%
    summarize_row_groups()

  tab2 <- build_table(lyt, DM)

  expect_equal(nrow(tab2), 10)
})

test_that("add_overall_level works", {
  l <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = FALSE)) %>%
    analyze("AGE")

  tab <- build_table(l, DM)

  lb <- basic_table() %>%
    split_cols_by("ARM", split_fun = add_overall_level("All Patients", first = TRUE)) %>%
    analyze("AGE")

  tab_b <- build_table(lb, DM)

  cvs <- cell_values(tab)
  expect_identical(
    cvs[c(4, 1:3)],
    cell_values(tab_b)
  )

  expect_identical(cvs[[4]], mean(DM$AGE))

  l2 <- basic_table() %>%
    split_rows_by("RACE", split_fun = add_overall_level("All Ethnicities")) %>%
    summarize_row_groups(label_fstr = "%s (n)") %>%
    analyze("AGE")

  tab2 <- build_table(l2, DM)
  expect_identical(
    c(nrow(DM), 1),
    cell_values(tab2)[[1]][[1]]
  )
})

# split_rows_by_multivar works -------------------------------------------------
test_that("split_rows_by_multivar and add_overall_level throw an error", {
  expect_silent(
    lyt <- basic_table() %>%
      split_rows_by_multivar(c("SEX", "STRATA1"),
        split_fun = add_overall_level("TOT")
      ) %>%
      summarize_row_groups()
  )
  expect_error(
    tbl1 <- build_table(lyt, DM),
    "does not make sense"
  )

  lyt <- basic_table() %>%
    split_rows_by_multivar(c("SEX", "STRATA1")) %>%
    summarize_row_groups()

  tbl1 <- build_table(lyt, DM)

  expect_identical(
    cell_values(tbl1),
    list(SEX.SEX = list(`all obs` = c(356, 1)), STRATA1.STRATA1 = list(`all obs` = c(356, 1)))
  )
})

# make_split_fun works ---------------------------------------------------------
test_that("make_split_fun works", {
  mysplitfun <- make_split_fun(
    pre = list(drop_facet_levels),
    post = list(add_overall_facet("ALL", "All Arms"))
  )

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = mysplitfun) %>%
    analyze("AGE")
  tbl <- build_table(lyt, subset(DM, ARM %in% c("B: Placebo", "C: Combination")))

  ccounts <- col_counts(tbl)
  expect_equal(ncol(tbl), 3L)
  expect_equal(ccounts[3], sum(DM$ARM %in% c("B: Placebo", "C: Combination")))

  lyt2a <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = trim_levels_in_group("SEX", drop_outlevs = TRUE)) %>%
    split_cols_by("SEX") %>%
    analyze("AGE")

  adslsub <- subset(ex_adsl, (ARM == "A: Drug X" & SEX == "F") | (ARM == "B: Placebo" & SEX == "M"))
  tbl2a <- build_table(lyt2a, adslsub)

  mysplitfun2 <- make_split_fun(
    pre = list(drop_facet_levels),
    post = list(trim_levels_in_facets("SEX"))
  )

  lyt2b <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = mysplitfun2) %>%
    split_cols_by("SEX") %>%
    analyze("AGE")

  tbl2b <- build_table(lyt2b, adslsub)

  expect_identical(cell_values(tbl2a), cell_values(tbl2b))
  expect_identical(row_paths(tbl2a), row_paths(tbl2b))
  expect_identical(col_paths(tbl2a), col_paths(tbl2b))
  expect_identical(
    matrix_form(tbl2a, TRUE),
    matrix_form(tbl2b, TRUE)
  )

  broken_on_purpose <- make_split_fun(pre = list(function(df, ...) stop("oopsie")))

  lyt3 <- basic_table() %>%
    split_cols_by("ARM", split_fun = broken_on_purpose) %>%
    analyze("ARM")

  expect_error(build_table(lyt3, DM), "Error applying custom split function: oopsie")

  ## overriding core core split functionality
  very_stupid_core <- function(spl, df, vals, labels, .spl_context) {
    make_split_result(
      c("stupid", "silly"),
      datasplit = list(df[1:10, ], df[11:30, ]),
      labels = c("first 10", "second 20"),
      subset_exprs = list(
        quote(seq_along(AGE) <= 10),
        quote(seq_along(AGE) %in% 11:30)
      )
    )
  }

  nonsense_splfun <- make_split_fun(
    core_split = very_stupid_core,
    post = list(add_combo_facet("dumb",
      label = "thirty patients",
      levels = c("stupid", "silly")
    ))
  )
  lyt4a <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = nonsense_splfun) %>%
    analyze("AGE")

  tbl4a <- build_table(lyt4a, DM)
  expect_equal(
    col_counts(tbl4a),
    c(10L, 20L, 30L)
  )


  lyt4b <- basic_table() %>%
    split_rows_by("ARM", split_fun = nonsense_splfun) %>%
    summarize_row_groups() %>%
    analyze("AGE")

  tbl4b <- build_table(lyt4b, DM)

  pths <- row_paths(tbl4b)
  ## check the counts, which checks whether our artificial
  ## facets were created correctly
  expect_equal(
    10,
    cell_values(tbl4b, pths[[1]])[[1]][[1]]
  )
  expect_equal(
    20,
    cell_values(tbl4b, pths[[3]])[[1]][[1]]
  )
  expect_equal(
    30,
    cell_values(tbl4b, pths[[5]])[[1]][[1]]
  )

  ## add_comb_facet within make_split_fun in column space, regression test

  combofun <- add_combo_facet("combo", "Drug X or Combo", c("A: Drug X", "C: Combination"))
  mysplfun <- make_split_fun(post = list(combofun))

  lyt5 <- basic_table() %>%
    split_cols_by("ARM", split_fun = mysplfun) %>%
    analyze("STRATA1")

  tbl5 <- build_table(lyt5, ex_adsl)
  ## each combo value is A count + C count
  vals <- cell_values(tbl5)
  expect_true(all(sapply(vals, function(vi) vi$combo == vi[[1]] + vi[[3]])))
})

test_that("spl_variable works", {
  rem_lev_facet <- function(torem) {
    function(df, spl, vals, labels, ...) {
      var <- spl_variable(spl)
      expect_identical(var, "ARM")
      vec <- df[[var]]
      bad <- vec == torem
      df <- df[!bad, ]
      levs <- if (is.character(vec)) unique(vec) else levels(vec)
      df[[var]] <- factor(as.character(vec[!bad]), levels = setdiff(levs, torem))
      df
    }
  }

  mysplitfun <- make_split_fun(pre = list(rem_lev_facet("A: Drug X")))

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by("ARM", split_fun = mysplitfun) %>%
    analyze("AGE")
  tbl <- expect_silent(build_table(lyt, DM))
  expect_equal(ncol(tbl), 2L)

  lyt <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by_multivar(c("ARM", "SEX"), split_fun = mysplitfun) %>%
    analyze("AGE")

  expect_error(
    build_table(lyt, DM),
    "Split class MultiVarSplit not associated with a single variable"
  )
})
