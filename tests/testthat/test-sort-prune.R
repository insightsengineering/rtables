context("sorting and pruning")

rawtable <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX") %>%
  split_rows_by("RACE") %>%
  summarize_row_groups() %>%
  split_rows_by("STRATA1") %>%
  summarize_row_groups() %>%
  analyze("AGE") %>%
  build_table(DM)


test_that("provided pruning functions work", {
  ## all_zero_or_na
  expect_false(all_zero_or_na(rrow("hi")), "Don't trim label rows")
  expect_true(all_zero_or_na(rrow("weird", NA, NaN, 0, 0L, Inf, -Inf)))

  ## content_all_zeros_nas
  racecounts <- table(DM$RACE)
  racecounts <- setNames(as.integer(racecounts), names(racecounts))
  expect_identical(sapply(tree_children(rawtable), content_all_zeros_nas), racecounts == 0)
})

test_that("pruning and trimming work", {
  silly_prune <- function(tt) {
    if (!is(tt, "TableRow") || is(tt, "LabelRow")) {
      return(FALSE)
    }
    all_zero_or_na(tt)
  }

  smallertab <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze("AGE") %>%
    build_table(DM)

  ptab <- prune_table(smallertab, silly_prune)
  ## ensure that empty subtables are removed when pruning

  expect_identical(
    prune_table(smallertab),
    smallertab[1:4, ]
  )


  biggertab <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    split_rows_by("STRATA1") %>%
    analyze("AGE") %>%
    build_table(subset(DM, STRATA1 != "C"))

  ## something trimmed from every outer facet
  pbtab <- prune_table(biggertab)
  expect_equal(nrow(pbtab), 10)


  ## this one doesn't remove NA rows
  expect_identical(
    prune_table(smallertab, prune_zeros_only),
    smallertab
  )
  expect_identical(dim(ptab), c(4L, 3L))
  trm1 <- trim_rows(smallertab)
  ## ensure/retain structure unawareness of trim_rows
  expect_identical(dim(trm1), c(6L, 3L))

  smallertab2 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    summarize_row_groups() %>%
    analyze("AGE") %>%
    build_table(DM)

  expect_identical(
    row.names(prune_table(smallertab)),
    row.names(prune_table(smallertab2))
  )

  expect_identical(
    prune_table(smallertab2, low_obs_pruner(60, type = "mean")),
    smallertab2[1:2, ]
  )

  expect_identical(
    prune_table(smallertab2, low_obs_pruner(60, type = "mean")),
    smallertab2[1:2, ]
  )

  expect_identical(
    prune_table(smallertab2, low_obs_pruner(180)),
    smallertab2[1:2, ]
  )
})

test_that("provided score functions work", {
  smallertab2 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    summarize_row_groups() %>%
    analyze("AGE") %>%
    build_table(DM)
  kids <- tree_children(smallertab2)
  scores <- sapply(kids, cont_n_allcols)
  counts <- table(DM$SEX)
  expect_identical(scores, setNames(as.numeric(counts), names(counts)))

  onecol_fun <- cont_n_onecol(1)
  scores2 <- sapply(kids, onecol_fun)
  dmsub <- subset(DM, ARM == "A: Drug X")
  counts2 <- table(dmsub$SEX)
  expect_identical(scores2, setNames(as.numeric(counts2), names(counts2)))
})


## todo test sorting proper


## contributed by daniel
test_that("sort_at_path just returns an empty input table", {
  silly_prune_condition <- function(tt) {
    TRUE
  }
  emptytable <- trim_rows(rawtable, silly_prune_condition)
  expect_identical(dim(emptytable), c(0L, ncol(rawtable)))
  result <- sort_at_path(
    emptytable,
    path = c("ARM", "*", "SEX"),
    scorefun = cont_n_allcols
  )
  expect_identical(emptytable, result)
})

test_that("trim_rows and prune_table do the same thing in normal cases", {
  bigtbl <- basic_table() %>%
    split_rows_by("RACE") %>%
    split_rows_by("COUNTRY") %>%
    analyze("AGE") %>%
    build_table(ex_adsl)

  ptbl <- prune_table(bigtbl)
  nspl <- split(ex_adsl, ex_adsl$RACE)
  num <- sum(
    sapply(nspl, function(df) 2 * length(unique(df$COUNTRY))),
    length(unique(ex_adsl$RACE))
  )
  expect_equal(nrow(ptbl), num)

  tr_tbl <- trim_rows(bigtbl)
  expect_true(nrow(tr_tbl) > num)
})



test_that("provided score functions throw informative errors when invalid and * in paths work", {
  grade_groups_dict <- list(
    "Any Grade" = c("1", "2", "3", "4", "5"),
    "Grade 1-2" = c("1", "2"),
    "1" = "1",
    "2" = "2",
    "Grade 3-4" = c("3", "4"),
    "3" = "3",
    "4" = "4",
    "Grade 5" = c("5")
  )


  basic_grade_count <- function(df, .var, .N_col, grade_groups = grade_groups_dict, id = "USUBJID", labelstr = "") {
    fvec <- unclass(df)[[.var]]
    newvals <- as.numeric(levels(fvec)[fvec])
    df$grade_num <- newvals
    form <- as.formula(sprintf("grade_num ~ %s", id))
    aggrdf <- stats::aggregate(form, data = df, FUN = max)

    in_rows(
      .list = lapply(grade_groups, function(x) {
        subdf <- aggrdf[aggrdf$grade_num %in% x, ]
        cnt <- length(unique(unclass(subdf)[[id]]))
        c(cnt, cnt / .N_col)
      }),
      .names = names(grade_groups),
      .formats = "xx (xx.x%)"
    )
  }


  real_scorefun <- function(tt) {
    row <- cell_values(tt, rowpath = c("AETOXGR", "Any Grade"))
    sum(unlist(row))
  }


  lyt_raw <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(var = "ACTARM", split_fun = add_overall_level("total", first = FALSE)) %>%
    summarize_row_groups("AETOXGR", cfun = basic_grade_count, extra_args = list(grade_groups = grade_groups_dict)) %>%
    split_rows_by("AEBODSYS",
      indent_mod = -1,
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = "aebod sys label",
      child_labels = "visible"
    ) %>%
    summarize_row_groups("AETOXGR", cfun = basic_grade_count, extra_args = list(grade_groups = grade_groups_dict)) %>%
    split_rows_by("AEDECOD",
      indent_mod = -1,
      split_fun = drop_split_levels,
      label_pos = "topleft",
      split_label = "aedecod label"
    ) %>%
    analyze("AETOXGR",
      basic_grade_count,
      extra_args = list(grade_groups = grade_groups_dict),
      indent_mod = -1
    )
  raw_tbl <- build_table(lyt_raw, ex_adae)

  expect_silent({
    stbl <- sort_at_path(raw_tbl,
      path = c("AEBODSYS", "*", "AEDECOD"),
      scorefun = real_scorefun, # cont_n_allcols,
      decreasing = TRUE
    )
  })

  ## leading "root" doesn't bother it #816
  expect_silent({
    stbl2 <- sort_at_path(raw_tbl,
      path = c("root", "AEBODSYS", "*", "AEDECOD"),
      scorefun = real_scorefun, # cont_n_allcols,
      decreasing = TRUE
    )
  })
  expect_identical(cell_values(stbl), cell_values(stbl2))

  ## spot check that things were reordered as we expect
  expect_identical(
    row_paths(raw_tbl)[63:71], ## "cl B.2" ->  "dcd B.2.1.2.1" old position
    row_paths(stbl)[72:80]
  ) ## "cl B.2" -> "dcd B.2.1.2.1" new position
  expect_error(
    {
      sort_at_path(raw_tbl,
        path = c("AEBODSYS", "*", "AEDECOD"),
        scorefun = cont_n_allcols,
        decreasing = TRUE
      )
    },
    "occurred at path: AEBODSYS -> * (cl A.1) -> AEDECOD -> dcd A.1.1.1.1",
    fixed = TRUE
  )
  expect_error(
    {
      sort_at_path(raw_tbl,
        path = c("AEBODSYS", "*", "AEDECOD"),
        scorefun = cont_n_onecol(1),
        decreasing = TRUE
      )
    },
    "occurred at path: AEBODSYS -> * (cl A.1) -> AEDECOD -> dcd A.1.1.1.1",
    fixed = TRUE
  )
  ## paths that are entirely wrong (don't exist at all) work out ok.
  expect_error(
    {
      sort_at_path(raw_tbl,
        path = c("AEBODSYS", "*", "WRONG"),
        scorefun = cont_n_onecol(1),
        decreasing = TRUE
      )
    },
    "occurred at path: AEBODSYS -> * (cl A.1)",
    fixed = TRUE
  )
})

test_that("paths come out correct when sorting with '*'", {
  tbl <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    summarize_row_groups() %>%
    analyze("STRATA1") %>%
    build_table(DM)

  scorefun <- function(tt) sum(unlist(row_values(tt)))

  tbl <- sort_at_path(tbl, c("RACE", "*", "STRATA1"), scorefun)

  res <- cell_values(
    tbl,
    c("RACE", "BLACK OR AFRICAN AMERICAN", "STRATA1", "C"),
    c("ARM", "A: Drug X")
  )
  expect_equal(
    res,
    list("A: Drug X" = 12)
  )
})


## rtables now guarantees uniqueness of sibling names so this
## is no longer an error
test_that("sort_at_path works with uniqified names", {
  # Related to regression test #864
  adsl <- ex_adsl
  adsl$flag <- sample(c("Y", "N"), nrow(adsl), replace = TRUE)

  lyt <- basic_table() %>%
    split_rows_by("flag", split_fun = keep_split_levels("Y")) %>%
    split_rows_by("SEX") %>%
    analyze("BMRKR1") %>%
    split_rows_by("flag", split_fun = keep_split_levels("Y")) %>%
    split_rows_by("SEX") %>%
    analyze("AGE")

  tbl <- build_table(lyt, adsl)

  scorefun <- function(tt) {
    unlist(cell_values(tt))
  }

  expect_silent(sort_at_path(tbl, c("root", "flag", "Y", "SEX"), scorefun))
})

test_that("passing extra stuff to sorting and pruning works", {
  prfun <- function(x, myarg) {
    force(myarg) ## cause error if it isn't passed
    TRUE
  }
  lyt <- basic_table() %>%
    split_rows_by("STRATA1") %>%
    analyze("SEX")

  tbl <- build_table(lyt, ex_adsl)

  expect_error(prune_table(tbl, prfun))
  expect_silent(prune_table(tbl, prfun, myarg = "hi"))

  scorefun1 <- function(x, decreasing) {
    force(decreasing)
    rnorm(1)
  }

  scorefun2 <- function(x, myarg) {
    force(myarg)
    rnorm(1)
  }

  scorefun3 <- function(x, decreasing, myarg) {
    force(decreasing)
    force(myarg)
    rnorm(1)
  }

  ## score functions that don't accept decreasing are tested elsewhere
  expect_error(sort_at_path(tbl, c("STRATA1", "*", "SEX"), scorefun2))
  expect_error(sort_at_path(tbl, c("STRATA1", "*", "SEX"), scorefun3))
  expect_silent(sort_at_path(tbl, c("STRATA1", "*", "SEX"), scorefun1))
  expect_silent(sort_at_path(tbl, c("STRATA1", "*", "SEX"), scorefun2, myarg = "hi"))
  expect_silent(sort_at_path(tbl, c("STRATA1", "*", "SEX"), scorefun3, myarg = "hi"))
})
