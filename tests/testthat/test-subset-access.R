context("Accessing and subsetting tables")

test_that("cell_values function works as desired", {
  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX") %>%
    split_rows_by("RACE") %>%
    summarize_row_groups() %>%
    split_rows_by("STRATA1") %>%
    analyze("AGE", afun = function(x, .N_col, .N_row) rcell(c(.N_row, .N_col), format = "(xx.x, xx.x)"))

  ourdat <- DM
  ourdat$SEX <- droplevels(ourdat$SEX)
  ourdat$RACE <- droplevels(ourdat$RACE)
  tbl <- build_table(l, ourdat)

  armsextab <- table(ourdat$SEX, ourdat$ARM)

  armaval <- "A: Drug X"
  cvres1 <- cell_values(tbl, c("RACE", "ASIAN"), c("ARM", "A: Drug X", "SEX", "M"))
  contcount <- nrow(subset(ourdat, RACE == "ASIAN" & ARM == armaval & SEX == "M"))
  asianstrata <- table(subset(ourdat, RACE == "ASIAN")$STRATA1)
  expect_equal(
    unname(cvres1),
    list(
      list("A: Drug X.M" = c(
        contcount,
        contcount / armsextab["M", armaval]
      )),
      list("A: Drug X.M" = c(
        unname(asianstrata["A"]),
        armsextab["M", armaval]
      )),
      list("A: Drug X.M" = c(
        unname(asianstrata["B"]),
        armsextab["M", armaval]
      )),
      list("A: Drug X.M" = c(
        unname(asianstrata["C"]),
        armsextab["M", armaval]
      ))
    )
  )



  cvres2 <- cell_values(
    tbl, c("RACE", "ASIAN", "STRATA1"),
    c("ARM", "A: Drug X", "SEX", "M")
  )
  expect_identical(unname(cvres1[2:4]), unname(cvres2))
  cvres3 <- cell_values(
    tbl, c("RACE", "ASIAN", "STRATA1", "B"),
    c("ARM", "A: Drug X", "SEX", "M")
  )
  expect_identical(cvres3, cvres1[[3]])
  ## any arm, male columns from the ASIAN content (i.e. summary) row
  cvres4 <- cell_values(tbl, c("RACE", "ASIAN", "@content"))
  expect_identical(cvres4[2], cvres1[[1]])

  cvres5 <- cell_values(
    tbl, c("RACE", "ASIAN", "@content"),
    c("ARM", "*", "SEX", "M")
  )
  expect_identical(cvres5, cvres4[seq(2, 6, by = 2)])
  ## all columns
  cvres6 <- cell_values(tbl, c("RACE", "ASIAN", "STRATA1", "B"))

  ## all columns for the Combination arm
  cvres7 <- cell_values(
    tbl, c("RACE", "ASIAN", "STRATA1", "B"),
    c("ARM", "C: Combination")
  )

  expect_identical(
    cvres6[5:6],
    cvres7
  )

  cvres8 <- cell_values(
    tbl, c("RACE", "ASIAN", "STRATA1", "B", "AGE"),
    c("ARM", "C: Combination", "SEX", "M")
  )
  vares8 <- value_at(
    tbl, c("RACE", "ASIAN", "STRATA1", "B", "AGE"),
    c("ARM", "C: Combination", "SEX", "M")
  )
  expect_identical(cvres8[[1]], vares8)
  expect_error(value_at(
    tbl, c("RACE", "ASIAN", "STRATA1", "B"),
    c("ARM", "C: Combination", "SEX", "M")
  ))
  expect_error(value_at(
    tbl, c("RACE", "ASIAN", "STRATA1", "B", "AGE"),
    c("ARM", "C: Combination", "SEX")
  ))
  expect_error(value_at(
    tbl, c("RACE", "ASIAN", "STRATA1", "B", "AGE"),
    c("ARM", "C: Combination")
  ))

  allrows <- collect_leaves(tbl, TRUE, TRUE)
  crow <- allrows[[1]]
  lrow <- allrows[[2]]
  expect_error(
    cell_values(crow, rowpath = "@content"),
    "cell_values on TableRow objects must have NULL rowpath"
  )
  expect_error(
    cell_values(lrow),
    "cell_values on LabelRow is not meaningful"
  )
})

test_that("Subsetting by integer(0) keeps decorations", {
  # Regression #870
  test_tbl <- basic_table(title = "t", subtitles = "s", main_footer = "mf", prov_footer = "pf") %>%
    analyze("BMRKR1") %>%
    build_table(DM)

  expect_equal(main_title(test_tbl), main_title(test_tbl[integer(), , keep_titles = TRUE]))
  expect_equal(subtitles(test_tbl), subtitles(test_tbl[integer(), , keep_titles = TRUE]))
  expect_equal(all_footers(test_tbl), all_footers(test_tbl[integer(), , keep_footers = TRUE]))

  expect_no_error(test_tbl[, NA])
  expect_no_error(test_tbl[NA, NA])
  expect_no_error(test_tbl[, c(0, 1)])
  expect_no_error(test_tbl[, c(NA, 1)])

  expect_error(
    test_tbl[, integer()],
    "No column selected."
  )
})


test_colpaths <- function(tt) {
  cdf <- make_col_df(tt, visible_only = TRUE)
  cdf2 <- make_col_df(tt, visible_only = FALSE)
  res3 <- lapply(cdf$path, function(pth) rtables:::subset_cols(tt, pth))
  res4 <- lapply(cdf2$path, function(pth) rtables:::subset_cols(tt, pth))
  expect_identical(res3, res4[!is.na(cdf2$abs_pos)])
  expect_identical(res3, lapply(
    seq_len(ncol(tt)),
    function(j) tt[, j]
  ))
  TRUE
}


test_rowpaths <- function(tt, visonly = TRUE) {
  cdf <- make_row_df(tt, visible_only = visonly)
  res3 <- lapply(cdf$path, function(pth) cell_values(tt, pth)) # nolint
  TRUE
}



test_that("make_row_df, make_col_df give paths which all work", {
  skip_if_not_installed("tibble")
  require(tibble, quietly = TRUE)

  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("SEX", ref_group = "F") %>%
    analyze("AGE", mean, show_labels = "hidden") %>%
    analyze("AGE", refcompmean,
      show_labels = "hidden",
      table_names = "AGE2a"
    ) %>%
    split_rows_by("RACE", nested = FALSE, split_fun = drop_split_levels) %>%
    analyze("AGE", mean, show_labels = "hidden") %>%
    analyze("AGE", refcompmean, show_labels = "hidden", table_names = "AGE2b")


  tab <- build_table(lyt, rawdat)
  rdf1a <- make_row_df(tab)
  rdf1b <- make_row_df(tab, visible_only = FALSE)
  expect_true(
    all.equal(rdf1a, rdf1b[!is.na(rdf1b$abs_rownumber), ],
      check.attributes = FALSE
    ), ## rownames ugh
    "visible portions of row df not identical between
visible_only and not"
  )

  allcvs <- cell_values(tab)
  allcvs_un <- unname(allcvs)
  pathres <- lapply(rdf1b$path, function(pth) unname(cell_values(tab, pth)))
  expect_identical(
    pathres,
    list(
      allcvs_un,
      allcvs_un[1:2], ## XXX this probably shouldn't be here
      unname(allcvs_un[[1]]),
      unname(allcvs_un[[1]]),
      unname(allcvs_un[[2]]),
      unname(allcvs_un[[2]]),
      allcvs_un[3:6], ## RACE
      allcvs_un[3:4], ## WHITE Tabletree
      allcvs_un[3:4], ## WHITE LabelRow
      unname(allcvs_un[[3]]), ## white age ElemtaryTable
      unname(allcvs_un[[3]]), ## white age DataRow
      unname(allcvs_un[[4]]), ## white compare ElemtaryTable
      unname(allcvs_un[[4]]), ## white compare DataRow
      allcvs_un[5:6], ## BLACK TableTree
      allcvs_un[5:6], ## BLACK LabelRow
      unname(allcvs_un[[5]]), ## black ageElemtaryTable
      unname(allcvs_un[[5]]), ## black age DataRow
      unname(allcvs_un[[6]]), ## black compare ElemtaryTable
      unname(allcvs_un[[6]])
    )
  ) ## black compare DataRow

  test_colpaths(tab)




  combodf <- tribble(
    ~valname, ~label, ~levelcombo, ~exargs,
    "A_", "Arm 1", c("A: Drug X"), list(),
    "B_C", "Arms B & C", c("B: Placebo", "C: Combination"), list()
  )

  l2 <- basic_table(show_colcounts = TRUE) %>%
    split_cols_by(
      "ARM",
      split_fun = add_combo_levels(combodf, keep_levels = c("A_", "B_C"))
    ) %>%
    analyze(c("AGE", "AGE"),
      afun = list(mean, range),
      show_labels = "hidden", table_names = c("AGE mean", "AGE range")
    )

  tab2 <- build_table(l2, DM)
  test_colpaths(tab2)
  cdf2 <- make_col_df(tab2)
  ## res5 <- lapply(cdf2$path, function(pth) subset)cols
})


test_that("Duplicate colvars path correctly", {
  l <- basic_table() %>%
    split_cols_by_multivar(c("AGE", "BMRKR1", "AGE"), varlabels = c("Age", "Biomarker 1", "Second Age")) %>%
    analyze_colvars(mean)

  tbl <- build_table(l, DM)

  matform <- matrix_form(tbl)
  expect_identical(
    matrix(
      c(
        "", "Age", "Biomarker 1", "Second Age",
        "mean", mean(DM$AGE), mean(DM$BMRKR1), mean(DM$AGE)
      ),
      nrow = 2, byrow = TRUE
    ),
    matform$strings
  )

  res <- cell_values(tbl, colpath = c("multivars", "AGE._[[2]]_."))
  expect_identical(
    list("AGE._[[2]]_." = mean(DM$AGE, na.rm = TRUE)),
    res
  )
})

test_that("top_left, title, footers retention behaviors are correct across all scenarios", {
  # topleft
  tlval <- "hi"

  # title
  ti <- "ti"
  sti <- "sti"

  # footers
  mf <- "mf"
  pf <- "pf"
  rf <- "rf"

  lyt <- basic_table(
    title = ti, subtitles = sti,
    main_footer = mf, prov_footer = pf
  ) %>%
    split_cols_by("ARM") %>%
    append_topleft(tlval) %>%
    split_rows_by("SEX") %>%
    analyze("AGE", mean)
  tbl <- build_table(lyt, DM)
  fnotes_at_path(tbl, rowpath = c("SEX", "F", "AGE", "mean")) <- rf
  fnotes_at_path(tbl, rowpath = c("SEX", "M", "AGE", "mean")) <- rf

  # topleft
  expect_identical(top_left(tbl), tlval)
  expect_identical(top_left(tbl[, 1]), tlval) ## default column-only subsetting is TRUE
  expect_identical(top_left(tbl[, 1, keep_topleft = FALSE]), character())
  expect_identical(top_left(tbl[, 1, keep_topleft = TRUE]), tlval)
  expect_identical(top_left(tbl[1, ]), character()) ## default with any row subsetting is FALSE
  expect_identical(top_left(tbl[1, , keep_topleft = FALSE]), character())
  expect_identical(top_left(tbl[1, , keep_topleft = TRUE]), tlval)
  expect_identical(top_left(tbl[1:2, 1:2]), character())
  expect_identical(top_left(tbl[1:2, 1:2, keep_topleft = FALSE]), character())
  expect_identical(top_left(tbl[1:2, 1:2, keep_topleft = TRUE]), tlval)

  # drop = TRUE works
  expect_identical(suppressWarnings(tbl[1, 1, drop = TRUE]), NULL)
  expect_warning(tbl[1, 1, drop = TRUE])
  expect_equal(tbl[2, 1, drop = TRUE], 33.71, tolerance = 0.01)

  # referential footnotes
  expect_identical(
    mf_rfnotes(matrix_form(tbl[2, 1])),
    paste0("{1} - ", rf)
  )
  expect_identical(
    mf_rfnotes(matrix_form(tbl[4, 1])),
    paste0("{1} - ", rf)
  )
  expect_identical(
    mf_rfnotes(matrix_form(tbl[4, 1, reindex_refs = FALSE])),
    paste0("{1} - ", rf)
  )
  expect_identical(mf_rfnotes(matrix_form(tbl[1, 1])), character())

  # titles and footers
  expect_identical(main_title(tbl[1, 1]), "")
  expect_identical(main_title(tbl[1, 1, keep_titles = FALSE]), "")
  expect_identical(main_footer(tbl[1, 1, keep_titles = FALSE]), character())
  expect_identical(main_title(tbl[1, 1, keep_titles = TRUE]), ti)
  expect_identical(subtitles(tbl[1, 1, keep_titles = TRUE]), sti)
  expect_identical(main_footer(tbl[1, 1, keep_footers = TRUE]), mf)
  expect_identical(prov_footer(tbl[1, 1, keep_footers = TRUE]), pf)

  # Further testing drop = TRUE
  tbl1 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze("AGE", function(x) list("m (sd)" = c(mean(x), sd(x)))) %>%
    build_table(DM)
  tbl2 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX", child_labels = "hidden") %>%
    analyze("AGE", mean) %>%
    build_table(DM)
  # row with only numbers -> warning
  expect_warning(tbl[4, , drop = TRUE])
  # warnings for label row
  expect_warning(tbl[, 1, drop = TRUE])
  expect_warning(tbl[3, , drop = TRUE])
  # warnings for more than one values
  expect_warning(tbl1[4, , drop = TRUE])
  expect_warning(tbl1[2, 1:2, drop = TRUE])
})

test_that("setters work ok", {
  tlval <- "hi"
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    summarize_row_groups() %>%
    analyze("AGE", mean)
  tbl <- build_table(lyt, DM)

  tbl2 <- tbl

  tbl2[1, 1] <- CellValue(c(1, .1))
  matform2 <- matrix_form(tbl2)
  expect_identical("1 (10.0%)", matform2$strings[2, 2])

  tbl3 <- tbl
  tbl3[3, 1:2] <- list(CellValue(c(1, 1)), CellValue(c(1, 1)))
  matform3 <- matrix_form(tbl3)
  expect_identical(rep("1 (100.0%)", 2), matform3$strings[4, 2:3])

  tbl2 <- tbl
  tt_at_path(tbl2, c("SEX", "UNDIFFERENTIATED")) <- NULL
  expect_equal(nrow(tbl2), 6)

  tbl3 <- tbl
  tt_at_path(tbl3, c("SEX", "UNDIFFERENTIATED", "AGE", "mean")) <- NULL
  expect_equal(nrow(tbl3), 7)

  lyt4 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze("AGE", mean)
  tbl4 <- build_table(lyt4, DM)

  tbl4[5:6, ] <- list(rrow("new label"), rrow("new mean", 5, 7, 8))
  mform4 <- matrix_form(tbl4)
  expect_identical(
    mform4$strings[6, , drop = TRUE],
    c("new label", "", "", "")
  )
  expect_identical(
    cell_values(tbl4)[["U.AGE.mean"]],
    list(5, 7, 8)
  )

  tbl5 <- tbl
  tbl5[4, 1] <- rcell(999, format = "xx.xx")
  tbl5[5, 2] <- list(c(3, 0.25))
  tbl5[5, 3] <- rcell(NA, format_na_str = "<NA>")
  tbl5[6, ] <- list(-111, -222, -333)
  matform5 <- matrix_form(tbl5)
  expect_identical(
    c("mean", "999.00", "32.1", "34.2794117647059"),
    mf_strings(matform5)[5, ]
  )
  expect_identical(
    c("U", "0 (0.0%)", "3 (25.0%)", "<NA>"),
    mf_strings(matform5)[6, ]
  )
  expect_identical(
    c("mean", "-111", "-222", "-333"),
    mf_strings(matform5)[7, ]
  )
  expect_identical(
    c("", "xx.xx", "xx", "xx"),
    mf_formats(matform5)[5, ]
  )
})

test_that("cell_values and value_at work on row objects", {
  tbl <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_cols_by("STRATA2") %>%
    analyze("AEDECOD") %>%
    build_table(ex_adae, ex_adsl)

  first_row <- collect_leaves(tbl)[[1]]

  va <- value_at(first_row, colpath = c("ARM", "A: Drug X", "STRATA2", "S2"))

  cv <- cell_values(first_row, colpath = c("ARM", "C: Combination"))

  expect_identical(va, 33L)

  expect_identical(
    cv,
    setNames(
      list(32L, 56L),
      c(
        "C: Combination.S1",
        "C: Combination.S2"
      )
    )
  )
})

test_that("label_at_path works", {
  lyt <- make_big_lyt()

  tab <- build_table(lyt, rawdat)
  orig_labs <- row.names(tab)

  tab4 <- tab

  label_at_path(tab4, c("root", "RACE", "WHITE", "FACTOR2", "B", "AGE")) <- NA_character_

  expect_identical(row.names(tab4), orig_labs[-9])

  tab5 <- tab

  newlab5 <- "race var label"
  label_at_path(tab5, c("root", "RACE")) <- newlab5
  expect_identical(row.names(tab5), c(newlab5, orig_labs))

  rps <- row_paths(tab)

  labs <- vapply(rps, function(pth) label_at_path(tab, pth), "",
    USE.NAMES = FALSE
  )
  expect_identical(labs, orig_labs)

  newthangalangs <- paste(orig_labs, "redux")

  tab7 <- tab
  for (i in seq_along(orig_labs)) label_at_path(tab7, rps[[i]]) <- newthangalangs[i]

  expect_identical(
    newthangalangs,
    row.names(tab7)
  )
})

test_that("insert_row_at_path works", {
  lyt <- basic_table() %>%
    split_rows_by("COUNTRY", split_fun = keep_split_levels(c("CHN", "USA"))) %>%
    summarize_row_groups() %>%
    analyze("AGE")

  tab <- build_table(lyt, DM)
  orig_rns <- row.names(tab)
  tab2 <- insert_row_at_path(
    tab, c("COUNTRY", "CHN", "AGE", "Mean"),
    rrow("new row", 555)
  )
  expect_identical(
    row.names(tab2),
    c(
      orig_rns[1],
      "new row",
      orig_rns[-1]
    )
  )

  tab3 <- insert_row_at_path(tab2, c("COUNTRY", "CHN", "AGE", "Mean"),
    rrow("new row redux", 888),
    after = TRUE
  )
  expect_identical(
    row.names(tab3),
    c(
      orig_rns[1],
      "new row",
      orig_rns[2],
      "new row redux",
      orig_rns[-c(1:2)]
    )
  )

  myrow <- rrow("whaaat", 578)
  rps <- row_paths(tab)
  msg <- "path must resolve fully to a non-content data row."
  expect_error(insert_row_at_path(tab, c("root", "COUNTRY"), myrow), msg)
  expect_error(insert_row_at_path(tab, c("root", "COUNTRY", "CHN"), myrow), msg)
  expect_error(insert_row_at_path(tab, c("root", "COUNTRY", "CHN", "AGE"), myrow), msg)
  expect_error(insert_row_at_path(tab, rps[[1]], myrow), msg)

  lyt4 <- basic_table() %>%
    split_rows_by("COUNTRY", split_fun = keep_split_levels(c("CHN", "USA"))) %>%
    analyze("AGE")

  tab4 <- build_table(lyt4, DM)

  expect_identical(
    label_at_path(tab4, c("COUNTRY", "CHN")),
    "CHN"
  )

  label_at_path(tab4, c("COUNTRY", "CHN")) <- "China"

  expect_identical(
    row.names(tab4),
    c("China", "Mean", "USA", "Mean")
  )

  label_at_path(tab4, c("COUNTRY", "CHN", "AGE", "Mean")) <- "Age Mean"
  expect_identical(
    row.names(tab4),
    c("China", "Age Mean", "USA", "Mean")
  )
})


test_that("bracket methods all work", {
  tbl <- tt_to_export()

  nrtot <- nrow(tbl)

  tbl_a_white <- tbl[1:19, ]
  expect_identical(
    tbl[rep(c(TRUE, FALSE), c(19, nrtot - 19)), ],
    tbl_a_white
  )
  expect_identical(
    tt_at_path(tbl_a_white, c("STRATA1", "A", "RACE", "WHITE")),
    tt_at_path(tbl, c("STRATA1", "A", "RACE", "WHITE"))
  )


  tbl_sub1 <- tbl[1:25, c(1, 4, 6)]

  expect_identical(
    tbl_sub1,
    tbl[
      rep(c(TRUE, FALSE), c(25, nrtot - 25)),
      c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
    ]
  )

  expect_identical(
    tbl[, c(1, 4, 6)],
    tbl[, c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)]
  )
})

test_that("tt_at_path and cell_values work with values even if they differ in naming", {
  # see issue #794
  tbl <- basic_table() %>%
    split_cols_by(var = "ARM", split_label = "asdar") %>%
    # split_rows_by(var = "SEX") %>%
    add_colcounts() %>%
    analyze("AGE",
      afun = function(x) {
        out_list <- list(a = mean(x), b = 3)
        labs <- c("argh", "argh2")
        attr(out_list[[1]], "label") <- "aa"
        attr(out_list[[2]], "label") <- "aa2"
        in_rows(.list = out_list, .labels = labs, .names = labs)
      },
      show_labels = "visible", table_names = "nope"
    ) %>%
    build_table(df = DM)

  rdf <- make_row_df(tbl)
  names(rdf$path[[2]]) <- c("a", "b")
  expect_silent(tt_at_path(tbl, rdf$path[[2]]))
})

test_that("tt_at_path works with identical split names", {
  # Regression test #864
  adsl <- ex_adsl
  adsl$flag <- sample(c("Y", "N"), nrow(adsl), replace = TRUE)

  afun <- function(x, ...) rcell(label = "Flagged Pop. Count", sum(x == "Y"))

  lyt <- basic_table() %>%
    analyze("flag", afun = afun) %>%
    split_rows_by("flag", split_fun = keep_split_levels("Y"), child_labels = "hidden") %>%
    split_rows_by("SEX") %>%
    analyze("BMRKR1")

  tbl <- build_table(lyt, adsl)

  expect_equal(
    tt_at_path(tbl, c("root", "flag", "Y")),
    tree_children(tree_children(tbl)[[2]])[[1]]
  )

  # Even with deeper branching
  lyt <- basic_table() %>%
    split_rows_by("flag", split_fun = keep_split_levels("Y"), child_labels = "hidden") %>%
    split_rows_by("SEX", split_fun = keep_split_levels("U")) %>%
    analyze("BMRKR1") %>%
    split_rows_by("flag", split_fun = keep_split_levels("Y"), child_labels = "hidden") %>%
    split_rows_by("SEX", split_fun = keep_split_levels("U")) %>%
    analyze("AGE")

  tbl <- build_table(lyt, adsl)

  expect_equal(
    names(tt_at_path(tbl, c("root", "flag", "Y", "SEX", "U"))),
    rep("flag", 2)
  )
})

test_that("tt_at_path gives an informative error when labels are used instead of row names", {
  # Issue #1004
  adsl <- ex_adsl

  out <- basic_table() %>%
    split_rows_by("ARM") %>%
    analyze("BMRKR1", afun = mean, show_labels = "visible", var_labels = "An error may occur") %>%
    build_table(adsl)

  real_path <- row_paths(out)[[3]]
  expect_silent(nothing <- tt_at_path(out, real_path))

  real_path[3] <- "An error may occur" # using the labels
  expect_error(
    tt_at_path(out, real_path),
    "Path appears invalid for this tree at step \\'An error may occur\\'. Please use only row names and NOT"
  )
})
