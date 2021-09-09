context("Accessing and subsetting tables")

test_that("cell_values function works as desired", {
  l <- basic_table() %>% split_cols_by("ARM") %>%
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

  armaval = "A: Drug X"
  cvres1 = cell_values(tbl, c("RACE", "ASIAN"), c("ARM", "A: Drug X", "SEX", "M"))
  contcount = nrow(subset(ourdat, RACE == "ASIAN" & ARM == armaval & SEX == "M"))
  asianstrata <- table(subset(ourdat, RACE == "ASIAN")$STRATA1)
  expect_identical(unname(cvres1),
                   list(list("A: Drug X.M" = c(contcount, contcount/armsextab["M", armaval])),
                        list("A: Drug X.M" = c(unname(asianstrata["A"]), armsextab["M", armaval])),
                        list("A: Drug X.M" = c(unname(asianstrata["B"]), armsextab["M", armaval])),
                        list("A: Drug X.M" = c(unname(asianstrata["C"]), armsextab["M", armaval]))))



  cvres2 <- cell_values(tbl, c("RACE", "ASIAN", "STRATA1"), c("ARM", "A: Drug X", "SEX", "M"))
  expect_identical(unname(cvres1[2:4]), unname(cvres2))
 cvres3 <- cell_values(tbl, c("RACE", "ASIAN", "STRATA1", "B"), c("ARM", "A: Drug X", "SEX", "M"))
  expect_identical(cvres3, cvres1[[3]])
  ## any arm, male columns from the ASIAN content (ie summary) row
  cvres4 <- cell_values(tbl, c("RACE", "ASIAN", "@content"))
  expect_identical(cvres4[2], cvres1[[1]])

 cvres5 <- cell_values(tbl, c("RACE", "ASIAN", "@content"), c("ARM", "*", "SEX", "M"))
  expect_identical(cvres5, cvres4[seq(2, 6, by=2)])
 ## all columns
 cvres6 <- cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B"))

 ## all columns for the Combination arm
cvres7 <-  cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B"), c("ARM", "C: Combination"))

  expect_identical(cvres6[5:6],
                   cvres7)

  cvres8 <- cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B", "AGE"), c("ARM", "C: Combination", "SEX", "M"))
  vares8 <- value_at(tbl,  c("RACE", "ASIAN", "STRATA1", "B", "AGE"), c("ARM", "C: Combination", "SEX", "M"))
  expect_identical(cvres8[[1]], vares8)
  expect_error(value_at(tbl,  c("RACE", "ASIAN", "STRATA1", "B"), c("ARM", "C: Combination", "SEX", "M")))
  expect_error(value_at(tbl,  c("RACE", "ASIAN", "STRATA1", "B", "AGE"), c("ARM", "C: Combination", "SEX")))
  expect_error(value_at(tbl,  c("RACE", "ASIAN", "STRATA1", "B", "AGE"), c("ARM", "C: Combination")))
})


test_colpaths <- function(tt) {

    cdf <- make_col_df(tt, visible_only = TRUE)
    cdf2 <- make_col_df(tt, visible_only = FALSE)
    res3 <- lapply(cdf$path, function(pth) rtables:::subset_cols(tt, pth))
    res4 <- lapply(cdf2$path, function(pth) rtables:::subset_cols(tt, pth))
    expect_identical(res3, res4[!is.na(cdf2$abs_pos)])
    expect_identical(res3, lapply(1:ncol(tt),
                                  function(j) tt[,j]))
    TRUE
}


test_rowpaths <- function(tt, visonly = TRUE) {

    cdf <- make_row_df(tt, visible_only = visonly)
    res3 <- lapply(cdf$path, function(pth) cell_values(tt, pth))
    TRUE
}



test_that("make_row_df, make_col_df give paths which all work", {
    ## duplicated from test-lyt-tabulation.R :(
    ## lyt = basic_table() %>% split_cols_by("ARM") %>%
    ##     ## add nested column split on SEX with value lables from gend_label
    ##     split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
    ##     ## No row splits have been introduced, so this adds
    ##     ## a root split and puts summary content on it labelled Overall (N)
    ##     ## add_colby_total(label = "All") %>%
    ##     ##    summarize_row_groups(label = "Overall (N)", format = "(N=xx)") %>%
    ##     add_colcounts() %>%
    ##     ## add a new subtable that splits on RACE, value labels from ethn_label
    ##     split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
    ##     summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
    ##     ##
    ##     ## Add nested row split within Race categories for FACTOR2
    ##     ## using a split function that excludes level C
    ##     ## value labels from fac2_label
    ##     split_rows_by("FACTOR2", "Factor2",
    ##                         split_fun = remove_split_levels("C"),
    ##                         labels_var = "fac2_label") %>%
    ##     ## Add count summary within FACTOR2 categories
    ##     summarize_row_groups("FACTOR2") %>%
    ##     ## Add analysis/data rows by analyzing AGE variable
    ##     ## Note afun is a function that returns 2 values in a named list
    ##     ## this will create 2 data rows
    ##     analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
    ##                                                                      median = median(x)),
    ##                       format = "xx.xx") %>%
    ##     ## adding more analysis vars "compounds them", placing them at the same
    ##     ## level of nesting as all previous analysis blocks, rather than
    ##     ## attempting to further nest them
    ##     analyze("AGE", "Age Analysis redux", afun = range, format = "xx.x - xx.x",
    ##             table_names = "AgeRedux") %>%

    ##     ## Note nested=TRUE, this creates a NEW subtable directly under the
    ##     ## root split
    ##     ## afun of table() gives us k count rows, where k is the number of
    ##     ## levels of VAR3, in this case 2.
    ##     analyze("VAR3", "Var3 Counts", afun = list_wrap_x(table), nested = FALSE)

     lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", ref_group = "F") %>%
        analyze("AGE", mean, show_labels = "hidden") %>%
        analyze("AGE", refcompmean, show_labels="hidden", table_names = "AGE2a") %>%
        split_rows_by("RACE", nested = FALSE, split_fun = drop_split_levels) %>%
        analyze("AGE", mean, show_labels = "hidden") %>%
        analyze("AGE", refcompmean, show_labels = "hidden", table_names = "AGE2b")


    tab <- build_table(lyt, rawdat)
    rdf1a <- make_row_df(tab)
    rdf1b <- make_row_df(tab, visible_only = FALSE)
    expect_true(all.equal(rdf1a, rdf1b[!is.na(rdf1b$abs_rownumber),],
                          check.attributes = FALSE), ## rownames ugh
                     "visible portions of row df not identical between
visible_only and not")

    allcvs <- cell_values(tab)
    allcvs_un <- unname(allcvs)
    pathres <- lapply(rdf1b$path, function(pth) unname(cell_values(tab, pth)))
    expect_identical(pathres,
                     list(allcvs_un,
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
                          unname(allcvs_un[[6]]))) ## black compare DataRow

    test_colpaths(tab)




    combodf <- tibble::tribble(
                           ~valname, ~label, ~levelcombo, ~exargs,
                           "A_", "Arm 1", c("A: Drug X"), list(),
                           "B_C", "Arms B & C", c("B: Placebo", "C: Combination"), list())

    l2 <- basic_table() %>%
        split_cols_by(
            "ARM",
            split_fun = add_combo_levels(combodf, keep_levels = c("A_", "B_C"))) %>%
        add_colcounts() %>%
        analyze(c("AGE", "AGE"), afun = list(mean, range),
                show_labels = "hidden", table_names = c("AGE mean", "AGE range"))

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
    expect_identical(matrix(c("", "Age", "Biomarker 1", "Second Age",
                              "mean", mean(DM$AGE), mean(DM$BMRKR1), mean(DM$AGE)),
                            nrow = 2, byrow = TRUE),
                     matform$strings)

    res = cell_values(tbl, colpath = c( "multivars", "AGE._[[2]]_."))
    expect_identical(list("AGE._[[2]]_." = mean(DM$AGE, na.rm= TRUE)),
                     res)
})

test_that("top_left retention behavior is correct across all scenarios", {
    tlval <- "hi"
    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        append_topleft(tlval) %>%
        split_rows_by("SEX") %>%
        analyze("AGE", mean)
    tbl <- build_table(lyt, DM)

    expect_identical(top_left(tbl), tlval)
    expect_identical(top_left(tbl[,1]), tlval) ## default column-only subsetting is TRUE
    expect_identical(top_left(tbl[,1,keep_topleft = FALSE]), character())
    expect_identical(top_left(tbl[,1,keep_topleft = TRUE]), tlval)
    expect_identical(top_left(tbl[1,]), character()) ## default with any row subsetting is FALSE
    expect_identical(top_left(tbl[1, ,keep_topleft = FALSE]), character())
    expect_identical(top_left(tbl[1, ,keep_topleft = TRUE]), tlval)
    expect_identical(top_left(tbl[1:2, 1:2]), character())
    expect_identical(top_left(tbl[1:2, 1:2, keep_topleft = FALSE]), character())
    expect_identical(top_left(tbl[1:2, 1:2, keep_topleft = TRUE]), tlval)
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
       expect_identical("1 (10%)", matform2$strings[2, 2])

       tbl3 <- tbl
       tbl3[3, 1:2] <- list(CellValue(c(1, 1)), CellValue(c(1, 1)))
       matform3 <- matrix_form(tbl3)
       expect_identical(rep("1 (100%)", 2), matform3$strings[4, 2:3])
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

    expect_identical(cv,
                     setNames(list(32L, 56L),
                              c("C: Combination.S1",
                                "C: Combination.S2")))
})
