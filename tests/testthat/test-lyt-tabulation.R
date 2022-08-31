context("tabulation framework")


test_that("summarize_row_groups works with provided funcs", {
    l1 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("RACE") %>%
        summarize_row_groups() %>%
        analyze("AGE", mean)

    tb1 <- build_table(l1, DM)
    tbl_str <- toString(tb1)

    expect(TRUE, "succeeded")


})



## this
test_that("complex layout works", {
    lyt <- make_big_lyt()

    ## ensure print method works for predata layout
    tab <- build_table(lyt, rawdat)
    tab_str <- toString(tab)
    ## XXX TODO this assumes we want no var label on VAR3 subtable
    expect_identical(dim(tab), c(28L, 4L))
    expect_identical(row.names(tab), complx_lyt_rnames)

    tlvals <- c("Ethnicity", "Factor 2")
    lyt2 <- lyt %>% append_topleft(tlvals)
    tab2 <- build_table(lyt2, rawdat)
    expect_identical(top_left(tab2), tlvals)

})





test_that("existing table in layout works", {
    thing2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        ## add nested column split on SEX with value labels from gend_label
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        analyze(c("AGE", "AGE"), c("Age Analysis", "Age Analysis Redux"),
                afun = function(x) list(mean = mean(x),
                                        median = median(x)),
                format = "xx.xx",
                table_names = c("AGE1", "AGE2")
        )

tab2 <- build_table(thing2, rawdat)


    thing3 <- basic_table() %>%
        split_cols_by("ARM") %>%
        ## add nested column split on SEX with value labels from gend_label
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        analyze("AGE", "Age Analysis",
                          afun = function(x) list(mean = mean(x), median = median(x)),
                          format = "xx.xx") %>%
        ## stack an existing table onto the layout and thus the generated table
        add_existing_table(tab2)

    tab3 <- build_table(thing3, rawdat)
    expect_equal(nrow(tab3), 12)
    tab3
})

test_that("Nested splits in column space work", {

    dat2 <- subset(ex_adsl, SEX %in% c("M", "F"))
    tbl2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", split_fun = drop_split_levels) %>%
        analyze(c("AGE", "STRATA1")) %>%
        build_table(dat2)

    mf <- matrix_form(tbl2)
    expect_identical(unname(mf$strings[1, , drop = TRUE]),
                     c("", "A: Drug X", "A: Drug X", "B: Placebo", "B: Placebo",
                       "C: Combination", "C: Combination"))
    expect_identical(unname(mf$display[1, , drop = TRUE]),
                     c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

})


test_that("labelkids parameter works", {
    yeslabellyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", child_labels = "visible") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label", child_labels = "visible") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                format = "xx.xx",
                show_labels = "visible")

    tabyes <- build_table(yeslabellyt, rawdat)

    expect_identical(row.names(tabyes)[1:4],
                     c("Caucasian", "Caucasian (n)", "Level A", "Age Analysis"))


    misslabellyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", child_labels = "default") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label", child_labels = "default") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                      format = "xx.xx")

    tabmiss <- build_table(misslabellyt, rawdat)
    expect_identical(row.names(tabmiss)[1:4],
                     c("Caucasian (n)", "Level A", "mean", "median"))


    nolabellyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", child_labels = "hidden") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label", child_labels = "hidden") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                format = "xx.xx",
                show_labels = "hidden")


    tabno <- build_table(nolabellyt, rawdat)
    expect_identical(row.names(tabno)[1:4],
                     c("Caucasian (n)", "mean", "median", "mean"))

    mixedlyt2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", child_labels = "hidden") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label", child_labels = "hidden") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx",
                show_labels = "visible")

    tabmixed2 <- build_table(mixedlyt2, rawdat)
    expect_identical(row.names(tabmixed2)[1:4],
                     c("Caucasian (n)", "Age Analysis", "mean", "median"))


    mixedlyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", child_labels = "visible") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label", child_labels = "visible") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx",
                show_labels = "hidden")

    tabmixed <- build_table(mixedlyt, rawdat)
    expect_identical(row.names(tabmixed)[1:4],
                     c("Caucasian", "Caucasian (n)", "Level A", "mean"))


    varshowlyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      label_pos = "visible") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx",
                show_labels = "hidden")

    varshowtab <- build_table(varshowlyt, rawdat)
    expect_identical(row.names(varshowtab)[1:4],
                     c("Caucasian (n)", "Factor2", "Level A", "mean"))
})




test_that("ref_group comparisons work", {

    blthing <- basic_table() %>%
        split_cols_by("ARM", ref_group = "ARM1") %>%
        analyze("AGE", show_labels = "hidden") %>%
        analyze("AGE", refcompmean, show_labels = "hidden", table_names = "AGE2")
    ## function(x) list(mean = mean(x)))


    bltab <- build_table(blthing, rawdat)
    expect_identical(dim(bltab), c(2L, 2L))
    expect_null(bltab[2, 1, drop = TRUE])
    c1 <- bltab[1, 1, drop = TRUE]
    c2 <- bltab[1, 2, drop = TRUE]
    c3 <- bltab[2, 2, drop = TRUE]
    expect_equivalent(c2 - c1, c3)

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", ref_group = "F") %>%
        analyze("AGE", mean, show_labels = "hidden") %>%
        analyze("AGE", refcompmean, show_labels = "hidden",
                table_names = "AGE2a") %>%
        split_rows_by("RACE", nested = FALSE,
                      split_fun = drop_split_levels) %>%
        analyze("AGE", mean, show_labels = "hidden") %>%
        analyze("AGE", refcompmean, show_labels = "hidden", table_names = "AGE2b")

    bltab2 <- build_table(lyt, DM)
    d1 <- bltab2[4, 1, drop = TRUE]
    d2 <- bltab2[4, 2, drop = TRUE]
    d3 <- bltab2[5, 2, drop = TRUE]

    expect_equivalent(d2 - d1, d3)
    d4 <- bltab2[1, 3, drop = TRUE]
    d5 <- bltab2[1, 4, drop = TRUE]
    d6 <- bltab2[2, 4, drop = TRUE]
    expect_equivalent(d5 - d4, d6)

    d7 <- bltab2[4, 3, drop = TRUE]
    d8 <- bltab2[4, 4, drop = TRUE]
    d9 <- bltab2[5, 4, drop = TRUE]
    expect_equivalent(d8 - d7, d9)

    ## with combo levels
    combodf <- tribble(
        ~valname, ~label, ~levelcombo, ~exargs,
        "A_", "Arm 1", c("A: Drug X"), list(),
        "B_C", "Arms B & C", c("B: Placebo", "C: Combination"), list())

    l3 <- basic_table() %>%
        split_cols_by(
            "ARM",
            split_fun = add_combo_levels(combodf, keep_levels = c("A_", "B_C")),
            ref_group = "A_"
        ) %>%
        add_colcounts() %>%
        analyze(c("AGE", "AGE"), afun = list(mean, refcompmean),
                show_labels = "hidden", table_names = c("AGE1", "AGE2"))
    bltab3 <- build_table(l3, DM)
    d10 <- bltab3[1, 1, drop = TRUE]
    d11 <- bltab3[1, 2, drop = TRUE]
    d12 <- bltab3[2, 2, drop = TRUE]

    expect_null(cell_values(bltab3, "AGE2", c("ARM", "A_"))[[1]])
    expect_identical(d12, d11 - d10)
})

test_that("missing vars caught", {
    misscol <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SX", "Gender") %>%
        analyze("AGE", "Age Analysis",
                afun = function(x) list(mean = mean(x),
                                        median = median(x)), format = "xx.xx")

    expect_error(build_table(misscol, rawdat),
                 "Split variable [[]SX[]] not found in data being tabulated.")

    missrsplit <-  basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "gend_label") %>%
        split_rows_by("RACER", "ethn_label") %>%
        analyze("AGE", "Age Analysis",
                afun = function(x) list(mean = mean(x),
                                        median = median(x)), format = "xx.xx")

    expect_error(build_table(missrsplit, rawdat),
                 "Split variable [[]RACER[]] not found in data being tabulated.")

    missrsplit <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "gend_label") %>%
        split_rows_by("RACE", "ethnNA_label") %>%
        analyze("AGE", "Age Analysis",
                afun = function(x) list(mean = mean(x),
                                        median = median(x)), format = "xx.xx")

    expect_error(build_table(missrsplit, rawdat),
                 "Value label variable [[]ethnNA_label[]] not found in data being tabulated.")

    missavar <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", labels_var = "gend_label") %>%
        split_rows_by("RACE", labels_var = "ethn_label") %>%
        analyze("AGGE", "Age Analysis",
                afun = function(x) list(mean = mean(x),
                                        median = median(x)), format = "xx.xx")

    expect_error(build_table(missavar, rawdat),
                 ".*variable[(]s[)] [[]AGGE[]] not present in data. [(]AnalyzeVarSplit[)].*")
})

# https://github.com/Roche/rtables/issues/329
test_that("error localization works", {

    afun <- function(x, .spl_context) {
        if(NROW(.spl_context) > 0 &&
           .spl_context[NROW(.spl_context), "value", drop = TRUE] == "WHITE")
            stop("error for white statistics")

        in_rows(myrow = 5)
    }

    lyt <- basic_table() %>%
        split_rows_by("ARM") %>%
        split_rows_by("RACE") %>%
        analyze("BMRKR1", afun = afun)
# nolint start
    expect_error(build_table(lyt, DM),
                 "Error[^)]*analysis function \\(var[^B]*BMRKR1\\): error for white statistics.*ARM\\[A: Drug X\\]->RACE\\[WHITE\\]")
# nolint end
    cfun <- function(df, labelstr) {
        if(labelstr == "B: Placebo")
            stop("placebos are bad")
        in_rows(val = 5)
    }

    lyt2 <- basic_table() %>%
        split_rows_by("ARM") %>%
        summarize_row_groups(cfun = cfun) %>%
        split_rows_by("RACE") %>%
        analyze("BMRKR1", afun = mean)

    expect_error(build_table(lyt2, DM),
                 "Error in content.*function: placebos are bad.*path: ARM\\[B: Placebo\\]")

    splfun <- function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
        stop("oopsie daisy")
    }

    lyt3 <- basic_table() %>%
        split_rows_by("ARM") %>%
        summarize_row_groups() %>%
        split_rows_by("RACE", split_fun = splfun) %>%
        analyze("BMRKR1", afun = mean)
# nolint start
    expect_error(build_table(lyt3, DM),
                 "Error.*custom split function: oopsie daisy.*VarLevelSplit \\(RACE\\).*path: ARM\\[A: Drug X\\]")
# nolint end
})


test_that("cfun args", {
    cfun1 <- function(df, labelstr, .N_col, .N_total) {
        stopifnot(is(df, "data.frame"))
        in_rows(
            rcell(nrow(df) * c(1, 1 / .N_col), format = "xx (xx.xx%)"),
            .names = labelstr)
    }
    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        summarize_row_groups(cfun = cfun1)

    tbl <- build_table(lyt, rawdat)
    expect_identical(print(tbl), tbl)
})

## regression test for automatically not-nesting
## when a non-analyze comes after an analyze
test_that("split under analyze", {
    dontnest <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        analyze("AGE") %>%
        split_rows_by("VAR3") %>%
        analyze("AGE") %>%
        build_table(rawdat)
    expect_equal(nrow(dontnest), 5)
})


test_that("label_var works as expected", {
    yeslblslyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        split_rows_by("SEX", labels_var = "gend_label") %>%
        analyze("AGE")
    yeslbls <- build_table(yeslblslyt, rawdat)
    expect_identical(row.names(yeslbls)[1], "Male")

    nolbls <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        add_colcounts() %>%
        split_rows_by("SEX") %>%
        analyze("AGE") %>%
        build_table(rawdat)
    expect_identical(row.names(nolbls)[1], "M")

    ## create bad label col
    rawdat2 <- rawdat
    rawdat2$gend_label[5] <- "XXXXX"
    ## nolint start
    ## test check for label-value concordance.
    expect_error(build_table(yeslblslyt, rawdat2),
                 "There does not appear to be a 1-1 correspondence between values in split var \\[SEX\\] and label var \\[gend_label\\]")
    ## nolint end
})


test_that("factors with unobserved levels work as expected", {

    ## default behavior is that empty levels are NOT dropped
    ## rows
    lyt <- basic_table() %>%
        split_rows_by("SEX") %>%
        analyze("AGE")
    tab <- build_table(lyt, DM)

    expect_identical(dim(tab), c(8L, 1L))

    ## cols
    lyt2 <- basic_table() %>%
        split_cols_by("SEX") %>%
        analyze("AGE")
    tab2 <- build_table(lyt2, DM)
    expect_identical(dim(tab2), c(1L, 4L))
})


test_that(".N_row argument in afun works correctly", {

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        analyze("AGE", afun = function(x, .N_row) .N_row)
    tab <- build_table(lyt, rawdat)
    rows <- collect_leaves(tab)
    names(rows) <- substr(names(rows), 1, 1)
    ans <- tapply(rawdat$AGE, rawdat$SEX, function(x) rep(length(x), 2))
    res <- vapply(names(rows), function(nm) isTRUE(all.equal(unname(unlist(row_values(rows[[nm]]))), ans[[nm]])), NA)
    expect_true(all(res))
})


test_that("extra args works", {
    oldop <- options(warn = 2)
    on.exit(options(oldop))
    colfuns <- list(
        function(x, add = 0, na.rm = TRUE) {
            rcell(mean(c(NA, x), na.rm = na.rm) + add, format = "xx.x")
        },
        function(x, cutoff = .5, na.rm = TRUE) {
            rcell(sum(c(NA, x > cutoff), na.rm = na.rm), format = "xx")
        })

    l <-  basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_multivar(c("VALUE", "PCTDIFF")) %>%
        analyze_colvars(afun = colfuns)

    l

    tbl_noex <- build_table(l, rawdat2)

    ## one for each different function in colfuns, assigned correctly
    l2 <-  basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_multivar(c("VALUE", "PCTDIFF")) %>%
        analyze_colvars(afun = colfuns, extra_args = list(list(add = 5), list(cutoff = 100)))


    tbl_ex <- build_table(l2, rawdat2)

    vals_noex <- row_values(tree_children(tbl_noex)[[1]])
    vals_ex <-  row_values(tree_children(tbl_ex)[[1]])

    expect_identical(unlist(vals_noex[c(1, 3)]) + 5,
                     unlist(vals_ex[c(1, 3)]))
    truevals <- tapply(rawdat2$PCTDIFF,
                       rawdat2$ARM,
                       function(x) sum(x > 100, na.rm = TRUE),
                       simplify = FALSE)
    expect_equal(unname(unlist(truevals)),
                 unname(unlist(vals_ex[c(2, 4)])))

    vals_noex <- row_values(tree_children(tbl_noex)[[1]])
    vals_ex <-  row_values(tree_children(tbl_ex)[[1]])

    expect_identical(unlist(vals_noex[c(1, 3)]) + 5,
                     unlist(vals_ex[c(1, 3)]))
    truevals <- tapply(rawdat2$PCTDIFF,
                       rawdat2$ARM,
                       function(x) sum(x > 100, na.rm = TRUE),
                       simplify = FALSE)
    expect_equal(unname(unlist(truevals)),
                 unname(unlist(vals_ex[c(2, 4)])))

    ## single argument passed to all functions
    l2b <-  basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_multivar(c("VALUE", "PCTDIFF")) %>%
        analyze_colvars(afun = colfuns, extra_args = list(na.rm = FALSE))

    tbl_ex2 <- build_table(l2b, rawdat2)

    expect_true(all(is.na(unlist(rtables:::row_values(tree_children(tbl_ex2)[[1]])))))

    ## one argument for a single function.

    lyt <- basic_table() %>%
      analyze("Sepal.Length", afun = function(x, a) {
          in_rows(mean_a = rcell(mean(x) + a, format = "xx"))

      }, extra_args = list(a = 1))


    tbl <- build_table(lyt, iris)
    expect_equal(tbl[1, 1, drop = TRUE], mean(iris$Sepal.Length) + 1)

    ## two arguments for a single function
    lyt2 <- basic_table() %>%
        analyze("Sepal.Length", afun = function(x, a, b) {
          in_rows(mean_a = rcell(mean(x) + a + b, format = "xx"))

      }, extra_args = list(a = 1, b = 3))


    tbl2 <- build_table(lyt2, iris)
    expect_equal(tbl2[1, 1, drop = TRUE], mean(iris$Sepal.Length) + 1 + 3)
})


test_that("Colcounts work correctly", {
    lyt1 <- basic_table() %>%
        add_colcounts() %>%
        analyze("AGE")
    tbl1 <- build_table(lyt1, DM)

    expect_identical(col_counts(tbl1), nrow(DM))

    lyt2 <- lyt1 %>% split_cols_by("ARM")
    tbl2 <- build_table(lyt2, DM)

    expect_identical(col_counts(tbl2),
                     as.integer(table(DM$ARM)))

    DMchar <- DM
    DMchar$ARM <- as.character(DM$ARM)
    tbl2chr <- build_table(lyt2, DMchar)

    tbl3 <- build_table(lyt2, DM, col_counts = c(500L, NA, NA))
    expect_identical(col_counts(tbl3),
                     c(500L, as.integer(table(DM$ARM))[2:3]))
    expect_error(build_table(lyt2, DMchar, col_counts = c(500L, NA, NA)))
    expect_error(build_table(lyt2, DM, col_counts = c(20L, 40L)))
})

first_cont_rowvals <- function(tt)
    row_values(
        tree_children(
            content_table(
                tree_children(tt)[[1]]
            )
        )[[1]])

test_that("content extra args for summarize_row_groups works", {
    sfun <- function(x, labelstr, .N_col, a = 5, b = 6, c = 7) {
        in_rows(
            c(a, b),
            .formats = "xx - xx",
            .labels = labelstr)
    }
    ## specify single set of args for all columns
    l <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        summarize_row_groups(cfun = sfun,
                             extra_args = list(a = 9))
    tbl1 <- build_table(l, rawdat)
    expect_identical(first_cont_rowvals(tbl1),
                     list(ARM1 = c(9, 6),
                          ARM2 = c(9, 6)))

    ## specify different arg for each column
    l2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        summarize_row_groups(cfun = sfun,
                             extra_args = list(list(a = 9),
                                               list(b = 3)))
    tbl2 <- build_table(l2, rawdat)
    expect_identical(first_cont_rowvals(tbl2),
                     list(ARM1 = c(9, 6),
                          ARM2 = c(5, 3)))


    ## specify arg for only one col
    l3 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        summarize_row_groups(cfun = sfun,
                             extra_args = list(list(a = 9)))
    tbl3 <- build_table(l3, rawdat)
    expect_identical(first_cont_rowvals(tbl3),
                     list(ARM1 = c(9, 6),
                          ARM2 = c(5, 6)))

    ##works on root split

    l4 <- basic_table() %>%
        split_cols_by("ARM") %>%
        summarize_row_groups(cfun = sfun,
                             extra_args = list(a = 9))
    tbl4 <- build_table(l4, rawdat)
    expect_identical(row_values(tree_children(content_table(tbl4))[[1]]),
                      list(ARM1 = c(9, 6),
                          ARM2 = c(9, 6)))
})

test_that(".df_row analysis function argument works", {
    afun <- function(x, labelstr = "", .N_col, .df_row)  {
        rcell(c(nrow(.df_row), .N_col), format = "(xx.x, xx.x)")
    }

    l <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        analyze("AGE", afun)

    tbl <- build_table(l, rawdat)
    rws <- collect_leaves(tbl, add.labrows = FALSE)
    nmale <- sum(rawdat$SEX == "M")
    nfemale <- sum(rawdat$SEX == "F")
    narm1 <- sum(rawdat$ARM == "ARM1")
    narm2 <- sum(rawdat$ARM == "ARM2")

    expect_identical(unname(lapply(rws, row_values)),
                     list(list(ARM1 = c(nmale, narm1),
                               ARM2 = c(nmale, narm2)),
                          list(ARM1 = c(nfemale, narm1),
                               ARM2 = c(nfemale, narm2))))
})

test_that("analyze_colvars inclNAs works", {

    ## inclNAs
    test <- data.frame(
        a = c(1, 2),
        b = c(1, NA)
    )

    l <- basic_table() %>%
        split_cols_by_multivar(c("a", "b")) %>%
        analyze_colvars(afun = length, inclNAs = TRUE)

                                        # We expect:
    ans <- lapply(test, length)
                                        # a b
                                        # 2 2

                                        # But we get:
    tab <- build_table(l, test)
    res1 <- cell_values(tab)
    expect_equal(ans, res1)

    l2 <- basic_table() %>%
        split_cols_by_multivar(c("a", "b")) %>%
        analyze_colvars(afun = length, inclNAs = FALSE)

    ans2 <- lapply(test, function(x) sum(!is.na(x)))
    tab2 <- build_table(l2, test)
    res2 <- cell_values(tab2)
    expect_equal(ans2, res2)
})


test_that("analyze_colvars works generally", {
    op <- options(warn = 2)
    on.exit(options(op))
    test <- data.frame(
        a = 1,
        b = 2,
        c = 3,
        d = 4,
        e = 5
    )
    l1 <- basic_table() %>%
        split_cols_by_multivar(c("a", "b", "c", "d")) %>%
        analyze_colvars(afun = identity)
    tab1 <- build_table(l1, test)
    l2 <- basic_table() %>%
        split_cols_by_multivar(c("a", "b", "c", "d", "e")) %>%
        analyze_colvars(afun = identity)
    tab2 <- build_table(l2, test)

    colfuns <- list(function(x, labelstr) in_rows(summary = 5, .labels = "My Summary Row"),
                    function(x, labelstr) 6,
                    function(x, labelstr) 7,
                    function(x, labelstr) 8)

    l3 <- basic_table() %>%
        split_cols_by_multivar(c("a", "b", "c", "d")) %>%
        summarize_row_groups(cfun = colfuns, format = "xx") %>%
        analyze_colvars(afun = identity)
    tab3 <- build_table(l3, test)
    expect_identical(cell_values(content_table(tab3)),
                     list(a = 5, b = 6, c = 7, d = 8))
    expect_identical(obj_label(collect_leaves(tab3, TRUE, TRUE)[[1]]),
                     c(summary = "My Summary Row"))

    l4 <- basic_table() %>%
        split_cols_by_multivar(c("a", "b", "c", "d")) %>%
        summarize_row_groups() %>%
        analyze_colvars(afun = identity)
    tab4 <- build_table(l4, test)
    ## this broke before due to formatting missmatches
    toString(tab4)
    rws4 <- collect_leaves(tab4, TRUE, TRUE)
    expect_identical(obj_format(rws4[[1]]), "xx (xx.x%)")
    expect_identical(obj_format(rws4[[2]]), NULL)

    l5 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_multivar(c("AGE", "BMRKR1")) %>%
        split_rows_by("RACE") %>%
        summarize_row_groups(cfun = list(function(x, labelstr) "first fun",
                                         function(x, labelstr) "second fun"),
                             format = "xx")
    tab5 <- build_table(l5, DM)
    toString(tab5)
    rws5 <- collect_leaves(tab5, TRUE, TRUE)
    expect(all(vapply(rws5, function(x) identical(x, rws5[[1]]), NA)),
           "Multiple content fucntions didn't recycle properly in nested context")
    expect_identical(unname(cell_values(tab5)[[1]]),
                     rep(list("first fun", "second fun"), length.out = ncol(tab5)))


    ## single column in split_cols_by_multivar and analyze_colvars
    one_col_lyt <- basic_table() %>%
        split_cols_by_multivar(vars = "Sepal.Width") %>%
        analyze_colvars(afun = mean)
    one_col_tbl <- build_table(one_col_lyt, iris)

    expect_identical(cell_values(one_col_tbl),
                     list(Sepal.Width = mean(iris$Sepal.Width)))
})


test_that("alt_counts_df works", {
    minidm <- DM[1, ]

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        split_rows_by("SEX") %>%
        summarize_row_groups() %>%
        analyze("AGE")

    tbl <- build_table(lyt, DM, minidm)

    ## this inherently checks both taht the correct counts (0, 1, 0) are
    ## retrieved and that they propogate to the summary functions
    expect_identical(list("A: Drug X" = c(70, Inf), ##70/0
                          "B: Placebo" = c(56, 56), ## 56/1
                          "C: Combination" = c(61, Inf)), ##61/0
                     cell_values(tbl[1, ]))

    ## breaks (with useful message) when given incompatible alt_counts_df
    expect_error(build_table(lyt, DM, iris), "Offending column subset expression")
})



test_that("deeply nested and uneven column layouts work", {
    lyt <- basic_table() %>%
        split_cols_by(var = "ARM") %>%
        split_cols_by("STRATA1") %>%
        split_cols_by("STRATA2") %>%
        add_overall_col("All Patients") %>%
        add_colcounts() %>%
        analyze("AGE")
    tbl <- build_table(lyt, ex_adsl)
    ## printing machinery works
    str <- toString(tbl)
    expect_identical(ncol(tbl), 19L)

    lyt2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("STRATA1") %>%
        split_cols_by("STRATA2", nested = FALSE) %>%
        add_overall_col("All Patients") %>%
        add_colcounts() %>%
        analyze("AGE")
    tbl2 <- build_table(lyt2, ex_adsl)

    ## printing machinery works
    str <- toString(tbl2)
    expect_identical(ncol(tbl2), 12L)


})



test_that("topleft label position works", {

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        ## add nested column split on SEX with value lables from gend_label
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        ## No row splits have been introduced, so this adds
        ## a root split and puts summary content on it labelled Overall (N)
        ## add_colby_total(label = "All") %>%
        ##    summarize_row_groups(label = "Overall (N)", format = "(N=xx)") %>%
        add_colcounts() %>%
        ## add a new subtable that splits on RACE, value labels from ethn_label
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", label_pos = "topleft") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        ##
        ## Add nested row split within Race categories for FACTOR2
        ## using a split function that excludes level C
        ## value labels from fac2_label
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      label_pos = "topleft") %>%
        ## Add count summary within FACTOR2 categories
        summarize_row_groups("FACTOR2") %>%
        ## Add analysis/data rows by analyzing AGE variable
        ## Note afun is a function that returns 2 values in a named list
        ## this will create 2 data rows
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx")

    tab <- build_table(lyt, rawdat)
    expect_identical(c("Ethnicity", "  Factor2"),
                   top_left(tab))
    expect_identical(14L,
                   nrow(tab))
})



test_that(".spl_context works in content and analysis functions", {

    ageglobmean <- mean(DM$AGE)
    cfun <- function(df, labelstr, .spl_context) {
        stopifnot("A: Drug X.M" %in% names(.spl_context))
            lastrow <- .spl_context[nrow(.spl_context) - 1, ]
            in_rows(c(nrow(df), lastrow$cur_col_n),
                    .names = labelstr,
                    .labels = sprintf("%s (%d)", labelstr,
                                      nrow(lastrow$full_parent_df[[1]])),
                    .formats = "xx / xx")
    }

    afun <- function(x, .spl_context) {
        stopifnot("A: Drug X.M" %in% names(.spl_context))
        lastrow <- .spl_context[nrow(.spl_context), ]
        in_rows(c(sum(x >= ageglobmean), lastrow$cur_col_n),
                .names = "age_analysis",
                .labels = sprintf("counts (out of %d)",
                                  nrow(lastrow$full_parent_df[[1]])),
                .formats = "xx / xx")
    }


    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", split_fun = keep_split_levels(c("M", "F"))) %>%
        split_rows_by("COUNTRY", split_fun = keep_split_levels(c("CHN", "USA"))) %>%
        summarize_row_groups() %>%
        split_rows_by("STRATA1") %>%
        summarize_row_groups(cfun = cfun) %>%
        analyze("AGE", afun = afun)

    tab <-  build_table(lyt, DM)

    strmat <- matrix_form(tab)$strings

    rwcount4 <- as.integer(gsub("[^0-9]", "", strmat[4, 1]))
    crowvals <- cell_values(tab, c("COUNTRY", "CHN", "@content"))
    expect_equal(rwcount4,
                 sum(sapply(crowvals,
                            `[[`, 1)))

    expect_equal(crowvals[[1]][[1]],
                 cell_values(tab, c("COUNTRY", "CHN", "STRATA1", "A", "@content"))[[1]][[2]])

    expect_equal(unname(sapply(cell_values(tab, c("COUNTRY", "USA", "STRATA1", "B", "@content")),
                               `[[`, 1L)),
                 unname(sapply(cell_values(tab, c("COUNTRY", "USA", "STRATA1", "B", "AGE", "age_analysis")),
                               `[[`, 2L)))
})

test_that("cut functions work", {

    ctnames <- c("young", "medium", "old")
    ## split_cols_by_cuts
    l <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_cuts("AGE", split_label = "Age",
                           cuts = c(0, 25, 35, 1000),
                           cutlabels = ctnames) %>%
        analyze(c("BMRKR2", "STRATA2")) %>%
        append_topleft("counts")

    tbl <- build_table(l, ex_adsl)

    chkvals <- cell_values(tbl, c("BMRKR2", "LOW"), c("ARM", "A: Drug X"))
    expect_identical(unname(unlist(chkvals)),
                     c(nrow(subset(ex_adsl, ARM == "A: Drug X" & BMRKR2 == "LOW" & AGE <= 25)),
                       nrow(subset(ex_adsl, ARM == "A: Drug X" & BMRKR2 == "LOW" & AGE > 25 & AGE <= 35)),
                       nrow(subset(ex_adsl, ARM == "A: Drug X" & BMRKR2 == "LOW" & AGE > 35))))

    mf <- matrix_form(tbl)
    expect_identical(mf$strings[2, , drop = TRUE],
                     c("", rep(ctnames, 3)))

    lcm <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_cuts("AGE", split_label = "Age",
                           cuts = c(0, 25, 35, 1000),
                           cutlabels = c("young", "young+medium", "all"),
                           cumulative = TRUE) %>%
        analyze(c("BMRKR2", "STRATA2")) %>%
        append_topleft("counts")

    tblcm <- build_table(lcm, ex_adsl)

    medpth <- c("BMRKR2", "MEDIUM")
    bpth <- c("ARM", "B: Placebo")
    expect_identical(cumsum(unname(unlist(cell_values(tbl, medpth, bpth)))),
                     unname(unlist(cell_values(tblcm, medpth, bpth))))
    ## split_rows_by_cuts
    l2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by_cuts("AGE", split_label = "Age",
                           cuts = c(0, 25, 35, 1000),
                           cutlabels = ctnames) %>%
        analyze("BMRKR2") %>%
        append_topleft("counts")


    tbl2 <- build_table(l2, ex_adsl)

    mf2 <- matrix_form(tbl2)

    expect_identical(mf2$strings[c(2, 6, 10), 1, drop = TRUE],
                     ctnames)


    l2cm <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by_cuts("AGE", split_label = "Age",
                           cuts = c(0, 25, 35, 1000),
                           cutlabels = ctnames, cumulative = TRUE) %>%
        analyze("BMRKR2") %>%
        append_topleft("counts")


    tbl2cm <- build_table(l2cm, ex_adsl)

    medlow <- c("AGE", "young", "BMRKR2", "HIGH")
    cpth <- c("ARM", "C: Combination")
    getvals <- function(tt) {
        sapply(ctnames,
               function(pth) {
            unname(unlist(cell_values(tt, c("AGE", pth, "BMRKR2", "HIGH"), cpth)))
        })
    }
    expect_identical(getvals(tbl2cm),
                     cumsum(getvals(tbl2)))
 # split_cols_by_quartiles

    l3 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_cutfun("AGE") %>% ##(quartiles("AGE", split_label = "Age") %>%
        analyze("BMRKR2") %>%
        append_topleft("counts")

    tbl3 <- build_table(l3, ex_adsl)

    l3b <-  basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_cuts("AGE", cuts = rtables:::qtile_cuts(ex_adsl$AGE)) %>%
        analyze("BMRKR2") %>%
        append_topleft("counts")

    tbl3b <- build_table(l3b, ex_adsl)

    expect_identical(tbl3, tbl3b)

    l3c <-  basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_quartiles("AGE") %>%
        analyze("BMRKR2") %>%
        append_topleft("counts")

    tbl3c <- build_table(l3c, ex_adsl)

    expect_identical(unname(unlist(cell_values(tbl3))),
                     unname(unlist(cell_values(tbl3c))))


    l3c_cm <-  basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_quartiles("AGE", cumulative = TRUE) %>%
        analyze("BMRKR2") %>%
        append_topleft("counts")

    tbl3c_cm <- build_table(l3c_cm, ex_adsl)
    # split_rows_by_quartiles
    l4 <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        split_rows_by_quartiles("AGE", split_label = "Age") %>%
        analyze("BMRKR2") %>%
        append_topleft(c("Age Quartiles", " Counts BMRKR2"))

    tbl4 <- build_table(l4, ex_adsl)


    cvs4 <- unlist(cell_values(tbl4))

    valslst4 <- unlist(lapply(1:3, function(i) lapply(cvs4, function(lst) lst[i])))

    names(valslst4) <- gsub("^(.*)\\.BMRKR2\\.(.*)$", "\\2.\\1", names(valslst4))
    valslst3 <- unlist(cell_values(tbl3c))
    expect_identical(valslst3,
                     valslst4[names(valslst3)])

    l4cm <- basic_table() %>%
        split_cols_by("ARM") %>%
        add_colcounts() %>%
        split_rows_by_quartiles("AGE", split_label = "Age", cumulative = TRUE) %>%
        analyze("BMRKR2") %>%
        append_topleft(c("Age Cumulative Quartiles", " Counts BMRKR2"))
    tbl4cm <- build_table(l4cm, ex_adsl)


    cvs4cm <- unlist(cell_values(tbl4cm))

    valslst4cm <- unlist(lapply(1:3, function(i) lapply(cvs4cm, function(lst) lst[i])))

    names(valslst4cm) <- gsub("^(.*)\\.BMRKR2\\.(.*)$", "\\2.\\1", names(valslst4cm))
    valslst3cm <- unlist(cell_values(tbl3c_cm))
    expect_identical(valslst3cm,
                     valslst4cm[names(valslst3cm)])


})

## https://github.com/Roche/rtables/issues/323

test_that("empty factor levels represented correctly when ref group is set", {

    df <- data.frame(
        val = 1:10,
        grp = factor(rep("a", 10), levels = c("a", "b"))
    )


    tbl <- basic_table() %>%
        split_cols_by("grp", ref_group = "a") %>%
        analyze("val") %>%
        build_table(df)

    expect_identical(ncol(tbl), 2L)

})

test_that("error on empty level of splitting variable", {
    mydf <- data.frame(x = c("hi", "", "lo"), y = c(5, 10, 20),
                       stringsAsFactors = FALSE)

    mydf2 <- mydf
    mydf2$x <- factor(mydf2$x)

    lyt1 <- basic_table() %>%
        split_cols_by("x") %>%
        analyze("y")
    expect_error(build_table(lyt1, mydf),
                 "Got empty string level in splitting variable x")
    expect_error(build_table(lyt1, mydf2),
                 "Got empty string level in splitting variable x")

    lyt2 <- basic_table() %>%
        split_rows_by("x") %>%
        analyze("y")

    expect_error(build_table(lyt2, mydf),
                 "Got empty string level in splitting variable x")
    expect_error(build_table(lyt2, mydf2),
                 "Got empty string level in splitting variable x")
})


test_that("error when afun gives differing numbers of rows is informative", {
    afunconst <- function() {
        nr <- 1
        function(x, ...) {
            nr <<- nr + 1
            in_rows(.list = as.list(seq_len(nr)), .names = paste(seq_len(nr)))
        }
    }

    my_broken_afun <- afunconst()

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        analyze("AGE", my_broken_afun)

    expect_error(build_table(lyt, DM), "Number of rows generated by analysis function do not match across all columns.")
})

test_that("warning when same name siblings", {

    lyt <- basic_table() %>%
        analyze("AGE", mean) %>%
        analyze("AGE", mean, var_labels = "AGE2")

    expect_warning({tbl <- build_table(lyt, DM)},
                   "Non-unique sibling analysis table names")

    expect_identical(row_paths(tbl)[[3]][2],
                     "AGE2")
})
