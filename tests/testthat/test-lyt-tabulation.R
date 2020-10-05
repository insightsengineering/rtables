context("tabulation framework")




## this
test_that("complex layout works", {
    lyt = NULL %>% split_cols_by("ARM") %>%
        ## add nested column split on SEX with value lables from gend_label
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        ## No row splits have been introduced, so this adds
        ## a root split and puts summary content on it labelled Overall (N)
        ## add_colby_total(label = "All") %>%
        ##    summarize_row_groups(label = "Overall (N)", format = "(N=xx)") %>%
        add_colcounts() %>%
        ## add a new subtable that splits on RACE, value labels from ethn_label
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        ##
        ## Add nested row split within Race categories for FACTOR2
        ## using a split function that excludes level C
        ## value labels from fac2_label
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label") %>%
        ## Add count summary within FACTOR2 categories
        summarize_row_groups("FACTOR2") %>%
        ## Add analysis/data rows by analyzing AGE variable
        ## Note afun is a function that returns 2 values in a named list
        ## this will create 2 data rows
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                         median = median(x)),
                          format = "xx.xx") %>%
        ## adding more analysis vars "compounds them", placing them at the same
        ## level of nesting as all previous analysis blocks, rather than
        ## attempting to further nest them
        analyze("AGE", "Age Analysis redux", afun = range, format = "xx.x - xx.x") %>%

        ## Note nested=TRUE, this creates a NEW subtable directly under the
        ## root split
        ## afun of table() gives us k count rows, where k is the number of
        ## levels of VAR3, in this case 2.
        analyze("VAR3", "Var3 Counts", afun = list_wrap_x(table), nested = FALSE)


    expnames = c("Caucasian (n)", "Level A", "Age Analysis", "mean", "median",
                 "Age Analysis redux", "range", "Level B", "Age Analysis",
                 "mean", "median", "Age Analysis redux", "range",
                 "African American (n)", "Level A", "Age Analysis", "mean", "median",
                 "Age Analysis redux", "range", "Level B", "Age Analysis",
                 "mean", "median", "Age Analysis redux", "range",
                 "level1", "level2")
    tab = build_table(lyt, rawdat)
    print(tab)
    ## XXX TODO this assumes we want no var label on VAR3 subtable
    expect_identical(dim(tab), c(28L, 4L))
    expect_identical(row.names(tab), expnames)
})





test_that("existing table in layout works", {
    thing2 = NULL %>% split_cols_by("ARM") %>%
    ## add nested column split on SEX with value labels from gend_label
    split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
    analyze(c("AGE", "AGE"), c("Age Analysis", "Age Analysis Redux"), afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), format = "xx.xx")

tab2 = build_table(thing2, rawdat)


    thing3 = NULL %>% split_cols_by("ARM") %>%
        ## add nested column split on SEX with value labels from gend_label
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        analyze("AGE", "Age Analysis",
                          afun = function(x) list(mean = mean(x), median = median(x)),
                          format = "xx.xx") %>%
        ## stack an existing table onto the layout and thus the generated table
        add_existing_table(tab2)

    tab3 = build_table(thing3, rawdat)
    expect_equal(nrow(tab3), 12)
    tab3
})


test_that("labelkids parameter works", {
    yeslabellyt <- NULL %>% split_cols_by("ARM") %>%
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


    misslabellyt <- NULL %>%
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


    nolabellyt <- NULL %>%
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

    mixedlyt2 <- NULL %>%
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


    mixedlyt <- NULL %>%
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


    varshowlyt <- NULL %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      visible_label = TRUE) %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx",
                show_labels = "hidden")

    varshowtab <- build_table(varshowlyt, rawdat)
    expect_identical(row.names(varshowtab)[1:4],
                     c("Caucasian (n)", "Factor2", "Level A", "mean"))
})



refcompmean = function(x, .ref_group, .in_ref_col, ...) {
    if(.in_ref_col)
        val <- rcell(NULL)
    else
        val <- rcell(mean(x, ...) - mean(.ref_group,...), format = "xx.xx")

    in_rows(
        "Diff from reference - mean" = val
    )
}

test_that("ref_group comparisons work", {

    blthing = NULL %>% split_cols_by("ARM", ref_group = "ARM1") %>%
        analyze("AGE", show_labels = "hidden") %>%
        analyze("AGE", refcompmean, show_labels = "hidden")
    ## function(x) list(mean = mean(x)))


    bltab = build_table(blthing, rawdat)
    expect_identical(dim(bltab), c(2L,2L))
    expect_null(bltab[2,1, drop = TRUE])
    c1 = bltab[1,1, drop = TRUE]
    c2 = bltab[1,2, drop = TRUE]
    c3 = bltab[2,2, drop = TRUE]
    expect_equivalent(c2 - c1, c3)

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", ref_group = "F") %>%
        analyze("AGE", mean, show_labels = "hidden") %>%
        analyze("AGE", refcompmean, show_labels="hidden") %>%
        split_rows_by("RACE", nested = FALSE, split_fun = drop_split_levels) %>%
        analyze("AGE", mean, show_labels = "hidden") %>%
        analyze("AGE", refcompmean, show_labels = "hidden")

    bltab2 = build_table(lyt, DM)
    d1 = bltab2[4,1, drop = TRUE]
    d2 = bltab2[4,2, drop = TRUE]
    d3 = bltab2[5,2, drop = TRUE]

    expect_equivalent(d2 - d1, d3)
    d4 = bltab2[1,3, drop = TRUE]
    d5 = bltab2[1,4, drop = TRUE]
    d6 = bltab2[2,4, drop = TRUE]
    expect_equivalent(d5 - d4, d6)

    d7 = bltab2[4,3, drop = TRUE]
    d8 = bltab2[4,4, drop = TRUE]
    d9 = bltab2[5,4, drop = TRUE]
    expect_equivalent(d8 - d7, d9)
})

test_that("missing vars caught", {
    misscol = NULL %>% split_cols_by("ARM") %>%
    split_cols_by("SX", "Gender") %>%
    analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), format = "xx.xx")

    expect_error(build_table(misscol, rawdat))

    missrsplit =  NULL %>% split_cols_by("ARM") %>%
    split_cols_by("SEX", "Gender") %>%
    split_rows_by("RACER", "ethn") %>%
    analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                           median = median(x)), format = "xx.xx")

    expect_error(build_table(missrsplit, rawdat))

    missavar =  NULL %>% split_cols_by("ARM") %>%
    split_cols_by("SEX", "Gender") %>%
    split_rows_by("RACE", "ethn") %>%
    analyze("AGGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                                    median = median(x)), format = "xx.xx")

    expect_error(build_table(missavar, rawdat))
    })



test_that("cfun args", {
    cfun1 <- function(df, labelstr, .N_col, .N_total) {
        stopifnot(is(df, "data.frame"))
        in_rows(
            rcell(nrow(df) *c(1, 1/.N_col), format = "xx (xx.xx%)"),
            .names = labelstr)
    }
    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        summarize_row_groups(cfun = cfun1)

    tbl <- build_table(lyt, rawdat)
    expect_null(print(tbl))
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

    ## test check for label-value concordance.
    expect_error(build_table(yeslblslyt, rawdat2), "There does not appear to be a 1-1 correspondence between values in split var \\[SEX\\] and label var \\[gend_label\\]")
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
    res = vapply(names(rows), function(nm) isTRUE(all.equal(unname(unlist(row_values(rows[[nm]]))), ans[[nm]])), NA)
    expect_true(all(res))
})


test_that("extra args works", {
    oldop = options(warn=2)
    on.exit(options(oldop))
    colfuns <- list(function(x, add = 0, na.rm = TRUE) rcell(mean(c(NA,x), na.rm = na.rm)+ add, format = "xx.x"),
                    function(x, cutoff = .5, na.rm = TRUE) rcell(sum(c(NA, x > cutoff), na.rm = na.rm), format = "xx"))

    l <-  basic_table() %>% split_cols_by("ARM") %>%
        split_cols_by_multivar(c("VALUE", "PCTDIFF")) %>%
        analyze_colvars(afun = colfuns)

    l

    tbl_noex <- build_table(l, rawdat2)

    ## one for each different function in colfuns, assigned correctly
    l2 <-  basic_table() %>% split_cols_by("ARM") %>%
        split_cols_by_multivar(c("VALUE", "PCTDIFF")) %>%
        analyze_colvars(afun = colfuns, extra_args = list(list(add = 5), list(cutoff = 100)))


    tbl_ex <- build_table(l2, rawdat2)

    vals_noex <- row_values(tree_children(tbl_noex)[[1]])
    vals_ex <-  row_values(tree_children(tbl_ex)[[1]])

    expect_identical(unlist(vals_noex[c(1,3)]) + 5,
                     unlist(vals_ex[c(1,3)]))
    truevals <- tapply(rawdat2$PCTDIFF, rawdat2$ARM, function(x) sum(x>100, na.rm= TRUE), simplify = FALSE)
    expect_equal(unname(unlist(truevals)),
                 unname(unlist(vals_ex[c(2,4)])))

    vals_noex <- row_values(tree_children(tbl_noex)[[1]])
    vals_ex <-  row_values(tree_children(tbl_ex)[[1]])

    expect_identical(unlist(vals_noex[c(1,3)]) + 5,
                     unlist(vals_ex[c(1,3)]))
    truevals <- tapply(rawdat2$PCTDIFF, rawdat2$ARM, function(x) sum(x>100, na.rm= TRUE), simplify = FALSE)
    expect_equal(unname(unlist(truevals)),
                 unname(unlist(vals_ex[c(2,4)])))

    ## single argument passed to all functions
    l2b <-  basic_table() %>% split_cols_by("ARM") %>%
        split_cols_by_multivar(c("VALUE", "PCTDIFF")) %>%
        analyze_colvars(afun = colfuns, extra_args = list(na.rm = FALSE))

    tbl_ex2 <- build_table(l2b, rawdat2)

    expect_true(all(is.na(unlist(rtables:::row_values(tree_children(tbl_ex2)[[1]])))))

    ## one argument for a single function.

    lyt <- basic_table() %>%
      analyze("Sepal.Length", afun = function(x, a) {
          in_rows(mean_a = rcell(mean(x) + a , format = "xx"))

      }, extra_args = list( a = 1))


    tbl <- build_table(lyt, iris)
    expect_equal(tbl[1,1, drop = TRUE], mean(iris$Sepal.Length) + 1)

    ## two arguments for a single function
    lyt2 <- basic_table() %>%
        analyze("Sepal.Length", afun = function(x, a, b) {
          in_rows(mean_a = rcell(mean(x) + a + b , format = "xx"))

      }, extra_args = list( a = 1, b = 3))


    tbl2 <- build_table(lyt2, iris)
    expect_equal(tbl2[1,1, drop = TRUE], mean(iris$Sepal.Length) + 1 + 3)
})


test_that("make_afun unit tests", {
    value_labels <- rtables:::value_labels
    obj_format <- rtables:::obj_format
    ## use existing funcs to ensure coverage numbers are correct
    res_tafun1 <- rtables:::test_afun(1:10, 5)
    expect_identical(value_labels(res_tafun1),
                     list("Min." = "Minimum", "1st Qu." = "1st Quartile",
                          Median = "Median",
                          Mean = "Mean",
                          "3rd Qu." = "Third Quartile",
                          "Max." = "Maximum",
                          grp = "grp"))

    res_tafun2 <- rtables:::test_afun_grp(1:10, 5)
    expect_identical(lapply(res_tafun2, obj_format),
                     list("Min." = "xx.x", "1st Qu." = "xx.xx", Median = NULL, Mean = NULL,
                          "3rd Qu." = "xx.xx", "Max." = "xx.x",
                          range = "xx - xx", n_unique = "xx - xx"))

 s_summary <- function(x) {
   stopifnot(is.numeric(x))

   list(
     n = sum(!is.na(x)),
     mean_sd = c(mean = mean(x), sd = sd(x)),
     min_max = range(x)
   )
 }

 a_summary <- make_afun(
   fun = s_summary,
   .formats = c(n = "xx", mean_sd = "xx.xx (xx.xx)", min_max = "xx.xx - xx.xx"),
   .labels = c(n = "n", mean_sd = "Mean (sd)", min_max = "min - max")
 )
    expect_identical(formals(a_summary),
                     formals(s_summary))

    asres1 <- a_summary(x = iris$Sepal.Length)
    expect_identical(list(n = "n", mean_sd = "Mean (sd)", min_max = "min - max"),
                     rtables:::value_labels(asres1))

 a_summary2 <- make_afun(a_summary, .stats = c("n", "mean_sd"))
    expect_identical(formals(a_summary2),
                     formals(s_summary))
    asres1 <- a_summary2(x = iris$Sepal.Length)
    expect_identical(names(asres1), c("n", "mean_sd"))

 a_summary3 <- make_afun(a_summary, .formats = c(mean_sd = "(xx.xxx, xx.xxx)"))

    asres3 <- a_summary3(iris$Sepal.Length)
    expect_equal(lapply(asres3, rtables:::obj_format),
                 list(n = "xx", mean_sd = "(xx.xxx, xx.xxx)", min_max= "xx.xx - xx.xx"))

 s_foo <- function(df, .N_col, a = 1, b = 2) {
    list(
       nrow_df = nrow(df),
       .N_col = .N_col,
       a = a,
       b = b
    )
 }

  a_foo <- make_afun(s_foo, b = 4,
  .formats = c(nrow_df = "xx.xx", ".N_col" = "xx.", a = "xx", b = "xx.x"),
  .labels = c(nrow_df = "Nrow df", ".N_col" = "n in cols", a = "a value", b = "b value")
 )

 expect_identical(formals(a_foo),
                  formals(s_foo))
    ares1 <- a_foo(iris, .N_col = 40)
    expect_identical(unlist(unname(rtables:::value_labels(ares1))),
                     c("Nrow df", "n in cols", "a value", "b value"))

    expect_equal(unlist(ares1$b), 4)
    expect_equal(unlist(ares1$a), 1)

    ares1b <- a_foo(iris, .N_col = 40, b = 8)
    expect_identical(unlist(unname(rtables:::value_labels(ares1))),
                     c("Nrow df", "n in cols", "a value", "b value"))

    expect_equal(unlist(ares1b$b), 8)

 a_foo2 <- make_afun(a_foo, .labels = c(nrow_df = "Number of Rows"))
    ares2 <- a_foo2(iris, .N_col = 40, b = 6)
    expect(unlist(ares2$b), 6)
    expect_identical(unlist(unname(rtables:::value_labels(ares2))),
                     c("Number of Rows", "n in cols", "a value", "b value"))

 ## with dots

 sfun3 <- function(x, .ref_group = NULL, ...) "hi"
 afun3 <- make_afun(sfun3)
 expect_identical(formals(sfun3),
                  formals(afun3))
## ungrouping
    sfun4 <- function(x) {
        list(single1 = 5,
             single2 = 10,
             grouped1 = list(g1 = 11, g2 = 15, g3 = with_label(17, "sneaky label")),
             grouped2 = list(g4 = c(2,3), g5 = c(6, 10)))
    }
    afun4 <- make_afun(sfun4,
                       .labels = c(single1 = "first single val",
                                   single2 = "second single val"),
                       .formats = c(grouped2 = "xx - xx"),
                       .ungroup_stats = c("grouped1", "grouped2"))
    expect_identical(formals(sfun4), formals(afun4))
    ares4 <- afun4(5)
    expect_identical(names(ares4),
                     c("single1", "single2", "g1", "g2", "g3", "g4", "g5"))
    expect_identical(value_labels(ares4),
                     list(single1 ="first single val",
                          single2 = "second single val",
                          g1 = "g1",
                          g2 = "g2",
                          g3 = "sneaky label",
                          g4 = "g4",
                          g5 = "g5"))

    s_nodflt <- function(df, .N_col, a = 1, b) {
        list(
            nrow_df = nrow(df),
       .N_col = .N_col,
       a = a,
       b = b
       )
    }

    a_nodflt <- make_afun(s_nodflt, b = 4,
                          .formats = c(nrow_df = "xx.xx", ".N_col" = "xx.", a = "xx", b = "xx.x"),
                          .labels = c(nrow_df = "Nrow df", ".N_col" = "n in cols", a = "a value", b = "b value")
                          )
    a_nodflt(iris, 5)[["b"]]
})

test_that("make_afun+build_table integration tests", {

    ## summary function that uses with_label
    s_summary <- function(x) {
   stopifnot(is.numeric(x))

   list(
     n = with_label(sum(!is.na(x)), "N subjects"),
     mean_sd = with_label(c(mean = mean(x), sd = sd(x)), "My mean and SD"),
     min_max = with_label(range(x), "Range")
   )
 }
 a_summary <- make_afun(
   s_summary,
   .labels = c(n = "n subjects"),  # only overwrite the label of the `n` statistics here.
   .formats = c(n = "xx", mean_sd = "xx.xx (xx.xx)", min_max = "xx.xx - xx.xx")
 )
 tbl <- basic_table() %>%
   analyze(
     "Sepal.Length",
     afun = a_summary
   ) %>%
     build_table(iris)

    expect_identical(row.names(tbl),
                     c("n subjects",
                       "My mean and SD",
                       "Range"))

    tbl2 <-  basic_table() %>%
        split_cols_by("Species") %>%
   analyze(
     "Sepal.Length",
     afun = a_summary
   ) %>%
     build_table(iris)

    expect_identical(row.names(tbl2),
                     c("n subjects",
                       "My mean and SD",
                       "Range"))

    ## summary function that does not use with_label
    s_summary2 <- function(x) {
        stopifnot(is.numeric(x))

        list(
            n = sum(!is.na(x)),
            mean_sd = c(mean = mean(x), sd = sd(x)),
            min_max = range(x)
        )
    }

    a_summary2 <- make_afun(
        fun = s_summary2,
        .formats = c(n = "xx", mean_sd = "xx.xx (xx.xx)", min_max = "xx.xx - xx.xx"),
        .labels = c(n = "n subjects", mean_sd = "My mean and SD", min_max = "Range")
    )
     tbl3 <- basic_table() %>%
   analyze(
     "Sepal.Length",
     afun = a_summary2
   ) %>%
     build_table(iris)

    expect_identical(row.names(tbl3),
                     c("n subjects",
                       "My mean and SD",
                       "Range"))

    tbl4 <-  basic_table() %>%
        split_cols_by("Species") %>%
   analyze(
     "Sepal.Length",
     afun = a_summary2
   ) %>%
     build_table(iris)

    expect_identical(row.names(tbl4),
                     c("n subjects",
                       "My mean and SD",
                       "Range"))

    ## recursive make_afun applications
    ## with with_label

    a_function3 <- make_afun(
        a_summary,
        .labels = c(min_max = "New Range"))
    tbl5 <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze("Sepal.Length",
                afun = a_function3) %>%
        build_table(iris)

    expect_identical(row.names(tbl5),
                     c("n subjects",
                     "My mean and SD",
                     "New Range"))

    a_function4 <- make_afun(
        a_summary2,
        .labels = c(min_max = "New Range"))
    tbl5 <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze("Sepal.Length",
                afun = a_function4) %>%
        build_table(iris)

    expect_identical(row.names(tbl5),
                     c("n subjects",
                     "My mean and SD",
                     "New Range"))
})

test_that("Colcounts work correctly", {
    lyt1 <- basic_table() %>% add_colcounts() %>%
        analyze("AGE")
    tbl1 <- build_table(lyt1, DM)

    expect_identical(col_counts(tbl1), nrow(DM))

    lyt2 <- lyt1 %>% split_cols_by("ARM")
    tbl2 <- build_table(lyt2, DM)

    expect_identical(col_counts(tbl2),
                     as.integer(table(DM$ARM)))


    tbl3 <- build_table(lyt2, DM, col_counts = c(500L, NA, NA))
    expect_identical(col_counts(tbl3),
                     c(500L, as.integer(table(DM$ARM))[2:3]))

    expect_error(build_table(lyt2, DM, col_counts = c(20L, 40L)))
})
