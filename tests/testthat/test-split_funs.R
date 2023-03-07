context("Split Functions")

test_that("remove_split_levels works as expected with factor variables", {
  my_split_fun <- remove_split_levels(excl = "ASIAN")

  stopifnot(is.factor(DM$RACE))
  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups(format = "xx")

  tab <- build_table(l, DM)
  expect_identical(unname(unlist(cell_values(tab)[[1]])),
                   c(28L, 24L, 27L))

  expect_false("ASIAN" %in% row.names(tab))
})

test_that("remove_split_levels works as expected with character variables", {
  my_split_fun <- remove_split_levels(excl = "ASIAN")

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()

  DM2 <- DM
  DM2$RACE <- as.character(DM2$RACE)
  tab <- build_table(l, DM2)

  expect_false("ASIAN" %in% row.names(tab))
})

test_that("drop_and_remove_levels works as expected when dropping not appearing levels", {
  my_split_fun <- drop_and_remove_levels(excl = "ASIAN")

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()

  tab <- build_table(l, DM)

  expect_setequal(
    row.names(tab),
    setdiff(unique(DM$RACE), "ASIAN")
  )
})

test_that("drop_and_remove_levels also works with character variables", {
  my_split_fun <- drop_and_remove_levels(excl = "ASIAN")

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()

  DM2 <- DM
  DM2$RACE <- as.character(DM2$RACE)
  tab <- build_table(l, DM2)

  expect_setequal(
    row.names(tab),
    setdiff(unique(DM$RACE), "ASIAN")
  )
})

test_that("trim_levels_to_map split function works", {


    map <- data.frame(
        LBCAT = c("CHEMISTRY", "CHEMISTRY", "CHEMISTRY", "IMMUNOLOGY"),
        PARAMCD = c("ALT", "CRP", "CRP", "IGA"),
        ANRIND = c("LOW", "LOW", "HIGH", "HIGH"),
        stringsAsFactors = FALSE
    )

    lyt <- basic_table() %>%
        split_rows_by("LBCAT") %>%
        split_rows_by("PARAMCD", split_fun = trim_levels_to_map(map = map)) %>%
        analyze("ANRIND")
    tbl1 <- build_table(lyt, ex_adlb)

    expect_identical(row.names(tbl1),
                     c("CHEMISTRY", "ALT", "LOW",
                                    "CRP", "LOW",
                                           "HIGH",
                       "IMMUNOLOGY", "IGA", "HIGH"))

    map2 <- tribble(
        ~ARM, ~RACE,
        "A: Drug X", "ASIAN",
        "A: Drug X", "WHITE",
        "C: Combination", "BLACK OR AFRICAN AMERICAN",
        "C: Combination", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")

    lyt2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("RACE", split_fun = trim_levels_to_map(map = map2)) %>%
        analyze("AGE")

    expect_error(build_table(lyt2, DM), regexp = "map does not allow")

    lyt3 <- basic_table() %>%
        split_cols_by("ARM", split_fun = trim_levels_to_map(map = map2)) %>%
        split_cols_by("RACE", split_fun = trim_levels_to_map(map = map2)) %>%
        analyze("AGE")

    tbl3 <- build_table(lyt3, DM)

    coldf <- make_col_df(tbl3)
    expect_identical(unclass(coldf$path), ## unclass because of the "AsIs" 'class'
                     list(c("ARM", "A: Drug X", "RACE", "ASIAN"),
                          c("ARM", "A: Drug X", "RACE", "WHITE"),
                          c("ARM", "C: Combination", "RACE", "BLACK OR AFRICAN AMERICAN"),
                          c("ARM", "C: Combination", "RACE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")))


    data <- data.frame(LBCAT = c(rep("a", 4), rep("b", 4)),
                       PARAM = c(rep("param1", 4), rep("param2", 4)),
                       VISIT = rep(c("V1", "V2"), 4),
                       ABN = rep(c("H", "L"), 4),
                       stringsAsFactors = TRUE)

    map <- data.frame(LBCAT = c(rep("a", 4), rep("b", 4)),
                      PARAM = c(rep("param1", 4), rep("param2", 4)),
                      VISIT = rep(c("V1", "V1", "V2", "V2"), 2),
                      ABN = rep(c("H", "L"), 4),
                      stringsAsFactors = FALSE)

    lyt4 <- basic_table() %>%
        split_rows_by("LBCAT", split_fun = trim_levels_to_map(map = map)) %>%
        split_rows_by("PARAM", split_fun = trim_levels_to_map(map = map)) %>%
        split_rows_by("VISIT", split_fun = trim_levels_to_map(map = map)) %>%
        analyze("ABN")

    tbl4 <- build_table(lyt4, df = data)
    rpths4 <- row_paths(tbl4)
    expect_identical(rpths4[[7]],
                     c("LBCAT", "a", "PARAM", "param1", "VISIT", "V2", "ABN", "H"))

    expect_equal(unlist(cell_values(tbl4, rpths4[[7]]), use.names = FALSE), 0)
    expect_identical(rpths4[[13]],
                     c("LBCAT", "b", "PARAM", "param2", "VISIT", "V1", "ABN", "L"))

    expect_equal(unlist(cell_values(tbl4, rpths4[[13]]), use.names = FALSE), 0)

    expect_equal(length(rpths4), 16)

})

test_that("trim_levels_in_group works", {

    dat1 <- data.frame(
        l1 = factor(c("A", "B", "C"), levels = c("A", "B", "C")), # note that level X is not included
        l2 = factor(c("a", "b", "c"), levels = c("a", "b", "c", "x"))
    )

    ## This works
    tbl1 <- basic_table() %>%
        split_rows_by("l1", split_fun = trim_levels_in_group("l2")) %>%
        analyze("l2") %>%
        build_table(dat1)


    dat2 <- data.frame(
        l1 = factor(c("A", "B", "C"), levels = c("A", "B", "C", "X")), # here we add X to "l1"
        l2 = factor(c("a", "b", "c"), levels = c("a", "b", "c", "x"))
    )

    ## This previously gave an error because trim_levels_in_group did not drop the empty "l1" levels
    tbl2 <- basic_table() %>%
        split_rows_by("l1", split_fun = trim_levels_in_group("l2")) %>%
        analyze("l2") %>%
        build_table(dat2)

    expect_identical(nrow(tbl1), 6L)
    expect_identical(as.vector(compare_rtables(tbl1, tbl2)),
                     rep(".", nrow(tbl1)))

})


test_that("Custom functions in multivar splits work", {

    uneven_splfun <- function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
        ret <- do_base_split(spl, df, vals, labels, trim)
        if(NROW(df) == 0)
            ret <- lapply(ret, function(x) x[1])
        ret
    }

    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by_multivar(c("USUBJID", "AESEQ", "BMRKR1"),
                               varlabels = c("N", "E", "BMR1"),
                               split_fun = uneven_splfun) %>%
        analyze_colvars(list(USUBJID = function(x, ...) length(unique(x)),
                             AESEQ = max,
                             BMRKR1 = mean))

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
    expect_identical(cvs[c(4, 1:3)],
                     cell_values(tab_b))

    expect_identical(cvs[[4]], mean(DM$AGE))

    l2 <- basic_table() %>%
        split_rows_by("RACE", split_fun = add_overall_level("All Ethnicities")) %>%
        summarize_row_groups(label_fstr = "%s (n)") %>%
        analyze("AGE")

    tab2 <- build_table(l2, DM)
    expect_identical(c(nrow(DM), 1),
                     cell_values(tab2)[[1]][[1]])
})

test_that("split_rows_by_multivar works", {
    lyt <- basic_table() %>%
        split_rows_by_multivar(c("SEX", "STRATA1")) %>%
        summarize_row_groups()
    
    tbl1 <- build_table(lyt, DM)
    
    expect_identical(
        cell_values(tbl1), 
        list(SEX.SEX = list(`all obs` = c(356, 1)), STRATA1.STRATA1 = list(`all obs` = c(356, 1)))
    )
})

test_that("make_split_fun works", {

    mysplitfun <- make_split_fun(pre = list(drop_facet_levels),
                                 post = list(add_overall_facet("ALL", "All Arms")))

    lyt <- basic_table(show_colcounts = TRUE) %>%
        split_cols_by("ARM", split_fun = mysplitfun) %>%
        analyze("AGE")
    tbl <-  build_table(lyt, subset(DM, ARM %in% c("B: Placebo", "C: Combination")))

    ccounts <- col_counts(tbl)
    expect_equal(ncol(tbl), 3L)
    expect_equal(ccounts[3], sum(DM$ARM %in% c("B: Placebo", "C: Combination")))

    lyt2a <- basic_table(show_colcounts = TRUE) %>%
        split_cols_by("ARM", split_fun = trim_levels_in_group("SEX", drop_outlevs = TRUE)) %>%
        split_cols_by("SEX") %>%
        analyze("AGE")

    adslsub <- subset(ex_adsl, (ARM == "A: Drug X" & SEX == "F") |
                            (ARM == "B: Placebo" & SEX == "M"))
    tbl2a <- build_table(lyt2a, adslsub)

    mysplitfun2 <- make_split_fun(pre = list(drop_facet_levels),
                                 post = list(trim_levels_in_facets("SEX")))

    lyt2b <- basic_table(show_colcounts = TRUE) %>%
        split_cols_by("ARM", split_fun = mysplitfun2) %>%
        split_cols_by("SEX") %>%
        analyze("AGE")

    tbl2b <- build_table(lyt2b, adslsub)

    expect_identical(cell_values(tbl2a), cell_values(tbl2b))
    expect_identical(row_paths(tbl2a), row_paths(tbl2b))
    expect_identical(col_paths(tbl2a), col_paths(tbl2b))
    expect_identical(matrix_form(tbl2a, TRUE),
                     matrix_form(tbl2b, TRUE))

    broken_on_purpose <- make_split_fun(pre = list(function(df, ...) stop("oopsie")))

    lyt3 <- basic_table() %>%
        split_cols_by("ARM", split_fun = broken_on_purpose) %>%
        analyze("ARM")

    expect_error(build_table(lyt3, DM), "Error applying custom split function: oopsie")

    ## overriding core core split functionality
    very_stupid_core <- function(spl, df, vals, labels, .spl_context) {
        make_split_result(c("stupid", "silly"), datasplit = list(df[1:10,], df[11:30,]), labels = c("first 10", "second 20"))
    }

    nonsense_splfun <-  make_split_fun(core_split = very_stupid_core,
                                                        post = list(add_combo_facet("dumb", label = "thirty patients",
                                                                                    levels = c("stupid", "silly"))))
    lyt4a <- basic_table() %>%
        split_cols_by("ARM", split_fun = nonsense_splfun) %>%
        analyze("AGE")

    ## not supported in column space, currently
    expect_error(build_table(lyt4a, DM), "override core splitting")

    lyt4b <- basic_table() %>%
        split_rows_by("ARM", split_fun = nonsense_splfun) %>%
        summarize_row_groups() %>%
        analyze("AGE")

    tbl4b <- build_table(lyt4b, DM)

    pths <- row_paths(tbl4b)
    ## check the counts, which checks whether our artificial
    ## facets were created correctly
    expect_equal(10,
                 cell_values(tbl4b, pths[[1]])[[1]][[1]])
    expect_equal(20,
                 cell_values(tbl4b, pths[[3]])[[1]][[1]])
    expect_equal(30,
                 cell_values(tbl4b, pths[[5]])[[1]][[1]])
})
