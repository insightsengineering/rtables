context("regression tests")


test_that("unlisting rtables has no effect on them", {

  t1 <- rtable(header = c("A", "B"), format = "xx", rrow("row 1", 1, 2))

  expect_identical(t1, unlist(t1))
})


test_that("manually created label l rows are always visible",
          expect_true(rtables:::labelrow_visible(rrow(""))))


## was error before rtables 0.3.2.16
test_that("printing table with 0 rows works", {
    norows <- rtable(c("hi", "lo"))
    expect_null(print(norows))
})


## multivarsplit varlabels work correctly

test_that("MultiVarSplit varlabels work correctly", {

    tbl <- basic_table() %>% split_cols_by("ARM") %>%
        split_cols_by_multivar(c("VALUE", "PCTDIFF"), varlabels = c("Measurement", "Pct Diff")) %>%
        split_rows_by("RACE", split_label = "ethnicity", split_fun = drop_split_levels) %>%
        summarize_row_groups() %>%
        analyze_colvars(afun = mean, format = "xx.xx") %>%
        build_table(rawdat2)

    expnms <- c("ARM1.Measurement", "ARM1.Pct Diff",
                "ARM2.Measurement",
                "ARM2.Pct Diff")
    expect_identical(names(col_exprs(col_info(tbl))),
                     expnms)
})


test_that("inclNAs argument works as expected", {

    tinydat <- data.frame(RSP = c(TRUE, FALSE, NA, TRUE),
                                    ARM = factor(c("A", "A", "B", "B")))
    tbl1 <- basic_table() %>%
        split_cols_by("ARM") %>%
        analyze(vars = "RSP", inclNAs = FALSE) %>%
        build_table(df = tinydat)

    expect_equal(tbl1[1,2, drop=TRUE], 1)
    tbl2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        analyze(vars = "RSP", inclNAs = TRUE) %>%
        build_table(df = tinydat)

    expect_true(is.na(tbl2[1,2,drop = TRUE]))
})

test_that("head/tail work", {
    tbl = rtable(c("hi", "lo"),
                 rrow("rn", 5, 5))
    expect_false(is.null(head(tbl)))
    expect_false(is.null(tail(tbl)))
})

test_that("sort does not clobber top-level siblings", {
    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        analyze("AGE") %>%
        split_rows_by("SEX") %>%
        analyze("AGE", function(x) in_rows(mean = mean(x), "mean+5" = mean(x) +5))

    tbl <- build_table(lyt, rawdat)

    stbl <- sort_at_path(tbl, c("SEX", "*", "AGE"), function(tt) sum(unlist(row_values((tt)))), decreasing = TRUE)

    expnms = c("Mean", "M", "mean+5", "mean", "F", "mean+5", "mean")
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
    expect_equal(dim(tbl), c(1,1))
})


test_that("add_colcounts works as first call", {
    tbl <- basic_table() %>% add_colcounts() %>%
        analyze("AGE") %>% build_table(DM)

    expect_equal(tbl[1,1, drop = TRUE], mean(DM$AGE))
})

test_that("CellValue on something with object labels", {
    expect_identical(obj_label(CellValue(with_label(5, "hi"))),
                     "hi")

    expect_identical(obj_label(CellValue(with_label(5, "hi"),
                                         label = "")),
                     "")

    expect_identical(obj_label(CellValue(with_label(5, "hi"),
                                         label = NULL)),
                     "hi")
})


test_that("rcell on CellValue overrides attrs as necessary", {
    val <- CellValue(c(100, .5), format = "xx (xx.x%)", label = "oldlabel",
                     colspan = 2L,
                     indent_mod = 2L)
    val2 <-  CellValue(c(100, .5), format = "xx (xx.xx%)", label = "new label",
                     colspan = 3L,
                     indent_mod = 3L)
    expect_identical(rcell(val, format = "xx (xx.xx%)", label = "new label",
                           colspan = 3L, indent_mod = 3L),
                     val2)
})


test_that("cbind_rtables works", {

    x <- rtable(c("A", "B"), rrow("row 1", 1,2), rrow("row 2", 3, 4))

    y <- rtable("C", rrow("row 1", 5), rrow("row 2", 6))

    tab <- cbind_rtables(x, y)
    expect_equal(ncol(tab), 3)
    expect_equal(ncol(rtables:::tt_labelrow(tab)), 3)
    expect_equal(nrow(tab), 2)
})


test_that("cbind_rtables works with 3 tables", {

    tab1 <- rtable(
        header = "a",
        rrow("one", 1)
    )
    tab2 <- rtable(
        header = "b",
        rrow("one", 2)
    )
    tab3 <- rtable(
        header = "c",
        rrow("one", 3)
    )

    newtab <- cbind_rtables(tab1, tab2, tab3)
    expect_equal(ncol(newtab), 3)
    expect_equal(c(1, 2, 3), unlist(cell_values(newtab)))

})


test_that("cell formats not dropped when cbinding", {

    tab1 <- rtable(
        header = "a",
        rrow("one", rcell(1.1111111, format = "xx.x"))
    )
    tab2 <- rtable(
        header = "b",
        rrow("one", rcell(2.2222222, format = "xx.xxxx"))
    )

    cbtab <- cbind_rtables(tab1, tab2)
    expect_identical(rtables:::value_formats(tree_children(cbtab)[[1]]),
                     list("xx.x", "xx.xxxx"))


})
