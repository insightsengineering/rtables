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

