context("Accessor tests")

test_that("various accessors work at the layout/table level", {
    ## coltree
    col_extra_args <- rtables:::col_extra_args
    clayout_splits <- rtables:::clayout_splits

    l <- basic_table() %>% split_cols_by("ARM") %>%
        split_cols_by_multivar(c("AGE", "BMRKR1")) %>%
        analyze_colvars(list(mean, sd))

    pred <- coltree(l, DM)
    tbl <- build_table(l, DM)
    postd <- coltree(tbl)
    expect_identical(col_extra_args(pred),
                     col_extra_args(postd))
    expect_identical(names(pred),
                     names(postd))
    ## expect_identical(clayout_splits(pred),
    ##                  clayout_splits(postd))

})
