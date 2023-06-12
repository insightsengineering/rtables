context("ARDs")


test_that("ARD generation works v0", {

    ## change here (only) when v0 is crystalized (no longer experimental)
    spec_version <- "v0_experimental"
    lyt <- make_big_lyt()

    tbl <- build_table(lyt, rawdat)

    ard <- as_ard(tbl, spec_version)
    expect_identical(ard[2, "ARM1.M"][[1]],
                     c(37, 37/256))

    expect_identical(nrow(tbl) - 8L,
                     nrow(ard))

    expect_identical(names(ard)[1:5],
                     c("spl_var_1", "spl_value_1", "spl_var_2", "spl_value_2", "avar_name"))

    ## handle multiple analyses
    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("STRATA1") %>%
        analyze(c("AGE", "BMRKR2"))

    tbl2 <- build_table(lyt, ex_adsl)
    ard2 <- as_ard(tbl2, spec_version )

    ## regression test
    expect_false(any(is.na(ard2$spl_var_1)))

    ## test colvar analysis and bug regarding 1 row multi column tables
    test <- data.frame(
        a = c(1, 2),
        b = c(1, NA)
    )

    lyt3 <- basic_table() %>%
        split_cols_by_multivar(c("a", "b")) %>%
        analyze_colvars(afun = length, inclNAs = TRUE)

    tbl3 <- build_table(lyt3, test)
    ard3 <- as_ard(tbl3, spec_version)

    expect_identical(nrow(ard3), 1L)

})
