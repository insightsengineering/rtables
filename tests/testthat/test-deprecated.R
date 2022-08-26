
context("deprecated functionality")


test_that("deprecated things are still there and work kinda", {
    expect_warning(lyt11 <- split_cols_by(lyt = NULL, "ARM"), "deprecated")
    expect_identical(lyt11, basic_table() %>% split_cols_by("ARM"))
    expect_warning(lyt22 <- split_rows_by(lyt = NULL, "ARM"), "deprecated")
    expect_identical(lyt22, basic_table() %>% split_rows_by("ARM"))
})

test_that("deprecated insert_rrow and summarize_rows still currently work", {


    tbl <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze("Sepal.Length") %>%
        build_table(iris)

    expect_warning({res1 <- insert_rrow(tbl, rrow("Hello World"))}, "Deprecated")
    expect_warning({resdf <- summarize_rows(tbl)}, "Deprecated")
    row.names(resdf) <- NULL
    realdf <- make_row_df(tbl)
    cololaps <- intersect(names(resdf), names(realdf))
    expect_true(all.equal(resdf[, cololaps], realdf[, cololaps]))
    o <- options(warn = -1)
    mf1 <- matrix_form(res1)
    expect_identical(mf1$strings[2, , drop = TRUE], c("Hello World", "", "", ""))

    res2 <- insert_rrow(tbl, rrow("Hello World"), at = 2)
    mf2 <- matrix_form(res2)
    expect_identical(mf2$strings[3, , drop = TRUE], c("Hello World", "", "", ""))

    tbl2 <- basic_table() %>%
        split_cols_by("Species") %>%
        split_rows_by("Species") %>%
        analyze("Sepal.Length") %>%
        build_table(iris)
    ## for coverage
    expect_warning({resdf2 <- summarize_rows(tbl2)}, "Deprecated")
    res3 <- insert_rrow(tbl2, rrow("Hello World"))
    mf3 <- matrix_form(res3)
    expect_identical(mf3$strings[2, , drop = TRUE], c("Hello World", "", "", ""))
    res4 <- insert_rrow(tbl2, rrow("Hello World"), at = 2)
    mf4 <- matrix_form(res4)
    expect_identical(mf4$strings[3, , drop = TRUE], c("Hello World", "", "", ""))

    res5 <- insert_rrow(tbl2, rrow("Hello World"), at = 4)
    mf5 <- matrix_form(res5)
    expect_identical(mf5$strings[5, , drop = TRUE], c("Hello World", "", "", ""))
    res6 <- insert_rrow(tbl2, rrow("new row", 5, 6, 7))
    mf6 <- matrix_form(res6)
    expect_identical(mf6$strings[2, , drop = TRUE], c("new row", "5", "6", "7"))
    res7 <- insert_rrow(tbl2, rrow("new row", 5, 6, 7), at = 3)
    mf7 <- matrix_form(res7)
    expect_identical(mf7$strings[4, , drop = TRUE], c("new row", "5", "6", "7"))

    options(o)
})


test_that

test_that("split_rows_by_multivar works", {

    DM2 <- DM
    inds <- sample(seq_len(nrow(DM)), floor(.25 * nrow(DM)))
    DM2$STRATA1[inds] <- NA
    lyt <- basic_table() %>%
        rtables:::split_rows_by_multivar(c("RACE", "STRATA1")) %>%
        summarize_row_groups()

    tbl <- build_table(lyt, DM2)
    expect_true(TRUE)
})
