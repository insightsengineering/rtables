context("Printing tables")


test_that("labels correctly used for columns rather than names", {
    lyt <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        analyze("AGE")

    tbl <- build_table(lyt, rawdat)

    matform <- matrix_form(tbl)
    expect_identical(matform$strings[1:2,],
                     matrix(c("", rep(c("ARM1", "ARM2"), times = c(2, 2)),
                              "", rep(c("Male", "Female"), times = 2)),
                            byrow = TRUE, nrow = 2, dimnames = NULL))
    expect_identical(matform$spans,
                     matrix(c(1, rep(2, 4),
                              rep(1, 10)),
                            byrow = TRUE,
                            nrow = 3,
                            dimnames = list(NULL, c("", paste(rep(c("ARM1", "ARM2"),
                                                                      times = c(2,2)),
                                                              rep(c("M", "F"),
                                                                  times = 2),
                                                              sep = ".")))))

    ## multivarsplit varlabels work correctly
    tbl2 <- basic_table() %>% split_cols_by("ARM") %>%
        split_cols_by_multivar(c("VALUE", "PCTDIFF"), varlabels = c("Measurement", "Pct Diff")) %>%
        split_rows_by("RACE", split_label = "ethnicity", split_fun = drop_split_levels) %>%
        summarize_row_groups() %>%
        analyze_colvars(afun = mean, format = "xx.xx") %>%
        build_table(rawdat2)

    matform2 <- matrix_form(tbl2)

    expect_identical(matform2$strings[1:2,],
                     matrix(c("", rep(c("ARM1", "ARM2"), times = c(2, 2)),
                       "", rep(c("Measurement", "Pct Diff"), times = 2)),
                       byrow = TRUE, nrow = 2))

    ## same var different labels in split_by_multivar
    vlabs <- c("Age", "SecondAge", "Gender", "Age Redux")
    lyt3 <- basic_table() %>%
        split_cols_by_multivar(c("AGE", "AGE", "SEX", "AGE"),
                               varlabels = vlabs) %>%
        analyze_colvars(list(mean, median, function(x,...) max(table(x)), sd))

    tbl3 <- build_table(lyt3, rawdat)
    matform3 <- matrix_form(tbl3)
    expect_identical(matform3$strings[1,],
                     c("", vlabs))

})

test_that("nested identical labels work ok", {
    df <- data.frame(
        h2 = factor(c("<Missing>")),
        x = factor(c("<Missing>"))
    )

    t2 <- basic_table() %>%
        split_rows_by("h2") %>%
        analyze("x") %>%
        build_table(df)
    mat <- matrix_form(t2)
    expect_identical(mat$strings[,1], c("", "<Missing>", "<Missing>"))
})
