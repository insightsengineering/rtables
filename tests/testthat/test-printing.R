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
                            byrow = TRUE, nrow = 2))
    expect_identical(matform$spans,
                     matrix(c(1, rep(2, 4),
                              rep(1, 10)),
                            byrow = TRUE,
                            nrow = 3,
                            dimnames = list(NULL, c("", paste(rep(c("ARM1", "ARM2"),
                                                                      times = c(2,2)),
                                                              rep(c("Male", "Female"),
                                                                  times = 2),
                                                              sep = ".")))))
})
