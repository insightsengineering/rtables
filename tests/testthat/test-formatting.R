context("value formatting")


test_that("sprintf_format works correctly", {


    myfun <- sprintf_format("hi there %1.4f")

    lyt <- basic_table() %>%
        split_cols_by("Species") %>%
        analyze("Sepal.Width", afun = mean, format = myfun)

    tbl <- build_table(lyt, iris)

    matform <- matrix_form(tbl)

    expect_identical(matform$strings[2,],
                     c("mean", "hi there 3.4280", myfun(2.77), myfun(mean(subset(iris, Species == "virginica")$Sepal.Width))))
})

