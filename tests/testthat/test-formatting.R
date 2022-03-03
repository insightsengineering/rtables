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



test_that("table_shell works", {

    tbl <- rtable(c("A", "B"),
                  rrow("Hiya", rcell(c(2, .2), format = "xx (xx.x%)"), rcell(c(.1, .2)), format = "xx.x - xx.x"),
                  rrow("bye", 5.2345, 17.2),
                  format = "xx.xx")


    tblsh <- rtable(c("A", "B"),
                  rrow("Hiya", "xx (xx.x%)", "xx.x - xx.x"),
                  rrow("bye", "xx.xx", "xx.xx"))

    expect_identical(toString(tblsh),
                     paste0(capture_output(table_shell(tbl)), "\n"))
})
