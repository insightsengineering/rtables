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


test_that("formats supported", {


    forms <- list_rcell_format_labels()

    res <- sapply(forms, function(vc) all(sapply(vc, is_rcell_format)))
    expect_true(all(res))
})



test_that("each format", {
    values <- c(5.123456, 7.891112)

    expect_identical(format_rcell(rcell(values[1], format = "xx")),
                     paste(values[1]))

    expect_identical(format_rcell(rcell(values[1], format = "xx.")),
                     "5")

    expect_identical(format_rcell(rcell(values[1], format = "xx.x")),
                     "5.1")

    expect_identical(format_rcell(rcell(values[1], format = "xx.xx")),
                     "5.12")

    expect_identical(format_rcell(rcell(values[1], format = "xx.xxx")),
                     "5.123")

    expect_identical(format_rcell(rcell(values[1], format = "xx.xxxx")),
                     "5.1235")

    expect_identical(format_rcell(rcell(values[1], format = "xx%")),
                     paste0(values[1]*100, "%"))

    expect_identical(format_rcell(rcell(values[1], format = "xx.x%")),
                     "512.3%")

    expect_identical(format_rcell(rcell(values[1], format = "xx.xx%")),
                     "512.35%")

    expect_identical(format_rcell(rcell(values[1], format = "xx.xxx%")),
                     "512.346%")

    expect_identical(format_rcell(rcell(values[1], format = ">999.9")),
                     "5.1")

    expect_identical(format_rcell(rcell(10000, format = ">999.9")),
                     ">999.9")

    expect_identical(format_rcell(rcell(values[1], format = ">999.99")),
                     "5.12")

    expect_identical(format_rcell(rcell(10000, format = ">999.99")),
                     ">999.99")

    expect_identical(format_rcell(rcell(.0004, format = "x.xxxx | (<0.0001)")),
                     "0.0004")

    expect_identical(format_rcell(rcell(.00004, format = "x.xxxx | (<0.0001)")),
                     "<0.0001")

    expect_identical(format_rcell(rcell(values, format = "xx / xx")),
                     paste(values, collapse  = " / "))

    expect_identical(format_rcell(rcell(values, format = "xx. / xx.")),
                     "5 / 8")

    expect_identical(format_rcell(rcell(values, format = "xx.x / xx.x")),
                     "5.1 / 7.9")

    expect_identical(format_rcell(rcell(values, format = "xx.xx / xx.xx")),
                     "5.12 / 7.89")

    expect_identical(format_rcell(rcell(values, format = "xx.xxx / xx.xxx")),
                     "5.123 / 7.891")

    expect_identical(format_rcell(rcell(values, format = "xx (xx%)")),
                     paste0(values[1], " (", values[2]*100, "%)"))

    expect_identical(format_rcell(rcell(values, format = "xx (xx.%)")),
                     paste0(values[1], " (789%)"))

    expect_identical(format_rcell(rcell(values, format = "xx. (xx.%)")),
                     paste0(5, " (789%)"))

    expect_identical(format_rcell(rcell(values, format = "xx (xx.x%)")),
                     paste0(values[1], " (789.1%)"))

    expect_identical(format_rcell(rcell(values, format = "xx (xx.xx%)")),
                     paste0(values[1], " (789.11%)"))

    expect_identical(format_rcell(rcell(values, format = "xx.x (xx.x%)")),
                     "5.1 (789.1%)")

    expect_identical(format_rcell(rcell(values, format = "xx.xx (xx.xx%)")),
                     "5.12 (789.11%)")

    expect_identical(format_rcell(rcell(values, format = "xx.x (xx.x%)")),
                     "5.1 (789.1%)")

    expect_identical(format_rcell(rcell(values, format = "(xx, xx)")),
                     paste0("(", values[1], ", ", values[2], ")"))

    expect_identical(format_rcell(rcell(values, format = "(xx., xx.)")),
                     "(5, 8)")

    expect_identical(format_rcell(rcell(values, format = "(xx.x, xx.x)")),
                     "(5.1, 7.9)")

    expect_identical(format_rcell(rcell(values, format = "(xx.xx, xx.xx)")),
                     "(5.12, 7.89)")

    expect_identical(format_rcell(rcell(values, format = "(xx.xxx, xx.xxx)")),
                     "(5.123, 7.891)")

    expect_identical(format_rcell(rcell(values, format = "(xx.xxxx, xx.xxxx)")),
                     "(5.1235, 7.8911)")

    expect_identical(format_rcell(rcell(values, format = "xx - xx")),
                     paste(values, collapse  = " - "))

    expect_identical(format_rcell(rcell(values, format = "xx.x - xx.x")),
                     "5.1 - 7.9")

    expect_identical(format_rcell(rcell(values, format = "xx.xx - xx.xx")),
                     "5.12 - 7.89")

    expect_identical(format_rcell(rcell(values, format = "xx.x (xx.x)")),
                     "5.1 (7.9)")

    expect_identical(format_rcell(rcell(values, format = "xx.xx (xx.xx)")),
                     "5.12 (7.89)")

    expect_identical(format_rcell(rcell(values, format = "xx.x, xx.x")),
                     "5.1, 7.9")

    expect_identical(format_rcell(rcell(values, format = "xx.x to xx.x")),
                     "5.1 to 7.9")

    expect_identical(format_rcell(rcell(c(values, 10.1235), format = "xx.xx (xx.xx - xx.xx)")),
                     "5.12 (7.89 - 10.12)")
})
