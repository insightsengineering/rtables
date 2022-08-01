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

    tbl2 <-  rtable(c("A", "B"),
                  rrow("Hiya", rcell(c(2, .2), format = function(x,...) paste0(x)), rcell(c(.1, .2)), format = "xx.x - xx.x"),
                  rrow("bye", 5.2345, 17.2),
                  format = "xx.xx")

    tbl2sh <- rtable(c("A", "B"),
                    rrow("Hiya", "<fnc>", "xx.x - xx.x"),
                    rrow("bye", "xx.xx", "xx.xx"))

    expect_identical(toString(tbl2sh),
                     paste0(capture_output(table_shell(tbl2)), "\n"))

})

test_that("rcell format_na_str functionality works", {

    expect_identical(format_rcell(rcell(NA_real_,
                                        format = "xx.xx",
                                        format_na_str = "hiya")),
                     "hiya")

    ## default still works
    expect_identical(format_rcell(rcell(NA_real_, format = "xx.x")),
                     "NA")

    irs <- in_rows(val1 = NA_real_, val2=NA_integer_, .formats = list(val1 = "xx.x", val2 = "xx.x"),
                   .format_na_strs = list(val1 = "hiya", val2 = "lowdown"))
})

test_that("format_na_str functionality works in get_formatted_cells (ie printing) and make_afun", {

    DM2 <- subset(DM, COUNTRY %in% c("USA", "CAN", "CHN"))
    DM2$AGE <- NA
    DM2$AGE[1] <- 1

    s_summary <- function(x) {
        stopifnot(is.numeric(x))

        list(
            n = sum(!is.na(x)),
            mean = mean(x),
            min_max = range(x)
        )
    }

    a_summary <- make_afun(
        fun = s_summary,
        .formats = c(n = "xx", mean = "xx.xx", min_max = "xx.xx - xx.xx"),
        .labels = c(n = "n", mean = "Mean", min_max = "min - max")
    )

    a_summary3 <- make_afun(a_summary,
                            .formats = c(mean = "xx.xxx"),
                            .format_na_strs = c(mean = "Ridiculous"))

    l <- basic_table() %>% split_cols_by("ARM") %>%
        split_rows_by("COUNTRY", split_fun = drop_split_levels) %>%
        summarize_row_groups(label_fstr = "%s (n)") %>%
        analyze("AGE", afun = a_summary3, format = "xx.xx")

    tbl <- build_table(l, DM2)
    tbl
    expect_identical(get_formatted_cells(tbl)[3,1, drop = TRUE],
                     "Ridiculous")
})
