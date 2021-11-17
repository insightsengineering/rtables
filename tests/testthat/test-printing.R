context("Printing tables")

test_that("toString method works correclty", {

    tbl <- basic_table() %>%
        split_cols_by("Species") %>%
        add_colcounts() %>%
        analyze(c("Sepal.Length", "Petal.Width"), function(x) {
            in_rows(
                mean_sd = c(mean(x), sd(x)),
                var = var(x),
                min_max = range(x),
                .formats = c("xx.xx (xx.xx)", "xx.xxx", "xx.xx - xx.xx"),
                .labels = c("Mean (sd)", "Variance", "Min - Max")
            )
        }) %>%
        build_table(iris)

    print(tbl)

    expect_identical(
        toString(tbl),
        paste(
            c("                 setosa      versicolor     virginica ",
              "                 (N=50)        (N=50)        (N=50)   ",
              "------------------------------------------------------",
              "Sepal.Length                                          ",
              "  Mean (sd)    5.01 (0.35)   5.94 (0.52)   6.59 (0.64)",
              "  Variance        0.124         0.266         0.404   ",
              "  Min - Max     4.3 - 5.8      4.9 - 7      4.9 - 7.9 ",
              "Petal.Width                                           ",
              "  Mean (sd)    0.25 (0.11)   1.33 (0.2)    2.03 (0.27)",
              "  Variance        0.011         0.039         0.075   ",
              "  Min - Max     0.1 - 0.6      1 - 1.8      1.4 - 2.5 \n"),
            collapse = "\n"
        )
    )
})

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


test_that("newline in column names and possibly cell values work", {


    df <- data.frame(
        n = 1,
        median = 10
    )

    lyt <- basic_table() %>%
        split_cols_by_multivar(vars = c("n", "median"), varlabels = c("N", "Median\n(Days)")) %>%
        analyze_colvars(afun = mean)
    tbl <- build_table(lyt, df)

    mat <- matrix_form(tbl)
    expect_identical(mat$strings,
                     matrix(c("", "", "Median",
                              "", "N", "(Days)",
                              "mean", "1", "10"),
                            nrow = 3, byrow = TRUE))
    ## Test top_left preservation
    rawdat2 <- rawdat
    rawdat2$arm_label <- ifelse(rawdat2$ARM=="ARM1", "Arm\n 1 ", "Arm\n 2 ")

    lyt2 <- basic_table() %>% split_cols_by("ARM", labels_var = "arm_label") %>%
        split_cols_by("SEX", "Gender", labels_var = "gend_label") %>%
        add_colcounts() %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", label_pos = "topleft") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      label_pos = "topleft") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx")

    tbl2 <- build_table(lyt2, rawdat2)
    matform2 <- matrix_form(tbl2)
    expect_identical(dim(matform2$strings),
                     c(18L, 5L))
    expect_identical(attr(matform2, "nlines_header"),
                     4L)
    expect_identical(matform2$strings[1:4, 1, drop = TRUE],
                     c("Ethnicity", "  Factor2", "", ""))

    ## cell has \n

    lyt3 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_rows_by("SEX") %>%
        analyze("AGE", afun = function(x) {
            mn <- round(mean(x), 2)
            if(!is.nan(mn) && mn > mean(DM$AGE))
                val <- paste(mn, "  ^  ", sep = "\n")
            else
                val <- paste(mn)
            in_rows(my_row_label = rcell(val,
                                         format = "xx"))
        })
    tbl3 <- build_table(lyt3, DM)
    matform3 <- matrix_form(tbl3)
    expect_identical(matform3$strings[,1, drop = TRUE],
                     c("",
                       "F", "my_row_label", "",
                       "M", "my_row_label", "",
                       "U", "my_row_label",
                       "UNDIFFERENTIATED", "my_row_label"))
    expect_identical(matform3$strings[,2, drop = TRUE],
                     c("A: Drug X",
                       "", "33.71", "",
                       "", "36.55", "  ^  ",
                       "", "NaN",
                       "", "NaN"))


})


test_that("alignment works", {

    lyt <- basic_table() %>%
        analyze("AGE", function(x) {
            in_rows(left = rcell("l", align = "left"),
                    right = rcell("r", align = "right"),
                    center = rcell("c", align = "center"))
        })

    aligntab <- build_table(lyt, DM)

    matform <- matrix_form(aligntab)
    expect_identical(matform$aligns,
                     cbind("left", c("center", "left", "right", "center")))

    str <- toString(aligntab)
    expect_identical(str,
                     "         all obs\n----------------\nleft     l      \nright          r\ncenter      c   \n")

    lyt2 <-  basic_table() %>%
        analyze("AGE", function(x) {
            in_rows(.list = list(left = "l", right = "r", center = "c"),
                    .aligns = c(left = "left", right = "right", center = "center"))
        })

    aligntab2 <- build_table(lyt, DM)
    expect_identical(aligntab, aligntab2)


})
