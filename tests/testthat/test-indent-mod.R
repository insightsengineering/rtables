context("Indent modifiers")

test_that("indent modifiers propogated from analyze calls properly", {
    lyt  <- basic_table() %>%
        analyze("Sepal.Width", afun = mean, show_labels = "visible") %>%
        analyze("Sepal.Width", afun = median, show_labels = "hidden", indent_mod = 2L,
                table_names = "SecondAge")
    tab <-  build_table(lyt,iris)
    expect_equal(rtables:::indent_mod(tree_children(tab)[[2]]), 2L)

    expect_equal(summarize_rows(tab)$indent, c(0, 1, 2))


})


test_that("indents are correct in make_pagdf", {
    l1 <- basic_table() %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                            split_fun = remove_split_levels("C"),
                            labels_var = "fac2_label") %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx")

    t1 <- build_table(l1, rawdat)
    pgdf1 <- make_pagdf(t1)
    expect_identical(rep(c(0, 1, 2, 2, 1, 2, 2), 2),
                     pgdf1$indent)

    l2 <- l1 %>% analyze("AGE", var_labels = "Age Analysis Redux", table_names = "AgeRedux",
                         afun = range, format = "xx - xx")

    t2 <- build_table(l2, rawdat)
    pgdf2 <- make_pagdf(t2)
    expect_identical(rep(c(0, 1, 2, 3, 3, 2, 3, 1, 2, 3, 3, 2, 3), 2),
                     pgdf2$indent)


     l3 <- basic_table() %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      indent_mod = 0) %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx", indent_mod = -1)

    t3 <- build_table(l3, rawdat)
    pgdf3 <- make_pagdf(t3)
    expect_identical(rep(c(0, 1, 1, 1, 1, 1, 1), 2),
                     pgdf3$indent)

    l4 <-  basic_table() %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label") %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      indent_mod = -1) %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx", indent_mod = 1)
    t4 <- build_table(l4, rawdat)
    pagdf4 <- make_pagdf(t4)
    expect_identical(rep(c(0, 0, 2, 2, 0, 2, 2), 2),
                     pagdf4$indent)

    l5 <-  basic_table() %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", indent_mod = 2) %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      indent_mod = -2) %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx", indent_mod = 1)
    t5 <- build_table(l5, rawdat)
    pgdf5 <- make_pagdf(t5)
    expect_identical(rep(c(2, 1, 3, 3, 1, 3, 3), 2),
                     pgdf5$indent)

    l6 <-  basic_table() %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", indent_mod = 0,
                      visible_label = TRUE) %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      indent_mod = 0) %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx", indent_mod = 0)
    t6 <- build_table(l6, rawdat)
    pgdf6 <- make_pagdf(t6)
    expect_identical(c(0, rep(c( 1, 2, 3, 3, 2, 3, 3), 2)),
                     pgdf6$indent)




    ## children inherit table idnent mod, NOT content indent_mod, when thsoe two
    ## values are different.

    l7 <-  basic_table() %>%
        split_rows_by("RACE", "Ethnicity", labels_var = "ethn_label", indent_mod = 2,
                      visible_label = TRUE) %>%
        summarize_row_groups("RACE", label_fstr = "%s (n)", indent_mod = -1) %>%
        split_rows_by("FACTOR2", "Factor2",
                      split_fun = remove_split_levels("C"),
                      labels_var = "fac2_label",
                      indent_mod = 0) %>%
        analyze("AGE", "Age Analysis", afun = function(x) list(mean = mean(x),
                                                               median = median(x)),
                format = "xx.xx", indent_mod = 0)
    t7 <- build_table(l7, rawdat)
    pgdf7 <- make_pagdf(t7)
    expect_identical(c(2, rep(c( 1, 3, 4, 4, 3, 4, 4), 2)),
                     pgdf7$indent)
})

