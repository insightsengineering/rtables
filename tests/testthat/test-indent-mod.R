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
