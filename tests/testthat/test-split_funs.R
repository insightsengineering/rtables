context("Split Functions")

test_that("remove_split_levels works as expected with factor variables", {
  my_split_fun <- remove_split_levels(excl = "ASIAN")

  stopifnot(is.factor(DM$RACE))
  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()

  tab <- build_table(l, DM)

  expect_false("ASIAN" %in% row.names(tab))
})

test_that("remove_split_levels works as expected with character variables", {
  my_split_fun <- remove_split_levels(excl = "ASIAN")

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()

  DM2 <- DM
  DM2$RACE <- as.character(DM2$RACE)
  tab <- build_table(l, DM2)

  expect_false("ASIAN" %in% row.names(tab))
})

test_that("drop_and_remove_levels works as expected when dropping not appearing levels", {
  my_split_fun <- drop_and_remove_levels(excl = "ASIAN")

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()

  tab <- build_table(l, DM)

  expect_setequal(
    row.names(tab),
    setdiff(unique(DM$RACE), "ASIAN")
  )
})

test_that("drop_and_remove_levels also works with character variables", {
  my_split_fun <- drop_and_remove_levels(excl = "ASIAN")

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()

  DM2 <- DM
  DM2$RACE <- as.character(DM2$RACE)
  tab <- build_table(l, DM2)

  expect_setequal(
    row.names(tab),
    setdiff(unique(DM$RACE), "ASIAN")
  )
})

test_that("trim_levels_to_map split function works", {


    map <- data.frame(
        LBCAT = c("CHEMISTRY", "CHEMISTRY", "CHEMISTRY", "IMMUNOLOGY"),
        PARAMCD = c("ALT", "CRP", "CRP", "IGA"),
        ANRIND = c("LOW", "LOW", "HIGH", "HIGH"),
        stringsAsFactors = FALSE
    )

    lyt <- basic_table() %>%
        split_rows_by("LBCAT") %>%
        split_rows_by("PARAMCD", split_fun = trim_levels_to_map(map = map)) %>%
        analyze("ANRIND")
    tbl1 <- build_table(lyt, ex_adlb)

    expect_identical(row.names(tbl1),
                     c("CHEMISTRY", "ALT", "LOW",
                                    "CRP", "LOW",
                                           "HIGH",
                       "IMMUNOLOGY", "IGA", "HIGH"))

    map2 <- tibble::tribble(
        ~ARM, ~RACE,
        "A: Drug X", "ASIAN",
        "A: Drug X", "WHITE",
        "C: Combination", "BLACK OR AFRICAN AMERICAN",
        "C: Combination", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")

    lyt2 <- basic_table() %>%
        split_cols_by("ARM") %>%
        split_cols_by("RACE", split_fun = trim_levels_to_map(map = map2)) %>%
        analyze("AGE")

    expect_error(build_table(lyt2, DM), regexp = "map does not allow")

    lyt3 <- basic_table() %>%
        split_cols_by("ARM", split_fun = trim_levels_to_map(map = map2)) %>%
        split_cols_by("RACE", split_fun = trim_levels_to_map(map = map2)) %>%
        analyze("AGE")

    tbl3 <- build_table(lyt3, DM)

    coldf <- make_col_df(tbl3)
    expect_identical(unclass(coldf$path), ## unclass because of the "AsIs" 'class'
                     list(c("ARM", "A: Drug X", "RACE", "ASIAN"),
                          c("ARM", "A: Drug X", "RACE", "WHITE"),
                          c("ARM", "C: Combination", "RACE", "BLACK OR AFRICAN AMERICAN"),
                          c("ARM", "C: Combination", "RACE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")))


    data <- data.frame(LBCAT = c(rep("a", 4), rep("b", 4)),
                       PARAM = c(rep("param1", 4), rep("param2", 4)),
                       VISIT = rep(c("V1", "V2"), 4),
                       ABN = rep(c("H", "L"), 4),
                       stringsAsFactors = TRUE)

    map <- data.frame(LBCAT = c(rep("a", 4), rep("b", 4)),
                      PARAM = c(rep("param1", 4), rep("param2", 4)),
                      VISIT = rep(c("V1", "V1", "V2", "V2"), 2),
                      ABN = rep(c("H", "L"), 4),
                      stringsAsFactors = FALSE)

    lyt4 <- basic_table() %>%
        split_rows_by("LBCAT", split_fun = trim_levels_to_map(map = map)) %>%
        split_rows_by("PARAM", split_fun = trim_levels_to_map(map = map)) %>%
        split_rows_by("VISIT", split_fun = trim_levels_to_map(map = map)) %>%
        analyze("ABN")

    tbl4 <- build_table(lyt4, df = data)
    rpths4 <- row_paths(tbl4)
    expect_identical(rpths4[[7]],
                     c("LBCAT", "a", "PARAM", "param1", "VISIT", "V2", "ABN", "H"))

    expect_equal(unlist(cell_values(tbl4, rpths4[[7]]), use.names = FALSE), 0)
    expect_identical(rpths4[[13]],
                     c("LBCAT", "b", "PARAM", "param2", "VISIT", "V1", "ABN", "L"))

    expect_equal(unlist(cell_values(tbl4, rpths4[[13]]), use.names = FALSE), 0)

    expect_equal(length(rpths4), 16)

})

test_that("trim_levels_in_group works", {

    dat1 <- data.frame(
        l1 = factor(c("A", "B", "C"), levels = c("A", "B", "C")), # note that level X is not included
        l2 = factor(c("a", "b", "c"), levels = c("a", "b", "c", "x"))
    )

    ## This works
    tbl1 <- basic_table() %>%
        split_rows_by("l1", split_fun = trim_levels_in_group("l2")) %>%
        analyze("l2") %>%
        build_table(dat1)


    dat2 <- data.frame(
        l1 = factor(c("A", "B", "C"), levels = c("A", "B", "C", "X")), # here we add X to "l1"
        l2 = factor(c("a", "b", "c"), levels = c("a", "b", "c", "x"))
    )

    ## This previously gave an error because trim_levels_in_group did not drop the empty "l1" levels
    tbl2 <- basic_table() %>%
        split_rows_by("l1", split_fun = trim_levels_in_group("l2")) %>%
        analyze("l2") %>%
        build_table(dat2)

    expect_identical(nrow(tbl1), 6L)
    expect_identical(as.vector(compare_rtables(tbl1, tbl2)),
                     rep(".", nrow(tbl1)))

})
