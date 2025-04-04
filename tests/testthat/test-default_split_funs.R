context("Default Split Functions")

# Default split functions ------------------------------------------------------
test_that("keep_split_levels(reorder) works correctly", {
  lyt <- basic_table() %>%
    split_rows_by("COUNTRY",
      split_fun = keep_split_levels(c("JPN", "USA", "NGA"),
        reorder = FALSE
      )
    ) %>%
    summarize_row_groups() # for simplicity

  tbl <- build_table(lyt, DM)

  expect_equal(
    row.names(tbl),
    c("USA", "NGA", "JPN")
  )

  # reorder = TRUE
  lyt <- basic_table() %>%
    split_rows_by("COUNTRY",
      split_fun = keep_split_levels(c("JPN", "USA", "NGA"))
    ) %>%
    summarize_row_groups()

  tbl <- build_table(lyt, DM)

  expect_equal(
    row.names(tbl),
    c("JPN", "USA", "NGA")
  )

  # Error only during build
  expect_silent(
    lyt <- basic_table() %>%
      split_rows_by("COUNTRY",
        split_fun = keep_split_levels(c("AbsentCountry", "USA", "NGA"))
      ) %>%
      summarize_row_groups()
  )
  expect_error(build_table(lyt, DM), "AbsentCountry")
})

test_that("keep_split_level works also with empty splits", {
  # Regression #1010
  iris2 <- iris %>%
    mutate(chr_split = as.character(Species)) %>%
    mutate(fct_split = factor(rep(1, nrow(iris)), levels = c(1, 2)))

  lyt <- basic_table() %>%
    split_rows_by("fct_split") %>%
    split_rows_by("chr_split", split_fun = keep_split_levels(c("setosa"))) %>%
    analyze("Sepal.Length")

  expect_silent(tbl <- build_table(lyt, iris2))

  # Error with character
  lyt <- basic_table() %>%
    split_rows_by("chr_split", split_fun = keep_split_levels(c("a"))) %>%
    analyze("Sepal.Length")

  expect_error(
    tbl <- build_table(lyt, iris2),
    "Attempted to keep character value"
  )

  # Error with factor
  iris2$chr_split <- as.factor(iris2$chr_split)
  lyt <- basic_table() %>%
    split_rows_by("chr_split", split_fun = keep_split_levels(c("a"))) %>%
    analyze("Sepal.Length")

  expect_error(
    tbl <- build_table(lyt, iris2),
    "Attempted to keep factor level"
  )
})

test_that("reorder_split_levels(drlevels = TRUE) works", {
  lyt <- basic_table() %>%
    split_rows_by(
      "SEX",
      split_fun = reorder_split_levels(
        neworder = c("U", "F"),
        newlabels = c(U = "Uu", `F` = "Female")
      )
    ) %>%
    summarize_row_groups()
  tab <- build_table(lyt, DM)

  expect_identical(
    c("Uu", "Female"),
    row.names(tab)
  )

  # Error when not present
  lyt <- basic_table() %>%
    split_rows_by(
      "SEX",
      split_fun = reorder_split_levels(
        neworder = c("U", "F", "NotPresent"),
        newlabels = c(U = "Uu", `F` = "Female")
      )
    ) %>%
    summarize_row_groups()

  expect_error(
    tab <- build_table(lyt, DM),
    "NotPresent"
  )

  # Error when name not present
  lyt <- basic_table() %>%
    split_rows_by(
      "SEX",
      split_fun = reorder_split_levels(
        neworder = c("U", "F"),
        newlabels = c(U = "Uu", Fem = "Female")
      )
    ) %>%
    summarize_row_groups()

  expect_error(
    tab <- build_table(lyt, DM),
    "Fem"
  )

  # Error when vector of different lengths
  lyt <- basic_table() %>%
    split_rows_by(
      "SEX",
      split_fun = reorder_split_levels(
        neworder = c("U", "F"),
        newlabels = c("Uu", "Female", "NotPresent")
      )
    ) %>%
    summarize_row_groups()

  expect_error(
    tab <- build_table(lyt, DM),
    "Current neworder"
  )
})

test_that("reorder_split_levels(drlevels = FALSE) works", {
  lyt <- basic_table() %>%
    split_rows_by(
      "SEX",
      split_fun = reorder_split_levels(
        neworder = c("U", "F"),
        newlabels = c(U = "Uu", `F` = "Female"),
        drlevels = FALSE
      )
    ) %>%
    summarize_row_groups()
  tab <- build_table(lyt, DM)

  expect_identical(
    c("Uu", "Female", "M", "UNDIFFERENTIATED"),
    row.names(tab)
  )

  # Error when newlabels too many
  lyt <- basic_table() %>%
    split_rows_by(
      "SEX",
      split_fun = reorder_split_levels(
        neworder = c("F", "U"),
        newlabels = c(U = "Uu", `F` = "Female", "ThisNotGood"),
        drlevels = FALSE
      )
    ) %>%
    summarize_row_groups()

  expect_error(
    tab <- build_table(lyt, DM),
    "Add labels for current neworder"
  )

  # Error when newlabels have empty "" names (but some names)
  lyt <- basic_table() %>%
    split_rows_by(
      "SEX",
      split_fun = reorder_split_levels(
        neworder = c("F", "U"),
        newlabels = c(U = "Uu", "Female"),
        drlevels = FALSE
      )
    ) %>%
    summarize_row_groups()

  expect_error(
    tab <- build_table(lyt, DM),
    "names for levels that are not present"
  )
})

## regression test (https://github.com/insightsengineering/rtables/issues/191)
test_that("regression for reorder_split_levels(newlabels)", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by(
      "COUNTRY",
      split_fun = reorder_split_levels(
        neworder = c("CAN", "PAK", "BRA"),
        newlabels = c(CAN = "Canada", PAK = "Pakistan", BRA = "Brazil")
      )
    ) %>%
    analyze("AGE")
  tab <- build_table(lyt, ex_adsl)
  expect_identical(
    c("Canada", "Mean", "Pakistan", "Mean", "Brazil", "Mean"),
    row.names(tab)
  )
})

test_that("remove_split_levels works as expected with factor variables", {
  my_split_fun <- remove_split_levels(excl = "ASIAN")

  l <- basic_table() %>%
    split_rows_by("RACE", split_fun = my_split_fun) %>%
    summarize_row_groups()

  tab <- build_table(l, DM)

  expect_false("ASIAN" %in% row.names(tab))

  # No error if not present
  l <- basic_table() %>%
    split_rows_by("RACE", split_fun = remove_split_levels("a")) %>%
    summarize_row_groups()

  expect_silent(tab <- build_table(l, DM))
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
  skip_if_not_installed("tibble")
  require(tibble, quietly = TRUE)

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

  expect_identical(
    row.names(tbl1),
    c(
      "CHEMISTRY", "ALT", "LOW",
      "CRP", "LOW",
      "HIGH",
      "IMMUNOLOGY", "IGA", "HIGH"
    )
  )

  map2 <- tribble(
    ~ARM, ~RACE,
    "A: Drug X", "ASIAN",
    "A: Drug X", "WHITE",
    "C: Combination", "BLACK OR AFRICAN AMERICAN",
    "C: Combination", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER"
  )

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
  expect_identical(
    unclass(coldf$path), ## unclass because of the "AsIs" 'class'
    list(
      c("ARM", "A: Drug X", "RACE", "ASIAN"),
      c("ARM", "A: Drug X", "RACE", "WHITE"),
      c("ARM", "C: Combination", "RACE", "BLACK OR AFRICAN AMERICAN"),
      c("ARM", "C: Combination", "RACE", "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER")
    )
  )


  data <- data.frame(
    LBCAT = c(rep("a", 4), rep("b", 4)),
    PARAM = c(rep("param1", 4), rep("param2", 4)),
    VISIT = rep(c("V1", "V2"), 4),
    ABN = rep(c("H", "L"), 4),
    stringsAsFactors = TRUE
  )

  map <- data.frame(
    LBCAT = c(rep("a", 4), rep("b", 4)),
    PARAM = c(rep("param1", 4), rep("param2", 4)),
    VISIT = rep(c("V1", "V1", "V2", "V2"), 2),
    ABN = rep(c("H", "L"), 4),
    stringsAsFactors = FALSE
  )

  lyt4 <- basic_table() %>%
    split_rows_by("LBCAT", split_fun = trim_levels_to_map(map = map)) %>%
    split_rows_by("PARAM", split_fun = trim_levels_to_map(map = map)) %>%
    split_rows_by("VISIT", split_fun = trim_levels_to_map(map = map)) %>%
    analyze("ABN")

  tbl4 <- build_table(lyt4, df = data)
  rpths4 <- row_paths(tbl4)
  expect_identical(
    rpths4[[7]],
    c("LBCAT", "a", "PARAM", "param1", "VISIT", "V2", "ABN", "H")
  )

  expect_equal(unlist(cell_values(tbl4, rpths4[[7]]), use.names = FALSE), 0)
  expect_identical(
    rpths4[[13]],
    c("LBCAT", "b", "PARAM", "param2", "VISIT", "V1", "ABN", "L")
  )

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
  expect_identical(
    as.vector(compare_rtables(tbl1, tbl2)),
    rep(".", nrow(tbl1))
  )
})
