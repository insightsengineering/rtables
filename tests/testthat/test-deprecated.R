context("deprecated functionality")


test_that("deprecated things are still there and work kinda", {
  expect_warning(lyt11 <- split_cols_by(lyt = NULL, "ARM"), "deprecated")
  expect_identical(lyt11, basic_table() %>% split_cols_by("ARM"))
  expect_warning(lyt22 <- split_rows_by(lyt = NULL, "ARM"), "deprecated")
  expect_identical(lyt22, basic_table() %>% split_rows_by("ARM"))
})

test_that("deprecated insert_rrow still currently works", {
  tbl <- basic_table() %>%
    split_cols_by("Species") %>%
    analyze("Sepal.Length") %>%
    build_table(iris)

  expect_warning(res1 <- insert_rrow(tbl, rrow("Hello World")), "Deprecated")
  realdf <- make_row_df(tbl)
  o <- options(warn = -1)
  mf1 <- matrix_form(res1)
  expect_identical(mf1$strings[2, , drop = TRUE], c("Hello World", "", "", ""))

  res2 <- insert_rrow(tbl, rrow("Hello World"), at = 2)
  mf2 <- matrix_form(res2)
  expect_identical(mf2$strings[3, , drop = TRUE], c("Hello World", "", "", ""))

  tbl2 <- basic_table() %>%
    split_cols_by("Species") %>%
    split_rows_by("Species") %>%
    analyze("Sepal.Length") %>%
    build_table(iris)
  ## for coverage
  res3 <- insert_rrow(tbl2, rrow("Hello World"))
  mf3 <- matrix_form(res3)
  expect_identical(mf3$strings[2, , drop = TRUE], c("Hello World", "", "", ""))
  res4 <- insert_rrow(tbl2, rrow("Hello World"), at = 2)
  mf4 <- matrix_form(res4)
  expect_identical(mf4$strings[3, , drop = TRUE], c("Hello World", "", "", ""))

  res5 <- insert_rrow(tbl2, rrow("Hello World"), at = 4)
  mf5 <- matrix_form(res5)
  expect_identical(mf5$strings[5, , drop = TRUE], c("Hello World", "", "", ""))
  res6 <- insert_rrow(tbl2, rrow("new row", 5, 6, 7))
  mf6 <- matrix_form(res6)
  expect_identical(mf6$strings[2, , drop = TRUE], c("new row", "5", "6", "7"))
  res7 <- insert_rrow(tbl2, rrow("new row", 5, 6, 7), at = 3)
  mf7 <- matrix_form(res7)
  expect_identical(mf7$strings[4, , drop = TRUE], c("new row", "5", "6", "7"))

  options(o)
})

test_that("add_colcounts works", {
  tbl1 <- basic_table() %>%
    add_colcounts() %>%
    analyze("AGE") %>%
    build_table(DM)

  tbl2 <- basic_table(show_colcounts = TRUE) %>%
    analyze("AGE") %>%
    build_table(DM)

  expect_true(identical(tbl1, tbl2))
})

test_that("add_colcounts format argument works", {
  # 2d count (%) format works
  tbl1 <- basic_table() %>%
    add_colcounts(format = "xx (xx%)") %>%
    split_cols_by("ARM") %>%
    build_table(DM)
  mf_tbl1_colcounts <- matrix_form(tbl1)$strings[2, ]
  expect_identical(mf_tbl1_colcounts, c("", "121 (100%)", "106 (100%)", "129 (100%)"))

  # correct error message for 2d format without %
  lyt <- basic_table() %>%
    add_colcounts(format = "xx (xx)") %>%
    split_cols_by("ARM")
  expect_error(
    matrix_form(build_table(lyt, DM)),
    paste(
      "This 2d format is not supported for column counts.",
      "Please choose a 1d format or a 2d format that includes a % value."
    )
  )

  # correct error message for 3d colcount format
  lyt <- basic_table() %>%
    add_colcounts(format = "xx.x (xx.x - xx.x)") %>%
    split_cols_by("ARM")
  expect_error(matrix_form(build_table(lyt, DM)), "3d formats are not supported for column counts.")
})
