context("table structure validation")

test_that("table structure validation and sanitization work", {
  bad_dat <- DM
  bad_dat$RACE <- NA
  bad_lyt1 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    analyze("AGE")
  bad_tab1 <- build_table(bad_lyt1, bad_dat)

  expect_identical(validate_table_struct(bad_tab1),
    FALSE)

  expect_error(assert_valid_table(bad_tab1),
    "Likely Cause: Empty data")

  san_tab1 <- sanitize_table_struct(bad_tab1)
  expect_identical(cell_values(san_tab1),
    list("-- This Section Contains No Data --"))
  expect_identical(col_info(san_tab1),
    col_info(bad_tab1))

  bad_lyt2 <-  basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE") %>%
    split_rows_by("SEX") %>%
    summarize_row_groups()

  bad_tab2 <- build_table(bad_lyt2, DM)
  expect_error(assert_valid_table(bad_tab2),
    "Cause: Layout did not contain any analyze\\(\\) calls")

  san_tab2 <- sanitize_table_struct(bad_tab2, "my awesome empty msg")
  expect_equal(nrow(san_tab2),
    nrow(bad_tab2) + length(levels(DM$RACE)) * length(levels(DM$SEX)))

  expect_equal(unname(tail(unlist(cell_values(san_tab2)), 1)),
    "my awesome empty msg")
})
