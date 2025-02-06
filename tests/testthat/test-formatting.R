context("value formatting")

test_that("sprintf_format works correctly", {
  myfun <- sprintf_format("hi there %1.4f")

  lyt <- basic_table() %>%
    split_cols_by("Species") %>%
    analyze("Sepal.Width", afun = mean, format = myfun)

  tbl <- build_table(lyt, iris)

  matform <- matrix_form(tbl)

  expect_identical(
    matform$strings[2, ],
    c(
      "mean", "hi there 3.4280",
      myfun(2.77),
      myfun(mean(subset(iris, Species == "virginica")$Sepal.Width))
    )
  )
})



test_that("table_shell works", {
  tbl <- rtable(c("A", "B"),
    rrow("Hiya",
      rcell(c(2, .2),
        format = "xx (xx.x%)"
      ),
      rcell(c(.1, .2)),
      format = "xx.x - xx.x"
    ),
    rrow("bye", 5.2345, 17.2),
    format = "xx.xx"
  )


  tblsh <- rtable(
    c("A", "B"),
    rrow("Hiya", "xx (xx.x%)", "xx.x - xx.x"),
    rrow("bye", "xx.xx", "xx.xx")
  )

  expect_identical(
    toString(tblsh),
    paste0(capture_output(table_shell(tbl)), "\n")
  )

  tbl2 <- rtable(c("A", "B"),
    rrow("Hiya",
      rcell(c(2, .2),
        format = function(x, ...) paste0(x)
      ),
      rcell(c(.1, .2)),
      format = "xx.x - xx.x"
    ),
    rrow("bye", 5.2345, 17.2),
    format = "xx.xx"
  )

  tbl2sh <- rtable(
    c("A", "B"),
    rrow("Hiya", "<fnc>", "xx.x - xx.x"),
    rrow("bye", "xx.xx", "xx.xx")
  )

  expect_identical(
    toString(tbl2sh),
    paste0(capture_output(table_shell(tbl2)), "\n")
  )
})

test_that("rcell format_na_str functionality works", {
  expect_identical(
    format_rcell(rcell(NA_real_,
      format = "xx.xx",
      format_na_str = "hiya"
    )),
    "hiya"
  )

  ## default still works
  expect_identical(
    format_rcell(rcell(NA_real_, format = "xx.x")),
    "NA"
  )

  irs <- in_rows(
    val1 = NA_real_, val2 = NA_integer_,
    .formats = list(val1 = "xx.x", val2 = "xx.x"),
    .format_na_strs = list(val1 = "hiya", val2 = "lowdown")
  )
})

test_that("format_na_str functionality works in get_formatted_cells (i.e. printing) and make_afun", {
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
    .format_na_strs = c(mean = "Ridiculous")
  )

  l <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("COUNTRY", split_fun = drop_split_levels) %>%
    summarize_row_groups(label_fstr = "%s (n)") %>%
    analyze("AGE", afun = a_summary3, format = "xx.xx")

  tbl <- suppressWarnings(build_table(l, DM2))
  tbl
  expect_identical(
    get_formatted_cells(tbl)[3, 1, drop = TRUE],
    "Ridiculous"
  )
})

test_that("format and na_str inheritance", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  # Test data
  DM2 <- DM %>%
    filter(ARM != levels(DM$ARM)[3]) %>%
    mutate(ARM = as.factor(as.character(ARM)))
  DM2$AGE[1] <- NA # Adding one NA

  # Manually building the table
  weird_afun <- function(x, ...) {
    in_rows(
      cell_fmt = rcell(mean(x, na.rm = TRUE), format = "xx.x"),
      no_cell_fmt = rcell(median(x, na.rm = TRUE)),
      no_cell_na_str = rcell(NA, format = "xx.x"),
      cell_na_str_no_cell_fmt = rcell(NA, format_na_str = "what"),
      cell_fmt_range = rcell(range(x, na.rm = TRUE), format = "xx.x - xx.x"),
      cell_na_str_range = rcell(c(NA, 2), format = "xx.x - xx.x", format_na_str = "bah"),
      no_cell_na_str_no_cell_fmt_range = rcell(c(NA, 2), format = "xx.x - xx.x")
    )
  }

  # Main builder
  tbl <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze("AGE", weird_afun, format = "xx.xx", na_str = "lol") %>%
    build_table(DM2)

  # Get the ASCII table
  result <- get_formatted_cells(tbl) # Main function

  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  # Expected data-set is built with dplyr
  expected <- DM2 %>%
    dplyr::group_by(ARM) %>%
    summarise(
      m_age = format_value(mean(AGE, na.rm = TRUE), format = "xx.x"),
      me_age = format_value(median(AGE, na.rm = TRUE), format = "xx.xx"),
      m_range = paste(sapply(range(AGE, na.rm = TRUE), format_value, format = "xx.x"), collapse = " - ")
    ) %>%
    mutate(b = "lol", c = "what", d = "bah - 2.0", e = "lol - 2.0") %>%
    select(m_age, me_age, b, c, m_range, d, e) %>%
    mutate_all(as.character) %>%
    t() %>%
    as.matrix()
  dimnames(expected) <- NULL # Fixing attributes

  # Check if it preserves the format and na_str replacements
  expect_identical(result, expected)

  # Get the ASCII table of formats (shell)
  result <- get_formatted_cells(tbl, shell = TRUE) # Main function

  # Expected ASCII table (manual insertion)
  one_col <- c("xx.x", "xx.xx", "xx.x", "xx.xx", "xx.x - xx.x", "xx.x - xx.x", "xx.x - xx.x")
  expected <- cbind(one_col, one_col)
  dimnames(expected) <- NULL # Fixing attributes

  # Check if it preserves the shell format
  expect_identical(result, expected)
})


test_that("sas-style formatting works", {
  dumbafun <- function(x) rcell(0.845, format = "xx.xx")
  lyt <- basic_table() |>
    analyze("AGE", dumbafun)

  tbl <- build_table(lyt, DM)
  str1 <- toString(tbl)
  str2 <- toString(tbl, round_type = "sas")
  expect_true(grepl("0.84", str1, fixed = TRUE))
  expect_true(grepl("0.85", str2, fixed = TRUE))
})
