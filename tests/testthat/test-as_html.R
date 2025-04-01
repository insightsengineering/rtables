context("Exporting to HTML")

test_that("as_html smoke test", {
  skip_if_not_installed("xml2")
  require(xml2, quietly = TRUE)

  tmpf <- tempfile(fileext = ".html")

  tbl <- tt_to_export()
  oldo <- options(viewer = identity)
  expect_silent(fl <- Viewer(tbl))
  xml2::read_html(fl)
  expect_true(TRUE)
  options(oldo)
})

test_that("as_html Viewer with newline test", {
  skip_if_not_installed("xml2")
  tmpf <- tempfile(fileext = ".html")

  colfuns <- list(
    function(x) rcell(mean(x), format = "xx.x"),
    function(x) rcell(sd(x), format = "xx.x")
  )
  varlabs <- c("Mean Age", "SD\nLine Break!!! \nAge")

  lyt <- basic_table() %>%
    split_cols_by_multivar(c("AGE", "AGE"), varlabels = varlabs) %>%
    analyze_colvars(afun = colfuns)

  tbl_wrapping <- build_table(lyt, DM)

  tbl_normal <- rtable(
    header = c("Treatement\nN=100", "Comparison\nN=300"),
    format = "xx (xx.xx%)",
    rrow("A", c(104, .2), c(100, .4)),
    rrow("B", c(23, .4), c(43, .5)),
    rrow(),
    rrow("this is a very long section header"),
    rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
    rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
  )
  oldo <- options(viewer = identity)
  expect_silent(fl <- Viewer(tbl_wrapping))
  expect_silent(fl <- Viewer(tbl_normal))
  xml2::read_html(fl)
  expect_true(TRUE)
  options(oldo)
})

test_that("as_html does not trim whitespace", {
  tbl <- rtable(
    header = LETTERS[1:3],
    format = "xx",
    rrow("  r1", 1, 2, 3),
    rrow(" r 2  ", 4, 3, 2, indent = 1),
    rrow("r3   ", indent = 2)
  )
  html_tbl <- as_html(tbl)
  html_parts <- html_tbl$children[[1]][[3]]$children
  expect_true(all(sapply(1:4, function(x) "white-space: pre;" %in% html_parts[[x]]$attribs)))
})

test_that("as_html bolding works", {
  tbl <- rtable(
    header = LETTERS[1:3],
    format = "xx",
    rrow("  r1", 1, 2, 3),
    rrow(" r 2  ", 4, 3, 2, indent = 1),
    rrow("r3   ", indent = 2)
  )
  html_tbl <- as_html(tbl, bold = "row_names")
  html_parts <- html_tbl$children[[1]][[3]]$children
  expect_true(all(sapply(2:4, function(x) "font-weight: bold;" %in% html_parts[[x]]$children[[1]][[1]]$attribs)))
})

test_that("as_html header line works", {
  tbl <- rtable(
    header = LETTERS[1:3],
    format = "xx",
    rrow("  r1", 1, 2, 3),
    rrow(" r 2  ", 4, 3, 2, indent = 1),
    rrow("r3   ", indent = 2)
  )
  html_tbl <- as_html(tbl, header_sep_line = TRUE)
  html_parts <- html_tbl$children[[1]][[3]]$children[[1]]$children[[1]]
  expect_true(all(sapply(1:4, function(x) "border-bottom: 1px solid black;" %in% html_parts[[x]]$attribs)))
})

# https://github.com/insightsengineering/rtables/issues/872
test_that("as_html indentation is translated to rows with linebreaks", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("SEX") %>%
    analyze("AGE", afun = function(x) {
      mn <- round(mean(x), 2)
      if (!is.nan(mn) && mn > mean(DM$AGE)) {
        val <- paste(mn, "  ^  ", sep = "\n")
      } else {
        val <- paste(mn)
      }
      in_rows(my_row_label = rcell(val,
        format = "xx"
      ))
    })
  tbl <- build_table(lyt, DM)

  # Resolves correctly \n
  expect_silent(res <- as_html(tbl, expand_newlines = TRUE))
  expect_equal(
    as.character(res$children[[1]][[3]]$children[[7]]$children[[1]][[1]]),
    '<td style="text-align: left; padding-left: 3ch;"></td>'
  )
  expect_equal(
    as.character(res$children[[1]][[3]]$children[[7]]$children[[1]][[2]]),
    '<td style="text-align: center;">  ^  </td>'
  )
})

test_that("as_html expands or not newlines depending on expand_newlines", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  # Table with both col/row names with newlines
  iris_mod <- iris %>%
    mutate(Species2 = as.factor(paste0("General", "\n ", as.character(Species)))) %>%
    mutate(Species = as.factor(sample(paste0("Petal", "\n ", as.character(Species)))))

  # Also the statistic has a newline
  lyt <- basic_table() %>%
    split_cols_by("Species") %>%
    split_rows_by("Species2") %>%
    analyze("Sepal.Length", afun = function(x) {
      list(
        "mean \n (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
        "range" = diff(range(x))
      )
    })

  tbl <- build_table(lyt, iris_mod)
  thtml <- as_html(tbl)
  thtml_expanded_newlines <- as_html(tbl, expand_newlines = TRUE)

  expect_true(grepl(as.character(thtml), pattern = "General\\n setosa"))
  expect_false(grepl(as.character(thtml_expanded_newlines), pattern = "General\\n setosa")) # diff cells!
})
