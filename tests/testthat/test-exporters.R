context("Exporting to txt, pdf, rtf, and docx")

test_that("export_as_txt works with and without pagination", {
  lyt <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze(c("AGE", "BMRKR2", "COUNTRY"))

  tbl <- build_table(lyt, ex_adsl)

  tmptxtf <- tempfile()
  export_as_txt(tbl, file = tmptxtf, paginate = TRUE, lpp = 8, verbose = TRUE)
  txtlns <- readLines(tmptxtf)
  expect_identical(
    grep("\\\\s\\\\n", txtlns),
    c(9L, 17L)
  )

  expect_identical(
    toString(tbl),
    export_as_txt(tbl, file = NULL, paginate = FALSE)
  )
})

test_that("export_as_txt works with wrapping", {
  clw <- c(5, 7, 6, 6) + 12
  lpp_tmp <- 18

  ## no vert pagination because lpp is so big
  tmptxtf1 <- tempfile()
  export_as_txt(tt_for_wrap,
    file = tmptxtf1,
    paginate = TRUE,
    lpp = 150,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 20, cpp = 80, verbose = TRUE
  )
  txtlns1 <- readLines(tmptxtf1)
  pagepos1 <- grep("\\\\s\\\\n", txtlns1)
  expect_identical(pagepos1, 30L) ## c(30L, 58L))

  ## explicitly no vertical pagination (lpp = NULL)
  tmptxtf1b <- tempfile()
  export_as_txt(tt_for_wrap,
    file = tmptxtf1b,
    paginate = TRUE,
    lpp = NULL,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 20, cpp = 80
  )

  txtlns1b <- readLines(tmptxtf1b)
  expect_identical(txtlns1, txtlns1b)




  ## no horiz pagination, tf_wrap FALSE

  tmptxtf2 <- tempfile()
  expect_warning(export_as_txt(tt_for_wrap,
    file = tmptxtf2,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = FALSE,
    max_width = 20, verbose = TRUE
  ))
  txtlns2 <- readLines(tmptxtf2)
  pagepos2 <- grep("\\\\s\\\\n", txtlns2)
  expect_identical(pagepos2, 18L) ## c(26L, 50L))

  tmptxtf2b <- tempfile()
  expect_error(export_as_txt(tt_for_wrap,
    file = tmptxtf2b,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 20, verbose = TRUE
  ))
  export_as_txt(tt_for_wrap,
    file = tmptxtf2b,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 40, verbose = TRUE
  )
  txtlns2b <- readLines(tmptxtf2b)
  pagepos2b <- grep("\\\\s\\\\n", txtlns2b)
  expect_identical(pagepos2b, c(16L, 33L, 49L)) ## 16 because we dont' get our first pick of pagination spots anymore

  ## both vertical and horizontal pagination #458
  tmptxtf3 <- tempfile()
  ## this fails, no valid pagination after both heade rand footer
  ## are wrapped to 20
  expect_error(export_as_txt(tt_for_wrap,
    file = tmptxtf3,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 20,
    cpp = 80
  ))
  export_as_txt(tt_for_wrap,
    file = tmptxtf3,
    paginate = TRUE,
    lpp = lpp_tmp,
    colwidths = clw,
    tf_wrap = TRUE,
    max_width = 40,
    cpp = 80, verbose = TRUE
  )

  txtlns3 <- readLines(tmptxtf3)
  pagepos3 <- grep("\\\\s\\\\n", txtlns3)
  expect_identical(pagepos3[1], pagepos2b[1])
})


test_that("export_as_txt works with sas rounding", {
  ## this also tests pagination and all supported formats with 2 decimals of precision
  vals <- list(
    0.845,
    c(1.845, 0.845),
    c(10.845, 8.845, 12.845)
  )
  forms <- lapply(
    list_valid_format_labels(),
    function(x) grep("xx[.]xx([^x%]|$)", x, value = TRUE)
  )
  vals <- rep(vals, times = lengths(forms))
  names(vals) <- paste0("row", seq_along(vals))
  forms <- setNames(unlist(forms), nm = names(vals))
  superdumbafun <- function(x) {
    in_rows(.list = vals, .formats = forms)
  }
  superdumbfun2 <- function(x) {
    txtvals <- mapply(format_value, x = vals, format = forms, round_type = "sas")
    in_rows(.list = txtvals)
  }
  lyt <- basic_table() |>
    analyze("AGE", afun = superdumbafun)
  tbl <- build_table(lyt, DM)
  txt <- export_as_txt(tbl, round_type = "sas")
  lyt2 <- basic_table() |>
    analyze("AGE", afun = superdumbfun2)
  tbl2 <- build_table(lyt2, DM)
  expect_identical(
    export_as_txt(tbl, round_type = "sas"),
    export_as_txt(tbl2)
  )
  expect_identical(
    toString(tbl, round_type = "sas"),
    export_as_txt(tbl, round_type = "sas")
  )
})

test_that("tsv roundtripping for path_enriched_df", {
  tbl2 <- tt_to_export()

  df <- path_enriched_df(tbl2)

  tmptsv <- tempfile(fileext = ".tsv")

  export_as_tsv(tbl2, file = tmptsv)

  newdf <- import_from_tsv(tmptsv)

  expect_true(all(sapply(newdf, is.list)))
  expect_equal(
    unclass(newdf[1, 2]), # AsIs "class"
    list(as.character(c(
      16,
      16 / sum(ex_adsl$ARM == "A: Drug X" & ex_adsl$SEX == "M")
    )))
  )
})

test_that("export_as_pdf works", {
  tbl <- tt_to_export()
  tmpf <- tempfile(fileext = ".pdf")

  expect_warning(
    export_as_pdf(tbl, file = tmpf, landscape = TRUE, height = 1000, width = 3, paginate = FALSE),
    "width of page 1 exceeds the available space"
  )
  expect_true(file.exists(tmpf))
  file.remove(tmpf)
  expect_warning(
    export_as_pdf(tbl, file = tmpf, height = 3, width = 1000, paginate = FALSE),
    "height of page 1 exceeds the available space"
  )

  res <- export_as_pdf(tbl, file = tmpf)
  expect_equal(res$npages, 3)

  ## non-monospace fonts work
  ## this tests the actual pagination behavior...
  fspec <- font_spec("Times", 20, 1.2)
  file.remove(tmpf)
  expect_error(export_as_pdf(tbl, file = tmpf, fontspec = fspec), "non-monospace")
  file.remove(tmpf) ## blank file created (currently, this could be better)
  res <- export_as_pdf(tbl, file = tmpf, fontspec = fspec, ttype_ok = TRUE)
})


# test_that("exporting pdfs gives the correct values", {
#     if (check_pdf) {
#         lyt <- basic_table(title = " ") %>%
#             split_rows_by("SEX", page_by = TRUE) %>%
#             analyze("AGE")
#
#         # Building the table
#         tbl <- build_table(lyt, DM)
#
#         tmpf <- tempfile(fileext = ".pdf")
#         res <- export_as_pdf(tbl, file = tmpf, hsep = "=", lpp = 20)
#         res_pdf <- pdf_text(tmpf)
#
#         # Removing spaces and replacing separators
#         res_pdf <- gsub(res_pdf, pattern = "==*", replacement = "+++")
#         res_pdf <- gsub(res_pdf, pattern = "  +", replacement = " ")
#         res_pdf <- gsub(res_pdf, pattern = " \n", replacement = "")
#
#         # Pagination is present as vector in pdf_text. Doing the same with tbl
#         expected <- sapply(paginate_table(tbl), function(x) toString(x, hsep = "="), USE.NAMES = FALSE)
#         names(expected) <- NULL
#
#         # Removing spaces and replacing separators
#         expected <- gsub(expected, pattern = "==*", replacement = "+++")
#         expected <- gsub(expected, pattern = "  +", replacement = " ")
#         expected <- gsub(expected, pattern = " \n", replacement = "\n")
#         expected <- gsub(expected, pattern = "^\n", replacement = "")
#         expect_identical(res_pdf, expected)
#         ## TODO understand better how to compare exactly these outputs
#     }
# })

test_that("exporting pdf does the inset", {
  tbl <- tt_to_export()
  table_inset(tbl) <- 100
  tmpf <- tempfile(fileext = ".pdf")

  expect_error(export_as_pdf(tbl, file = tmpf), "Width of row labels equal to or larger than")
})

## https://github.com/insightsengineering/rtables/issues/308
test_that("path_enriched_df works for tables with a column that has all length 1 elements", {
  my_table <- basic_table() %>%
    split_rows_by("Species") %>%
    analyze("Petal.Length") %>%
    build_table(df = iris)
  mydf <- path_enriched_df(my_table)
  expect_identical(dim(mydf), c(3L, 2L))
})

test_that("export_as_rtf works", {
  testthat::skip_if_not_installed("r2rtf")
  tbl <- tt_to_export()
  tmpf <- tempfile(fileext = ".rtf")

  expect_error(
    export_as_rtf(tbl, file = tmpf, landscape = TRUE, margins = c(2, 2, 2, 2), colwidths = 2),
    "non-null colwidths argument"
  )

  res <- export_as_rtf(tbl, file = tmpf)
  expect_true(file.exists(tmpf))
})
