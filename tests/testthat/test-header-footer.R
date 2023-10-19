context("headers and footers")
test_that("referential footnotes work", {
  analysisfun <- function(x, ...) {
    in_rows(
      row1 = 5,
      row2 = c(1, 2),
      .row_footnotes = list(row1 = "row 1 rfn"),
      .cell_footnotes = list(row2 = "row 2 cfn")
    )
  }

  lyt <- basic_table(
    title = "Title says Whaaaat", subtitles = "Oh, ok.",
    main_footer = "ha HA! Footer!"
  ) %>%
    split_cols_by("ARM") %>%
    analyze("AGE", afun = analysisfun)

  result <- build_table(lyt, ex_adsl)

  rdf <- make_row_df(result)
  expect_identical(rdf[2, "nrowrefs"], 0L)
  expect_identical(rdf[1, "nrowrefs"], 1L)
  expect_identical(rdf[2, "ncellrefs"], 3L)
  expect_identical(rdf[1, "ncellrefs"], 0L)

  rfn_out <- row_footnotes(result)
  expect_identical(
    list(
      row1 = list(rtables:::RefFootnote("row 1 rfn", 1L)),
      row2 = list()
    ),
    rfn_out
  )
  cfn_out <- cell_footnotes(result)
  expect_identical(
    cfn_out[2, 1][[1]],
    list(rtables:::RefFootnote("row 2 cfn", 2L))
  )

  analysisfun2 <- function(x, cutoff, ...) {
    mn <- mean(x)
    if (mn >= cutoff) {
      cf <- list(mean = "Elevated group mean")
    } else {
      cf <- list()
    }
    in_rows(
      mean = mean(x),
      range = range(x),
      .cell_footnotes = cf,
      .formats = list(mean = "xx.x", range = "xx - xx")
    )
  }
  lyt2 <- basic_table() %>%
    split_cols_by("ARM") %>%
    analyze("AGE", afun = analysisfun2, extra_args = list(cutoff = mean(ex_adsl$AGE)))
  tbl <- build_table(lyt2, ex_adsl)
  rdf2 <- make_row_df(tbl)
  expect_identical(rdf2$nrowrefs, c(0L, 0L))
  expect_identical(rdf2$ncellrefs, c(2L, 0L))
})


test_that("post-processing addition of referential footnotes works", {
  race_levels <- c(
    "WHITE",
    "BLACK OR AFRICAN AMERICAN",
    "ASIAN",
    "AMERICAN INDIAN OR ALASKA NATIVE",
    "MULTIPLE"
  )
  l1 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = keep_split_levels(race_levels)) %>%
    summarize_row_groups() %>%
    analyze("AGE", mean, format = "xx.x")


  tb1 <- build_table(l1, DM)
  fnotes_at_path(tb1,
    rowpath = c("RACE", "WHITE", "AGE", "mean"),
    colpath = c("ARM", "B: Placebo")
  ) <- "white arm b mean"

  fnotes_at_path(tb1,
    rowpath = c("RACE", "ASIAN", "@content", "ASIAN"),
    colpath = c("ARM", "C: Combination")
  ) <- "asian arm c content"

  fnotes_at_path(tb1,
    rowpath = c("RACE", "MULTIPLE", "@content", "MULTIPLE"),
    colpath = NULL
  ) <- c("race multiple row fn 1", "race multiple row fn 2")

  fnotes_at_path(tb1,
    rowpath = NULL,
    colpath = c("ARM", "A: Drug X")
  ) <- "drug x is a fake drug that isn't real"

  fnotes_at_path(tb1, rowpath = c("RACE", "WHITE")) <- "didn't specify content"
  fnotes_at_path(tb1,
    rowpath = c("RACE", "WHITE"),
    colpath = c("ARM", "C: Combination")
  ) <- "cell fnote didn't specify content"
  mform <- matrix_form(tb1)

  expect_identical(
    unname(mform$ref_footnotes),
    c(
      "{1} - drug x is a fake drug that isn't real",
      "{2} - didn't specify content",
      "{3} - cell fnote didn't specify content",
      "{4} - white arm b mean",
      "{5} - asian arm c content",
      "{6} - race multiple row fn 1",
      "{7} - race multiple row fn 2"
    )
  )

  expect_identical(
    mform$strings[10, 1, drop = TRUE],
    "MULTIPLE {6, 7}"
  )

  expect_identical(
    mform$strings[1, 2, drop = TRUE],
    "A: Drug X {1}"
  )

  expect_identical(
    mform$strings[3, 3, drop = TRUE],
    "36.9 {4}"
  )
  rdf <- make_row_df(tb1)
  expect_identical(rdf$nrowrefs[[9]], 2L)
  expect_identical(
    rdf$ncellrefs,
    c(1L, 1L, 0L, 0L, 1L, rep(0L, 5))
  )



  l2 <- basic_table() %>%
    split_cols_by("ARM") %>%
    split_rows_by("RACE", split_fun = keep_split_levels(race_levels)) %>%
    analyze("AGE", mean, format = "xx.x")

  tb2 <- build_table(l2, DM)

  fnotes_at_path(tb2, rowpath = c("RACE", "WHITE")) <- "fnote on label row"

  mform2 <- matrix_form(tb2)

  expect_identical(
    unname(mform2$ref_footnotes),
    "{1} - fnote on label row"
  )

  ## test that tf_wrap and pagination work in the presence of ref fnotes

  pag_nowrap <- paginate_table(tb1, lpp = 20, verbose = TRUE)
  pag_wrap <- paginate_table(tb1, lpp = 20, max_width = 20, tf_wrap = TRUE)

  expect_equal(nrow(pag_nowrap[[1]]), 8)
  expect_equal(nrow(pag_wrap[[1]]), 4)
})
