context("Printing tables with proper round_type")

prep_exp_str <- function(colheader, txtvals, txt, totl = 28, len = 6) {
  colheader <- substr(paste(strrep(" ", len), paste(sprintf("%-7s", colheader), collapse = " ")), 1, totl)
  txtvals <- paste(substr(paste(txt, paste(sprintf("%-7s", txtvals), collapse = " ")), 1, totl - 1), "\n")

  expstr_lns <- c(
    colheader,
    strrep("—", totl),
    txtvals
  )

  exp_str <- paste(expstr_lns, collapse = "\n")
  exp_str
}

txtvals_iec <- vals_round_type_fmt(vals = vals_round_type, round_type = "iec")
txtvals_sas <- vals_round_type_fmt(vals = vals_round_type, round_type = "sas")

# confirm that at least one value is different
expect_true(any(txtvals_iec != txtvals_sas))

test_that("round_type can be set on basic_table", {
  skip_if_not_installed("dplyr")

  tbl_sas <- tt_to_test_round_type(round_type = "sas")

  expect_identical(
    obj_round_type(tbl_sas),
    "sas"
  )

  # rounding method can be changed without the need to rebuild the table
  tbl_iec <- tbl_sas
  obj_round_type(tbl_iec) <- "iec"

  expect_identical(
    obj_round_type(tbl_iec),
    "iec"
  )

  # disallowed rounding method will give error
  tbl_fake <- tbl_sas
  expect_error(obj_round_type(tbl_fake) <- "fake")

  # actual formatted values are as required
  colheader <- c("ARM A", "ARM B", "ARM C")
  names(txtvals_iec) <- colheader
  names(txtvals_sas) <- colheader

  exp_str_iec <- prep_exp_str(colheader, txtvals_iec, "Mean  ")
  exp_str_sas <- prep_exp_str(colheader, txtvals_sas, "Mean  ")

  expect_identical(
    toString(tbl_iec),
    exp_str_iec
  )

  expect_identical(
    toString(tbl_sas),
    exp_str_sas
  )
})


test_that("toString method works correctly with user defined round_type", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  tbl_sas <- tt_to_test_round_type(round_type = "sas")
  tbl_iec <- tt_to_test_round_type(round_type_tbl = "iec")

  # round type can be modified without re-building table
  tbl_iec2 <- tbl_sas
  obj_round_type(tbl_iec2) <- "iec"


  colheader <- c("ARM A", "ARM B", "ARM C")
  names(txtvals_iec) <- colheader
  names(txtvals_sas) <- colheader

  exp_str_iec <- prep_exp_str(colheader, txtvals_iec, "Mean  ")
  exp_str_sas <- prep_exp_str(colheader, txtvals_sas, "Mean  ")

  expect_identical(
    toString(tbl_iec),
    exp_str_iec
  )

  expect_identical(
    toString(tbl_sas),
    exp_str_sas
  )

  expect_identical(
    toString(tbl_iec, round_type = "sas"),
    exp_str_sas
  )

  expect_identical(
    obj_round_type(tbl_iec),
    "iec"
  )

  expect_identical(
    obj_round_type(tbl_sas),
    "sas"
  )


  # round_type is maintained when subtable from ElementaryTable (select 2 columns)
  sub_tbl_sas <- tbl_sas[, c(1, 2)]
  expect_identical(
    obj_round_type(sub_tbl_sas),
    "sas"
  )

  # testing as_result_df
  df_iec <- as_result_df(tbl_iec, data_format = "strings")
  df_sas <- as_result_df(tbl_sas, data_format = "strings")
  df_iec2 <- as_result_df(tbl_sas, data_format = "strings", round_type = "iec")

  df_x <- df_iec[, 1:6]
  tdf_iec <- cbind(df_x, as.data.frame(t(txtvals_iec)))
  tdf_sas <- cbind(df_x, as.data.frame(t(txtvals_sas)))

  expect_identical(
    df_iec,
    tdf_iec
  )

  expect_identical(
    df_sas,
    tdf_sas
  )

  expect_identical(
    df_iec,
    df_iec2
  )


  # testing as_html
  skip_if_not_installed("xml2")
  require(xml2, quietly = TRUE)

  html_tbl_iec <- as_html(tbl_iec)
  html_tbl_sas <- as_html(tbl_sas)
  html_tbl_iec2 <- as_html(tbl_sas, round_type = "iec")

  expect_identical(
    html_tbl_iec,
    html_tbl_iec2
  )

  html_parts_iec <- html_tbl_iec$children[[1]][[3]]$children[[2]]$children[[1]]
  html_parts_sas <- html_tbl_sas$children[[1]][[3]]$children[[2]]$children[[1]]

  get_value_from_html <- function(html) {
    doc <- xml2::read_html(as.character(html))
    val <- xml2::xml_text(doc)
  }

  from_html_iec <- sapply(html_parts_iec, get_value_from_html)[2:4]
  from_html_sas <- sapply(html_parts_sas, get_value_from_html)[2:4]

  expect_identical(
    from_html_iec,
    unname(txtvals_iec)
  )

  expect_identical(
    from_html_sas,
    unname(txtvals_sas)
  )
})

test_that("round_type still available on subtable", {
  tbl_iec <- tt_to_test_round_type2(round_type = "iec")
  tbl_sas <- tt_to_test_round_type2(round_type = "sas")

  sub_tbl <- tbl_sas[c("SEX", "F"), ]
  expect_identical(
    obj_round_type(sub_tbl),
    "sas"
  )

  sub_tbl_iec <- tbl_iec[c("SEX", "F"), ]
  expect_identical(
    obj_round_type(sub_tbl_iec),
    "iec"
  )
})


test_that("test for get_formatted_cells", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  tbl_sas <- tt_to_test_round_type(round_type = "sas")

  form_cells <- get_formatted_cells(tbl_sas)

  form_cells_iec <- get_formatted_cells(tbl_sas, round_type = "iec")

  expect_identical(
    form_cells[1, ],
    txtvals_sas
  )

  expect_identical(
    form_cells_iec[1, ],
    txtvals_iec
  )
})

test_that("test for matrix_form", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  tbl_sas <- tt_to_test_round_type(round_type = "sas")

  # when round_type is not specified, the round_type attribute from the table will be used
  mpf <- matrix_form(tbl_sas)

  expect_identical(
    mpf$round_type,
    "sas"
  )

  expect_identical(
    mpf$strings[2, 2:4],
    txtvals_sas
  )

  # when round_type is specified, this round_type will be used
  mpf_iec <- matrix_form(tbl_sas, round_type = "iec")
  expect_identical(
    mpf_iec$round_type,
    "iec"
  )

  expect_identical(
    mpf_iec$strings[2, 2:4],
    txtvals_iec
  )
})

test_that("test for round_type and tt_at_path", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  tbl_sas <- tt_to_test_round_type2(round_type = "sas")

  expect_identical(
    obj_round_type(tbl_sas),
    "sas"
  )

  sub_tbl <- tt_at_path(tbl_sas, path = c("SEX", "F"))
  expect_identical(
    obj_round_type(sub_tbl),
    "sas"
  )
})

test_that("test for obj_round_type setter", {
  skip_if_not_installed("dplyr")
  require(dplyr, quietly = TRUE)

  tbl <- tt_to_export()

  kids <- tree_children(tbl)
  round_type_kids <- vapply(kids, obj_round_type, "")
  expect_identical(
    unname(round_type_kids),
    rep("iec", 3)
  )

  gkids <- tree_children(kids[[1]])
  round_type_gkids <- vapply(gkids, obj_round_type, "")
  expect_identical(
    unname(round_type_gkids),
    rep("iec", 1)
  )

  # now modify the round_type using obj_round_type setter on table
  # all children/grand_children will be updated
  tbl_sas <- tbl
  obj_round_type(tbl_sas) <- "sas"
  expect_identical(
    obj_round_type(tbl_sas),
    "sas"
  )

  kids <- tree_children(tbl_sas)
  round_type_kids <- vapply(kids, obj_round_type, "")
  expect_identical(
    unname(round_type_kids),
    rep("sas", 3)
  )

  gkids <- tree_children(kids[[1]])
  round_type_gkids <- vapply(gkids, obj_round_type, "")
  expect_identical(
    unname(round_type_gkids),
    rep("sas", 1)
  )
})

test_that("test round_type in rtable", {
  t1 <- rtable(header = c("A", "B"), format = "xx.xx", rrow("row 1", vals_round_type[1], vals_round_type[2]))
  expect_identical(obj_round_type(t1), "iec")

  t2 <- t1
  obj_round_type(t2) <- "sas"
  expect_identical(obj_round_type(t2), "sas")

  t3 <- rtable(
    header = c("A", "B"),
    format = "xx.xx",
    rrow("row 1", vals_round_type[1], vals_round_type[2]),
    round_type = "sas"
  )
  expect_identical(t2, t3)
})

test_that("test round_type in rrow and rrowl", {
  rrw1 <- rrow("row 1", vals_round_type[1], vals_round_type[2], format = "xx.xx")
  expect_identical(obj_round_type(rrw1), "iec")

  rrw2 <- rrw1
  obj_round_type(rrw2) <- "sas"
  expect_identical(obj_round_type(rrw2), "sas")

  rrw3 <- rrow("row 1", vals_round_type[1], vals_round_type[2], format = "xx.xx", round_type = "sas")
  expect_identical(rrw2, rrw3)

  rrwl1 <- rrowl("row 1", vals_round_type[1], vals_round_type[2], format = "xx.xx")
  expect_identical(obj_round_type(rrwl1), "iec")

  rrwl2 <- rrwl1
  obj_round_type(rrwl2) <- "sas"
  expect_identical(obj_round_type(rrwl2), "sas")

  rrwl3 <- rrowl("row 1", vals_round_type[1], vals_round_type[2], format = "xx.xx", round_type = "sas")
  expect_identical(rrwl2, rrwl3)
})
