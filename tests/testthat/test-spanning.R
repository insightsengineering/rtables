context("column spanning")

test_that("column spanning works", {

  tbl <- rtable(
    header = c("Treatment\nN=100", "Comparison\nN=300", "otherstuff\nN=10"),
    format = "xx (xx.xx%)",
    rrow("A", c(104, .2), c(100, .4), rcell(15, format = "xx")),
    rrow("B", c(23, .4), c(43, .5), rcell(256, format = "xx")),
    rrow(),
    rrow("this is a very long section header"),
    rrow("estimate", rcell(55.23, "xx.xx", colspan = 2), rcell(12, format = "xx")),
    rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2), rcell(17, format = "xx")))


  rws <- collect_leaves(tbl)

  cirow <- tail(rws, 1)[[1]]

  spanned_values <- rtables:::spanned_values
  `spanned_values<-` <- rtables:::`spanned_values<-`

  expect_identical(length(spanned_values(cirow)), 3L)

  spanned_values(cirow) <- list(rcell(c(7, 15)), rcell(c(7, 15)), rcell(13, format = "xx"))

  expect_error({
    spanned_values(cirow) <- list(5, 7, 10)
  },
  "Got more than one unique")

  mylrow <- rrow("hiya!!!")

  expect_error({
    spanned_values(mylrow) <- list(5)
  }, "Label rows can't have non-null")
})
