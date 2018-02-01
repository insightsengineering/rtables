
context("rtables")

test_that("rtable creation works", {
  t1 <- rtable(header = c("A", "B"), format = "xx", rrow("row 1", 1, 2))
  
#  expect_identical(names(t1), c("A", "B"))
  expect_equal(dim(t1), c(1, 2))
  expect_identical(row.names(t1), "row 1")
  expect_identical(as.vector(t1[1,1]), 1)
  expect_identical(as.vector(t1[1,2]), 2)

  
  
  tbl <- rtable(
    header = c("Treatement\nN=100", "Comparison\nN=300"),
    format = "xx (xx.xx%)",
    rrow("A", c(104, .2), c(100, .4)),
    rrow("B", c(23, .4), c(43, .5)),
    rrow(),
    rrow("this is a very long section header"),
    rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
    rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
  )
  
  #expect_identical(names(tbl), c("Treatement\nN=100", "Comparison\nN=300"))
  expect_equal(dim(tbl) , c(6, 2))
  expect_identical(row.names(tbl), c("A", "B", "", "this is a very long section header", "estimate", "95% CI"))
  
  expect_identical(as.vector(tbl[1,1]), c(104, .2))
  expect_identical(as.vector(tbl[1,2]), c(100, .4))
  
  expect_identical(as.vector(tbl[2,1]), c(23, .4))
  expect_identical(as.vector(tbl[2,2]), c(43, .5))
  
  expect_true(is.null(tbl[3,1]))
  expect_true(is.null(tbl[3,2]))
  
  expect_true(is.null(tbl[4,1]))
  expect_true(is.null(tbl[4,2]))
  
  expect_identical(as.vector(tbl[5,1]), 55.23)
  expect_identical(as.vector(tbl[5,2]), 55.23)
  
  expect_identical(as.vector(tbl[6,1]), c(44.8, 67.4))
  expect_identical(as.vector(tbl[6,2]), c(44.8, 67.4))
  
  t3 <- rtable(
    header = "B",
    format = "xx (xx.xx%)",
    rrow("group 1", c(1.1234, .20222)),
    rrow("group 2", c(4.3214, .432132))
  )
  t3
  
})


test_that("multi-header tables work", {
  
  t1 <- rtable(
    header = rheader(
      rrow(NULL, rcell("A", colspan = 2), rcell("B", colspan = 2)),
      rrow(NULL, "x", "y", "x", "y")
    ),
    rrowl(row.name = "row 1", 1:4),
    rrowl(row.name = "row 2", 4:1)
  )
  
  expect_equal(nrow(t1), 2)
  expect_equal(ncol(t1), 4)
  
})