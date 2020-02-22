context("rtabulate")

test_that("rtabulate length tests", {
  
    ##cells <- function(x) as.vector(unlist(unclass(x))) # current implementation
    cells <- function(x) as.vector(unlist(lapply(rtables:::collect_leaves(x, add.labrows=TRUE), rtables:::row_values)))
  
  cb <- letters[1:2]
  cbf <- factor(cb, levels = cb)
  
  rt_test <- function(x) {
    tbl <- rtabulate(x, cbf, length)
    expect_true(all(cells(tbl) == 1), "subset fail")
    
    tbl <- rtabulate(x[1], cbf[1], length)
    expect_identical(cells(tbl), c(1L, 0L), "0 count")
    
    tbl <- rtabulate(numeric(0), cbf[c(FALSE, FALSE)], length)
    expect_identical(cells(tbl), c(0L, 0L), "0 count 2")
  }
  
  
  rt_test(seq_along(cb)) # numeric
  rt_test(rep(TRUE, length(cb))) # logical
  
  # factor
  
  tbl <- rtabulate(cbf, cbf)
  expect_identical(cells(tbl), c(1L, 0L, 0L, 1L), "factor")
  
  tbl <- rtabulate(factor(c("X", "X"), c("X", "Y")), cbf, length)
  expect_identical(cells(tbl), c(1L, 1L, 0L, 0L), "factor")
  
  tbl <- rtabulate(factor(c("X", "Y"), c("X", "Y")), factor(c("a", "a"), c("a", "b")), length)
  expect_identical(cells(tbl), c(1L, 0L, 1L, 0L), "factor")
  
  tbl <- rtabulate(factor(c("Y", "Y"), c("X", "Y")), factor(c("b", "b"), c("a", "b")), length)
  expect_identical(cells(tbl), c(0L, 0L, 0L, 2L), "factor")
  
  
})


test_that("rtabulate: col_wise_args argument", {
  
  get_elmts <- function(x, i) {
    stopifnot(is(x, "VTableTree") && nrow(x) == 1)
    as.vector(sapply(row_values(collect_leaves(x)[[1]]), '[[', i))
  }
  
  check_row1 <- function(x) {
    expect_identical(get_elmts(x, 2), c(3, 2, 1))
    expect_identical(get_elmts(x, 3), LETTERS[1:3])
  }
  
  tbl1 <- rtabulate(1:3, factor(letters[1:3]), function(xi, a, b) {
    list(xi, a, b)
  }, col_wise_args = list(a = c(3, 2, 1), b = LETTERS[1:3]), row.name = "-")
  
  check_row1(tbl1)

  
  tbl2 <- rtabulate((1:3)>2, factor(letters[1:3]), function(xi, a, b) {
    list(xi, a, b)
  }, col_wise_args = list(a = c(3, 2, 1), b = LETTERS[1:3]), row.name = "-")
  
  check_row1(tbl2)
  
  check_all_rows <- function(x) {
    for (i in 1:nrow(x)) {
      check_row1(x[i, ])
    }
  }
  
  tbl3 <- rtabulate(factor(letters[1:3]), factor(letters[1:3]), function(xi, a, b) {
    list(if (length(xi) == 0) "-" else xi, a, b)
  }, col_wise_args = list(a = c(3, 2, 1), b = LETTERS[1:3]))
  
  check_all_rows(tbl3)
  
  df <- data.frame(
    v1 = 1:3, v2 = 3:5
  )
  tbl4 <- rtabulate(df, factor(letters[1:3]), factor(LETTERS[1:3]),  function(xi, a, b) {
    list(nrow(xi), a, b)
  }, col_wise_args = list(a = c(3, 2, 1), b = LETTERS[1:3]))
  
  check_all_rows(tbl4)
  
})
