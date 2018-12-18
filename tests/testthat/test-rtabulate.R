context("rtabulate")

test_that("rtabulate length tests", {
  
  cells <- function(x) as.vector(unlist(unclass(x))) # current implementation
  
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
