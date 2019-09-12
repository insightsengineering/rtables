context("test-col_by")

test_that("by_factor_to_matrix works", {
  x <- with_label(factor(c("a", "b", "a", "a", "b")), "exampleLabel")
  expect_equal(
    by_factor_to_matrix(x),
    structure(data.frame(
      a = c(TRUE, FALSE, TRUE, TRUE, FALSE), 
      b = c(FALSE, TRUE, FALSE, FALSE, TRUE)
    ), label = "exampleLabel")
  )
})

# todo: expand tests

test_that("by_add_total works", {
  x <- factor(c("a", "b", "a", "a", "b"))
  expect_equal(
    by_add_total(by_factor_to_matrix(x), label = "tot"),
    with_by_header(
      data.frame(
        a = c(TRUE, FALSE, TRUE, TRUE, FALSE), 
        b = c(FALSE, TRUE, FALSE, FALSE, TRUE), 
        tot = c(TRUE, TRUE, TRUE, TRUE, TRUE)
      ),
      rheader(c("a", "b", "tot"))
    )
  )
  
  x <- iris$Species
  expect_equal(
    by_add_total(by_factor_to_matrix(x)),
    by_add_total(x)
  )
  
  expect_equal(
    by_add_total(NULL, "tot", n = 3),
    data.frame(tot = c(TRUE, TRUE, TRUE))
  )
})

test_that("col_by_to_matrix works", {
  expect_error(
    col_by_to_matrix(c("a", "b", "a", "a", "b")),
    "unknown class"
  )
  x <- factor(c("a", "b", "a", "a", "b"))
  expect_equal(
    col_by_to_matrix(x),
    col_by_to_matrix(col_by_to_matrix(x))
  )
  expect_error(col_by_to_matrix(by_all("my_total")), "is.null")
  expect_equal(
    col_by_to_matrix(by_all("my_total"), x = 1:3),
    data.frame(my_total = c(TRUE, TRUE, TRUE))
  )
  expect_equal(
    col_by_to_matrix(by_all("my_total"), x = data.frame(var1 = 1:3)),
    data.frame(my_total = c(TRUE, TRUE, TRUE))
  )
})

test_that("col_by_to_factor works", {
  x <- factor(c("a", "b", "a", "a", "b"))
  expect_equal(col_by_to_factor(x), x)
  expect_equal(col_by_to_factor(col_by_to_matrix(x)), x)
  expect_equal(
    col_by_to_factor(data.frame(
      x1 = c(TRUE, TRUE, FALSE, FALSE),
      x2 = c(FALSE, FALSE, TRUE, FALSE),
      x3 = c(FALSE, FALSE, FALSE, TRUE)
    )),
    factor(c("x1", "x1", "x2", "x3"))
  )
  
  expect_error(
    col_by_to_factor(data.frame(
      x1 = c(TRUE, TRUE, TRUE, FALSE),
      x2 = c(FALSE, FALSE, TRUE, FALSE),
      x3 = c(FALSE, FALSE, FALSE, TRUE)
    )),
    "not disjoint"
  )
})
