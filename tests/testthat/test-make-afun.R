context("make_afun and related machinery")

value_labels <- rtables:::value_labels
test_that("afun internals coverage", {
  ## use existing funcs to ensure coverage numbers are correct
  res_tafun1 <- rtables:::test_afun(1:10, 5)
  expect_identical(
    value_labels(res_tafun1),
    c(
      "Min." = "Minimum",
      "1st Qu." = "1st Quartile",
      Median = "Median",
      Mean = "Mean",
      "3rd Qu." = "Third Quartile",
      "Max." = "Maximum",
      grp = "grp"
    )
  )

  res_tafun2 <- rtables:::test_afun_grp(1:10, 5)
  expect_identical(
    lapply(res_tafun2, obj_format),
    list(
      "Min." = "xx.x",
      "1st Qu." = "xx.xx",
      Median = NULL,
      Mean = NULL,
      "3rd Qu." = "xx.xx",
      "Max." = "xx.x",
      range = "xx - xx",
      n_unique = "xx - xx"
    )
  )


  foo <- function(x, .N_col, ...) list(a = character(0))
  afoo <- make_afun(foo)
  expect_silent(afoo(factor(character(0)), .N_col = 100))
})


test_that("make_afun works for x arg", {
  s_summary <- function(x) {
    stopifnot(is.numeric(x))

    list(
      n = sum(!is.na(x)),
      mean_sd = c(mean = mean(x), sd = sd(x)),
      min_max = range(x)
    )
  }

  a_summary <- make_afun(
    fun = s_summary,
    .formats = c(n = "xx", mean_sd = "xx.xx (xx.xx)", min_max = "xx.xx - xx.xx"),
    .labels = c(n = "n", mean_sd = "Mean (sd)", min_max = "min - max")
  )
  expect_identical(
    formals(a_summary),
    formals(s_summary)
  )

  asres1 <- a_summary(x = iris$Sepal.Length)
  expect_identical(
    c(n = "n", mean_sd = "Mean (sd)", min_max = "min - max"),
    value_labels(asres1)
  )

  a_summary2 <- make_afun(a_summary, .stats = c("n", "mean_sd"))
  expect_identical(
    formals(a_summary2),
    formals(s_summary)
  )
  asres1 <- a_summary2(x = iris$Sepal.Length)
  expect_identical(names(asres1), c("n", "mean_sd"))

  a_summary3 <- make_afun(a_summary, .formats = c(mean_sd = "(xx.xxx, xx.xxx)"))

  asres3 <- a_summary3(iris$Sepal.Length)
  expect_equal(
    lapply(asres3, obj_format),
    list(
      n = "xx",
      mean_sd = "(xx.xxx, xx.xxx)",
      min_max = "xx.xx - xx.xx"
    )
  )
})


## also used for .indent_mod test, by happenstance
s_foo <- function(df, .N_col, a = 1, b = 2) {
  list(
    nrow_df = nrow(df),
    .N_col = .N_col,
    a = a,
    b = b
  )
}
test_that("make_afun works for df functions", {
  a_foo <- make_afun(s_foo,
    b = 4,
    .formats = c(nrow_df = "xx.xx", ".N_col" = "xx.", a = "xx", b = "xx.x"),
    .labels = c(
      nrow_df = "Nrow df",
      ".N_col" = "n in cols",
      a = "a value",
      b = "b value"
    )
  )

  expect_identical(
    formals(a_foo),
    formals(s_foo)
  )
  ares1 <- a_foo(iris, .N_col = 40)
  expect_identical(
    unlist(unname(value_labels(ares1))),
    c("Nrow df", "n in cols", "a value", "b value")
  )

  expect_equal(unlist(ares1$b), 4)
  expect_equal(unlist(ares1$a), 1)

  ares1b <- a_foo(iris, .N_col = 40, b = 8)
  expect_identical(
    unlist(unname(value_labels(ares1))),
    c("Nrow df", "n in cols", "a value", "b value")
  )

  expect_equal(unlist(ares1b$b), 8)

  a_foo2 <- make_afun(a_foo, .labels = c(nrow_df = "Number of Rows"))
  ares2 <- a_foo2(iris, .N_col = 40, b = 6)
  expect_equal(unlist(ares2$b), 6)
  expect_identical(
    unlist(unname(value_labels(ares2))),
    c("Number of Rows", "n in cols", "a value", "b value")
  )
})


test_that("make_afun works for funs with ...", {
  ## with dots


  sfun3 <- function(x, .ref_group = NULL, ...) "hi"
  afun3 <- make_afun(sfun3)
  expect_identical(
    formals(sfun3),
    formals(afun3)
  )
})


test_that("ungrouping .ungroup_stats works", {
  ## ungrouping
  sfun4 <- function(x) {
    list(
      single1 = 5,
      single2 = 10,
      grouped1 = list(g1 = 11, g2 = 15, g3 = with_label(17, "sneaky label")),
      grouped2 = list(g4 = c(2, 3), g5 = c(6, 10))
    )
  }
  afun4 <- make_afun(sfun4,
    .labels = c(
      single1 = "first single val",
      single2 = "second single val"
    ),
    .formats = c(grouped2 = "xx - xx"),
    .ungroup_stats = c("grouped1", "grouped2")
  )
  expect_identical(formals(sfun4), formals(afun4))
  ares4 <- afun4(5)
  expect_identical(
    names(ares4),
    c("single1", "single2", "g1", "g2", "g3", "g4", "g5")
  )
  expect_identical(
    value_labels(ares4),
    c(
      single1 = "first single val",
      single2 = "second single val",
      g1 = "g1",
      g2 = "g2",
      g3 = "sneaky label",
      g4 = "g4",
      g5 = "g5"
    )
  )
})

get_imods <- function(obj) {
  setNames(rtables:::indent_mod(obj), names(obj))
}

test_that("make_afun .indent_mods argument works", {
  ## .indent_mod
  a_imod <- make_afun(s_foo, .indent_mods = c(
    nrow_df = -1L,
    a = 1L,
    b = 2L
  ))
  imodres1 <- a_imod(iris, 5)
  expect_identical(
    get_imods(imodres1),
    c(nrow_df = -1L, .N_col = 0L, a = 1L, b = 2L)
  )

  a_imod2 <- make_afun(a_imod, .indent_mods = c(
    nrow_df = 2L,
    .N_col = 1L
  ))
  imodres2 <- a_imod2(iris, 5)
  expect_identical(
    get_imods(imodres2), # vapply(imodres2, rtables:::indent_mod, 1L),
    c(nrow_df = 2L, .N_col = 1L, a = 1L, b = 2L)
  )

  tbl <- basic_table() %>%
    analyze("Sepal.Length", a_imod) %>%
    build_table(iris)
  rows <- tree_children(tbl)
  expect_identical(
    vapply(rows, rtables:::indent_mod, 1L),
    c(nrow_df = -1L, .N_col = 0L, a = 1L, b = 2L)
  )
})

test_that("make_afun override arg that has no default", {
  s_nodflt <- function(df, .N_col, a = 1, b) {
    list(
      nrow_df = nrow(df),
      .N_col = .N_col,
      a = a,
      b = b
    )
  }

  a_nodflt <- make_afun(s_nodflt,
    b = 4,
    .formats = c(
      nrow_df = "xx.xx",
      ".N_col" = "xx.",
      a = "xx",
      b = "xx.x"
    ),
    .labels = c(
      nrow_df = "Nrow df",
      ".N_col" = "n in cols",
      a = "a value",
      b = "b value"
    )
  )
  nodfltres <- a_nodflt(iris, 5)
  expect_equal(4, rtables:::rawvalues(nodfltres[["b"]]))
})




test_that("make_afun+build_table integration tests", {
  ## summary function that uses with_label
  s_summary <- function(x) {
    stopifnot(is.numeric(x))

    list(
      n = with_label(sum(!is.na(x)), "N subjects"),
      mean_sd = with_label(
        c(
          mean = mean(x),
          sd = sd(x)
        ),
        "My mean and SD"
      ),
      min_max = with_label(range(x), "Range")
    )
  }
  a_summary <- make_afun(
    s_summary,
    .labels = c(n = "n subjects"), # only overwrite the label of the `n` statistics here.
    .formats = c(
      n = "xx",
      mean_sd = "xx.xx (xx.xx)",
      min_max = "xx.xx - xx.xx"
    )
  )
  tbl <- basic_table() %>%
    analyze(
      "Sepal.Length",
      afun = a_summary
    ) %>%
    build_table(iris)

  expect_identical(
    row.names(tbl),
    c(
      "n subjects",
      "My mean and SD",
      "Range"
    )
  )

  tbl2 <- basic_table() %>%
    split_cols_by("Species") %>%
    analyze(
      "Sepal.Length",
      afun = a_summary
    ) %>%
    build_table(iris)

  expect_identical(
    row.names(tbl2),
    c(
      "n subjects",
      "My mean and SD",
      "Range"
    )
  )

  ## summary function that does not use with_label
  s_summary2 <- function(x) {
    stopifnot(is.numeric(x))

    list(
      n = sum(!is.na(x)),
      mean_sd = c(mean = mean(x), sd = sd(x)),
      min_max = range(x)
    )
  }

  a_summary2 <- make_afun(
    fun = s_summary2,
    .formats = c(
      n = "xx",
      mean_sd = "xx.xx (xx.xx)",
      min_max = "xx.xx - xx.xx"
    ),
    .labels = c(
      n = "n subjects",
      mean_sd = "My mean and SD",
      min_max = "Range"
    )
  )
  tbl3 <- basic_table() %>%
    analyze(
      "Sepal.Length",
      afun = a_summary2
    ) %>%
    build_table(iris)

  expect_identical(
    row.names(tbl3),
    c(
      "n subjects",
      "My mean and SD",
      "Range"
    )
  )

  tbl4 <- basic_table() %>%
    split_cols_by("Species") %>%
    analyze(
      "Sepal.Length",
      afun = a_summary2
    ) %>%
    build_table(iris)

  expect_identical(
    row.names(tbl4),
    c(
      "n subjects",
      "My mean and SD",
      "Range"
    )
  )

  ## recursive make_afun applications
  ## with with_label

  a_function3 <- make_afun(
    a_summary,
    .labels = c(min_max = "New Range")
  )
  tbl5 <- basic_table() %>%
    split_cols_by("Species") %>%
    analyze("Sepal.Length",
      afun = a_function3
    ) %>%
    build_table(iris)

  expect_identical(
    row.names(tbl5),
    c(
      "n subjects",
      "My mean and SD",
      "New Range"
    )
  )

  a_function4 <- make_afun(
    a_summary2,
    .labels = c(min_max = "New Range")
  )
  tbl5 <- basic_table() %>%
    split_cols_by("Species") %>%
    analyze("Sepal.Length",
      afun = a_function4
    ) %>%
    build_table(iris)

  expect_identical(
    row.names(tbl5),
    c(
      "n subjects",
      "My mean and SD",
      "New Range"
    )
  )

  ## .indent_mod appears in .indent_mod unit test section
  ## currently
})

## regression for bug #102

test_that("call-time ... passed down correctly by funs constructed by make_afun", {
  f <- function(x, a, ...) {
    list(a = a, ...)
  }

  af <- make_afun(f, .stats = "b")
  res1 <- af(5, a = 6, b = 7)
  expect_identical(list(b = 7), rtables:::rawvalues(res1))


  f2 <- function(df, a, ...) {
    list(a = a, ...)
  }

  af2 <- make_afun(f2, .stats = "b")
  res2 <- af2(iris, a = 5, b = 7)
  expect_identical(list(b = 7), rtables:::rawvalues(res2))
})


test_that(".format_na_strs works in make_afun", {
  s_fun <- function(x, ...) list(stuff = NA)

  afun <- make_afun(s_fun,
    .stats = "stuff",
    .formats = list(stuff = "xx.x"),
    .format_na_strs = list(stuff = "wat")
  )
  res <- afun(1:10)
  expect_identical(
    format_rcell(res[[1]]),
    "wat"
  )
})

test_that("list_wrap functions work", {
  f1 <- list_wrap_x(summary)

  expect_identical(
    f1(1:10),
    as.list(summary(1:10))
  )


  infun <- function(df) summary(df[[1]])
  f2 <- list_wrap_df(infun)
  expect_identical(
    f2(mtcars),
    f1(mtcars[[1]])
  )
})
