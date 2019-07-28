# the functions in this document should eventually be moved to the rtables project


#' Stack Tables Stored in a nested list of depth 2
#'
#' \code{recursive_stack_rtables} expects a list with lists of rtables to be stacked. Sometimes
#' these tables have repeated information at the top and hence the first n rows
#' can be optionally removed from the tables that are not first in the lists.
#'
#' @param x list with lists of rtables
#'
#' @return rtable
#'
#' @template author_waddella
#'
#' @noRd
#'
#' @examples
#'
#' l_tbls <- list(
#'   list(
#'     rtabulate(iris$Sepal.Length, iris$Species, mean),
#'     rtabulate(iris$Sepal.Length, iris$Species, sd)
#'   ),
#'   list(
#'     rtabulate(iris$Sepal.Width, iris$Species, mean),
#'     rtabulate(iris$Sepal.Width, iris$Species, sd)
#'   ),
#'   list(
#'     rtabulate(iris$Petal.Length, iris$Species, mean),
#'     rtabulate(iris$Petal.Length, iris$Species, sd)
#'   )
#' )
#'
#' tern:::recursive_stack_rtables(l_tbls)
#'
recursive_stack_rtables <- function(x) {
  tbls <- unlist_rtables(x)
  rbindl_rtables(tbls, gap = 1)
}


#' Unlist a Nested Lists with rtables as leafes
#'
#' Often it is useful to flatten a nested lists with rtables as leafes to a list
#' of rtables. The algorithm used is a depth first tree traversal.
#'
#' @param x a nested list of with rtables as leaf object
#'
#' @return a list of rtables
#'
#' @noRd
#'
#' @examples
#'
#' l_tbls <- list(
#'   list(
#'     rtabulate(iris$Sepal.Length, iris$Species, mean),
#'     rtabulate(iris$Sepal.Length, iris$Species, sd)
#'   ),
#'   list(
#'     rtabulate(iris$Sepal.Width, iris$Species, mean)
#'   )
#' )
#'
#' tern:::unlist_rtables(l_tbls)
#'
unlist_rtables <- function(x) {
  
  n <- 0
  incr_n_if_rtable <- function(x) {
    if (is(x, "rtable")) {
      if (is_non_empty_rtable(x)) {
        n <<- n + 1
      }
    } else {
      lapply(x, incr_n_if_rtable)
    }
  }
  incr_n_if_rtable(x)
  
  i <- 1
  tbls <- vector(mode = "list", length = n)
  
  add_tbls <- function(x) {
    if (is(x, "rtable")) {
      if (is_non_empty_rtable(x)) {
        tbls[[i]] <<- x
        i <<- i + 1
      }
    } else {
      lapply(x, add_tbls)
    }
  }
  
  if (n > 0) {
    add_tbls(x)
  }
  
  tbls
  
}

# todo: document what this is doing
shift_label_table <- function(tbl, term) {
  t_grade <- rtablel(rheader(rrow("", "."), rrow("", "Grade")), c(lapply(row.names(tbl), function(xi) rrow("", xi))))
  attr(t_grade[[1]], "row.name") <- term
  cbind_rtables(t_grade, tbl)
}


row_names_as_col <- function(tbl, header_label) {
  
  nr_h <- nrow(header(tbl))
  
  if (missing(header_label)) {
    header_label <- rep("", nr_h)
  } else if (length(header_label) != nr_h) {
    stop("dimension mismatch")
  }
  
  h <- do.call(rheader, lapply(header_label, function(x) rrow("", x)))
  
  tbl_rn <- rtablel(header = h, c(lapply(row.names(tbl), function(xi) rrow("", xi))))
  cbind_rtables(tbl_rn, tbl)
}


#' insert rrows at a specific location
#'
#' @noRd
#'
#' @examples
#' tbl <- rtabulate(iris$Sepal.Length, iris$Species)
#'
#' insert_rrow(tbl, rrow("Hello World"))
insert_rrow <- function(tbl, rrow, at = 1) {
  stopifnot(
    is_rtable(tbl),
    is(rrow, "rrow")
  )
  if (is_empty_rtable(tbl)) {
    stop("tbl is empty rtable")
  }
  
  nr <- nrow(tbl)
  stopifnot(
    at >= 1 && at <= nr + 1,
    length(rrow) == 0 || length(rrow) == ncol(tbl)
  )
  
  header <- header(tbl)
  
  attributes(tbl) <- NULL
  body <- if (at == 1) {
    c(list(rrow), tbl)
  } else if (at == (nr + 1)) {
    c(tbl, list(rrow))
  } else {
    c(tbl[1:(at - 1)], rrow, tbl[(at + 1):nr])
  }
  
  rtablel(header, body)
  
}

#' cbind two rtables
#' @noRd
#'
#' @examples
#' x <- rtable(c("A", "B"), rrow("x row 1", 1,2), rrow("x row 2", 3, 4))
#'
#' y <- rtable("C", rrow("y row 1", 5), rrow("y row 2", 6))
#'
#' cbind_rtables(x, y)
cbind_rtables <- function(x, y) {
  stopifnot(
    is_rtable(x) && is_rtable(y),
    nrow(x) == nrow(y)
  )
  if (is_empty_rtable(x)) {
    return(y)
  }
  if (is_empty_rtable(y)) {
    return(x)
  }
  header_x <- header(x)
  header_y <- header(y)
  stopifnot(nrow(header_x) == nrow(header_y))
  
  header <- do.call(rheader, combine_rrows(header_x, header_y))
  body <- combine_rrows(unclass(x), unclass(y))
  rtablel(header, body)
}

combine_rrows <- function(x, y) {
  
  Map(function(xi, yi) {
    #todo: optionally check row names are the same
    rrowl(attr(xi, "row.name"), c(xi, yi))
  }, x, y)
  
}
