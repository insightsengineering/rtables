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
#' rtables:::recursive_stack_rtables(l_tbls)
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
#' rtables:::unlist_rtables(l_tbls)
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


#' insert rrows at a specific location
#'
#' @param tbl rtable
#' @param rrow rrow to append to rtable
#' @param at position into which to put the rrow, defaults to beginning
#' 
#' @return rtable
#' 
#' @export
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
    c(tbl[1:(at - 1)], list(rrow), tbl[at:nr])
  }
  
  rtablel(header, body)
  
}

#' cbind two rtables
#' 
#' @param x table 1
#' @param y table 2
#' 
#' @export
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

#' Combines two lists of rrows to a list of rrows
#' 
#' @param x first list of rows, row.names are taken from this list
#' @param y second list
#' 
#' @return list of combined rows
#' 
combine_rrows <- function(x, y) {
  Map(function(xi, yi) {
    #todo: optionally check row names are the same
    rrowl(attr(xi, "row.name"), c(xi, yi))
  }, x, y)
}
