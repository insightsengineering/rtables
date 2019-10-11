#' Stack rtable and rrow objects 
#' 
#' Note that the columns order are not mached by the header: the first table
#' header is taken as the reference.
#' 
#' @param ... \code{\link{rtable}} or length 0 \code{\link{rcell}} objects, the first object must be an
#'   \code{\link{rtable}} object
#' @param gap number of empty rows to add between tables
#'   
#' @return an \code{\link{rtable}} object
#' 
#' @export
#' 
#' @examples 
#' 
#' mtbl <- rtable(
#'   header = rheader(
#'     rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan=2)),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "All Species",
#'     mean(iris$Sepal.Length), median(iris$Sepal.Length),
#'     mean(iris$Petal.Length), median(iris$Petal.Length),
#'     format = "xx.xx"
#'   )
#' )
#' 
#' mtbl2 <- with(subset(iris, Species == 'setosa'), rtable(
#'   header = rheader(
#'     rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan=2)),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "Setosa",
#'     mean(Sepal.Length), median(Sepal.Length),
#'     mean(Petal.Length), median(Petal.Length),
#'     format = "xx.xx"
#'   )
#' ))
#' 
#' rbind(mtbl, mtbl2)
#' 
#' rbind(mtbl, rrow(), mtbl2)
#' 
#' rbind(mtbl, rrow("aaa"), indent(mtbl2))
#' 
#' rbind(mtbl, mtbl2, gap = 1)
#' 
#' rbind(mtbl, mtbl2, gap = 2)
#' 
rbind.rtable <- function(..., gap = 0) {
  dots <- Filter(Negate(is.null), list(...))
  rbindl_rtables(dots, gap = gap)
}

#' Stack a list of rtables
#' 
#' See \code{\link{rbind.rtable}} for details
#' The difference is that this function takes a list of rtables as an argument whereas
#' rbind.rtable takes a list of arguments (...), each corresponding to an rtable.
#' 
#' empty rtables and NULLs in list are filtered out at first.
#' To bind rtables, you must make sure that the list contains at least one rtable and the header
#' of the first rtable will be taken. (Note that every rtable must contain a header). We do not test
#' that all rtable headers are identical for performance reasons.
#' When the list has zero length, return an empty rtable
#' 
#' @param x a list of rtable objects
#' @inheritParams rbind.rtable
#' @param check_headers whether to check that headers of involved rtables are identical
#'   this may lead to performance problems
#' 
#' @export
#' 
rbindl_rtables <- function(x, gap = 0, check_headers = FALSE) {
  stopifnot(is.list(x))
  x <- x[!vapply(x, is_empty_rtable, logical(1))]
  if (length(x) == 0) {
    return(empty_rtable())
  }
  stopifnot(is.numeric(gap), gap >= 0)
  
  is_rtable <- vapply(x, is, logical(1), "rtable")
  is_rrow <- vapply(x, is, logical(1), "rrow")
  
  # in order to get the reference header
  stopifnot(sum(is_rtable) > 0)
  if (!all(is_rtable | is_rrow)) {
    stop("elements are not all of class rtable or rrow")
  }
  if (!num_all_equal(vapply(x[is_rtable], ncol, numeric(1)))) {
    stop("non-matching number of columns between tables")
  }
  if (any(vapply(x[is_rrow], length, numeric(1)) != 0)) {
    stop("only rrows with no cells are allowed to be used in rbind")
  }

  x <- lapply(x, function(xi) if (is(xi, "rrow")) list(xi) else xi)
  tbl <- if (gap != 0) {
    gap_rows <- replicate(gap, rrow(), simplify = FALSE)
    Reduce(function(tbl1, tbl2) c(tbl1, gap_rows, tbl2), x)
  } else {
    unlist(x, recursive = FALSE)
  }
  
  # commented out for performance reasons
  stopifnot(!check_headers) # todo: currently not working because headers have attributes in random order -> identical not working
  #nolintr start
  # if (check_headers) {
  #   stopifnot(all(vapply(
  #     x[is_rtable], 
  #     function(tbl) {
  #       identical(header(tbl), header(x[[1]]))
  #     }, 
  #     logical(1)
  #   ))) 
  # }
  #nolintr end
  ref_header <- header(x[[ which(is_rtable)[[1]] ]])  
  class(tbl) <- "rtable"
  attr(tbl, "header") <- ref_header
  attr(tbl, "nrow") <- length(tbl)
  attr(tbl, "ncol") <- ncol(ref_header)
  
  tbl
}  


#' Unlist method for rtables
#'
#' rtable, rrow, and rcell objects should not be unlisted. This allows us to create nested lists with rtables objects
#' and then flatten them to a list of rtable objects.
#'
#' @inheritParams base::unlist
#'
#' @return rtable object
#'
#' @method unlist rtable
#' @export
unlist.rtable <- function(x, recursive = TRUE, use.names = TRUE) {
  x
}

#' Unlist method for rrow
#'
#' rtable, rrow, and rcell objects should not be unlisted. This allows us to create nested lists with rtables objects
#' and then flatten them to a list of rtable objects.
#'
#' @inheritParams base::unlist
#'
#' @return rrow object
#'
#' @method unlist rrow
#' @export
unlist.rrow <- function(x, recursive = TRUE, use.names = TRUE) {
  x
}

#' Unlist method for rcell
#'
#' rtable, rrow, and rcell objects should not be unlisted. This allows us to create nested lists with rtables objects
#' and then flatten them to a list of rtable objects.
#'
#' @inheritParams base::unlist
#'
#' @return rrow object
#'
#' @method unlist rcell
#' @export
unlist.rcell <- function(x, recursive = TRUE, use.names = TRUE) {
  x
}
