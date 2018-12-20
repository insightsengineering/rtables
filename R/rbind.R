#' Stack rtable objects 
#' 
#' Note that the columns order are not mached by the header: the first table
#' header is taken as the reference.
#' 
#' @param ... \code{\link{rtable}} objects
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
#' tbl <- rbind(mtbl, mtbl2)
#' tbl
#' 
#' tbl <- rbind(mtbl, mtbl2, gap = 1)
#' tbl 
#' 
#' tbl <- rbind(mtbl, mtbl2, gap = 2)
#' tbl 
#' 
rbind.rtable <- function(..., gap = 0) {
  dots <- Filter(Negate(is.null), list(...))
  rbindl_rtables(dots, gap = gap)
}

#' Stack a list of rtables
#' 
#' See \code{\link{rbind.rtable}} for details
#' 
#' @param x a list of rtable objects
#' @inheritParams rbind.rtable
#' 
#' @export
#' 
rbindl_rtables <- function(x, gap = 0) {
  
  stopifnot(is.list(x))
  stopifnot(are(x, "rtable"))
  stopifnot(length(x) > 0)
  stopifnot(is.numeric(gap), gap >= 0)
  
  if (!num_all_equal(vapply(x, ncol, numeric(1))))
    stop("non-matching number of columns between tables")
  
  tbl <- if (gap != 0) {
    gap_rows <- replicate(gap, rrow(), simplify = FALSE)
    Reduce(function(tbl1, tbl2) c(tbl1, gap_rows, tbl2), x)
  } else {
    unlist(x, recursive = FALSE)
  }
  
  ref_header <- header(x[[1]])  
  class(tbl) <- "rtable"
  attr(tbl, "header") <- ref_header
  attr(tbl, "nrow") <- length(tbl)
  attr(tbl, "ncol") <- ncol(ref_header)
  
  tbl
}  


#' The unlist method for rtables returns that rtable
#' 
#' Unlisting rtables will not affact the rtable
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
