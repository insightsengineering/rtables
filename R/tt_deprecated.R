

#'
#' Row names of an \code{\link{rtable}} object with spaces
#'
#' Retrieve the row names of an \code{\link{rtable}} object
#'
#' @param spaces numeric number of spaces per indent level
#' @param x deprecated.
#'
#' @return a vector with the row names
#' @rdname deprecated
#'
indented_row.names <- function(x, spaces = 2) {

  if (!is(x, "rtable") && !is(x, "rheader")) stop("x is required to be a rtable or a rheader")

  if (spaces < 0) stop("spaces needs to be >= 0")

  vapply(x, function(row) {
    rn <- attr(row, "row.name")
    indent <- strrep(" ", attr(row, "indent")*spaces)
    if (is.null(rn)) "" else paste0(indent, rn)
  }, character(1))
}
