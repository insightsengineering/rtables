
#' Change indentation of all rrows in an rtable
#' 
#' Change indentation of all rrows in an rtable
#' 
#' @param x \code{\link{rtable}} object
#' @param by integer to increase indentation of rows. Can be negative. If final indentation is smaller than 0 then the
#'   indentation is set to 0.
#' @param truncate_at_zero whether to truncate indent at zero or raise an error
#' 
#' @export
#' 
#' @examples 
#' is_setosa <- iris$Species == "setosa"
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
#'   ),
#'   rrow(
#'     row.name = "Setosa",
#'     mean(iris$Sepal.Length[is_setosa]), median(iris$Sepal.Length[is_setosa]),
#'     mean(iris$Petal.Length[is_setosa]), median(iris$Petal.Length[is_setosa]),
#'     format = "xx.xx"
#'   )
#' )
#' indent(mtbl)
#' indent(mtbl, 2)
#' indent(mtbl, -3, truncate_at_zero = TRUE)
#' 
indent <- function(x, by = 1, truncate_at_zero = FALSE) {
  stopifnot(is_rtable(x))
  stopifnot(length(by) == 1, is.numeric(by))
  stopifnot(is.logical(truncate_at_zero))
  
  if (is_empty_rtable(x)) {
    return(x)
  }
  
  for (i in 1:nrow(x)) {
    ind <- attr(x[[i]], "indent")
    stopifnot(truncate_at_zero || (ind + by >= 0))
    attr(x[[i]], "indent") <- max(0, ind + by)
  }
  x
}