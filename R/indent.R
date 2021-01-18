
#' Change indentation of all rrows in an rtable
#'
#' Change indentation of all rrows in an rtable
#'
#' @param x \code{\link{rtable}} object
#' @param by integer to increase indentation of rows. Can be negative. If final indentation is smaller than 0 then the
#'   indentation is set to 0.
#'
#' @export
#' @return \code{x} with its indent modifier incremented by \code{by}.
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
#'
indent <- function(x, by = 1) {
    if(nrow(x) == 0 || by == 0)
        return(x)

    indent_mod(x) <- indent_mod(x) + by
    x
}

