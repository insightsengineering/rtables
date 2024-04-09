#' Default tabulation
#'
#' This function is used when [analyze()] is invoked.
#'
#' @param x (`vector`)\cr the *already split* data being tabulated for a particular cell/set of cells.
#' @param ... additional parameters to pass on.
#'
#' @details This function has the following behavior given particular types of inputs:
#'   \describe{
#'     \item{numeric}{calls [mean()] on `x`.}
#'     \item{logical}{calls [sum()] on `x`.}
#'     \item{factor}{calls [length()] on `x`.}
#'   }
#'
#' The [in_rows()] function is called on the resulting value(s). All other classes of input currently lead to an error.
#'
#' @inherit in_rows return
#'
#' @author Gabriel Becker and Adrian Waddell
#'
#' @examples
#' simple_analysis(1:3)
#' simple_analysis(iris$Species)
#' simple_analysis(iris$Species == "setosa")
#'
#' @rdname rtinner
#' @export
setGeneric("simple_analysis", function(x, ...) standardGeneric("simple_analysis"))

#' @rdname rtinner
#' @exportMethod simple_analysis
setMethod(
  "simple_analysis", "numeric",
  function(x, ...) in_rows("Mean" = rcell(mean(x, ...), format = "xx.xx"))
)

#' @rdname rtinner
#' @exportMethod simple_analysis
setMethod(
  "simple_analysis", "logical",
  function(x, ...) in_rows("Count" = rcell(sum(x, ...), format = "xx"))
)

#' @rdname rtinner
#' @exportMethod simple_analysis
setMethod(
  "simple_analysis", "factor",
  function(x, ...) in_rows(.list = as.list(table(x)))
)

#' @rdname rtinner
#' @exportMethod simple_analysis
setMethod(
  "simple_analysis", "ANY",
  function(x, ...) {
    stop("No default simple_analysis behavior for class ", class(x), " please specify FUN  explicitly.")
  }
)
