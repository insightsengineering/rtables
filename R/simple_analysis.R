#' Default tabulation
#'
#'
#' This function is used when \code{\link{analyze}} is invoked
#'
#' @param x the \emph{already split} data being tabulated for a particular cell/set of cells
#' @param \dots passed on directly
#'
#' @details This function has the following behavior given particular types of inputs:
#' \describe{
#' \item{numeric}{calls \code{\link{mean}} on \code{x}}
#' \item{logical}{calls \code{\link{sum}} on \code{x}}
#' \item{factor}{calls \code{\link{length}} on \code{x}}
#' }
#'
#' \code{in_rows} is called on the resulting value(s).
#'
#' All other classes of input currently lead to an error.
#' @export
#' @rdname rtinner
#' @author Gabriel Becker and Adrian Waddell
#' @inherit in_rows return
#' @examples
#' simple_analysis(1:3)
#' simple_analysis(iris$Species)
#' simple_analysis(iris$Species == "setosa")
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
