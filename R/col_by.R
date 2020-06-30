#' @include utils.R
NULL

#' Cut by quartiles
#' 
#' @param x a numerical vector
#' @param cumulative logical, if cumulative col_by should be returned
#' 
#' @importFrom stats quantile
#' @export
#' 
#' @examples 
#' 
#' rtabulate(x = iris$Sepal.Length, col_by = by_quartile(iris$Sepal.Width), 
#'           FUN = mean, format = "xx.xx")
#' 
by_quartile <- function(x, cumulative = FALSE) {
  stopifnot(is.numeric(x), is.logical.single(cumulative), !any(is.na(x)))


  ret <- data.frame(x)
  class(ret) <- c(if(cumulative) "cmlquartcut_df" else "quartcut_df", class(ret))
  return(ret)
}

