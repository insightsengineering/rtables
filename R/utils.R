#' A spin on sprintf
#'
#' @description Allows for named ellipses arguments to be injected into
#' string without needing numerical index. Inspired by Python's format strings.
#'
#' @param s format string to be evaluated
#' @param ... named arguments to be used in formatting of string
#' @param envir the environment in which arguments should be source (defaults to
#'   environment in which function is called)
#' @param re regular expression to use for stripping named arguments. Expects a
#'   single capture group, which should contain the name of the argument.
#' @param re_group regex capture group, used as gsub replacement to strip name
#'   from regular expression match
#'
#' @return a character vector with regular expression named values replaced
#'   within string
#'
#' @examples
#' # using explicitly named arguments
#' spf('%(x)s + %(y)s = %(x + y)s', x = 2, y = 3)
#' ## [1] "2 + 3 = 5"
#'
#' # using globally defined arguments
#' x <- 3
#' y <- 4
#' spf('%(x)s + %(y)s = %(x + y)s')
#' ## [1] "3 + 4 = 7"
#'
#' # using a dataframe
#' head(spf('%(carb)d + %(wt).1f = %(carb + wt).2f', envir = mtcars))
#' ## [1] "4 + 2.6 = 4.78" "4 + 2.9 = 4.78" "1 + 2.3 = 4.78" "1 + 3.2 = 4.78"
#' ## [5] "2 + 3.4 = 4.78" "1 + 3.5 = 4.78"
#' 
#' # evaluating code within a format string 
#' # (not recommended for complex manipulations)
#' spf('%(max(vs) / max(carb)).2f', envir = mtcars)
#' ## [1] "0.12"
#' 
#' @importFrom utils modifyList
#' @importFrom stats setNames
#' 
#' @export
spf <- function(s, ..., envir = parent.frame(), 
                re = NULL, re_group = NULL) {
  
  if (is.null(re)) re <- '%(\\((?>[^()]|(?1))*\\))'
  re_group <- paste0('\\', if (is.null(re_group)) 1 else re_group)
  
  dots <- list(...)
  eval_env <- utils::modifyList(as.list(envir), dots)
  
  s_out <- ''
  while ((m <- regexpr(re, s, perl = TRUE)) > 0) {
    n <- gsub(re, re_group, regmatches(s, m), perl = TRUE)
    
    if (!length(i <- which(names(dots) == n))) {
      dots <- append(
        dots, 
        stats::setNames(list(eval(parse(text = n), eval_env)), n)
      )
      i <- length(dots)
    }
    
    s_out <- paste0(s_out, substr(s, 1, m - 1), '%', i, '$')
    s <- substr(s, m + attr(m, 'match.length'), nchar(s))
  }
  
  do.call(sprintf, c(paste0(s_out, s), setNames(dots, NULL)))
}


#' Search named sprintf string for variable names
#'
#' @param s format string to be evaluated
#' @param re regular expression to use for stripping named arguments. Expects a
#'   single capture group, which should contain the name of the argument.
#' @param re_group regex capture group, used as gsub replacement to strip name
#'   from regular expression match
#'
#' @return a character vector of included variable names
#'
#' @examples
#' spf_varnames('%(var1)s, %(var2 * var3)s')
#' ## [1] "var1" "var2" "var3"
#' 
#' @export
spf_varnames <- function(s, re = NULL, re_group = NULL) {
  if (is.null(re)) re <- '%(\\((?>[^()]|(?1))*\\))'
  re_group <- paste0('\\', if (is.null(re_group)) 1 else re_group)
  
  vars <- c()
  while ((m <- regexpr(re, s, perl = TRUE)) > 0) {
    n <- gsub(re, re_group, regmatches(s, m), perl = TRUE)
    vars <- c(vars, all.vars(parse(text = n)))
    s <- substr(s, m + attr(m, 'match.length'), nchar(s))
  }
  
  as.character(vars)
}