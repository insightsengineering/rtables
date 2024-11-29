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
  function(x, ...) in_rows("Mean" = rcell(mean(x, ...), stat_names = "mean", format = "xx.xx"))
)

#' @rdname rtinner
#' @exportMethod simple_analysis
setMethod(
  "simple_analysis", "logical",
  function(x, ...) in_rows("Count" = rcell(sum(x, ...), stat_names = "n", format = "xx"))
)

#' @rdname rtinner
#' @exportMethod simple_analysis
setMethod(
  "simple_analysis", "factor",
  function(x, ...) in_rows(.list = as.list(table(x)), .stat_names = "n")
)

#' @rdname rtinner
#' @exportMethod simple_analysis
setMethod(
  "simple_analysis", "ANY",
  function(x, ...) {
    stop("No default simple_analysis behavior for class ", class(x), " please specify FUN  explicitly.")
  }
)

#' Check if an object is a valid `rtable`
#'
#' @param x (`ANY`)\cr an object.
#'
#' @return `TRUE` if `x` is a formal `TableTree` object, `FALSE` otherwise.
#'
#' @examples
#' is_rtable(build_table(basic_table(), iris))
#'
#' @export
is_rtable <- function(x) {
  is(x, "VTableTree")
}

# nocov start
## is each object in a collection from a class
are <- function(object_collection, class2) {
  all(vapply(object_collection, is, logical(1), class2))
}

num_all_equal <- function(x, tol = .Machine$double.eps^0.5) {
  stopifnot(is.numeric(x))

  if (length(x) == 1) {
    return(TRUE)
  }

  y <- range(x) / mean(x)
  isTRUE(all.equal(y[1], y[2], tolerance = tol))
}

# copied over from utils.nest which is not open-source
all_true <- function(lst, fcn, ...) {
  all(vapply(lst, fcn, logical(1), ...))
}

is_logical_single <- function(x) {
  !is.null(x) &&
    is.logical(x) &&
    length(x) == 1 &&
    !is.na(x)
}

is_logical_vector_modif <- function(x, min_length = 1) {
  !is.null(x) &&
    is.logical(x) &&
    is.atomic(x) &&
    !anyNA(x) &&
    ifelse(min_length > 0, length(x) >= min_length, TRUE)
}
# nocov end

# Shorthand for functions that take df as first parameter
.takes_df <- function(f) {
  func_takes(f, "df", is_first = TRUE)
}

# Checking if function takes parameters
func_takes <- function(func, params, is_first = FALSE) {
  if (is.list(func)) {
    return(lapply(func, func_takes, params = params, is_first = is_first))
  }
  if (is.null(func) || !is(func, "function")) {
    # safe-net: should this fail instead?
    return(setNames(rep(FALSE, length(params)), params))
  }
  f_params <- formals(func)
  if (!is_first) {
    return(setNames(params %in% names(f_params), params))
  } else {
    if (length(params) > 1L) {
      stop("is_first works only with one parameters.")
    }
    return(!is.null(f_params) && names(f_params)[1] == params)
  }
}

#' Translate spl_context to a path to display in error messages
#'
#' @param ctx (`data.frame`)\cr the `spl_context` data frame where the error occurred.
#'
#' @return A character string containing a description of the row path corresponding to `ctx`.
#'
#' @export
spl_context_to_disp_path <- function(ctx) {
  ## this can happen in the first split in column space, but
  ## should never happen in row space
  if (length(ctx$split) == 0) {
    return("root")
  }
  if (ctx$split[1] == "root" && ctx$value[1] == "root") {
    ctx <- ctx[-1, ]
  }
  ret <- paste(sprintf("%s[%s]", ctx[["split"]], ctx[["value"]]),
    collapse = "->"
  )
  if (length(ret) == 0 || nchar(ret) == 0) {
    ret <- "root"
  }
  ret
}

# Utility function to paste vector of values in a nice way
paste_vec <- function(vec) {
  paste0('c("', paste(vec, collapse = '", "'), '")')
}

# Utility for checking if a package is installed
check_required_packages <- function(pkgs) {
  for (pkgi in pkgs) {
    if (!requireNamespace(pkgi, quietly = TRUE)) {
      stop(
        "This function requires the ", pkgi, " package. ",
        "Please install it if you wish to use it"
      )
    }
  }
}
