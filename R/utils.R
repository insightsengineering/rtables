

#' Check if an object is a valid rtable
#'
#' @param x an object
#'
#'
#' @export
#' @return \code{TRUE} if \code{x} is a formal Table object, \code{FALSE} otherwise.
#' @examples
#' is_rtable(build_table(basic_table(), iris))
is_rtable <- function(x) {
  is(x, "VTableTree")
}



# nocov start
## is each object in a collection from a class
are <- function(object_collection, class2) {
  all(vapply(object_collection, is, logical(1), class2))
}

num_all_equal <- function(x, tol = .Machine$double.eps ^ 0.5) {
  stopifnot(is.numeric(x))

  if (length(x) == 1) return(TRUE)

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

#' @title Alignment utils
#'
#' @description Currently supported cell value alignments. These values 
#'   may be used to set single cell alignment (`align` in [rcell()]) or row-wise 
#'   alignment (`.aligns` in [in_rows()]).
#' 
#' @return a vector of alignments currently supported.
#' 
#' @examples 
#' # See the alignments available in rtables
#' rtables_aligns()
#' 
#' # Right alignment of single cell
#' basic_table() %>%
#'   analyze("Species", function(x) in_rows(left = rcell("r", align = "right"))) %>%
#'   build_table(iris)
#'   
#' # Set multiple alignments using character vectors
#' basic_table() %>%
#'   analyze("Species", function(x) {
#'     in_rows(
#'       left = rcell("l"),
#'       right = rcell("r"),
#'       .aligns = c("left", "right")
#'     )
#'   }) %>%
#'   build_table(iris)
#' 
#' @seealso [in_rows()], [rcell()]
#' 
#' @export
rtables_aligns <- function() {
    c("left", "right", "center")
}


chk_rtables_align <- function(algn) {
    if(any(is.na(algn) | !(algn %in% rtables_aligns())))
        stop("Unsupported text-alignment: ", algn)
    algn
}

#' Translate spl_context to Path for display in error messages
#'
#'
#' @param ctx data.frame. The `spl_context` data.frame where the error occured
#'
#' @return A character string containing a description of the row path corresponding
#' to the `ctx`
#' @export
spl_context_to_disp_path <- function(ctx) {
    ret <- paste(sprintf("%s[%s]", ctx[["split"]], ctx[["value"]]),
                 collapse = "->")
    if(length(ret) == 0 ||
       nchar(ret) == 0)
        ret <- "root"
    ret
}
