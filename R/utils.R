

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
#'   may be used to set content alignment (`align` in [rcell()] or `.aligns`
#'   in [in_rows()]).
#'
#' @return a vector of alignments currently supported.
#'
#' @examples
#' # See the alignments available in rtables
#' rtables_aligns()
#'
#' # Right alignment with align in rcell()
#' lyt <- basic_table() %>%
#'   analyze("Species", function(x) in_rows(left = rcell("r", align = "right")))
#'
#' tbl <- build_table(lyt, iris)
#' tbl
#'
#' # Set multiple alignments using character vectors with .aligns in in_rows()
#' lyt2 <- basic_table() %>%
#'   analyze("Species", function(x) {
#'     in_rows(
#'       left = rcell("l"),
#'       right = rcell("r"),
#'       .aligns = c("left", "right")
#'     )
#'   })
#'
#' tbl2 <- build_table(lyt2, iris)
#' tbl2
#'
#' # Clinical data example:
#' lyt3 <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("SEX", split_fun = drop_split_levels) %>%
#'   analyze(c("AGE"), function(x) {
#'     in_rows(
#'       "mean" = rcell(mean(x), align = "right"),
#'       "sd" = rcell(sd(x), align = "left"), .formats = c("xx.x")
#'     )
#'   }, show_labels = "visible", na_str = "NE")
#'
#' tbl3 <- build_table(lyt3, ex_adsl)
#' tbl3
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
#' @param ctx data.frame. The `spl_context` data.frame where the error occurred
#'
#' @return A character string containing a description of the row path corresponding
#' to the `ctx`
#' @export
spl_context_to_disp_path <- function(ctx) {
    ## this can happen in the first split in column space, but
    ## should never happen in row space
    if(length(ctx$split) == 0)
        return("root")
    if(ctx$split[1] == "root" && ctx$value[1] == "root")
        ctx <- ctx[-1, ]
    ret <- paste(sprintf("%s[%s]", ctx[["split"]], ctx[["value"]]),
                 collapse = "->")
    if(length(ret) == 0 ||
       nchar(ret) == 0)
        ret <- "root"
    ret
}
