#' Format `rcell`
#'
#' This is a wrapper around
#' \code{\link[formatters:format_value]{formatters::format_value}} for use with
#' \code{CellValue} objects
#'
#' @param x an object of class \code{\link{CellValue}}, or a raw value.
#' @param format character(1) or function. The format label (string) or
#'   formatter function to apply to \code{x}.
#' @param output character(1). Output type.
#' @param na_str character(1). String that should be displayed when the value of
#'   \code{x} is missing. Defaults to \code{"NA"}.
#' @param pr_row_format list of default format coming from the general row.
#' @param pr_row_na_str list of default \code{"NA"} string coming from the general row.
#' @param shell logical(1). Should the formats themselves be returned instead of the
#' values with formats applied. Defaults to \code{FALSE}.
#'
#' @return formatted text representing the cell
#'
#' @examples
#' cll <- CellValue(pi, format = "xx.xxx")
#' format_rcell(cll)
#'
#' # Cell values precedes the row values
#' cll <- CellValue(pi, format = "xx.xxx")
#' format_rcell(cll, pr_row_format = "xx.x")
#'
#' # Similarly for NA values
#' cll <- CellValue(NA, format = "xx.xxx", format_na_str = "This is THE NA")
#' format_rcell(cll, pr_row_na_str = "This is NA")
#'
#' @export
format_rcell <- function(x, format,
                         output = c("ascii", "html"),
                         na_str = obj_na_str(x) %||% "NA",
                         pr_row_format = NULL,
                         pr_row_na_str = NULL,
                         shell = FALSE) {
  # Check for format and parent row format
  format <- if (missing(format)) obj_format(x) else format
  if (is.null(format) && !is.null(pr_row_format)) {
    format <- pr_row_format
  }
  # Check for na_str from parent
  if (is.null(obj_na_str(x)) && !is.null(pr_row_na_str)) {
    na_str <- pr_row_na_str
  }

  # Main call to external function or shell
  if (shell) {
    return(format)
  }
  format_value(rawvalues(x),
    format = format,
    output = output,
    na_str = na_str
  )
}
