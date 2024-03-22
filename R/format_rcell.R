#' Format `rcell` objects
#'
#' This is a wrapper for [formatters:format_value()] for use with `CellValue` objects
#'
#' @param x (`CellValue` or `any`)\cr an object of class `CellValue`, or a raw value.
#' @param format (`character(1)` or `function`)\cr the format label or formatter function to 
#'   apply to `x`.
#' @param output (`character(1)`)\cr output type.
#' @param na_str (`character(1)`)\cr string that should be displayed when the value of `x` is missing. 
#'   Defaults to `"NA"`.
#' @param pr_row_format (`list`)\cr list of default formats coming from the general row.
#' @param pr_row_na_str (`list`)\cr list of default `"NA"` strings coming from the general row.
#' @param shell (`logical(1)`)\cr whether the formats themselves should be returned instead of the
#'   values with formats applied. Defaults to `FALSE`.
#'
#' @return Formatted text.
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
