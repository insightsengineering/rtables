#' Format `rcell` objects
#'
#' This is a wrapper for [formatters::format_value()] for use with `CellValue` objects
#'
#' @inheritParams lyt_args
#' @inheritParams gen_args
#' @param x (`CellValue` or `ANY`)\cr an object of class `CellValue`, or a raw value.
#' @param format (`string` or `function`)\cr the format label or formatter function to
#'   apply to `x`.
#' @param output (`string`)\cr output type.
#' @param pr_row_format (`list`)\cr list of default formats coming from the general row.
#' @param pr_row_na_str (`list`)\cr list of default `"NA"` strings coming from the general row.
#' @param shell (`flag`)\cr whether the formats themselves should be returned instead of the
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
                         round_type,
                         shell = FALSE) {
  # Check for format and parent row format
  format <- if (missing(format)) obj_format(x) else format
  if (is.null(format) && !is.null(pr_row_format)) {
    format <- pr_row_format
  }
  if (missing(round_type) && !is.null(obj_round_type(x))) {
    round_type <- obj_round_type(x)
  }
  if (missing(round_type) && is.null(obj_round_type(x))) {
    round_type <- valid_round_type[1]
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
    na_str = na_str,
    round_type = round_type
  )
}
