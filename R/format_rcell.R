#' Format rcell
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
#' @param parent_row list of default format and \code{"NA"} string coming from.
#' the general row.
#'
#' @return formatted text representing the cell
#'
#' @examples
#' cll <- CellValue(pi, format = "xx.xxx")
#' format_rcell(cll)
#'
#' @export
format_rcell <- function(x, format,
                         output = c("ascii", "html"),
                         na_str = obj_na_str(x) %||% "NA",
                         parent_row = NULL) {

  # Check for format and parent row format
  format <- if (missing(format)) obj_format(x) else format
  if (is.null(format) && !is.null(parent_row[["format"]])) {
      format <- parent_row[["format"]]
  }
  # Check for na_str from parent
  if (is.null(obj_na_str(x)) && !is.null(parent_row[["na_str"]])) {
      na_str <- parent_row[["na_str"]]
  }

  # Main call to external function
  format_value(rawvalues(x),
    format = format,
    output = output,
    na_str = na_str
  )
}
