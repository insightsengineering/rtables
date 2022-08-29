#'Format rcell
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
#' @param shell logical(1). Should the formats themselves be returned instead of the
#' values with formats applied. Defaults to \code{FALSE}.
#' @return formatted text representing the cell
#' @export
#' @examples
#' cll <- CellValue(pi, format = "xx.xxx")
#' format_rcell(cll)

format_rcell <- function(x, format, 
                         output = c("ascii", "html"),
                         na_str = obj_na_str(x) %||% "NA", 
                         shell = FALSE) {
    
    # Check for format
    format <- if (missing(format)) obj_format(x) else format
    if (shell) format <- if (is.null(format)) "xx" else format
    
    # Main call to external function
    if (shell) return(format)
    format_value(rawvalues(x), 
                 format = format, 
                 output = output,
                 na_str = na_str)
}
