#'Format rcell
#'
#' This is a wrapper around \code{\link[formatters:format_value]{formatters::format_value}}
#' for use with \code{CellValue} objects
#'
#' @param x an object of class \code{\link{CellValue}}, or a raw value.
#' @param format character(1) or function. The format label (string) or formatter function to apply to \code{x}.
#' @param na_str character(1). String that should be displayed when the value of \code{x} is missing. Defaults to \code{"NA"}.
#' @param output character(1). output type
#' @return formatted text representing the cell
#' @export
#' @examples
#' cll <- CellValue(pi, format = "xx.xxx")
#' format_rcell(cll)

format_rcell <- function(x, format, output = c("ascii", "html"), na_str = "NA") {
    format <- if(missing(format)) obj_format(x) else format
    format_value(rawvalues(x), format = format, output = output, na_str = na_str)
}




#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze("AGE", function(x) {
#'     in_rows(
#'       "mean_sd" = c(mean(x), sd(x)),
#'       "range" = range(x),
#'       .formats = c(mean_sd = sprintf_format("%.4f - %.2f"), range = "xx.xx - xx.xx")
#'     )
#'   }) %>%
#'   build_table(DM)
#'
#' rcell(100, format = sprintf_format("(N=%i)"))
#'
#' rcell(c(4,9999999999), format = sprintf_format("(%.2f, >999.9)"))
#'
#' rtable(LETTERS[1:2], rrow("", 1 ,2), format = sprintf_format("%.2f"))


