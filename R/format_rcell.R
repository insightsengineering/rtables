
formats_1d <- c(
  "xx", "xx.", "xx.x", "xx.xx", "xx.xxx", "xx.xxxx",
  "xx%", "xx.x%", "xx.xx%", "xx.xxx%"
)

formats_2d <- c(
  "xx / xx", "xx. / xx.", "xx.x / xx.x", "xx.xx / xx.xx",
  "xx (xx%)", "xx (xx.%)", "xx (xx.x%)", "xx (xx.xx%)", 
  "xx. (xx.%)", "xx.x (xx.x%)", "xx.xx (xx.xx%)",
  "(xx, xx)", "(xx., xx.)", "(xx.x, xx.x)", "(xx.xx, xx.xx)",
  "xx - xx", "xx.x - xx.x", "xx.xx - xx.xx",
  "xx.x (xx.x)", "xx.xx (xx.xx)",
  "xx.x, xx.x",
  "xx.x to xx.x"
)


#' List with valid \code{\link{rcell}} formats labels grouped by 1d and 2d
#' 
#' Currently valid format lables can not be added dynamically. Format functions
#' must be used for special cases
#' 
#' @export
#' 
#' @examples 
#' 
#' get_rcell_formats()
#' 
get_rcell_formats <- function() {
  structure(  
    list(
      "1d" = formats_1d,
      "2d" = formats_2d
    ),
    info = "xx does not modify the element, and xx. rounds a number to 0 digits"
  )
}

#' Convert the contets of an \code{\link{rcell}} to a string using the
#' \code{format} information
#' 
#' @param x an object of class \code{\link{rcell}}
#' @inheritParams rtable
#' @param output output type
#' 
#' 
#' @export
format_rcell <- function(x, format, output = c("html", "ascii")) {
  
  
  if (length(x) == 0) return("")
  
  output <- match.arg(output)
  
  if (missing(format)) format <- attr(x, "format")
  
  if (is.null(format)) stop("format missing")
  
  
  if (is.character(format)) {
    l <- if (format %in% formats_1d) {
      1
    } else if (format %in% formats_2d) {
      2
    } else {
      stop("unknown format label: ", format, ". use get_rcell_formats() to get a list of all formats")
    }
    if (length(x) != l) stop("cell <", paste(x), "> and format ", format, " are of different length")
    
    switch(
      format,
      "xx" = if (is.na(x)) "NA" else as.character(x),
      "xx." = as.character(round(x, 0)),
      "xx.x" = as.character(round(x, 1)),
      "xx.xx" = as.character(round(x, 2)),
      "xx.xxx" = as.character(round(x, 3)),
      "xx.xxxx" = as.character(round(x, 4)),
      "xx%" = paste0(x * 100, "%"),
      "xx.%" = paste0(round(x * 100, 0), "%"),
      "xx.x%" = paste0(round(x * 100, 1), "%"),
      "xx.xx%" = paste0(round(x * 100, 2), "%"),
      "xx.xxx%" = paste0(round(x * 100, 3), "%"),
      "xx / xx" = paste(x, collapse = " / "),
      "xx. / xx." = paste(lapply(x, round, 0)),
      "xx.x / xx.x" = paste(lapply(x, round, 1)),
      "xx.xx / xx.xx" = paste(lapply(x, round, 2)),
      "xx.xxx / xx.xxx" = paste(lapply(x, round, 3)),
      "xx (xx%)" = paste0(x[1], " (", x[2]*100, "%)"),
      "xx (xx.%)" = paste0(x[1], " (", round(x[2]*100, 0), "%)"),
      "xx (xx.x%)" = paste0(x[1], " (", round(x[2]*100, 1), "%)"),
      "xx (xx.xx%)" = paste0(x[1], " (", round(x[2]*100, 2), "%)"),
      "xx. (xx.%)" = paste0(round(x[1],0), " (", round(x[2]*100, 1), "%)"),
      "xx.x (xx.x%)" = paste0(round(x[1],1), " (", round(x[2]*100, 1), "%)"),
      "xx.xx (xx.xx%)" = paste0(round(x[1],2), " (", round(x[2]*100, 2), "%)"),
      "(xx, xx)" = paste0("(",x[1], x[2], ")"),
      "(xx., xx.)" = paste0("(", paste(lapply(x, round, 0), collapse = ", ") , ")"),
      "(xx.x, xx.x)" = paste0("(", paste(lapply(x, round, 1), collapse = ", ") , ")"),
      "(xx.xx, xx.xx)" = paste0("(", paste(lapply(x, round, 2), collapse = ", ") , ")"),
      "xx - xx" = paste(x[1], "-", x[2]),
      "xx.x - xx.x" = paste(vapply(x, round, numeric(1), 1), collapse = " - "),
      "xx.xx - xx.xx" = paste(vapply(x, round, numeric(1), 2), collapse = " - "),
      "xx.x (xx.x)" = paste0(round(x[1], 1), " (",round(x[2], 1), ")"),
      "xx.xx (xx.xx)" = paste0(round(x[1], 2), " (",round(x[2], 2), ")"),
      "xx.x, xx.x" = paste(vapply(x, round, numeric(1), 1), collapse = ", "),
      "xx.x to xx.x" = paste(vapply(x, round, numeric(1), 1), collapse = " to ")
    )  
  } else if (is.function(format)) {
    format(x, output = output)
  }
  
}
