
formats_1d <- c(
  "xx", "xx.", "xx.x", "xx.xx", "xx.xxx", "xx.xxxx",
  "xx%", "xx.x%", "xx.xx%", "xx.xxx%", "(N=xx)", ">999.9",
  "x.xxxx | (<0.0001)"
)

formats_2d <- c(
  "xx / xx", "xx. / xx.", "xx.x / xx.x", "xx.xx / xx.xx",
  "xx (xx%)", "xx (xx.%)", "xx (xx.x%)", "xx (xx.xx%)", 
  "xx. (xx.%)", "xx.x (xx.x%)", "xx.xx (xx.xx%)",
  "(xx, xx)", "(xx., xx.)", "(xx.x, xx.x)", "(xx.xx, xx.xx)",
  "(xx.xxx, xx.xxx)", "(xx.xxxx, xx.xxxx)",
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
#' list_rcell_format_labels()
#' 
list_rcell_format_labels <- function() {
  structure(  
    list(
      "1d" = formats_1d,
      "2d" = formats_2d
    ),
    info = "xx does not modify the element, and xx. rounds a number to 0 digits"
  )
}

#' Check if a format is a valid rcell format
#'
#' @param x either format string or an object returned by \code{sprintf_format}
#' @param stop_otherwise logical, if \code{x} is not a format should an error be
#'   thrown
#' 
#' @export
#'   
is_rcell_format <- function(x, stop_otherwise=FALSE) {
  is_valid <- is.null(x) ||
    (length(x) == 1 && 
       (is.function(x) ||
          identical(attr(x, "format_type"), "sprintf") || 
          x %in% unlist(list_rcell_format_labels()))) 
  
  if (stop_otherwise && !is_valid)
    stop("format needs to be a format label, sprintf_format object, a function, or NULL")
  
  is_valid
}

#' Get format attribute of an rcell
#' 
#' @param x rcell object
#' @export
#' 
#' @examples
#' get_format(rcell(c(2, 0.34), format = "xx.xx (xx.xx%)"))
#' 
get_format <- function(x) {
  if (!is(x, "rcell")) stop("rcell object required")
  attr(x, "format")
}

set_format <- function(x, value) {
  if (!is(x, "rcell")) stop("rcell object required")
  is_rcell_format(format, stop_otherwise = TRUE)
  attr(x, "format") <- value
  x
}

#' Specify a rcell format based on sprintf formattig rules
#'
#' Format the rcell data with \code{\link[base]{sprintf}} formatting strings
#'
#' @inheritParams  base::sprintf
#'
#' @export
#' 
#' @seealso \code{\link[base]{sprintf}}
#' 
#' @examples 
#' 
#' rcell(100, format = sprintf_format("(N=%i)"))
#' 
#' rcell(c(4,9999999999), format = sprintf_format("(%.2f, >999.9)"))
#' 
#' rtable(LETTERS[1:2], rrow("", 1 ,2), format = sprintf_format("%.2f"))
#' 
sprintf_format <- function(fmt) {
  structure(fmt, "format_type" = "sprintf")
}


# # @export
# format.rcell <- function(x, ...) {
#   format_rcell(x, ...)
# }

#' Convert the contets of an \code{\link{rcell}} to a string using the
#' \code{format} information
#' 
#' @param x an object of class \code{\link{rcell}}
#' @inheritParams rtable
#' @param output output type
#' 
#' 
#' @export
format_rcell <- function(x, format, output = c("ascii", "html")) {
  
  
  if (length(x) == 0) return("")
  
  output <- match.arg(output)
  
  format <- if (!missing(format)) format else attr(x, "format")
  
  txt <- if (is.null(format)) {
    toString(x)
  } else if (identical(attr(format, "format_type"), "sprintf")) {
    do.call(sprintf, c(list(format), x))
  } else if (is.character(format)) {
    l <- if (format %in% formats_1d) {
      1
    } else if (format %in% formats_2d) {
      2
    } else {
      stop("unknown format label: ", format, ". use list_rcell_format_labels() to get a list of all formats")
    }
    if (format != "xx" && length(x) != l) stop("cell <", paste(x), "> and format ", format, " are of different length")
    
    switch(
      format,
      "xx" = if (all(is.na(x))) "NA" else as.character(x),
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
      "(N=xx)" = paste0("(N=", x, ")"),
      ">999.9" = ">999.9",
      "x.xxxx | (<0.0001)" = ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x)),
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
      "(xx.xxx, xx.xxx)" = paste0("(", paste(lapply(x, round, 3), collapse = ", ") , ")"), 
      "(xx.xxxx, xx.xxxx)" = paste0("(", paste(lapply(x, round, 4), collapse = ", ") , ")"),
      "xx - xx" = paste(x[1], "-", x[2]),
      "xx.x - xx.x" = paste(vapply(x, round, numeric(1), 1), collapse = " - "),
      "xx.xx - xx.xx" = paste(vapply(x, round, numeric(1), 2), collapse = " - "),
      "xx.x (xx.x)" = paste0(round(x[1], 1), " (",round(x[2], 1), ")"),
      "xx.xx (xx.xx)" = paste0(round(x[1], 2), " (",round(x[2], 2), ")"),
      "xx.x, xx.x" = paste(vapply(x, round, numeric(1), 1), collapse = ", "),
      "xx.x to xx.x" = paste(vapply(x, round, numeric(1), 1), collapse = " to "),
      paste("format string", format, "not found")
    )
  } else if (is.function(format)) {
    format(x, output = output)
  }
  
  if (output == "ascii") {
    txt
  } else if (output == "html") {
    ## convert to tagList
    ## convert \n to <br/>
    
    if (identical(txt, "")) {
      txt
    } else {
      els <- unlist(strsplit(txt, "\n", fixed = TRUE))
      Map(function(el, is.last) {
        tagList(el, if (!is.last) tags$br() else NULL)
      }, els, c(rep(FALSE, length(els) -1), TRUE))
    }

  } else {
    txt
  }
  
}
