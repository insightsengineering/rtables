formats_1d <- c(
  "xx", "xx.", "xx.x", "xx.xx", "xx.xxx", "xx.xxxx",
  "xx%", "xx.%", "xx.x%", "xx.xx%", "xx.xxx%", "(N=xx)", ">999.9", ">999.99",
  "x.xxxx | (<0.0001)"
)

formats_2d <- c(
  "xx / xx", "xx. / xx.", "xx.x / xx.x", "xx.xx / xx.xx", "xx.xxx / xx.xxx",
  "xx (xx%)", "xx (xx.%)", "xx (xx.x%)", "xx (xx.xx%)",
  "xx. (xx.%)", "xx.x (xx.x%)", "xx.xx (xx.xx%)",
  "(xx, xx)", "(xx., xx.)", "(xx.x, xx.x)", "(xx.xx, xx.xx)",
  "(xx.xxx, xx.xxx)", "(xx.xxxx, xx.xxxx)",
  "xx - xx", "xx.x - xx.x", "xx.xx - xx.xx",
  "xx.x (xx.x)", "xx.xx (xx.xx)",
  "xx.x, xx.x",
  "xx.x to xx.x"
)

formats_3d <- c(
  "xx.xx (xx.xx - xx.xx)"
)

#' List with valid \code{\link{rcell}} formats labels grouped by 1d, 2d and 3d
#'
#' Currently valid format labels can not be added dynamically. Format functions
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
      "2d" = formats_2d,
      "3d" = formats_3d
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
## ' rcell(100, format = sprintf_format("(N=%i)"))
## '
## ' rcell(c(4,9999999999), format = sprintf_format("(%.2f, >999.9)"))
#'
#' rtable(LETTERS[1:2], rrow("", 1 ,2), format = sprintf_format("%.2f"))
#'
sprintf_format <- function(format) {
  function(x,...) {
    do.call(sprintf, c(list(fmt = format), x))
  }
}

sprintf_format_old <- function(format) {
  structure(format, "format_type" = "sprintf")
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
#'
#' @examples
#'
#' x <- rcell(pi, format = "xx.xx")
#' x
#'
#' format_rcell(x, output = "ascii")
#'
format_rcell <- function(x, format, output = c("ascii", "html")) {
  ## if(is(x, "CellValue"))
  ##     x = x[[1]]
  
  if (length(x) == 0) return("")
  
  output <- match.arg(output)
  format <- if (!missing(format)) format else obj_format(x)
  x <- rawvalues(x)
  
  txt <- if (is.null(format)) {
    toString(x)
  } else if (identical(attr(format, "format_type"), "sprintf")) {
    do.call(sprintf, c(list(format), x))
  } else if (is.character(format)) {
    l <- if (format %in% formats_1d) {
      1
    } else if (format %in% formats_2d) {
      2
    } else if (format %in% formats_3d) {
      3
    } else {
      stop("unknown format label: ", format, ". use list_rcell_format_labels() to get a list of all formats")
    }
    if (format != "xx" && length(x) != l) stop("cell <", paste(x), "> and format ", format, " are of different length")
    
    switch(
      format,
      "xx" = if (all(is.na(x))) "NA" else as.character(x),
      "xx." = as.character(round(x, 0)),
      "xx.x" = sprintf("%.1f", x),
      "xx.xx" = sprintf("%.2f", x),
      "xx.xxx" = sprintf("%.3f", x),
      "xx.xxxx" = sprintf("%.4f", x),
      "xx%" = paste0(x * 100, "%"),
      "xx.%" = paste0(round(x * 100, 0), "%"),
      "xx.x%" = sprintf("%.1f%%", x*100),
      "xx.xx%" = sprintf("%.2f%%", x*100),
      "xx.xxx%" = sprintf("%.3f%%", x*100),
      "(N=xx)" = paste0("(N=", x, ")"),
      ">999.9" = ifelse(x > 999.9, ">999.9", sprintf("%.1f", x)),
      ">999.99" = ifelse(x > 999.99, ">999.99", sprintf("%.2f", x)),
      "x.xxxx | (<0.0001)" = ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x)),
      "xx / xx" = paste(x, collapse = " / "),
      "xx. / xx." = sprintf("%.0f / %.0f", x[1], x[2]),
      "xx.x / xx.x" = sprintf("%.1f / %.1f", x[1], x[2]),
      "xx.xx / xx.xx" = sprintf("%.2f / %.2f", x[1], x[2]),
      "xx.xxx / xx.xxx" = sprintf("%.3f / %.3f", x[1], x[2]),
      "xx (xx%)" = paste0(x[1], " (", x[2]*100, "%)"),
      "xx (xx.%)" = paste0(x[1], " (", round(x[2]*100, 0), "%)"),
      "xx (xx.x%)" = sprintf("%d (%.1f%%)", x[1], x[2]*100),
      "xx (xx.xx%)" = sprintf("%d (%.2f%%)", x[1], x[2]*100),
      "xx. (xx.%)" = sprintf("%.0f (%.0f%%)", x[1], x[2]*100),
      "xx.x (xx.x%)" = sprintf("%.1f (%.1f%%)", x[1], x[2]*100),
      "xx.xx (xx.xx%)" = sprintf("%.2f (%.2f%%)", x[1], x[2]*100),
      "(xx, xx)" = paste0("(", x[1], ", ", x[2], ")"),
      "(xx., xx.)" = sprintf("(%.0f, %.0f)", x[1], x[2]),
      "(xx.x, xx.x)" = sprintf("(%.1f, %.1f)", x[1], x[2]),
      "(xx.xx, xx.xx)" = sprintf("(%.2f, %.2f)", x[1], x[2]),
      "(xx.xxx, xx.xxx)" = sprintf("(%.3f, %.3f)", x[1], x[2]),
      "(xx.xxxx, xx.xxxx)" = sprintf("(%.4f, %.4f)", x[1], x[2]),
      "xx - xx" = paste(x[1], "-", x[2]),
      "xx.x - xx.x" = sprintf("%.1f - %.1f", x[1], x[2]),
      "xx.xx - xx.xx" = sprintf("%.2f - %.2f", x[1], x[2]),
      "xx.x (xx.x)" = sprintf("%.1f (%.1f)", x[1], x[2]),
      "xx.xx (xx.xx)" = sprintf("%.2f (%.2f)", x[1], x[2]),
      "xx.x, xx.x" = sprintf("%.1f, %.1f", x[1], x[2]),
      "xx.x to xx.x" = sprintf("%.1f to %.1f", x[1], x[2]),
      "xx.xx (xx.xx - xx.xx)" = sprintf("%.2f (%.2f - %.2f)", x[1], x[2], x[3]),
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
