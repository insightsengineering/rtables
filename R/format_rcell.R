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
    
    if (format == ">999.9") {
      ifelse(x > 999.9, ">999.9", sprintf("%.1f", x))
    } else if (format == ">999.99") {
      ifelse(x > 999.99, ">999.99", sprintf("%.2f", x))
    } else if (format == "x.xxxx | (<0.0001)") {
      ifelse(x < 0.0001, "<0.0001", sprintf("%.4f", x))
    } else {
      eval(parse_format_label(format))
    }
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

parse_format_label <- function(format) {
  pattern <- "xx\\.x*%{0,1}|xx%{0,1}"
  placeholders <- unlist(str_extract_all(format, pattern))
  is_pct_format <- grepl("%", placeholders)
  non_placeholders <- unlist(strsplit(format, pattern))
  n_decimal <- unlist(ifelse(
    placeholders %in% c("xx", "xx%"),
    NA,
    lapply(str_extract_all(placeholders, "\\.x*"), nchar)
  ))-1L
  format_strings <- ifelse(
    is.na(n_decimal),
    ifelse(is_pct_format, "%s%%", "%s"),
    paste0("%.", n_decimal, "f", ifelse(is_pct_format, "%%", ""))
  )
  fmt <- paste(interleave(non_placeholders, format_strings), collapse = "")
  if (length(placeholders) == 1L) {
    arg <- if (is_pct_format) quote(x*100) else quote(x)
    bquote(sprintf(.(fmt), .(arg)))
  } else {
    args <- Map(function(i, is_pct) {
      if (is_pct) bquote(x[.(i)]*100) else bquote(x[.(i)])
    }, seq_along(placeholders), is_pct_format)
    as.call(c(quote(sprintf), fmt, args))
  }
}
