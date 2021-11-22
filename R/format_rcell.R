
formats_1d <- c(
  "xx", "xx.", "xx.x", "xx.xx", "xx.xxx", "xx.xxxx",
  "xx%", "xx.x%", "xx.xx%", "xx.xxx%", "(N=xx)", ">999.9", ">999.99",
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
#' @return A nested list, with elements listing the supported 1d, 2d, and 3d format strings.
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
#' @note No check if the function is actually a formatter is performed.
#' @return \code{TRUE} if \code{x} is \code{NULL}, a supported format string, or a function; \code{FALSE} otherwise.
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
#' @param format character(1). A format string passed to sprintf.
#'
#' @export
#' @return A formating function which wraps and will apply the specified \code{printf} style format string \code{format}.
#' @seealso \code{\link[base]{sprintf}}
#'
#' @examples
#'
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
#'
sprintf_format <- function(format) {
    function(x,...) {
        do.call(sprintf, c(list(fmt = format), x))
    }
}

## sprintf_format_old <- function(format) {
##   structure(format, "format_type" = "sprintf")
## }



# # @export
# format.rcell <- function(x, ...) {
#   format_rcell(x, ...)
# }


#' Round and prepare a value for display
#'
#' This function is used within \code{\link{format_rcell}} to prepare numeric values within
#' cells for formatting and display.
#'
#' @aliases rounding
#' @param x numeric(1). Value to format
#' @param digits numeric(1). Number of digits to round to, or \code{NA} to convert to a
#' character value with no rounding.
#' @param na_str character(1). The value to return if \code{x} is \code{NA}.
#'
#' @details This function combines the rounding behavior of R's standards-complaint
#' \code{\link{round}} function (see the Details section of that documentation)
#' with the strict decimal display of \code{\link{sprintf}}. The exact behavior
#' is as follows:
#'
#' \enumerate{
#' \item{If \code{x} is NA, the value of \code{na_str} is returned}
#' \item{If \code{x} is non-NA but \code{digits} is NA, \code{x} is converted to a character
#' and returned}
#' \item{If \code{x} and \code{digits} are both non-NA, \code{round} is called first,
#' and then \code{sprintf} is used to convert the rounded value to a character with the
#' appropriate number of trailing zeros enforced.}
#' }
#'
#' @return A character value representing the value after rounding, containing
#' containing any trailling zeros required to display \emph{exactly} \code{digits}
#' elements.
#' @note This differs from the base R \code{\link{round}} function in that \code{NA}
#' digits indicate x should be passed converted to character and returned unchanged
#' whereas \code{round(x, digits =NA)} returns \code{NA} for all values of \code{x}.
#'
#' This behavior will differ from \code{as.character(round(x, digits = digits))}
#' in the case where there are not at least \code{digits} significant digits
#' after the decimal that remain after rounding. It \emph{may} differ from
#' \code{sprintf("%.Nf", x)} for values ending in '5' after the decimal place
#' on many popular operating systems due to \code{round}'s stricter adherence to the
#' IEC 60559 standard, particularly for R versions > 4.0.0 (see Warning in \code{round}
#' documentation).
#'
#' @export
#' @seealso [format_rcell()] [base::round()] [base::sprintf()]
#' @examples
#'
#' round_fmt(0, digits = 3)
#' round_fmt(.395, digits = 2)
#' round_fmt(NA, digits = 1)
#' round_fmt(NA, digits = 1, na_str = "-")
#' round_fmt(2.765923, digits = NA)
round_fmt <- function(x, digits, na_str = "NA") {
    if(!is.na(digits) && digits < 0)
        stop("round_fmt currentlyd does not support non-missing values of digits <0")
    if(is.na(x)) {
        na_str
    } else if(is.na(digits)) {
        paste0(x)
    } else {
        sprfmt <- paste0("%.", digits, "f")
        sprintf(fmt = sprfmt, round(x, digits = digits))
    }
}



val_pct_helper <- function(x, dig1, dig2, na_str, pct = TRUE) {
    if(pct)
        x[2] <- x[2] * 100
    paste0(round_fmt(x[1], digits = dig1, na_str = na_str),
           " (",
           round_fmt(x[2], digits = dig2, na_str = na_str),
           if(pct) "%", ")")
}

sep_2d_helper <- function(x, dig1, dig2, sep, na_str, wrap = NULL) {

    ret <- paste(mapply(round_fmt, x = x, digits = c(dig1, dig2), na_str = na_str),
                 collapse = sep)
    if(!is.null(wrap))
        ret <- paste(c(wrap[1], ret, wrap[2]), collapse = "")
    ret
}

na_or_round <- function(x, digits, na_str) {
    if(is.na(x))
        na_str
    else
        round(x, digits = digits)

}

#' Convert the contets of an \code{\link{rcell}} to a string using the
#' \code{format} information
#'
#' @param x an object of class \code{\link{rcell}}
#' @param na_str character(1). String that should be displayed when the value of \code{x} is missing. Defaults to \code{"NA"}.
#' @inheritParams rtable
#' @param output output type
#'
#' @return formatted text representing the cell \code{x}.
#' @export
#'
#' @seealso [round_fmt()]
#' @examples
#'
#' x <- rcell(pi, format = "xx.xx")
#' x
#'
#' format_rcell(x, output = "ascii")
#'
format_rcell <- function(x, format, output = c("ascii", "html"), na_str = "NA") {
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
                   "xx." = round_fmt(x, digits = 0, na_str = na_str), #as.character(round(x, 0)),
                   "xx.x" = round_fmt(x, digits = 1, na_str = na_str), ##as.character(round(x, 1)),
                   "xx.xx" = round_fmt(x, digits = 2, na_str = na_str), ##as.character(round(x, 2)),
                   "xx.xxx" = round_fmt(x, digits = 3, na_str = na_str), ##as.character(round(x, 3)),
                   "xx.xxxx" = round_fmt(x, digits = 4, na_str = na_str), ##as.character(round(x, 4)),
                   "xx%" = paste0(round_fmt(x*100, digits = NA, na_str = na_str), "%"), #paste0(x * 100, "%"),
                   "xx.%" = paste0(round_fmt(x * 100, digits = 0, na_str = na_str), "%"),
                   "xx.x%" = paste0(round_fmt(x * 100, digits = 1, na_str = na_str), "%"),
                   "xx.xx%" = paste0(round_fmt(x * 100, digits = 2, na_str = na_str), "%"),
                   "xx.xxx%" = paste0(round_fmt(x * 100, digits = 3, na_str = na_str), "%"),
                   "(N=xx)" = paste0("(N=", round_fmt(x, digits = NA, na_str = na_str), ")"),
                   ">999.9" = ifelse(x > 999.9, ">999.9", round_fmt(x, digits = 1, na_str = na_str)), #as.character(round(x, 1))),
                   ">999.99" = ifelse(x > 999.99, ">999.99", round_fmt(x, digits = 2, na_str = na_str)), #as.character(round(x, 2))),
                   "x.xxxx | (<0.0001)" = ifelse(x < 0.0001, "<0.0001", round_fmt(x, digits = 4, na_str = na_str)), #sprintf("%.4f", x)),
                   "xx / xx" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = " / ", na_str = na_str), #paste(x, collapse = " / "),
                   "xx. / xx." = sep_2d_helper(x, dig1 = 0, dig2 = 0, sep = " / ", na_str = na_str), #paste(lapply(x, round_fmt, digits = 0, na_str = na_str), collapse = " / "), #round, 0), collapse = " / "),
                   "xx.x / xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " / ", na_str = na_str), #paste(lapply(x, round_fmt, digits = 1, na_str = na_str), collapse = " / "), #round, 1), collapse = " / "),
                   "xx.xx / xx.xx" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = " / ", na_str = na_str), #paste(lapply(x, round_fmt, digits = 2, na_str = na_str), collapse = " / "), #round, 2), collapse = " / "),
                   "xx.xxx / xx.xxx" = sep_2d_helper(x, dig1 = 3, dig2 = 3, sep = " / ", na_str = na_str), #paste(lapply(x, round_fmt, digits = 3, na_str = na_str), collapse = " / "), #round, 3), collapse = " / "),
                   "xx (xx%)" = val_pct_helper(x, dig1 = NA, dig2 = NA, na_str = na_str), ##
                   "xx (xx.%)" = val_pct_helper(x, dig1 = NA, dig2 = 0, na_str = na_str), ##paste0(x[1], " (", round(x[2]*100, 0), "%)"),
                   "xx (xx.x%)" = val_pct_helper(x, dig1 = NA, dig2 = 1, na_str = na_str), ##paste0(x[1], " (", round(x[2]*100, 1), "%)"),
                   "xx (xx.xx%)" = val_pct_helper(x, dig1 = NA, dig2 = 2, na_str = na_str), ##paste0(x[1], " (", round(x[2]*100, 2), "%)"),
                   "xx. (xx.%)" = val_pct_helper(x, dig1 = 0, dig2 = 0, na_str = na_str), ##paste0(round(x[1],0), " (", round(x[2]*100, 0), "%)"),
                   "xx.x (xx.x%)" = val_pct_helper(x, dig1 = 1, dig2 = 1, na_str = na_str), ##paste0(round(x[1],1), " (", round(x[2]*100, 1), "%)"),
                   "xx.xx (xx.xx%)" = val_pct_helper(x, dig1 = 2, dig2 = 2, na_str = na_str), ##paste0(round(x[1],2), " (", round(x[2]*100, 2), "%)"),
                   "(xx, xx)" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = ", ", na_str = na_str, wrap = c("(", ")")), #paste0("(",x[1],", ", x[2], ")"),
                   "(xx., xx.)" = sep_2d_helper(x, dig1 = 0, dig2 = 0, sep = ", ", na_str = na_str, wrap = c("(", ")")), #paste0("(", paste(lapply(x, round, 0), collapse = ", ") , ")"),
                   "(xx.x, xx.x)" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = ", ", na_str = na_str, wrap = c("(", ")")), #paste0("(", paste(lapply(x, round, 1), collapse = ", ") , ")"),
                   "(xx.xx, xx.xx)" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = ", ", na_str = na_str, wrap = c("(", ")")), #paste0("(", paste(lapply(x, round, 2), collapse = ", ") , ")"),
                   "(xx.xxx, xx.xxx)" = sep_2d_helper(x, dig1 = 3, dig2 = 3, sep = ", ", na_str = na_str, wrap = c("(", ")")), #paste0("(", paste(lapply(x, round, 3), collapse = ", ") , ")"),
                   "(xx.xxxx, xx.xxxx)" = sep_2d_helper(x, dig1 = 4, dig2 = 4, sep = ", ", na_str = na_str, wrap = c("(", ")")), #paste0("(", paste(lapply(x, round, 4), collapse = ", ") , ")"),
                   "xx - xx" = sep_2d_helper(x, dig1 = NA, dig2 = NA, sep = " - ", na_str = na_str), #paste(x[1], "-", x[2]),
                   "xx.x - xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " - ", na_str = na_str), #paste(vapply(x, round, numeric(1), 1), collapse = " - "),
                   "xx.xx - xx.xx" = sep_2d_helper(x, dig1 = 2, dig2 = 2, sep = " - ", na_str = na_str), #paste(vapply(x, round, numeric(1), 2), collapse = " - "),
                   "xx.x (xx.x)" = val_pct_helper(x, dig1 = 1, dig2 = 1, na_str = na_str, pct = FALSE), #paste0(round(x[1], 1), " (",round(x[2], 1), ")"),
                   "xx.xx (xx.xx)" = val_pct_helper(x, dig1 = 2, dig2 = 2, na_str = na_str, pct = FALSE), #paste0(round(x[1], 2), " (",round(x[2], 2), ")"),
                   "xx.x, xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = ", ", na_str = na_str), #paste(vapply(x, round, numeric(1), 1), collapse = ", "),
                   "xx.x to xx.x" = sep_2d_helper(x, dig1 = 1, dig2 = 1, sep = " to ", na_str = na_str), #paste(vapply(x, round, numeric(1), 1), collapse = " to "),
                   "xx.xx (xx.xx - xx.xx)" = paste0(round_fmt(x[1], digits = 2, na_str = na_str), " ",
                                                    sep_2d_helper(x[2:3], dig1 = 2, dig2 = 2,
                                                                  sep = " - ", na_str = na_str,
                                                                  wrap = c("(", ")"))),
                   paste("format string", format, "not found")
               )
           } else if (is.function(format)) {
               format(x, output = output)
           }
    txt[is.na(txt)] <- na_str
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
