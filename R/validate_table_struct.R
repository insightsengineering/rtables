#' Find degenerate (sub)structures within a table
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function returns a list with the row-paths to all structural subtables which contain no data rows (even if
#' they have associated content rows).
#'
#' @param tt (`TableTree`)\cr a `TableTree` object.
#'
#' @return A list of character vectors representing the row paths, if any, to degenerate substructures within the table.
#'
#' @examples
#' find_degen_struct(rtable("hi"))
#'
#' @export
find_degen_struct <- function(tt) {
  degen <- list()

  recurse_check <- function(tti, path) {
    if (is(tti, "VTableTree")) {
      kids <- tree_children(tti)
      if (length(kids) == 0) {
        degen <<- c(degen, list(path))
      } else {
        for (i in seq_along(kids)) {
          recurse_check(kids[[i]], path = c(path, names(kids)[i]))
        }
      }
    }
  }
  recurse_check(tt, obj_name(tt) %||% "root")
  degen
}

#' Validate and assert valid table structure
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A `TableTree` (`rtables`-built table) is considered degenerate if:
#' \enumerate{
#'   \item{It contains no subtables or data rows (content rows do not count).}
#'   \item{It contains a subtable which is degenerate by the criterion above.}
#' }
#'
#' `validate_table_struct` assesses whether `tt` has a valid (non-degenerate) structure.
#'
#' `assert_valid_table` asserts a table must have a valid structure, and throws an informative error (the default) or
#' warning (if `warn_only` is `TRUE`) if the table is degenerate (has invalid structure or contains one or more
#' invalid substructures.
#'
#' @param tt (`TableTree`)\cr a `TableTree` object.
#'
#' @return
#' * `validate_table_struct` returns a logical value indicating valid structure.
#' * `assert_valid_table` is called for its side-effect of throwing an error or warning for degenerate tables.
#'
#' @note This function is experimental and the exact text of the warning/error is subject to change in future releases.
#'
#' @examples
#' validate_table_struct(rtable("hahaha"))
#' \dontrun{
#' assert_valid_table(rtable("oops"))
#' }
#'
#' @export
validate_table_struct <- function(tt) {
  degen_pths <- find_degen_struct(tt)
  length(degen_pths) == 0
}

## XXX this doesn't handle content paths correctly
.path_to_disp <- function(pth) {
  if (length(pth) == 1) {
    return(pth)
  }
  has_cont <- any(pth == "@content")
  if (has_cont) {
    contpos <- which(pth == "@content")
    cont_disp <- paste(tail(pth, length(pth) - contpos + 1),
      collapse = "->"
    )
    pth <- head(pth, contpos)
  } else {
    cont_disp <- character()
  }

  topaste <- character(0)
  fullpth <- pth
  while (length(pth) > 0) {
    if (length(pth) <= 1) {
      topaste <- c(topaste, pth)
      pth <- character()
    } else {
      topaste <- c(topaste, sprintf("%s[%s]", pth[1], pth[2]))
      pth <- tail(pth, -2)
    }
  }
  topaste <- c(topaste, cont_disp)
  paste(topaste, collapse = "->")
}

no_analyze_guess <- paste0(
  "Was this table created using ",
  "summarize_row_groups but no calls ",
  "to analyze?\n"
)

use_sanitize_msg <- paste("  Use sanitize_table_struct() to fix these issues")

make_degen_message <- function(degen_pths, tt) {
  msg <- sprintf(
    paste0(
      "Invalid table - found %d (sub)structures which contain no data rows.",
      "\n\tThe first occured at path:  %s"
    ),
    length(degen_pths), .path_to_disp(degen_pths[[1]])
  )
  if (length(degen_pths) == 1 && length(degen_pths[[1]]) == 1) {
    msg <- paste(msg, "  Likely Cause: Empty data or first row split on variable with only NA values",
      sep = "\n"
    )
  } else if (all(make_row_df(tt)$node_class %in% c("LabelRow", "ContentRow"))) {
    msg <- paste(msg, "  Cause: Layout did not contain any analyze() calls (only summarize_row_groups())",
      sep = "\n"
    )
  }
  msg <- paste(msg, use_sanitize_msg, sep = "\n")
  msg
}

#' @param warn_only (`flag`)\cr whether a warning should be thrown instead of an error. Defaults to `FALSE`.
#'
#' @rdname validate_table_struct
#' @export
assert_valid_table <- function(tt, warn_only = FALSE) {
  degen_pths <- find_degen_struct(tt)
  if (length(degen_pths) == 0) {
    return(TRUE)
  }

  ## we failed, now we build an informative error/warning message
  msg <- make_degen_message(degen_pths, tt)

  if (!warn_only) {
    stop(msg)
  }
  warning(msg)
  return(FALSE)
}

#' Sanitize degenerate table structures
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Experimental function to correct structure of degenerate tables by adding messaging rows to empty sub-structures.
#'
#' @param tt (`TableTree`)\cr a `TableTree` object.
#' @param empty_msg (`string`)\cr the string which should be spanned across the inserted empty rows.
#'
#' @details
#' This function locates degenerate portions of the table (including the table overall in the case of a table with no
#' data rows) and inserts a row which spans all columns with the message `empty_msg` at each one, generating a table
#' guaranteed to be non-degenerate.
#'
#' @return If `tt` is already valid, it is returned unmodified. If `tt` is degenerate, a modified, non-degenerate
#' version of the table is returned.
#'
#' @examples
#' sanitize_table_struct(rtable("cool beans"))
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("SEX") %>%
#'   summarize_row_groups()
#'
#' ## Degenerate because it doesn't have any analyze calls -> no data rows
#' badtab <- build_table(lyt, DM)
#' sanitize_table_struct(badtab)
#'
#' @export
sanitize_table_struct <- function(tt, empty_msg = "-- This Section Contains No Data --") {
  rdf <- make_row_df(tt)

  emptyrow <- DataRow(
    vals = list(empty_msg),
    name = "empty_section",
    label = "",
    cspan = ncol(tt),
    cinfo = col_info(tt),
    format = "xx",
    table_inset = table_inset(tt)
  )
  degen_pths <- find_degen_struct(tt)

  if (identical(degen_pths, list("root"))) {
    tree_children(tt) <- list(empty_row = emptyrow)
    return(tt)
  }

  for (pth in degen_pths) {
    ## FIXME this shouldn't be necessary. why is it?
    tti <- tt_at_path(tt, path = pth)
    tree_children(tti) <- list(empty_section = emptyrow)
    tt_at_path(tt, path = pth) <- tti
  }
  tt
}
