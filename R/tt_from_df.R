#' Create `ElementaryTable` from data.frame
#'
#' @param df data.frame.
#'
#' @return an \code{ElementaryTable} object with unnested columns corresponding to
#' \code{names(df)} and row labels corresponding to \code{row.names(df)}.
#'
#' @details
#' If row names are not defined in `df` (or they are simple numbers), then the 
#' row names are taken from the column `label_name`, if exists. If `label_name` exists, 
#' then it is also removed from the original data. Remember that this behavior is 
#' compatible with [as_result_df()], when `as_is = TRUE` and the row names are not unique.
#'
#' @seealso [as_result_df()] for the inverse operation.
#'
#' @examples
#' df_to_tt(mtcars)
#'
#' @export
df_to_tt <- function(df) {
  colnms <- colnames(df)
  cinfo <- manual_cols(colnms)
  rnames <- rownames(df)
  havern <- !is.null(rnames)

  if ((!havern || all(grepl("[0-9]+", rnames))) &&
    "label_name" %in% colnms) {
    rnames <- df$label_name
    df <- df[, -match("label_name", colnms)]
    colnms <- colnames(df)
    cinfo <- manual_cols(colnms)
    havern <- TRUE
  }

  kids <- lapply(seq_len(nrow(df)), function(i) {
    rni <- if (havern) rnames[i] else ""
    do.call(rrow, c(list(row.name = rni), unclass(df[i, ])))
  })

  ElementaryTable(kids = kids, cinfo = cinfo)
}
