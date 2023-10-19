#' Create `ElementaryTable` from data.frame
#' @param df data.frame.
#' @return an \code{ElementaryTable} object with unnested columns corresponding to
#' \code{names(df)} and row labels corresponding to \code{row.names(df)}
#' @examples
#' df_to_tt(mtcars)
#' @export
df_to_tt <- function(df) {
  colnms <- colnames(df)
  cinfo <- manual_cols(colnms)
  rnames <- rownames(df)
  havern <- !is.null(rnames)
  kids <- lapply(seq_len(nrow(df)), function(i) {
    rni <- if(havern) rnames[i] else ""
    do.call(rrow, c(list(row.name = rni), unclass(df[i, ])))
  })
  ElementaryTable(kids = kids, cinfo = cinfo)
}
