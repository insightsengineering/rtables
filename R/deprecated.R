#nocov start
add_analyzed_var <- function(lyt, var, label = var, afun,
                             format = NULL,
                             rowlabs = "",
                             nested = FALSE,
                             inclNAs = FALSE) {
  spl <- AnalyzeVarSplit(var, label,
    afun = afun,
    split_format = format,
    defrowlab = rowlabs,
    inclNAs = inclNAs)
  .Deprecated("analyze")

  if(!nested &&
    (is(last_rowsplit(lyt), "AnalyzeVarSplit") ||
      is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
    cmpnd_last_rowsplit(lyt, spl)
  } else {
    pos <- next_rpos(lyt, nested)
    split_rows(lyt, spl, pos)
  }
}

#' Trim Zero Rows
#'
#' @param tbl table object
#'
#' @return an `rtable` object
#'
#' @export
trim_zero_rows <- function(tbl) {
  .Deprecated(
    new = "prune_table(tbl, all_zero) or prune_table(tbl, prune_zeros_only)",
    old = "trim_zero_rows(tbl)"
  )

  stopifnot(is(tbl, "VTableTree"))

  rows <- collect_leaves(tbl, TRUE, TRUE)
  torm <- vapply(rows, function(x) {
    identical(unname(unlist(row_values(x))), rep(0L, ncol(tbl)))
  }, NA, USE.NAMES = FALSE)
  tbl[!torm, , keep_topleft = TRUE]

}
# nocov end
