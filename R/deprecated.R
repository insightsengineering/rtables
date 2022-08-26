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
# nocov end
