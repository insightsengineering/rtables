
add_analyzed_var = function(lyt, var, lbl = var, afun,
                            fmt = NULL,
                            rowlabs = "",
                            newtoplev = FALSE,
                            inclNAs = FALSE) {
  spl = AnalyzeVarSplit(var, lbl,
                        afun = afun,
                        splfmt = fmt,
                        defrowlab = rowlabs,
                        inclNAs = inclNAs)
  .Deprecated("add_analyzed_vars")
  
  if(!newtoplev &&
     (is(last_rowsplit(lyt), "AnalyzeVarSplit") ||
      is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
    cmpnd_last_rowsplit(lyt, spl)
  } else {
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
  }
}


