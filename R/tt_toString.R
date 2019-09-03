setGeneric("toString", function(x,...) standardGeneric("toString"))

ttrow_to_str = function(ttrow, nchar_rownames = 20, nchar_col = 5, gap =1 , indent.unit = 1L,  ...) {

    indent_lvl = length(pos_splvals(ttrow))
    row.name <-  if (nzchar(obj_label(ttrow))) {
                     paste(c(rep(" ", indent_lvl *indent.unit),
                             obj_label(ttrow)), collapse = "")
                 } else {
                     ""
                 }
    if (length(leaf_values(ttrow)) == 0) {
        row.name
    } else {
        rowfmt = obj_fmt(ttrow)
        cells <- lapply(leaf_values(ttrow), function(val, rwfmt) {
            ## XXX TODO do this for realg
            fmt = obj_fmt(val)
            if(is.null(fmt) && !is.null(rowfmt))
                fmt = rowfmt
            ## compatibility layer hack. do this right eventually
            cell = rcell(val, fmt)
            unlist(strsplit(format_rcell(cell, output = "ascii"), "\n", fixed = TRUE))
    }, rwfmt = rowfmt)
    
    nlines <- max(vapply(cells,length, numeric(1)))
    
    cells_same_length <- lapply(cells, function(x) {
      if (length(x) == nlines) {
        x
      } else {
        c(x, rep(NA_character_, nlines -length(x)))
      }
    })

        ##XXX TODO do this for real
    colspans <- rep(1, length(cells)) ##vapply(ttrow, function(cell) attr(cell, "colspan"), numeric(1))
    lines <- as.matrix(Reduce(cbind, cells_same_length))
    
    row_char <- paste0(
      padstr(row.name, nchar_rownames, "left"),
      paste(unlist(Map(function(cell, colspan) {
        list(spaces(gap), padstr(cell, colspan*nchar_col+ (colspan-1)*gap))
      }, lines[1,], colspans)), collapse = "")
    )
    
    if (nrow(lines) > 1) {
      additional_rows <- apply(lines[-1, , drop=FALSE], 1, function(r) {
        paste0(
          spaces(nchar_rownames),
          paste(unlist(Map(function(cell, colspan) {
            list(spaces(gap), padstr(cell, colspan*nchar_col + (colspan-1)*gap))
          }, r, colspans)), collapse = "")
        )
      })
      row_char <- paste(c(row_char, additional_rows), collapse = "\n")
    }
    row_char
  } 



}
setMethod("toString", "TableRow",
          function(x, ...) {
    ttrow_to_str(x, ...)
})

## preserve S3 behavior
setMethod("toString", "ANY", base:::toString)

.collapse_cspans = function(rvals) {
    n = length(rvals)
    ## cumsum trick to build a factor that increments
    ## anytime a logical vector is true, thus creating
    ## categorical variable from TRUE/FALSE event data
    inds = cumsum(mapply(identical, x = rvals[-1], y = rvals[-n]))
    inds = c(inds, max(inds)) ## last element is always ok

    spl = split(rvals, inds)
    cells = lapply(spl,
                   function(x) rcell(x[[1]], colspan = length(x),
                                     format = NULL))
    

    cells
    

}


.make_header_rrows = function(tt) {
    clyt = clayout(tt)
    leaves = collect_leaves(clyt, incl.cont = FALSE)
    vals = lapply(seq_along(leaves),
                  function(i) pos_splvals(leaves[[i]]))
    
    nvals = length(vals[[1]])
    numcols = length(vals)
    
    rrows = lapply(seq(1, nvals),
                   function(i) {
        
                    lst = c( row.name = NULL, format = NULL,
                            .collapse_cspans(lapply(vals, function(vlst) vlst[[i]])))
                    do.call(rrow, lst)
                    }) 
    rrows
}


setMethod("toString", "TableTree",
          function(x, ...) {



})
