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
    ##
    ## the meaning of inds is
    ## (2nd different from first, 3rd diff from as second, ...)
    diffinds = cumsum(!mapply(identical, x = rvals[-1], y = rvals[-n]))
    inds = c(1, diffinds + 1) ## first element is always a new entry

    spl = split(rvals, inds)
    cells = lapply(spl,
                   function(x) rcell(x[[1]], colspan = length(x),
                                     format = NULL))
    

    cells
    

}


.make_s3_header = function(tt) {
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
    rheaderl(rrows)
}


## temporary compatability layer to leverage existing
## ascii-rendering functionality
##
## We will want to replace this is if the S4 approach is
## adopted fully

setGeneric("to_s3compat", function(obj, ...) standardGeneric("to_s3compat"))
setMethod("to_s3compat", "TableRow",
           function(obj, ...)
{
    cells = mapply( function(val, span) {
        fmt = attr(val, "format")
        if(is.null(fmt))
            fmt = obj_fmt(obj)
        rcell(val, format = fmt, colspan = span)
    }, val = row_values(obj),
    span = row_cspans(obj),
    SIMPLIFY = FALSE)
    rrowl(row.name = obj_label(obj),
          cells, 
          indent = tt_level(obj),
          format = obj_fmt(obj))
    
})

setMethod("to_s3compat", "TableTree",
          function(obj, ...) {
    header = .make_s3_header(obj)
    rows = lapply(collect_leaves(obj, incl.cont = TRUE),
                  to_s3compat)
    rtablel(header = header, rows, format = obj_fmt(obj))
})
## this is currently implemented as a
## light-as-possible compatibility layer on top
## of the existing toString logic.
##
## That will likely eventually want to change, but for now
## it is sufficient.
setMethod("toString", "TableTree",
          function(x, ...) {

    header <- .make_header_rrows(x)
    body <- x
    
    header_body <- c(header, body)
    
    nchar_rownames <- max_nchar_rownames(header_body, indent.unit = indent.unit)
    
    nchar_col <- max_nchar_cols(header_body)
    
    txt_header <- lapply(header, function(row) {
        row_to_str(row, nchar_rownames, nchar_col, gap, indent.unit)
    })
    
    txt_body <- lapply(x, function(row) {
        row_to_str(row, nchar_rownames, nchar_col, gap, indent.unit)
    })
    
    
    paste(
        c(
            txt_header,
            paste(rep("-", nchar_rownames + ncol(x)*(nchar_col + gap)), collapse = ""),
            txt_body
        ),
        collapse = "\n"
    )  
})
