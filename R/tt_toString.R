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

.collapse_cspans = function(rvals, breakpts) {
    n = length(rvals)
    ## cumsum trick to build a factor that increments
    ## anytime a logical vector is true, thus creating
    ## categorical variable from TRUE/FALSE event data
    ##
    ## the meaning of inds is
    ## (2nd different from first, 3rd diff from as second, ...)
    breaklgl = !mapply(identical, x = rvals[-1], y = rvals[-n])
    ## add ones we've already found
    breaklgl[breakpts - 1] = TRUE
    #0 combines with first run
    inds = c(0,cumsum(breaklgl)) 
    breakpts = which(breaklgl) + 1 
    
    spl = split(rvals, inds)
    cells = lapply(spl,
                   function(x) rcell(x[[1]], colspan = length(x),
                                     format = NULL))
    

    list(cells = cells, breakpts = breakpts)
    

}


.make_s3_header2 = function(tt) {
    clyt = coltree(tt)

    atrow = 1
    rows = list()
    kids = tree_children(clyt)
    while(length(kids) > 0 && atrow < 1000) { #we will break from this
   
        labs = names(kids)
        cells = lapply(names(kids), function(x) {
            rcell(x, colspan = length(collect_leaves(kids[[x]], incl.cont = FALSE)))
        })
        rows[[atrow]] = rrowl(row.name = "",
                              cells)
        atrow = atrow + 1
        ## otherwise its pasted with the next level...
        ## XXX todo figure out a better way to do this
        names(kids) = NULL
        kids = unlist(lapply(kids,
                             function(k) {
            if(is(k, "LayoutColLeaf"))
                NULL
            else
                tree_children(k)
        }),
        recursive = FALSE)
    }
    rheaderl(rows)
}



.make_s3_header = function(tt) {
    clyt = coltree(tt)
    dispcounts = disp_ccounts(tt)
    
    leaves = collect_leaves(clyt, incl.cont = FALSE)
    vals = lapply(seq_along(leaves),
                  function(i) splv_rawvalues(pos_splvals(leaves[[i]])))
    
    nvals = length(vals[[1]])
    numcols = length(vals)


    breakpts = 1 ## first value is always a "new" value
    rrows = vector("list",length = nvals + as.integer(dispcounts))
    for(i in seq(1, nvals)) {
        ret = .collapse_cspans(lapply(vals, function(vlst) vlst[[i]]), breakpts)
        breakpts = ret$breakpts
        args = c(list(row.name = NULL, format = NULL),
                                        ret$cells)
        rrows[[i]] = do.call(rrow, args)
    }
    if(dispcounts) {
        ccounts = col_counts(tt)
        countcells = lapply(ccounts, rcell,
                            format = colcount_fmt(tt))                            
        rrows[[nvals + 1L]] = do.call(rrow, c(countcells, row.name = NULL, format = NULL))

    }
        
    ## rrows = lapply(seq(1, nvals),
    ##                function(i) {
        
    ##                 lst = c( row.name = NULL, format = NULL,
    ##                         .collapse_cspans(lapply(vals, function(vlst) vlst[[i]])))
    ##                 do.call(rrow, lst)
    ##                 }) 
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

setMethod("to_s3compat", "LabelRow",
           function(obj, ...)
{
    rrowl(row.name = obj_label(obj),
          list(), 
          indent = tt_level(obj),
          format = obj_fmt(obj))
    
})

setMethod("to_s3compat", "VTableTree",
          function(obj, ...) {
    header = .make_s3_header2(obj)
    rows = lapply(collect_leaves(obj, incl.cont = TRUE, add.labrows = TRUE),
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
