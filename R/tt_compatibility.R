tt_rrow = function(row.name, ..., format = NULL, indent = 0) {
    vals = list(...)
    if(is.null(row.name))
        row.name = ""
    DataRow(val = vals, lev = as.integer(indent), lab = row.name,
             cspan = as.integer(sapply(vals, function(x) {
                 sp = attr(x, "colspan")
                 if(is.null(sp))
                     sp = 1L
                 sp
             })))
}


tt_rowl = function (row.name, ..., format = NULL, indent = 0) 
{
    dots <- list(...)
    args_list <- c(list(row.name = row.name, format = format, 
        indent = indent), unlist(lapply(dots, as.list), recursive = FALSE))
    do.call(tt_rrow, args_list)
}

tt_rcell = function(x, format = NULL, colspan = NULL) {
    if(!is.null(format))
        attr(x, "format") = format
    if(!is.null(colspan))
        attr(x, "colspan") = colspan
    x
}



##inefficient trash
paste_em_n = function(lst, n) {
    ret = character()
    for(i in seq_len(n)) {
        ret = paste(ret, lst[[i]])
    }
    ret
}


hrows_to_colinfo = function(rows) {
    nr = length(rows)
    stopifnot(nr > 0)
    cspans = lapply(rows, row_cspans)
    vals = lapply(rows, function(x) unlist(row_values(x)))
    unqvals = lapply(vals, unique)
    fmts = lapply(rows, obj_fmt)
    counts = NULL
    if( fmts[ nr ] == "(N=xx)" ) { ## count row
        counts = vals[[ nr ]]
        vals = vals[ -nr ]
        cspans = cspans[ -nr ]
        nr = nr - 1
    }
    ## easiest case, one header row no counts. we're done
    ## XXX could one row but cspan ever make sense????
    ## I don't think so?
    if(nr == 1) { ## && all(cspans == 1L)) {
        ret = manual_cols( vals[[ 1 ]] )
        if(!is.null(counts)) {
            col_counts(ret) = counts
            disp_ccounts(ret) = TRUE
        }
        return(ret)
    }
    ## second easiest case full repeated nestin
    repvals = mapply(function(v, csp) rep(v, times = csp),
                     v= vals, csp = cspans, SIMPLIFY=FALSE)

    ## nr > 1 here
    fullnest = TRUE
    for(i in 2:nr) {
        psted = paste_em_n(repvals, i-1)
        spl = split(repvals[[i]], psted)
        if( !all( sapply(spl, function(x)
                 identical(x, spl[[1]]) ) ) ) {
            
            fullnest = FALSE
            break
        }
    }

    ## if its full nesting we're done, so put
    ## the counts on as necessary and return.
    if( fullnest ) {
        ret = manual_cols(.lst = unqvals)
        if(!is.null(counts)) {
            col_counts(ret) = counts
            disp_ccounts(ret) = TRUE
        }
        return(ret)
    }

    ## booo. the fully complex case where the multiple rows
    ## really don't represent nesting at all, each top level
    ## can have different sub labels

    ## we will build it up as if it were full nesting and then prune
    ## based on the columns we actually want.

 
    fullbusiness = unqvals
    for(i in 2:nr) {
        nvi = length(unqvals[[i]])
        nfb = length(fullbusiness)
        fullbusiness = paste(rep(fullbusiness, length.out = nvi*nfb),
                             rep(unqvals[[i]], times = rep(nfb, nvi)))
    }

    wanted = paste_em_n(repvals, nr)
    wantcols = match(wanted, fullbusiness)
    stopifnot(all(!is.na(wantcols)))
    
    fullcolinfo = manual_cols(.lst = unqvals)
    subset_cols(fullcolinfo, wantcols)
}


tt_rheader = function(..., format = "xx") {
    args = list(...)
    rrows <- if (length(args) == 1 && !is(args[[1]], "rrow")) {
        list(tt_rrowl(row.name = NULL, args[[1]], format = format))
    } else if (are(args, "TableRow")) {
        args
    }

    hrows_to_colinfo(rrows)
}
    
tt_rtable = function(header, ..., format = NULL) {
    if(is.list(header)) {
        if(are(header, "TableRow"))
            colinfo = hrows_to_colinfo(rrows)
        else if(are(header, "list"))
            colinfo = do.call(tt_rheader, header)
    } else if(is(header, "InstantiatedColumnInfo")) {
        colinfo = header
    } else {
        stop("problems")

    }
        
    body = list(...)
    TableTree(kids = body, fmt = format, cinfo = colinfo)
}
