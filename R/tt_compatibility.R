
#' row
#' 
#' @inheritParams rrow
#' 
#' @export
#' 
#' @examples 
#' 
#' tt_rrow("ABC", c(1,2), c(3,2), format = "xx (xx.%)")
#' rrow("")
#' 
rrow = function(row.name = "", ..., format = NULL, indent = 0) {
    vals = list(...)
    if(is.null(row.name))
        row.name = ""
    else if (!is(row.name, "character"))
        stop() #row.name = as.character(row.name)
    if(length(vals) == 0L) {
        LabelRow(lev = as.integer(indent),
                 lab = row.name)
    } else {
        csps = as.integer(sapply(vals, function(x) {
            attr(x, "colspan") %||% 1L
        }))
        ## vals = mapply(function(v, sp) {
        ##     attr(v, "colspan") = NULL
        ##     rep(list(v), sp)
        ## }, v = vals, sp = csps,
        ## SIMPLIFY=FALSE)
        ## vals = unlist(vals,
        ##               recursive = FALSE)
        fmts = sapply(vals, obj_fmt)
        if(is.character(fmts) && length(unique(fmts)) == 1L && is.null(format))
            format = unique(fmts)
        DataRow(val = vals, lev = as.integer(indent), lab = row.name,
                name = row.name, ## XXX TODO
                cspan = csps,
                fmt = format)
    }
}


#' rrowl
#' 
#' @inheritParams rrowl
#' 
#' @export
#' 
#' @examples 
#' rrowl("a", c(1,2,3), format = "xx")
#' rrowl("a", c(1,2,3), c(4,5,6), format = "xx")
#' 
#' 
#' rrowl("N", table(iris$Species))
#' rrowl("N", table(iris$Species), format = "xx")
#' 
#' x <- tapply(iris$Sepal.Length, iris$Species, mean, simplify = FALSE)
#' 
#' rrow(row.name = "row 1", x)
#' rrow("ABC", 2, 3)
#' 
#' rrowl(row.name = "row 1", c(1, 2), c(3,4))
#' rrow(row.name = "row 2", c(1, 2), c(3,4))
rrowl = function (row.name, ..., format = NULL, indent = 0)  {
    dots <- list(...)
    args_list <- c(list(row.name = row.name, format = format, 
        indent = indent), val = unlist(lapply(dots, as.list), recursive = FALSE))
    do.call(rrow, args_list)
}


#' Rcell
#' 
#' @inheritParams rcell
#' 
#' @export
#' 
rcell = function(x, format = NULL, colspan = NULL) {
    ## if(length(x) != 1)
    ##     x = list(x)
    if(!is.null(format))
        attr(x, "format") = format
    if(!is.null(colspan))
        attr(x, "colspan") = colspan
    x
}



##inefficient trash
paste_em_n = function(lst, n, sep = ".") {
    ret = lst[[1]]
    if(n > 1) {
        for(i in 2:n) {
            ret = paste(ret, lst[[i]], sep = sep)
        }
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
        ret = manual_cols( unlist(vals[[ 1 ]] ) )
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

 
    ## fullbusiness = unqvals[[1]]
    ## for(i in 2:nr) {
    ##     nvi = length(unqvals[[i]])
    ##     nfb = length(fullbusiness)
    ##     fullbusiness = paste(rep(fullbusiness, length.out = nvi*nfb),
    ##                          rep(unqvals[[i]], times = rep(nfb, nvi)))
    ## }
    fullcolinfo = manual_cols(.lst = unqvals)
    fullbusiness = names(collect_leaves(coltree(fullcolinfo)))
    wanted = paste_em_n(repvals, nr)
    wantcols = match(wanted, fullbusiness)
    stopifnot(all(!is.na(wantcols)))
    
   
    subset_cols(fullcolinfo, wantcols)
}


#' Create a header
#' 
#' @inheritParams rheader
#' 
#' @export
#' 
#' @examples
#' 
#' h1 <- rheader(c("A", "B", "C"))
#' 
#' h2 <- rheader(
#'   rrow(NULL, rcell("group 1", colspan = 2), rcell("group 2", colspan = 2)),
#'   rrow(NULL, "A", "B", "A", "B")
#' )
#' 
#' h1
#' 
#' h2
#' 
rheader = function(..., format = "xx", .rowlist = NULL) {
    if(!is.null(.rowlist))
        args = .rowlist
    else 
        args = list(...)
    rrows <- if (length(args) == 1 && !is(args[[1]], "TableRow")) {
        list(rrowl(row.name = NULL, val = args[[1]], format = format))
    } else if (are(args, "TableRow")) {
        args
    }

    hrows_to_colinfo(rrows)
}




#' Create a Table
#' 
#' @inheritParams argument_conventions
#' 
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' mtbl <- rtable(
#'     header = rheader(
#'         rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2),
#'                 rcell("Petal.Length", colspan=2)),
#'         rrow(NULL, "mean", "median", "mean", "median")
#'     ),
#'     rrow(
#'         row.name = "All Species",
#'         mean(iris$Sepal.Length), median(iris$Sepal.Length),
#'         mean(iris$Petal.Length), median(iris$Petal.Length),
#'         format = "xx.xx"
#'     )
#' )
#' 
#' mtbl
#' 
#' 
#' # Table with multirow header
#' mtbl <- rtable(
#'   header = rheader(
#'     rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan=2)),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "All Species",
#'     mean(iris$Sepal.Length), median(iris$Sepal.Length),
#'     mean(iris$Petal.Length), median(iris$Petal.Length),
#'     format = "xx.xx"
#'   )
#' )
#' 
#' # TODO Warning message:
#' 
#' mtbl
#' 
#' names(mtbl) # always first row of header
#' 
#' # Single row header
#' 
#'# tbl <- rtable(
#'#   header = rheader(rrow(NULL, rcell("Treatement N=100"), rcell("Comparison N=300"))),
#'#   format = "xx (xx.xx%)",
#'#   rrow("A", c(104, .2), c(100, .4)),
#'#   rrow("B", c(23, .4), c(43, .5)),
#'#   rrow(""),
#'#   rrow("this is a very long section header"),
#'#   rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
#'#   rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
#'# )
#' # TODO: fix
#' # TODO: coerce c(...) to rheader
#' tbl
#' 
#'# row.names(tbl) # TODO # row.lables
#'# names(tbl)
#'# 
#'# 
#'# # Subsetting
#'# tbl[1,2]
#'# tbl[2, 1]
#'# # TODO access to the cell
#'# tbl[[2, 1]] # cell ?
#'# tbl[[c("All Species"), 1]]
#'# tbl[3,2]
#'# tbl[5,1]
#'# tbl[5,2]
#'# tbl[1:3]
#'# 
#'# 
#'# # Data Structure methods
#'# dim(tbl) # TODO
#'# nrow(tbl)
#'# ncol(tbl)
#'# names(tbl)
#'# 
#'# 
#'# # Colspans
#'# 
#'# tbl2 <- rtable(
#'#   c("A", "B", "C", "D", "E"),
#'#   format = "xx",
#'#   rrow("r1", 1, 2, 3, 4, 5),
#'#   rrow("r2", rcell("sp2", colspan = 2), "sp1", rcell("sp2-2", colspan = 2))
#'# )
#'# 
#'# tbl2
#'# 
#'# 
#'# # Custom format with functions (might be deprecated soon)
#'# my_format <- function(x, output) {
#'#    paste(x, collapse = "/")
#'# }
#'# tbl3 <- rtable(
#'#   c("A", "B"),
#'#   format = my_format,
#'#   rrow("row1", c(1,2,3,4), letters[1:10])
#'# )
#'# tbl3
#' @rdname rtable
rtable = function(header, ..., format = NULL) {
    if(is.character(header))
        header = list(rrowl(NULL, header))
    if(is.list(header)) {
        if(are(header, "TableRow"))
            colinfo = hrows_to_colinfo(header)
        else if(are(header, "list"))
            colinfo = do.call(rheader, header)
    } else if(is(header, "InstantiatedColumnInfo")) {
        colinfo = header
    } else if (is(header, "TableRow")) {
        colinfo = hrows_to_colinfo(list(header))
    } else {
        stop("problems")

    }
        
    body = list(...)
    ## XXX this shouldn't be needed. hacky
    if(length(body) == 1 && is.list(body[[1]]))
        body = body[[1]]
    if(are(body, "ElementaryTable") &&
       all(sapply(body, function(tb) {
           nrow(tb) == 1 && obj_name(tb) == ""
       }))) {
        body = lapply(body, function(tb) tree_children(tb)[[1]])
    }
        
    lapply(body, function(x)
        if(any(row_cspans(x) > 1))
            stop("colspans in TableTree bodies are currently not supported.")
        )
    TableTree(kids = body, fmt = format, cinfo = colinfo,
              labrow = LabelRow(lev = 0L, lab = "", vis = FALSE))
}

#' @rdname rtable
#' @export
rtablel = function (header, ..., format = NULL) 
{
    dots <- list(...)
    args_list <- c(list(header = header, format = format), unlist(lapply(dots, 
        as.list), recursive = FALSE))
    do.call(rtable, args_list)
}

rbindl_rtables <- function(x, gap = 0, check_headers = FALSE) {
    if(!check_headers)
        warning("check_headers = FALSE is no longer supported, ignoring.")

    firstcols = col_info(x[[1]])
    if(!all(sapply(x, function(xi) identical(col_info(xi), firstcols))))
        stop("column structure didn't match in rbindl_rtables call. This is no longer supported")

    if(are(x, "ElementaryTable") &&
       all(sapply(x, function(tb) {
           nrow(tb) == 1 && obj_name(tb) == ""
       }))) {
        body = lapply(x, function(tb) tree_children(tb)[[1]])
    }

    
    
    TableTree(kids = x, cinfo = firstcols, name = "rbind_root", lab = "")
    
}

setMethod("rbind", "VTableTree",
          function(..., deparse.level = 1) {
    rbindl_rtables(list(...))
})

setMethod("rbind2", "VTableTree",
          function(x, y) {
    rbindl_rtables(list(x, y))
})


header_add_N = function(x, N) {
    col_counts(x) = as.integer(N)
    colcount_fmt(x) = "(N=xx)"
    disp_ccounts(x) = TRUE
    x
}

#' export
#' @rdname compatability
`header<-` = function(x, value) {
    if(is(value, "list")) {
        value = rheader(.rowlist = value)
    } else if(!is(value, "InstantiatedColumnInfo")) {
        ## XXX we could be more defensive here, some
        ## bad invalid values could get through.
        value = rheader(value)
    }

    stopifnot(ncol(value) == ncol(x))
    ## value must be an InstantiatedColumnInfo object
    ## by this point.

    ## this is recursive so its all we need here.
    col_info(x) = value
    x
}

#' export
#' @rdname compatability
header = col_info
