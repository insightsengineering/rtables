
#' row
#' 
#' @inheritParams rrow
#' 
#' @export
#' 
#' @examples 
#' 
#' tt_rrow("ABC", c(1,2), c(3,2), format = "xx (xx.%)")
#' tt_rrow("")
#' 
tt_rrow = function(row.name, ..., format = NULL, indent = 0) {
    vals = list(...)
    if(is.null(row.name))
        row.name = ""
    DataRow(val = vals, lev = as.integer(indent), lab = row.name,
            name = row.name, ## XXX TODO
            cspan = as.integer(sapply(vals, function(x) {
                sp = attr(x, "colspan")
                if(is.null(sp))
                    sp = 1L
                sp
            })),
            fmt = format)
}


#' rrowl
#' 
#' @inheritParams 
#' 
#' @export
#' 
#' @examples 
#' tt_rrowl("a", c(1,2,3), format = "xx")
#' tt_rrowl("a", c(1,2,3), c(4,5,6), format = "xx")
#' 
#' 
#' tt_rrowl("N", table(iris$Species))
#' tt_rrowl("N", table(iris$Species), format = "xx")
#' 
#' x <- tapply(iris$Sepal.Length, iris$Species, mean, simplify = FALSE)
#' 
#' tt_rrow(row.name = "row 1", x)
#' tt_rrow("ABC", 2, 3)
#' 
#' tt_rrowl(row.name = "row 1", c(1, 2), c(3,4))
#' tt_rrow(row.name = "row 2", c(1, 2), c(3,4))
tt_rrowl = function (row.name, ..., format = NULL, indent = 0)  {
    dots <- list(...)
    args_list <- c(list(row.name = row.name, format = format, 
        indent = indent), val = unlist(lapply(dots, as.list), recursive = FALSE))
    do.call(tt_rrow, args_list)
}


#' Rcell
#' 
#' @inheritParams 
#' 
#' @export
#' 
tt_rcell = function(x, format = NULL, colspan = NULL) {
    if(length(x) != 1)
        x = list(x)
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


#' Create a header
#' 
#' @inheritParams rheader
#' 
#' @export
#' 
#' @examples
#' 
#' h1 <- tt_rheader(c("A", "B", "C"))
#' 
#' h2 <- tt_rheader(
#'   tt_rrow(NULL, rcell("group 1", colspan = 2), rcell("group 2", colspan = 2)),
#'   tt_rrow(NULL, "A", "B", "A", "B")
#' )
#' 
#' h1
#' 
#' h2
#' 
tt_rheader = function(..., format = "xx") {
    args = list(...)
    rrows <- if (length(args) == 1 && !is(args[[1]], "rrow")) {
        list(tt_rrowl(row.name = NULL, val = args[[1]], format = format))
    } else if (are(args, "TableRow")) {
        args
    }

    hrows_to_colinfo(rrows)
}


#' Create a Table
#' 
#' @inheritParams rtable
#' 
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' mtbl <- tt_rtable(
#'     header = tt_rheader(
#'         tt_rrow(row.name = NULL, tt_rcell("Sepal.Length", colspan = 2),
#'                 tt_rcell("Petal.Length", colspan=2)),
#'         tt_rrow(NULL, "mean", "median", "mean", "median")
#'     ),
#'     tt_rrow(
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
#' mtbl <- tt_rtable(
#'   header = tt_rheader(
#'     tt_rrow(row.name = NULL, tt_rcell("Sepal.Length", colspan = 2), tt_rcell("Petal.Length", colspan=2)),
#'     tt_rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   tt_rrow(
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
#' tbl <- tt_rtable(
#'   header = tt_rheader(tt_rrow(NULL, tt_rcell("Treatement N=100"), tt_rcell("Comparison N=300"))),
#'   format = "xx (xx.xx%)",
#'   tt_rrow("A", c(104, .2), c(100, .4)),
#'   tt_rrow("B", c(23, .4), c(43, .5)),
#'   tt_rrow(""),
#'   tt_rrow("this is a very long section header"),
#'   tt_rrow("estimate", tt_rcell(55.23, "xx.xx", colspan = 2)),
#'   tt_rrow("95% CI", indent = 1, tt_rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
#' )
#' # TODO: fix
#' # TODO: coerce c(...) to tt_rheader
#' tbl
#' 
#' row.names(tbl) # TODO # row.lables
#' names(tbl)
#' 
#' 
#' # Subsetting
#' tbl[1,2]
#' tbl[2, 1]
#' # TODO access to the cell
#' tbl[[2, 1]] # cell ?
#' tbl[[c("All Species"), 1]]
#' tbl[3,2]
#' tbl[5,1]
#' tbl[5,2]
#' tbl[1:3]
#' 
#' 
#' # Data Structure methods
#' dim(tbl) # TODO
#' nrow(tbl)
#' ncol(tbl)
#' names(tbl)
#' 
#' 
#' # Colspans
#' 
#' tbl2 <- tt_rtable(
#'   c("A", "B", "C", "D", "E"),
#'   format = "xx",
#'   tt_rrow("r1", 1, 2, 3, 4, 5),
#'   tt_rrow("r2", tt_rcell("sp2", colspan = 2), "sp1", tt_rcell("sp2-2", colspan = 2))
#' )
#' 
#' tbl2
#' 
#' 
#' # Custom format with functions (might be deprecated soon)
#' my_format <- function(x, output) {
#'    paste(x, collapse = "/")
#' }
#' tbl3 <- tt_rtable(
#'   c("A", "B"),
#'   format = my_format,
#'   tt_rrow("row1", c(1,2,3,4), letters[1:10])
#' )
#' tbl3
#' 
tt_rtable = function(header, ..., format = NULL) {
    if(is.character(header))
        header = list(tt_rrowl(NULL, header))
    if(is.list(header)) {
        if(are(header, "TableRow"))
            colinfo = hrows_to_colinfo(header)
        else if(are(header, "list"))
            colinfo = do.call(tt_rheader, header)
    } else if(is(header, "InstantiatedColumnInfo")) {
        colinfo = header
    } else {
        stop("problems")

    }
        
    body = list(...)
    TableTree(kids = body, fmt = format, cinfo = colinfo,
              labrow = LabelRow(lev = 0L, lab = "", vis = FALSE))
}
