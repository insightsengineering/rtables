
#' row
#' 
#' @inheritParams argument_conventions
#' 
#' @export
#' 
#' @examples 
#' 
#' rrow("ABC", c(1,2), c(3,2), format = "xx (xx.%)")
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
                 lab = row.name,
                 name = row.name)
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
#' @inheritParams argument_conventions
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
#' @inheritParams argument_conventions
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
#' @inheritParams argument_conventions
#' 
#' @export
#'
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


.char_to_hrows = function(hdr) {
    nlfnd = grep("\n", hdr, fixed = TRUE)
    if(length(nlfnd) == 0)
        return(list(rrowl(NULL, hdr)))

    stopifnot(length(nlfnd) == length(hdr))
    raw = strsplit(hdr, "\n", fixed = TRUE)
    lens = unique(sapply(raw, length))
    stopifnot(length(lens) == 1L)
    lapply(seq(1, lens),
           function(i) {
        rrowl(NULL, vapply(raw, `[`, NA_character_, i = i))
    })
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
#' # tbl
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
#' 
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
        header = .char_to_hrows(header) #list(rrowl(NULL, header))
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
    i = 1
    while(no_colinfo(firstcols) &&
          i < length(x))
        firstcols <- col_info(x[[i]])
    
    lapply(x, function(xi) chk_compat_cinfos(firstcols, col_info(xi)))


    ## if we got only ElementaryTable and
    ## TableRow objects, construct a new
    ## elementary table with all the rows
    ## instead of adding nesting.
    if(all(sapply(x, function(xi) {
        (is(xi, "ElementaryTable") && !labrow_visible(xi) ) ||
            is(xi, "TableRow") && !is(xi, "LabelRow")}))) {
        x <- unlist(lapply(x, function(xi) {
            if(is(xi, "TableRow"))
                xi
            else
                tree_children(xi)
        }))
    }


    
    
    TableTree(kids = x, cinfo = firstcols, name = "rbind_root", lab = "")
    
}

setMethod("rbind", "VTableNodeInfo",
          function(..., deparse.level = 1) {
    rbindl_rtables(list(...), check_headers = TRUE)
})

setMethod("rbind2", "VTableNodeInfo",
          function(x, y) {
    rbindl_rtables(list(x, y), check_headers = TRUE)
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

# NOTE: header <- col_info breaks pkgdown::build_reference_index
#' export
#' @rdname compatability
header <- function(...) col_info(...)


#' Add column representing all rows
#' 
#' @export
#' @rdname compatability

by_all <- function(name) {
    NULL %>% rtables:::add_col_total(lbl = name)
}

## XXX this is named this only for testing
## replace with better name once all ttestts are passing

setGeneric("col_by_to_matrix", function(col_by, x = NULL)
    standardGeneric("col_by_to_matrix"))

setMethod("col_by_to_matrix", "PreDataTableLayouts",
          function(col_by, x = NULL) col_by)

setMethod("col_by_to_matrix",
          "character",
          function(col_by, x = NULL) {

    ret = NULL
    for(col in col_by) {
        ret <- ret %>% add_colby_varlevels(col)
    }
    ret
})



combine_cinfo = function(ci1, ci2) {
    stopifnot(is(ci1, "InstantiatedColumnInfo"),
              is(ci2, "InstantiatedColumnInfo"))

    ctree1 = coltree(ci1)
    ctree2 = coltree(ci2)
    newctree = LayoutColTree(kids = list(ctree1, ctree2))
    ## c(tree_children(ctree1),
    ##                                   tree_children(ctree2)))
    
    newcounts = c(col_counts(ci1), col_counts(ci2))
    newexprs = c(col_exprs(ci1), col_exprs(ci2))
    newexargs = c(cextra_args(ci1),cextra_args(ci2))
    newdisp = disp_ccounts(ci1) || disp_ccounts(ci2)
    InstantiatedColumnInfo(treelyt = newctree,
                           csubs = newexprs,
                           extras = newexargs,
                           cnts = newcounts,
                           dispcounts = newdisp,
                           countfmt = colcount_fmt(ci1))
}



chk_cbindable <- function(x,y) {
    if(is(x, "TableRow") &&
       is(y, class(x)))
        return(TRUE)
    
    if(!is(x, "VTableTree"))
        stop("x must be a TableTree or ElementaryTableTree, got class ", class(x))
    if(!is(y, class(x)))
        stop("y must be of a class that inherits from the class of x (", class(x), ") got ", class(y))
    if(nrow(x) != nrow(y))
        stop("x and y must have the same number of rows, got different row counts (", nrow(x), " / ", nrow(y), ")")
    rnx = row.names(x)
    rny = row.names(y)
    if(!all(rnx == rny || !nzchar(rny))) 
        stop("Non-empty row.names in y do not match row.names of x")
    
    rwsx = collect_leaves(x, add.labrows = TRUE)
    rwsy = collect_leaves(y, add.labrows = TRUE)
    rwclsx = sapply(rwsx, class)
    rwclsy = sapply(rwsy, class)
    if(!identical(rwclsx, rwclsy)) {
        misses = which(rwclsx != rwclsy)
        frstmiss = min(misses)
        stop("Row object classes do not match (",
             length(misses), " of ", nrow(x),
             " rows). First mismatch:\n x rowname: ",
             rnx[frstmiss], " x row class: ",
             rwclsx[frstmiss], " y row class: ",
             rwclsy[frstmiss])
    }
    TRUE
}

cbind_rtables <-  function(x,y) {
    
    recurse_cbind(x, y, NULL)


}
setGeneric("recurse_cbind", function(x,y, cinfo = NULL) standardGeneric("recurse_cbind"))

setMethod("recurse_cbind", c("VTableNodeInfo",
                             "VTableNodeInfo",
                             "NULL"),
          function(x,y, cinfo) {
    recurse_cbind(x,y, combine_cinfo(col_info(x),
                                     col_info(y)))
})

setMethod("recurse_cbind", c("TableTree",
                             "TableTree",
                             "InstantiatedColumnInfo"),
          function(x, y, cinfo) {
    chk_cbindable(x, y)
    if(nrow(content_table(x)) == 0 && 
       nrow(content_table(y)) == 0) {
        cont = ElementaryTable(cinfo = cinfo)
    } else {
        cont = recurse_cbind(content_table(x),
                             content_table(y),
                             cinfo = cinfo)
    }

    kids = mapply(recurse_cbind,
                  x = tree_children(x),
                  y = tree_children(y),
                  MoreArgs = list(cinfo = cinfo),
                  SIMPLIFY = FALSE)
    names(kids) = names(tree_children(x))
    TableTree(kids = kids, labrow = tt_labelrow(x),
              cont = cont,
              name = obj_name(x),
              lev = tt_level(x),
              cinfo = cinfo,
              fmt = obj_fmt(x))
})

setMethod("recurse_cbind", c("ElementaryTable",
                             "ElementaryTable",
                             "InstantiatedColumnInfo"),
          function(x, y, cinfo) {
    chk_cbindable(x,y)
    if(nrow(x) == 0 &&
       nrow(y) == 0)
        return(x) ## this needs testing...
    
    kids = mapply(recurse_cbind,
                  x = tree_children(x),
                  y = tree_children(y),
                  MoreArgs = list(cinfo = cinfo),
                  SIMPLIFY = FALSE)
    names(kids) = names(tree_children(x))
    ElementaryTable(kids = kids, labrow = tt_labelrow(x),
                  name = obj_name(x),
                  lev = tt_level(x),
                  cinfo = cinfo,
                  fmt = obj_fmt(x),
                  var = obj_avar(x))
})


setMethod("recurse_cbind", c("TableRow", "TableRow",
                             "InstantiatedColumnInfo"),
          function(x,y, cinfo = NULL) {
    if(!identical(obj_avar(x), obj_avar(y)))
        stop("Got rows that don't analyze the same variable")
    vx <- row_values(x)
    vy <- row_values(y)

    cspx <- row_cspans(x)
    cspy <- row_cspans(y)
    ## combine the spans IFF they are already
    ## bigger than 1 and x ends with the same
    ## value y begins with
    if(vx[[length(vx)]] == vy[[1]] && 
       (tail(cspx, 1) > 1 || head(cspy, 1) > 1)) {
        retv <- c(vx, vy[-1])
        retcsp <- c(head(cspx, -1),
                    tail(cspx,1) + head(cspy, 1),
                    cspy[-1])
    } else {
        retv <- c(vx, vy)
        retcsp <- c(cspx, cspy)
    }

    ## Could be DataRow or ContentRow
    ## This is ok because LabelRow is special cased
    constr_fun <- get(class(x), mode = "function")
    constr_fun(val = retv,
               cspan = retcsp,
               cinfo = cinfo,
               var = obj_avar(x),
               fmt = obj_fmt(x),
               name = obj_name(x),
               lab = obj_label(x))
})

setMethod("recurse_cbind", c("LabelRow", "LabelRow",
                             "InstantiatedColumnInfo"),
          function(x,y, cinfo = NULL) x)

## we don't care about the following discrepencies:
## - ci2  having NA counts when ci1 doesn't
## - mismatching display_ccounts values
## - mismatching colcount formats
## 

chk_compat_cinfos <- function(ci1, ci2) {

    if(no_colinfo(ci2))
        return(TRUE)
    ## this will enforce same length and
    ## same names, in addition to same
    ## expressions so we dont need
    ## to check those separateley
    if(!identical(col_exprs(ci1), col_exprs(ci2))) {
        stop("Column structures not compatible: subset expression lists not identical")
    }
    
    if (any(!is.na(col_counts(ci2))) &&
        !identical(col_counts(ci1),
                   col_counts(ci2))) {
        stop("Column structures not compatible: 2nd column structure has non-matching, non-null column counts")
    }
    
    if (any(sapply(cextra_args(ci2),
                   function(x) length(x)>0)) &&
        !identical(cextra_args(ci1),
                   cextra_args(ci2))) {
        stop("Column structures not compatible: 2nd column structure has non-matching, non-null extra args")
          
    }
    TRUE
}


#' insert rrows at (before) a specific location
#'
#' @param tbl rtable
#' @param rrow rrow to append to rtable
#' @param at position into which to put the rrow, defaults to beginning (ie 1)
#' 
#' @return A TableTree of the same specific class as \code{tbl}
#' 
#' @export
#'
#' @note Label rows (ie a row with no data values, only a row.name) can only be inserted at positions which do not already contain a label row when there is a non-trivial nested row structure in \code{tbl}
#' @examples
#' tbl <- rtabulate(iris$Sepal.Length, iris$Species)
#'
#' insert_rrow(tbl, rrow("Hello World"))
#' insert_rrow(tbl, rrow("Hello World"), at = 2)
#' tbl2 <- rtabulate(iris$Sepal.Length, iris$Species, row_by = iris$Species)
#' insert_rrow(tbl2, rrow("Hello World"))
#' insert_rrow(tbl2, rrow("Hello World"), at = 2)
#' insert_rrow(tbl2, rrow("Hello World"), at = 4)
#' insert_rrow(tbl2, rrow("Hello World"), at = 6)
#' insert_rrow(tbl2, rrow("Hello World"), at = 7)
#' ##errors b/c label rows
#' try(insert_rrow(tbl2, rrow("Hello World"), at = 3))
#' try(insert_rrow(tbl2, rrow("Hello World"), at = 5))
#'
#' insert_rrow(tbl2, rrow("new row", 5, 6, 7))
#'
#' insert_rrow(tbl2, rrow("new row", 5, 6, 7), at = 3)
#' 
#' 
insert_rrow <- function(tbl, rrow, at = 1,
                        ascontent = FALSE) {
    stopifnot(is(tbl, "VTableTree"),
              is(rrow, "TableRow"),
              at >= 1 && at <= nrow(tbl) + 1)
    chk_compat_cinfos(col_info(tbl),
                      col_info(rrow))
    if(no_colinfo(rrow))
        col_info(rrow) <- col_info(tbl)
    
    if (at == 1) {
        return(rbindl_rtables(list(rrow, tbl),
                              check_headers = TRUE))
    } else if (at == nrow(tbl) + 1) {
        return(rbind2(tbl, rrow))
    }

    ret <- recurse_insert(tbl, rrow, at = at,
                          pos = 0,
                          ascontent = ascontent)
    ret
}


.insert_helper = function(tt, row, at, pos,
                          ascontent = FALSE) {
    islab <- is(row, "LabelRow")
    kids <- tree_children(tt)
    numkids <- length(kids)
    kidnrs <- sapply(kids, nrow)
    cumpos <- pos + cumsum(kidnrs)
    contnr = if(is(tt, "TableTree"))
                 nrow(content_table(tt))
             else
                 0
    contnr <- contnr + as.numeric(labrow_visible(tt))
    
  
    totnr = nrow(tt)
    endpos = pos + totnr
    atend = !islab && endpos == at - 1
    if(at == pos + 1
       && islab) {
        if(labrow_visible(tt_labelrow(tt)))
            stop("Inserting a label row at a position that already has a label row is not currently supported")
        tt_labelrow(tt) <- row
        return(tt)
    }
    
    if(numkids == 0) {
        kids = list(row)
    } else if (atend) {
        if(are(kids, "TableRow")) {
            kids = c(kids, list(row))
        } else {
            kids[[numkids]] = recurse_insert(kids[[numkids]], row =  row, at = at, pos = pos + contnr + sum(kidnrs[-numkids]), ascontent =  ascontent)
        }
    } else { #have >0 kids
        kidnrs <- sapply(kids, nrow)
        cumpos <- pos + cumsum(kidnrs)
        ## if (atend) {
        ##     if(are(kids, "TableRow")) {
        ##         kids = c(kids, list(row))
        ##     } else { #not all table rows
        ##         kids[[numkids]] <- recurse_insert(kids[[numkids]], row = row, pos = at - 1,  at = at, ascontent = ascontent)
        ##     } # end are(kids, "TableRow")
        ## } else { # not at end
            
            
            ## data rows go in the end of the
            ## preceding subtable (if applicable)
            ## label rows go in the beginning of
            ##  one at at
            ind <- min(which((cumpos + !islab) >= at),
                       numkids )
            thekid  = kids[[ind]]
            
            if(is(thekid, "TableRow")) {
                tt_level(row) = tt_level(thekid)
                if(ind == 1) {
                    bef = integer()
                    aft = 1:numkids
                    ## } else if(atend ) {
                    ##     bef = 1:numkids
                    ##     aft = integer()
                } else if(ind == numkids) {
                    bef = 1:(ind -1)
                    aft = ind
                } else {
                    bef = 1:ind
                    aft = (ind + 1):numkids
                }
                kids <- c(kids[bef], list(row),
                          kids[aft])
            } else { # kid is not a table row
                newpos <- if(ind==1)
                              pos + contnr
                          else 
                              cumpos[ind - 1]
                
                kids[[ind]] <- recurse_insert(thekid,
                                              row,
                                              at,
                                              pos = newpos,
                                              ascontent = ascontent)
            } # end kid is not table row
  ##      } # end not at the end
    }
    tree_children(tt) <- kids
    tt
}

    
setGeneric("recurse_insert", function(tt, row, at, pos, ascontent = FALSE) standardGeneric("recurse_insert"))
setMethod("recurse_insert", "TableTree",
          function(tt, row, at, pos, ascontent = FALSE) {
    ctab = content_table(tt)
    contnr = nrow(ctab)
    contpos = pos + contnr
    islab = is(row, "LabelRow")
    ## this will NOT insert it as 
    if((contnr > 0 || islab) &&
       contpos > at) {
        content_table(tt) <- recurse_insert(ctab, row, at, pos, TRUE)
        return(tt)
    }

    .insert_helper(tt, row, at = at, pos= pos + contnr,
                   ascontent = ascontent)
})

setMethod("recurse_insert", "ElementaryTable",
          function(tt, row, at, pos, ascontent = FALSE) {
    .insert_helper(tt, row, at = at, pos = pos,
                   ascontent = FALSE)
})








order_rrows = function(x, indices = c(1, 1), ...) {




}
