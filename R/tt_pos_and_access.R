
do_recursive_replace = function(tab, path, incontent = FALSE, rows = NULL,
                                cols = NULL, value) {
    ## don't want this in the recursive function
    ## so thats why we have the do_ variant
    if(is.character(path) && length(path) > 1)
        path = as.list(path)
   recursive_replace(tab, path, incontent, rows, cols,value)
}


## different cases we want to support:
## 1. Replace entire children for a particular node/position in the tree
## 2. Replace entire rows at a particular (ElementaryTable) position within the tree
## 3. Replace specific cell values within a set of row x column positions within an ElementaryTable at a particular position within the tree
## 3. replace entire content table at a node position
## 4. replace entire rows within the content table at a particular node position in the tree
## 5. replace data cell values for specific row/col positions within the content table at a particular position within the tree

## XXX This is wrong, what happens if a split (or more accurately, value)  happens more than once in the overall tree???
recursive_replace = function(tab, path, incontent = FALSE, rows = NULL, cols = NULL, value) {
    if(length(path) == 0) { ## done recursing
        if(is.null(rows) && is.null(cols)) { ## replacing whole subtree a this position
            if(incontent) {
                newkid = tab
                content_table(newkid) = value
            } else
                newkid = value
            ## newkid has either thee content table
            ## replaced on the old kid or is the new
            ## kid
        } else { ## rows or cols (or both)  non-null
            if(incontent) {
                ctab = content_table(tab)
                ctab[rows, cols] = value
                content_table(tab) = ctab
                newkid = tab

            } else {
                allkids = tree_children(tab)
                stopifnot(are(allkids, "TableRow"))
                newkid = tab
                newkid[rows, cols] = value
            }
        }
        return(newkid)
    } else { ## length(path) > 1, more recursing to do
        kidel = path[[1]]
        ## broken up for debugabiliity, could be a single complex
        ## expression
        ## for now only the last step supports selecting
        ## multiple kids
        stopifnot(length(kidel) == 1,
                  is.character(kidel) || is.factor(kidel))
        knms = names(tree_children(tab))
        if(!(kidel %in% knms))
            stop(sprintf("position element %s not in names of next level children", kidel))
        else if (sum(kidel == knms) > 1)
            stop(sprintf("position element %s appears more than once, not currently supported", kidel))
        if(is.factor(kidel)) kidel = levels(kidel)[kidel]
        newkid = recursive_replace(
            tree_children(tab)[[kidel]],
            path[-1],
            incontent = incontent,
            rows = rows,
            cols = cols,
            value)
        tree_children(tab)[[kidel]] = newkid
        tab
    }


}

setGeneric("tt_at_path", function(tt, path, ...) standardGeneric("tt_at_path"))
setMethod("tt_at_path", "VTableTree",
           function(tt, path, ...) {
    stopifnot(is(path, "character"),
              length(path) > 0,
              !anyNA(path))
    ## handle pathing that hits the root split by name
    if(obj_name(tt) == path[1])
        path = path[-1]
    cur <- tt
    curpath <- path
    while(length(curpath > 0)) {
        kids <- tree_children(cur)
        curname <- curpath[1]
        if(curname == "@content")
            cur <- content_table(cur)
        else if(curname %in% names(kids)) {
            cur <- kids[[curname]]
        } else {
            stop("Path appears invalid for this tree at step ", curname)
        }
        curpath <- curpath[-1]
    }
    cur
})

setGeneric("tt_at_path<-", function(tt, path, ..., value) standardGeneric("tt_at_path<-"))
setMethod("tt_at_path<-", c(tt = "VTableTree", value = "VTableTree"),
          function(tt, path, ..., value) {
    recursive_replace(tt, path = path, value = value)

})

setGeneric("replace_rows", function(x, i, value) standardGeneric("replace_rows"))
setMethod("replace_rows", c(value = "list"),
          function(x, i, value) {


    if(is.null(i)) {
        i = seq_along(tree_children(x))
        if(labelrow_visible(x))
            i = i[-1]
    } else if(is.logical(i)) {
        i = which(rep(i, length.out = length(collect_leaves(x, TRUE, TRUE))))
    }

    if(labelrow_visible(x) && 1 %in% i && !are(value, "TableRow") && !is.null(value[[1]]))
        stop("attempted to assign values into a LabelRow")

    if(length(value) != length(i))
        value = rep(value, length.out = length(i))

    if(are(value, "TableRow")) {
        newrows =value
    } else {
        newrows = lapply(i,
                         function(ind) {
            .tablerow(value[[ind]],
                     cinfo = col_info(x),
                     klass = class(tree_children(x)[[ind]]),
                     )
        })
    }

    kids = tree_children(x)
    kids[i] = newrows
    tree_children(x) = kids
    x
})



setMethod("replace_rows", c(value = "ElementaryTable"),
           function(x,i,value) {
    stopifnot(identical(col_info(x), col_info(value)))
    replace_rows(x, i, tree_children(value))
})







#' retrieve and assign elements of a TableTree
#' 
#' @rdname brackets
#' 
#' @param x TableTree
#' @param i index
#' @param j index
#' @param drop logical(1). Should the value in the cell be returned if only one cell is selected by the combination of \code{i} and \code{j}. Defaults to \code{FALSE}
#' @param \dots Includes \emph{keep_topleft} logical(1) (\code{[} only) Should the 'top-left' material for the table be retained after subsetting. Defaults to \code{NA}, which retains the material if all rows are included (ie subsetting was by column), and drops it otherwise.
#' @param value Replacement value (list, TableRow, or TableTree)
#' 
#' @exportMethod [<-
#' 
#' @examples 
#' l <- basic_table() %>%
#'    split_cols_by("ARM") %>%
#'    analyze(c("SEX", "AGE"))
#'    
#' tbl <- build_table(l, DM)
#' 
#' tbl
#' 
#' tbl[1, ]
#' tbl[1:2, 2]
#' 
#' tbl[2, 1]
#' tbl[2, 1, drop = TRUE]
#' 
#' tbl[, 1]
#' 
#' tbl[-2, ]
#' tbl[, -1]
setMethod("[<-", c("VTableTree", value = "list"),
          function(x, i, j, ...,  value) {


    nr = nrow(x)
    i = .j_to_posj(i, nr)
    if(missing(j)) {
        j = seq_along(col_exprs(col_info(x)))
    } else {
        j = .j_to_posj(j, ncol(x))
    }

    if(length(i) > 1 && length(j) < ncol(x))
        stop("cannot modify multiple rows in not all columns.")

    if(are(value, "TableRow"))

        value = rep(value, length.out = length(i))
    else
        value = rep(value, length.out = length(i) * length(j))

    counter = 0
    ## this has access to value, i, and j by scoping
    replace_rowsbynum = function(x, i, valifnone = NULL) {
        maxi = max(i)
        if(counter >= maxi)
            return(valifnone)

        if(labelrow_visible(x)) {
            counter <<- counter + 1
            if(counter %in% i) {
                nxtval = value[[1]]
                if(is(nxtval, "LabelRow")) {
                    tt_labelrow(x) = nxtval
                } else {
                    stop("can't replace label with value of class", class(nxtval))
                }
                ## we're done with this one move to
                ## the next
                value <<- value[-1]
            }
        }
        if(is(x, "TableTree") && nrow(content_table(x)) > 0) {
            ctab = content_table(x)

            content_table(x) = replace_rowsbynum(ctab, i)
        }
        if(counter >= maxi) { #already done
            return(x)
        }
        kids = tree_children(x)

        if(length(kids) > 0) {
            for(pos in seq_along(kids)) {
                curkid = kids[[pos]]
                if(is(curkid, "TableRow")) {
                    counter <<- counter + 1
                    if(counter %in% i) {
                        nxtval = value[[1]]
                        if(is(nxtval, class(curkid))) {
                            if(no_colinfo(nxtval) &&
                               length(row_values(nxtval)) == ncol(x)) {
                                col_info(nxtval) = col_info(x)
                            }
                            stopifnot(identical(col_info(x), col_info(nxtval)))
                            curkid = nxtval
                            value = value[-1]
                        } else {
                            rvs = row_values(curkid)
                            rvs[j] = value[1:length(j)]
                            row_values(curkid) = rvs
                            value = value[-(1:length(j))]
                        }
                        kids[[pos]] = curkid
                    }
                } else {
                    kids[[pos]] = replace_rowsbynum(curkid, i)
                }
                if(counter >= maxi)
                    break
            }
        }
        tree_children(x) = kids
        x
    }
    replace_rowsbynum(x, i, ...)
})

#' @exportMethod [<-
#' @rdname brackets
setMethod("[<-", c("VTableTree", value = "CellValue"),
          function(x, i, j, ...,  value) {
    x[i = i, j = j, ...] <- unclass(value)
    x
})



## this is going to be hard :( :( :(

### selecting/removing columns

## we have two options here: path like we do with rows and positional
## in leaf space.

setGeneric("subset_cols", function(tt, j, newcinfo = NULL, keep_topleft = TRUE, ...) standardGeneric("subset_cols"))

setMethod("subset_cols", c("TableTree", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft, ...) {
    j = .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo = col_info(tt)
        newcinfo = subset_cols(cinfo, j, keep_topleft = keep_topleft, ...)
    }
    ## topleft taken care of in creation of newcinfo
    kids = tree_children(tt)
    newkids = lapply(kids, subset_cols, j= j, newcinfo = newcinfo,  ...)
    cont = content_table(tt)
    newcont = subset_cols(cont, j, newcinfo = newcinfo,  ...)
    tt2 = tt
    col_info(tt2) <- newcinfo
    content_table(tt2) <- newcont
    tree_children(tt2) <- newkids
    tt_labelrow(tt2) = subset_cols(tt_labelrow(tt2), j, newcinfo,  ...)
    ## if(keep_topleft)
    ##     top_left(tt2) <- top_left(tt)
    ## else
    ##     top_left(tt2) <- character()
    tt2
})

setMethod("subset_cols", c("ElementaryTable", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft, ...) {
    j = .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo = col_info(tt)
        newcinfo = subset_cols(cinfo, j, keep_topleft = keep_topleft, ...)
    }
    ## topleft handled in creation of newcinfo
    kids = tree_children(tt)
    newkids = lapply(kids, subset_cols, j= j, newcinfo = newcinfo,  ...)
    tt2 = tt
    col_info(tt2) <- newcinfo
    tree_children(tt2) <- newkids
    tt_labelrow(tt2) = subset_cols(tt_labelrow(tt2), j, newcinfo, ...)
    ## if(keep_topleft)
    ##     top_left(tt2) <- top_left(tt)
    tt2
})


## small utility to transform any negative
## indices into positive ones, given j
## and total length

.j_to_posj = function(j, n) {
    ## This will work for logicals, numerics, integers
    j = seq_len(n)[j]
    j
}

#' @noRd
#' @param spanfunc is the thing that gets the counts after subsetting
## should be n_leaves for a column tree structure and NROW for
## a table tree
.colpath_to_j <- function(path, ctree, offset = 0) {
     if(length(path) == 0) {
        if(is(ctree, "VLeaf"))
            ret = offset + 1
        else
            ret = offset + 1:n_leaves(ctree)
        return(ret)
    }
    ## the columntree is collapsed so we go to the kids and look
    ## at their position information which has both split and
    ## split value information
    kids = tree_children(ctree)
    splname = obj_name(tail(pos_splits(kids[[1]]), 1)[[1]])
    if(splname != path[1]) {
        stop("Path in column space appears to be invalid at step ", path[1])
    }
    if(length(path) == 1) {
        ret = offset + 1:n_leaves(ctree)
    } else {
        cj <- path[2]

        nkids = sapply(kids, n_leaves)
        kidoffsets = cumsum(nkids) - nkids[1]
        if(cj == "*") {
            ret = sort(unlist(mapply(.colpath_to_j,
                                     MoreArgs = list(path = tail(path, -2)),
                                     ctree = kids,
                                     offset = offset + kidoffsets,
                                     SIMPLIFY=FALSE)))
        } else if (cj %in% names(kids)) {
            ret = .colpath_to_j(tail(path, -2), ctree = kids[[cj]],
                               offset = offset + kidoffsets[which(cj == names(kids))])
        } else {
            stop("Path in column space appears to be invalid at step ", cj)
        }
    }
    sort(unlist(ret))
}

.path_to_pos <- function(path, fullidx, ctree, spanfunc) {
    retidx = fullidx
    stopifnot(length(path) > 0)
    cj = path[-1]
    curcj = path[1]
    while(length(cj) >= 0) {
        colcounts = sapply(tree_children(ctree), spanfunc)
        cnms <- names(tree_children(ctree))
        ## this will ONLY find the first match in the case of duplciated names!!!!
        fidx <- match(cj, cnms)
        if(anyNA(fidx))
            stop("Path element ", cj, "did not match any remaining names [", paste(cnms, collapse = ", "), "]")
        if(fidx == 1L) {
            retidx <- retidx[1:colcounts[1]]
        } else {
            strtpos <- sum(colcounts[1:fidx]) + 1
            retidx <- retidx[strtpos:(strtpos + colcounts[fidx])]
        }
        ctree <- tree_children(ctree)[[fidx]]
        curcj <- cj[1]
        cj <- cj[-1]
    }
    retidx
}

## fix column spans that would be invalid
## after some columns are no longer there
.fix_rowcspans = function(rw, j) {
    cspans = row_cspans(rw)
    nc = sum(cspans)
    j = .j_to_posj(j, nc)
    ## this is overly complicated
    ## we need the starting indices
    ## but the first span might not be 1, so
    ## we pad with 1 and then take off the last
    start = cumsum(c(1,head(cspans, -1)))
    ends = c(tail(start, -1) -1, nc)
    res = mapply(function(st, en) {
        sum(j >= st & j <= en)
    }, st = start, en = ends)
    res = res[res>0]
    stopifnot(sum(res) == length(j))
    res

}

select_cells_j = function(cells, j) {
    if(length(j) != length(unique(j)))
        stop("duplicate column selections is not currently supported")
    spans = vapply(cells, function(x) cell_cspan(x),
                   integer(1))
    inds = rep(seq_along(cells), times = spans)
    selinds = inds[j]
    retcells = cells[selinds[!duplicated(selinds)]]
    newspans = vapply(split(selinds, selinds),
                      length,
                      integer(1))

    mapply(function(cl, sp) {
        cell_cspan(cl) = sp
        cl
    }, cl = retcells, sp = newspans, SIMPLIFY=FALSE)
}

setMethod("subset_cols", c("ANY", "character"),
          function(tt, j, newcinfo = NULL, keep_topleft = TRUE, ...) {
    j <- .colpath_to_j(j, coltree(tt))
    subset_cols(tt, j, newcinfo = newcinfo, keep_topleft = keep_topleft,  ...)
})

setMethod("subset_cols", c("TableRow", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft = TRUE,  ...) {
    j = .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo = col_info(tt)
        newcinfo = subset_cols(cinfo, j, keep_topleft = keep_topleft,  ...)
    }
    tt2 = tt
    row_cells(tt2) =  select_cells_j(row_cells(tt2), j)

    if(length(row_cspans(tt2)) > 0)
        row_cspans(tt2) = .fix_rowcspans(tt2, j)
    col_info(tt2) = newcinfo
    tt2
})

setMethod("subset_cols", c("LabelRow", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft = TRUE, ...) {
    j = .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo = col_info(tt)
        newcinfo = subset_cols(cinfo, j, keep_topleft = keep_topleft, ...)
    }
    col_info(tt) = newcinfo
    tt
})


setMethod("subset_cols", c("InstantiatedColumnInfo", "numeric"),
          function(tt, j, newcinfo = NULL, keep_topleft = TRUE,  ...) {
    if(!is.null(newcinfo))
        return(newcinfo)
    j <- .j_to_posj(j, length(col_exprs(tt)))
    newctree <- subset_cols(coltree(tt), j, NULL)
    newcextra <- col_extra_args(tt)[j]
    newcsubs <- col_exprs(tt)[j]
    newcounts <- col_counts(tt)[j]
    tl <- if(keep_topleft) top_left(tt) else character()
    InstantiatedColumnInfo(treelyt = newctree,
                           csubs = newcsubs,
                           extras = newcextra,
                           cnts = newcounts,
                           dispcounts = disp_ccounts(tt),
                           countformat = colcount_format(tt),
                           topleft = tl)
})

setMethod("subset_cols", c("LayoutColTree", "numeric"),
          function(tt, j, newcinfo = NULL, ...) {
    lst = collect_leaves(tt)
    j = .j_to_posj(j, length(lst))

    ## j has only non-negative values from
    ## this point on
    counter = 0
    prune_children = function(x, j) {
        kids = tree_children(x)
        newkids = kids
         for(i in seq_along(newkids)) {
            if(is(newkids[[i]], "LayoutColLeaf")) {
                counter <<- counter + 1
                if(!(counter %in% j))
                    newkids[[i]] = list() ## NULL removes the position entirely
            } else {
                newkids[[i]] = prune_children(newkids[[i]], j)
            }
        }

        newkids = newkids[sapply(newkids, function(thing) length(thing) > 0)]
        if(length(newkids) > 0){
            tree_children(x) = newkids
            x
        } else {
            list()
        }
    }
    prune_children(tt, j)
})




## label rows ARE included in the count
subset_by_rownum = function(tt, i, keep_topleft = NA, ... ) {
    stopifnot(is(tt, "VTableNodeInfo"))
    counter = 0
    nr = nrow(tt)
    i = .j_to_posj(i, nr)
    if(length(i) == 0) {
        ret <- TableTree(cinfo = col_info(tt))
        if(isTRUE(keep_topleft))
            top_left(ret) <- top_left(tt)
        return(ret)
    }

    prune_rowsbynum = function(x, i, valifnone = NULL) {
        maxi = max(i)
        if(counter > maxi)
            return(valifnone)

        if(labelrow_visible(x)) {
            counter <<- counter + 1
            if(!(counter %in% i)) {
                ## XXX this should do whatever
                ## is required to 'remove' the Label Row
                ## (currently implicit based on
                ## the value of the label but
                ## that shold really probably change)
                labelrow_visible(x) <- FALSE
            }
        }
        if(is(x, "TableTree") && nrow(content_table(x)) > 0) {
            ctab = content_table(x)

            content_table(x) = prune_rowsbynum(ctab, i,
                                               valifnone = ElementaryTable(cinfo = col_info(ctab), iscontent = TRUE))
        }
        kids = tree_children(x)
        if(counter > maxi) { #already done
            kids = list()
        } else if(length(kids) > 0) {
            for(pos in seq_along(kids)) {
                if(is(kids[[pos]], "TableRow")) {
                    counter <<- counter + 1
                    if(!(counter %in% i)) {
                        kids[[pos]] = list()
                    }
                } else {
                    kids[[pos]] = prune_rowsbynum(kids[[pos]], i, list())
                }
            }
            kids = kids[sapply(kids, function(x) NROW(x) > 0)]
        }
        if(length(kids) == 0 &&
           NROW(content_table(x)) == 0 &&
           !labelrow_visible(x))
            return(valifnone)
        else {
            tree_children(x) = kids
            x
        }
        ## ## if(length(kids) == 0) {
        ## ##     if(!is(x, "TableTree"))
        ## ##         return(valifnone)
        ## ## }
        ## if(is(x, "VTableTree") && nrow(x) > 0) {
        ##     x
        ## } else {
        ##     valifnone
        ## }
    }
    ret <- prune_rowsbynum(tt, i)
    if(isTRUE(keep_topleft))
        top_left(ret) <- top_left(tt)
    ret
}


#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "logical", "logical"),
          function(x, i, j, ..., drop = FALSE) {
    i = .j_to_posj(i, nrow(x))
    j = .j_to_posj(j, ncol(x))
    x[i,j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "logical", "ANY"),
          function(x, i, j, ..., drop = FALSE) {
    i = .j_to_posj(i, nrow(x))
    x[i,j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "logical", "missing"),
          function(x, i, j, ..., drop = FALSE) {
    j = seq_len(ncol(x))
    i = .j_to_posj(i, nrow(x))
    x[i,j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "ANY", "logical"),
          function(x, i, j, ..., drop = FALSE) {
    j = .j_to_posj(j, ncol(x))
    x[i,j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "ANY", "missing"),
          function(x, i, j, ..., drop = FALSE) {
    j = seq_len(ncol(x))
    x[i = i,j = j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "missing", "ANY"),
          function(x, i, j, ..., drop = FALSE) {
    i = seq_len(nrow(x))
    x[i = i,j = j, ..., drop = drop]
})



#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "ANY", "character"),
          function(x, i, j, ..., drop = FALSE) {
    j <- .colpath_to_j(j, coltree(x))
    x[i = i,j = j, ..., drop = drop]
})

#' @exportMethod [
#' @rdname brackets
setMethod("[", c("VTableTree", "character", "ANY"),
          function(x, i, j, ..., drop = FALSE) {
    i <- .path_to_pos(i, seq_len(nrow(x)), x, NROW)
    x[i = i,j = j, ..., drop = drop]
})

## to avoid dispatch ambiguity. Not necessary, possibly not a good idea at all
#' @exportMethod [
#' @rdname brackets
setMethod("[", c("VTableTree", "character", "character"),
          function(x, i, j, ..., drop = FALSE) {
    i <- .path_to_pos(i, seq_len(nrow(x)), x, NROW)
    j <- .colpath_to_j(j, coltree(x))
    x[i = i, j = j, ..., drop = drop]
})


#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "missing", "numeric"),
          function(x, i, j, ..., drop = FALSE) {
    i = seq_len(nrow(x))
    x[i,j, ..., drop = drop]
})


#' @exportMethod [
#' @rdname brackets

setMethod("[", c("VTableTree", "numeric", "numeric"),
          function(x, i, j, ..., drop = FALSE) {
    ## have to do it this way because we can't add an argument since we don't
    ## own the generic declaration
    keep_topleft <- list(...)[["keep_topleft"]] ## returns NULL if not presesnt
    if(is.null(keep_topleft))
        keep_topleft <- NA

    nr <- nrow(x)
    nc <- ncol(x)
    i <- .j_to_posj(i, nr)
    j <- .j_to_posj(j, nc)

    ##  if(!missing(i) && length(i) < nr) {
    if(length(i) < nr) { ## already populated by .j_to_posj
        keep_topleft <- isTRUE(keep_topleft)
        x <- subset_by_rownum(x, i, keep_topleft = keep_topleft)
    } else {
        keep_topleft <- !identical(FALSE, keep_topleft)
    }
    ##  if(!missing(j) && length(j) < nc)
    if(length(j) < nc)
        x <- subset_cols(x, j, keep_topleft = keep_topleft)

    if(length(j) == 1L &&
       length(i) == 1L &&
       drop) {
        rw <- collect_leaves(x, TRUE, TRUE)[[1]]
        if(is(rw, "LabelRow"))
            x <- NULL
        else
            x <- row_values(rw)[[1]]
    }
    if(!drop && !keep_topleft)
        top_left(x) <- character()
    x
})

#' @exportMethod [[
#' @rdname brackets

setMethod("[[", c("VTableTree", "list", "ANY"),
          function(x, i, j, ...) {

    subtree = x
    nms = i
    while(length(nms) > 0 && is(subtree, "VTableTree")) {
        nm = nms[[1]]
        nms = nms[-1]
        ## support the name[2] type pathing
        ## XXX should we just modify the names
        ## so the actual name is name[2] instead?
        stopifnot(is(nm, "character"))
        nm = strsplit(nm, "[][]")[[1]]
        if(length(nm) > 1)
            pos = as.integer(nm[2])
        else
            pos = 1L
        nm = nm[1]
        kids = tree_children(subtree)
        chosen = which(sapply(kids, obj_name) == nm)[pos]
        subtree = kids[[chosen]]
    }
    ## we have row selections
    if(length(nms)) {
        stop("stop being so clever. row selection in path subsetting not supported yet")
    }
    if(!missing(j))
        subtree = subset_cols(subtree, j)
    if(is(subtree, "TableRow"))
        subtree = TableTree(list(subtree), cinfo = col_info(subtree))
    subtree


})

extract_colvals = function(tt, j) {



}

setGeneric("tail", tail)
setMethod("tail", "VTableTree",
          function(x, n = 6L, ...) {
    tail.matrix(x, n, addrownums = FALSE)
})

setGeneric("head", head)
setMethod("head", "VTableTree",
          function(x, n= 6L, ...) {
    head.matrix(x, n)
}
)

#' Retrieve cell values by row and column path
#' 
#' @rdname cell_values
#'
#' @inheritParams gen_args
#' @param rowpath character. Path in row-split space to the desired row(s). Can include \code{"@content"}.
#' @param colpath character. Path in column-split space to the desired column(s). Can include \code{"*"}.
#' @param omit_labrows logical(1). Should label rows underneath \code{rowpath} be omitted
#' (\code{TRUE}, the default), or return empty lists of cell "values" (\code{FALSE}).
#'
#' @return a \emph{list} (regardless of the type of value the cells hold). if \code{rowpath} defines a path to a single row, \code{cell_values} returns the list of cell values for that
#' row, otherwise a list of such lists, one for each row captured underneath \code{rowpath}.
#'
#' @export
#' 
#' @examples
#'  l <- basic_table() %>% split_cols_by("ARM") %>%
#'    split_cols_by("SEX") %>%
#'    split_rows_by("RACE") %>%
#'    summarize_row_groups() %>%
#'    split_rows_by("STRATA1") %>%
#'    analyze("AGE")
#'
#' library(dplyr) ## for mutate
#' tbl <- build_table(l, DM %>% mutate(SEX = droplevels(SEX), RACE = droplevels(RACE)))
#' 
#' row_paths_summary(tbl)
#' col_paths_summary(tbl)
#' 
#' cell_values(tbl, c("RACE", "ASIAN", "STRATA1", "B"), c("ARM", "A: Drug X", "SEX", "F"))
#' 
#' # it's also possible to access multiple values by being less specific 
#' cell_values(tbl, c("RACE", "ASIAN", "STRATA1"), c("ARM", "A: Drug X", "SEX", "F"))
#' cell_values(tbl, c("RACE", "ASIAN"), c("ARM", "A: Drug X", "SEX", "M"))
#'
#'
#' ## any arm, male columns from the ASIAN content (ie summary) row
#' cell_values(tbl, c("RACE", "ASIAN", "@content"), c("ARM", "B: Placebo", "SEX", "M"))
#' cell_values(tbl, c("RACE", "ASIAN", "@content"), c("ARM", "*", "SEX", "M"))
#'
#' ## all columns
#' cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B"))
#'
#' ## all columns for the Combination arm
#' cell_values(tbl,  c("RACE", "ASIAN", "STRATA1", "B"), c("ARM", "C: Combination"))
setGeneric("cell_values", function(tt, rowpath = NULL, colpath = NULL, omit_labrows = TRUE)
    standardGeneric("cell_values"))
#'@rdname cell_values
#' @exportMethod cell_values
setMethod("cell_values", "VTableTree",
          function(tt, rowpath, colpath = NULL, omit_labrows = TRUE){
    if(is.null(rowpath))
        subtree <- tt
    else
        subtree <- tt_at_path(tt, rowpath)
    if(!is.null(colpath))
        subtree <- subset_cols(subtree, colpath)

    rows <- collect_leaves(subtree, TRUE, !omit_labrows)
    if(length(rows)== 1)
        row_values(rows[[1]])
    else
        lapply(rows, row_values)
})
