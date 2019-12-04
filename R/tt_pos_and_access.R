## pos_from_values = function(vals, tab) {
##     splvec = list()
##     curtree = tab
##     for(v in vals) {
##         if(is.factor(v)) v = levels(v)[v]
##         kids = tree_children(curtree)
##         stopifnot(v %in% names(kids))
##         splvec = c(splvec, list(current_spl(curtree)))
##         curtree = kids[[v]]
##     }
    


## }

do_recursive_replace = function(tab, posvals, incontent = FALSE, rows = NULL,
                                cols = NULL, value) {
    ## don't want this in the recursive function
    ## so thats why we have the do_ variant
    if(is.character(posvals) && length(posvals) > 1)
        posvals = as.list(posvals)
   recursive_replace(tab, posvals, incontent, rows, cols,value)
}


## different cases we want to support:
## 1. Replace entire children for a particular node/position in the tree
## 2. Replace entire rows at a particular (ElementaryTable) position within the tree
## 3. Replace specific cell values within a set of row x column positions within an ElementaryTable at a particular position within the tree
## 3. replace entire content table at a node position
## 4. replace entire rows within the content table at a particular node position in the tree
## 5. replace data cell values for specific row/col positions within the content table at a particular position within the tree

## XXX This is wrong, what happens if a split (or more accurately, value)  happens more than once in the overall tree???
recursive_replace = function(tab, posvals, incontent = FALSE, rows = NULL, cols = NULL, value) {
    if(length(posvals) == 0) { ## done recursing
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
    } else { ## length(posvals) > 1, more recursing to do
        kidel = posvals[[1]]
        ## broken up for debugabiliity, could be a single complex
        ## expression
        ## for now only the last step supports selecting
        ## multiple kids
        stopifnot(length(kidel) == 1)
        if(is.factor(kidel)) kidel = levels(kidel)[kidel]
        newkid = recursive_replace(
            tree_children(tab)[[kidel]],
            posvals[-1],
            incontent = incontent,
            rows = rows,
            cols = cols,
            value)
        tree_children(tab)[[kidel]] = newkid
        tab
    }
 
    
}

setGeneric("replace_rows", function(x, i, value) standardGeneric("replace_rows"))
setMethod("replace_rows", c(value = "list"),
          function(x, i, value) {

    
    if(is.null(i)) {
        i = seq_along(tree_children(x))
        if(labrow_visible(x))
            i = i[-1]
    } else if(is.logical(i)) {
        i = which(rep(i, length.out = length(collect_leaves(x, TRUE, TRUE))))
    }

    if(labrow_visible(x) && 1 %in% i && !are(value, "TableRow"))
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






## This is currently NOT the fully general method for TableTree objects
## it will not handle tree structure and ignores content rows entirely!!!
##
## But it works for ElementaryTables and TableTrees which have a content (to
## be ignored) and only TableRows as children.
## setMethod("[<-", "VTableTree",
##           function(x, i, j, ...,  value) {
##     stopifnot(are(tree_children(x), "TableRow"))
##     if(is.null(j)) {
##         replace_rows(x, i, value)
    
##     } else { ## replacing only elements within certain rows
##         if(is.null(i)) ## all of them
##             i = seq_along(tree_children(x))

##         if(is.null(dim(value)))
##             value = matrix(value, nrow = length(i), ncol = length(j))
        
##         rws = tree_children(x)
##         modrws = lapply(seq_along(i),
##                      function(pos) {
##             r = rws[[ i[pos] ]]
##             rvals = row_values(r)
##             rvals[j] =  value[pos,] #whole row of value matrix
##             row_values(r) = rvals
##             r
##         })
##         rws[i] = modrws
##         tree_children(x) = rws
##     }
##     x
## })


    
    

## setMethod("[<-", c("VTableTree", j = "missing", value = "list"),
##           function(x, i, j, ...,  value) {


##     nr = nrow(x)
##     i = .j_to_posj(i, nr)
##     nc = ncol(x)
##     if(are(value, "TableRow"))
##         value = rep(value, length.out = length(i))
##     else
        
##     counter = 0
##     vrow = 1

##     ## this has access to value, i, and j by scoping
##     replace_rowsbynum = function(x) {
##         maxi = max(i)
##         if(counter >= maxi)
##             return(valifnone)
        
##         if(labrow_visible(x)) {
##             counter <<- counter + 1
##             if(counter %in% i) {
##                 nxtval = value[[1]]
##                 if(is(nxtval, "character") &&
##                    length(nxtval) == 1L) {
##                     obj_label(x) = nxtval
##                 } else if(is(nxtrow, "LabelRow")) {
##                     tt_labelrow(x) = nxtval
##                 } else {
##                     stop("can't replace label with value of class", class(nxtval))
##                 }
##                 ## we're done with this one move to
##                 ## the next
##                 value <<- value[-1]
##             }
##         }
##         if(is(x, "TableTree") && nrow(content_table(x)) > 0) {
##             ctab = content_table(x)
            
##             content_table(x) = replace_rowsbynum(ctab, i)
##         }
##         if(counter >= maxi) { #already done
##             return(x)
##         }
##         kids = tree_children(x)
        
##         if(length(kids) > 0) {
##             for(pos in seq_along(kids)) {
##                 curkid = kids[[pos]]
##                 if(is(curkid, "TableRow")) {
##                     counter <<- counter + 1
##                     if(!(counter %in% i)) {
##                         nxtval = value[[1]]
##                         if(is(nxtval, class(curkid))) {
##                             curkid = nxtval
##                         } else {
##                             row_values(curkid) = rep(value[[vrow]], length.out = ncol(x))
##                         }
##                         vrow <<- vrow + 1
##                         kids[[pos]] = curkid
##                     }
##                 } else {
##                     kids[[pos]] = replace_rowsbynum(curkid)
##                 }
##                 if(counter >= maxi)
##                     break
##             }
##         }
##         tree_children(x) = kids
##         x
##     }
##     replace_rowsbynum(tt, i)
## })





setMethod("[<-", c("VTableTree", value = "list"),
          function(x, i, j, ...,  value) {


    nr = nrow(x)
    i = .j_to_posj(i, nr)
    if(missing(j)) {
        j = seq_along(col_exprs(col_info(tab)))
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
    replace_rowsbynum = function(x, i) {
        maxi = max(i)
        if(counter >= maxi)
            return(valifnone)
        
        if(labrow_visible(x)) {
            counter <<- counter + 1
            if(counter %in% i) {
                nxtval = value[[1]]
                if(is(nxtrow, "LabelRow")) {
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
    replace_rowsbynum(x, i)
})




        
## this is going to be hard :( :( :(

### selecting/removing columns

## we have two options here: path like we do with rows and positional
## in leaf space. 

setGeneric("subset_cols", function(tt, j, newcinfo = NULL, ...) standardGeneric("subset_cols"))

setMethod("subset_cols", c("TableTree", "numeric"),
          function(tt, j, newcinfo = NULL, ...) {
    j = .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo = col_info(tt)
        newcinfo = subset_cols(cinfo, j, ...)
    }
    kids = tree_children(tt)
    newkids = lapply(kids, subset_cols, j= j, newcinfo = newcinfo,  ...)
    cont = content_table(tt)
    newcont = subset_cols(cont, j, newcinfo = newcinfo,  ...)
    tt2 = tt
    col_info(tt2) <- newcinfo
    content_table(tt2) <- newcont
    tree_children(tt2) <- newkids
    tt2
})

setMethod("subset_cols", c("ElementaryTable", "numeric"),
          function(tt, j, newcinfo = NULL, ...) {
    j = .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo = col_info(tt)
        newcinfo = subset_cols(cinfo, j, ...)
    }
    kids = tree_children(tt)
    newkids = lapply(kids, subset_cols, j= j, newcinfo = newcinfo,  ...)
    tt2 = tt
    col_info(tt2) <- newcinfo
    tree_children(tt2) <- newkids
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
setMethod("subset_cols", c("TableRow", "numeric"),
          function(tt, j, newcinfo = NULL,  ...) {
    j = .j_to_posj(j, ncol(tt))
    if(is.null(newcinfo)) {
        cinfo = col_info(tt)
        newcinfo = subset_cols(cinfo, j, ...)
    }
    tt2 = tt
    row_values(tt2) = row_values(tt2)[j]
    if(length(row_cspans(tt2)) > 0) 
        row_cspans(tt2) = .fix_rowcspans(tt2, j)
    col_info(tt2) = newcinfo
    tt2
})

setMethod("subset_cols", c("LabelRow", "numeric"),
          function(tt, j, newcinfo = NULL,  ...) {
    tt
})


setMethod("subset_cols", c("InstantiatedColumnInfo", "numeric"),
          function(tt, j, newcinfo = NULL,  ...) {
    if(!is.null(newcinfo))
        return(newcinfo)
    j = .j_to_posj(j, length(col_exprs(tt)))
    newctree = subset_cols(coltree(tt), j, NULL)
    newcextra = cextra_args(tt)[j]
    newcsubs = col_exprs(tt)[j]
    newcounts = col_counts(tt)[j]
    InstantiatedColumnInfo(treelyt = newctree,
                           csubs = newcsubs,
                           extras = newcextra,
                           cnts = newcounts,
                           dispcounts = disp_ccounts(tt),
                           countfmt = colcount_fmt(tt))
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
subset_by_rownum = function(tt, i, ... ) {
    stopifnot(is(tt, "VTableNodeInfo"))
    counter = 0
    nr = nrow(tt)
    i = .j_to_posj(i, nr)
    
    prune_rowsbynum = function(x, i, valifnone = NULL) {
        maxi = max(i)
        if(counter >= maxi)
            return(valifnone)
        
        if(labrow_visible(x)) {
            counter <<- counter + 1
            if(!(counter %in% i)) {
                ## XXX this should do whatever
                ## is required to 'remove' the Label Row
                ## (currently implicit based on
                ## the value of the label but
                ## that shold really probably change)
                labrow_visible(x) <- FALSE
            }
        }
        if(is(x, "TableTree") && nrow(content_table(x)) > 0) {
            ctab = content_table(x)
            
            content_table(x) = prune_rowsbynum(ctab, i,
                                               valifnone = ElementaryTable(cinfo = col_info(ctab), iscontent = TRUE))
        }
        kids = tree_children(x)
        if(counter >= maxi) { #already done
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
            kids = kids[sapply(kids, function(x) length(x) > 0)]
        }
        tree_children(x) = kids
        if(length(kids) == 0) {
            if(!is(x, "TableTree"))
                return(valifnone)
        }
        if(is(x, "VTableTree") && nrow(x) > 0) {
            x
        } else {
            valifnone
        }
    }
    prune_rowsbynum(tt, i)
}



setMethod("[", c("VTableTree", "logical", "logical"),
          function(x, i, j, ..., drop = FALSE) {
    i = .j_to_posj(i, nrow(x))
    j = .j_to_posj(j, ncol(x))
    x[i,j, ..., drop = drop]
})

setMethod("[", c("VTableTree", "logical", "ANY"),
          function(x, i, j, ..., drop = FALSE) {
    i = .j_to_posj(i, nrow(x))
    x[i,j, ..., drop = drop]
})

setMethod("[", c("VTableTree", "ANY", "logical"),
          function(x, i, j, ..., drop = FALSE) {
    j = .j_to_posj(j, ncol(x))
    x[i,j, ..., drop = drop]
})

setMethod("[", c("VTableTree", "ANY", "missing"),
          function(x, i, j, ..., drop = FALSE) {
    j = seq_len(ncol(x))
    x[i = i,j = j, ..., drop = drop]
})


setMethod("[", c("VTableTree", "missing", "numeric"),
          function(x, i, j, ..., drop = FALSE) {
    i = seq_len(nrow(x))
    x[i,j, drop = drop]
})



setMethod("[", c("VTableTree", "numeric", "numeric"),
          function(x, i, j, ..., drop = FALSE) {
    nr = nrow(x)
    nc = ncol(x)
    i = .j_to_posj(i, nr)
    j = .j_to_posj(j, nc)
    
    if(!missing(j) && length(j) < nc)
        x = subset_cols(x, j)
    if(!missing(i) && length(i) < nr)
        x = subset_by_rownum(x, i)
    if(length(j) == 1L &&
       length(i) == 1L &&
       drop) {
        rw = collect_leaves(x, TRUE, TRUE)[[1]]
        if(is(rw, "LabelRow"))
            x = NULL
        else
            x = row_values(rw)[[1]]
    }
    x
})


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
    subtree
    

})

extract_colvals = function(tt, j) {
    


}
