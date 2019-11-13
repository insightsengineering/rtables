pos_from_values = function(vals, tab) {
    splvec = list()
    curtree = tab
    for(v in vals) {
        if(is.factor(v)) v = levels(v)[v]
        kids = tree_children(curtree)
        stopifnot(v %in% names(kids))
        splvec = c(splvec, list(current_spl(curtree)))
        curtree = kids[[v]]
    }
    


}

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

    if(is.null(i))
        i = seq_along(tree_children(x))
    
    if(length(value) != length(i))
        value = rep(value, length.out = length(i))
    
    if(are(value, "TableRow")) {
        newrows =value
    } else {
        newrows = lapply(i,
                         function(ind) {
            TableRow(value[[ind]],
                     cinfo = col_info(x), tpos = make_rowpos(tree_pos(x),i[ind]))
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
setMethod("[<-", "VTableTree",
          function(x, i, j, ...,  value) {
    stopifnot(are(tree_children(x), "TableRow"))
    if(is.null(j)) {
        replace_rows(x, i, value)
    
    } else { ## replacing only elements within certain rows
        if(is.null(i)) ## all of them
            i = seq_along(tree_children(x))

        if(is.null(dim(value)))
            value = matrix(value, nrow = length(i), ncol = length(j))
        
        rws = tree_children(x)
        modrws = lapply(seq_along(i),
                     function(pos) {
            r = rws[[ i[pos] ]]
            rvals = row_values(r)
            rvals[j] =  value[pos,] #whole row of value matrix
            row_values(r) = rvals
            r
        })
        rws[i] = modrws
        tree_children(x) = rws
    }
    x
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
    if(any(j < 0))
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


setMethod("subset_cols", c("InstantiatedColumnInfo", "numeric"),
          function(tt, j, newcinfo = NULL,  ...) {
    if(!is.null(newcinfo))
        return(newcinfo)
    j = .j_to_posj(j, length(length(col_exprs(tt))))
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
            NULL
        }
    }
    prune_children(tt, j)
})



numrows = function(tt, labs = TRUE) length(collect_leaves(tt, TRUE, labs))
## TODO: we really should choose whether
## label rows are counted or not here, I think
## allowing it both ways is probably just too
## much complexity
subset_by_rownum = function(tt, i, inc.labrows = FALSE, ...) {
    stopifnot(is(tt, "VTableNodeInfo"))
    counter = 0
    nr = numrows(tt, inc.labrows)
    i = .j_to_posj(i, nr)

    prune_rowsbynum = function(x, i, valifnone = NULL) {
        maxi = max(i)
        if(counter >= maxi)
            return(valifnone)
        
        if(inc.labrows && nzchar(obj_label(x))) {
            counter <<- counter + 1
            if(!(counter %in% i)) {
                ## XXX this should do whatever
                ## is required to 'remove' the Label Row
                ## (currently implicit based on
                ## the value of the label but
                ## that shold really probably change)
                obj_label(x) = ""
            }
        }
        if(is(x, "TableTree") && nrow(content_table(x)) > 0) {
            ctab = content_table(x)
            
            content_table(x) = prune_rowsbynum(ctab, i, valifnone = ElementaryTable(cinfo = col_info(ctab), tpos = tree_pos(ctab), iscontent = TRUE))
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
            if(is(x, "TableTree"))
                current_spl(x) = NULLSplit()
            else
                return(valifnone)
        }
        if(is(x, "VTableTree") && numrows(x, inc.labrows) > 0) {
            x
        } else {
            valifnone
        }
    }
    prune_rowsbynum(tt, i)
}


## trow_to_pathdf = function(tr, i) {
##     path = dput(pos_splvals(tr))
    
##     data.frame(row.names = path, check.rows = FALSE, 



## }




## pathmap = function(tt) {
##     leaves = collect_leaves(tt, incl.cont = TRUE, add.labrows = FALSE)
##     do.call(rbind.data.frame,
##             lapply(leaves, 
    




## }
