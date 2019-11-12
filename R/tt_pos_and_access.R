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
        
        



trow_to_pathdf = function(tr, i) {
    path = dput(pos_splvals(tr))
    
    data.frame(row.names = path, check.rows = FALSE, 



}




pathmap = function(tt) {
    leaves = collect_leaves(tt, incl.cont = TRUE, add.labrows = FALSE)
    do.call(rbind.data.frame,
            lapply(leaves, 
    




}
