
##Split types
## variable: split on distinct values of a variable
## all: include all observations (root 'split')
## rawcut: cut on static values of a variable
## quantilecut: cut on quantiles of observed values for a variable
## missing: split obs based on missingness of a variable/observation. This could be used for compare to baseline??
## multicolumn: each child analyzes a different column
## arbitrary: children are not related to eachother in any systematic fashion.


setOldClass("expression")
setClassUnion("SubsetDef", c("expression", "logical", "integer", "numeric"))

## should XXX [splits, s_values, sval_labels, subset(?)] be a data.frame?
setClass("TreePos", representation(splits = "list",
                                   s_values = "list",
                                   sval_labels = "character",
                                   subset = "SubsetDef"),
         validity = function(object) {
    length(unique(length(object@splits),
                  length(object@s_values),
                  length(object@sval_labels))) == 1L
})


## we may come back to needing this but I don't think we do right now.

## setClass("TableRowPos",
##          representation(parent_pos = "TableTreePos",
##                         is_content = "logical",
##                         local_rownum = "numeric"))

## these default to NULL even though NULL does not appear first
setClassUnion("functionOrNULL", c("function", "NULL"))
setClassUnion("FormatSpec",c("NULL", "character", "function"))


setClass("Split", contains = "VIRTUAL",
         representation(
             payload = "character",
             split_label = "character",
             split_format = "FormatSpec",
             ## NB this is the function which is applied to
             ## get the content rows for the CHILDREN of this
             ## split!!!
             content_fun = "functionOrNULL",
             content_format = "FormatSpec"))


setClass("CustomizableSplit", contains = "Split",
         representation( split_fun = "functionOrNULL"))

setClass("VarLevelSplit", contains = "CustomizableSplit",
         representation(value_lbl_var = "character",
                        value_order = "ANY"))

VarLevelSplit = function(var, splbl, valuelblvar = NULL, cfun = NULL, cfmt = NULL, splfun = NULL, splfmt = NULL, valorder = NULL ) {
    if(is.null(valuelblvar))
        valuelblvar = var
    new("VarLevelSplit", payload = var, split_label = splbl,
        value_lbl_var = valuelblvar,
        content_fun = cfun,
        content_format = cfmt,
        split_fun = splfun,
        split_format = splfmt,
        value_order = NULL
        )
}
setClass("NULLSplit", contains = "Split",
         validity = function(object) {
length(object@payload) == 0 && length(object@split_label) == 0})
 
NULLSplit = function(...) {
    ## only(!!) valid instantiation of NULLSplit class
    new("NULLSplit", payload = character(), split_label = character(), content_fun = NULL, content_format= NULL, split_format = NULL)
}

setClass("AllSplit", contains = "Split",
         validity = function(object) length(object@payload) == 0
         )

AllSplit = function(splbl = "", cfun = NULL, cfmt = NULL, splfmt = NULL, ...) {
    new("AllSplit", split_label = splbl,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt)
}

setClass("RootSplit", contains = "AllSplit")

RootSplit = function(splbl = "", cfun = NULL, cfmt = NULL, splfmt= NULL, ...) {
    new("RootSplit", split_label = splbl,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt)
}


## splits across which variables are being analynzed
setClass("MultiVarSplit", contains = "Split",
         representation(var_labels = "character"),
         validity = function(object) length(object@payload) > 1 && all(!is.na(object@payload)) && length(object@payload) == length(object@var_labels))


MultiVarSplit = function(vars, splbl, varlbls = NULL, cfun = NULL, cfmt = NULL, splfmt = NULL) {
    if(length(vars) == 1 && grepl(":", vars))
        vars = strsplit(vars, ":")[[1]]
    if(length(varlbls) == 0) ## covers NULL and character()
       varlbls = vars
    new("MultiVarSplit", payload = vars, split_label = splbl,
        var_labels = varlbls, content_fun = cfun,
        content_format = cfmt, split_format = splfmt)
}


setClass("VarStaticCutSplit", contains = "Split",
         representation(cuts = "numeric",
                        cut_labels = "character"))

VarStaticCutSplit = function(var, splbl, cuts, cutlbls = NULL, cfun = NULL, cfmt = NULL, splfmt = NULL) {
    if(is.list(cuts) && is.numeric(cuts[[1]]) &&
       is.character(cuts[[2]]) &&
       length(cuts[[1]]) == length(cuts[[2]])) {
        cutlbls = cuts[[2]]
        cuts = cuts[[1]]
    }
    if(is.unsorted(cuts, strictly = TRUE))
        stop("invalid cuts vector. not sorted unique values.")
    
    if(is.null(cutlbls) && !is.null(names(cuts)))
        cutlbls = names(cuts)
    new("VarStaticCutSplit", payload = var,
        split_label = splbl,
        cuts = cuts,
        cut_labels = cutlbls,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt)
}


## do we want this to be a CustomizableSplit instead of
## taking cut_fun?
## cut_funct must take avector and no other arguments
## and return a named vector of cut points
setClass("VarDynCutSplit", contains = "Split",
         representation(cut_fun = "function"))
                        
VarDynCutSplit = function(var, splbl, cutfun, cfun = NULL, cfmt = NULL, splfmt = NULL) {
    new("VarDynCutSplit", payload = var,
        split_label = splbl,
        cut_fun = cutfun,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt)
}

setClass("AnalyzeVarSplit", contains = "Split",
         representation(analysis_fun = "function"))

AnalyzeVarSplit = function(var, splbl, afun, cfun = NULL, cfmt = NULL, splfmt = NULL) {
    new("AnalyzeVarSplit",
        payload = var,
        split_label = splbl,
        content_fun = cfun,
        analysis_fun = afun,
        content_format = cfmt,
        split_format = splfmt)
}



## TODO in the future 
## setClass("BaselineCompSplit", contains = "Split",
##          representation(
##              factor_var = "character",
##              baseline_level = "character"))

        
Split = function(var, type, lbl, cfun = NULL,  cfmt = NULL, splfmt = NULL, extra = NULL) {
    switch(type,
           varlevels = VarLevelSplit(var, lbl, extra, cfun = cfun, cfmt = cfmt, splfmt = splfmt),
           multivar = MultiVarSplit(var, lbl, extra, cfun = cfun, cfmt = cfmt, splfmt = splfmt),
           null = NULLSplit(),
           all = AllSplit(splbl = lbl, cfun = cfun, cfmt = cfmt, splfmt = splfmt),
           staticcut = VarStaticCutSplit(var, lbl, extra, cfun = cfun, cfmt = cfmt, splfmt = splfmt),
           dyncut = VarDynCutSplit(var, lbl, extra, cfun = cfun, cfmt = cfmt, splfmt = splfmt),
           root = RootSplit(splbl = lbl, cfun = cfun, cfmt = cfmt, splfmt = splfmt),
           stop("Don't know how to make a split of type ", type)
           )
}


###
### Tree Position Representation
###
### Class(es) that represent position with in a
### tree as parallel vectors of Split objects and
### values chosen at that split, plus labeling info

TreePos = function(spls = list(), svals = list(), svlbls =  character(), sub = NULL) {
    if(is.null(sub)) {
        if(length(spls) > 0) {
            sub = make_pos_subset(spls = spls,
                                  svals = svals)
        } else {
            sub = expression(TRUE)
        }
    }
    new("TreePos", splits = spls, s_values = svals,
        sval_labels = svlbls,
        subset = sub)
}


setClass("TableTreePos", contains = "TreePos",
         representation(is_content="logical"),
         prototype = list(is_content = FALSE))

TableTreePos = function(iscontent = FALSE, ...) {
    tp = TreePos(...)
    new("TableTreePos", tp, is_content = iscontent)
}

setClass("TableRowPos", contains = "TableTreePos",
         representation(local_rownum = "numeric"),
         prototype = list(is_content = FALSE))


TableRowPos = function(localrow, iscontent = FALSE, ...) {
    ttpos = TableTreePos(iscontent, ...)
    new("TableRowPos", ttpos, local_rownum = localrow)
}


##
## Tree position convenience functions
##

make_child_pos = function(parpos, newspl, newval, newlab = newval) {
    newpos = TreePos(
        spls = c(pos_splits(parpos), newspl),
        svals = c(pos_splvals(parpos), newval),
        svlbls = c(pos_splval_lbls(parpos), newlab),
        sub = .combine_subset_exprs(pos_subset(parpos),
                                    make_subset_expr(newspl, newval)))
}

make_tablepos= function(treepos, iscontent) {
    ttpos = new("TableTreePos", treepos,
                is_content = iscontent)
}


make_rowpos = function(tp, rownum) {
    new("TableRowPos", tp,
        local_rownum = rownum)
}





### 
### Virtual Classes
###
### Virtual class hiearchy for the various types of
### trees in use in the S4 implementation of the TableTree
### machinery
###

## core basics
setClass("VNodeInfo", contains = "VIRTUAL",
         representation(level = "integer",
                        label = "character",
                        pos_in_tree = "TreePos"))
setClass("VTree", contains = c("VIRTUAL", "VNodeInfo"), representation(children = "list"))
setClass("VLeaf", contains = c("VIRTUAL", "VNodeInfo"))


## Layout trees
#setClass("VLayoutNode", contains= c("VIRTUAL", "VNodeInfo"))
setClass("VLayoutLeaf", contains = c("VIRTUAL", "VLeaf"))
setClass("VLayoutTree", contains = c("VIRTUAL", "VTree"),
         representation(split = "Split"))
setClassUnion("VLayoutNode", c("VLayoutLeaf", "VLayoutTree"))

## TableTrees
setClass("VTableNodeInfo", contains = c("VNodeInfo", "VIRTUAL"),
         representation(col_layout = "VLayoutNode",
                        format = "FormatSpec"))

setClass("VTableTree", contains = c("VIRTUAL", "VTableNodeInfo", "VTree"),
         representation(children = "list",
                        rowspans = "data.frame"
                        ))
setClass("VTableLeafInfo", contains = c("VIRTUAL", "VLeaf", "VTableNodeInfo"), 
         representation(leaf_value = "ANY",
                        var_analyzed = "character",
                        var_label = "character",
                        value_type = "ANY"))


##
## LayoutAxisTree classes 
##

setOldClass("function")
setOldClass("NULL")
setClassUnion("FunctionOrNULL", c("function", "NULL"))

setClass("LayoutAxisTree", contains= "VLayoutTree",
         representation(summary_func = "FunctionOrNULL"),
         validity = function(object) all(sapply(object@children, function(x) is(x, "LayoutAxisTree") || is(x, "LayoutAxisLeaf"))))

setClass("LayoutAxisLeaf", contains = "VLayoutLeaf", ##"VNodeInfo",
         representation(func = "function"))


setClass("LayoutColTree", contains = "LayoutAxisTree")
setClass("LayoutColLeaf", contains = "LayoutAxisLeaf")
setClass("LayoutRowTree", contains = "LayoutAxisTree")
setClass("LayoutRowLeaf", contains = "LayoutAxisLeaf")
LayoutColTree = function(lev = 0L,
                         lab = "",
                         kids = list(),
                         spl = NULL, 
                         tpos = TreePos(), 
                         summary_function = NULL ){## ,
                         ## sub = expression(TRUE),
                         ## svar = NA_character_,
    ## slab = NA_character_) {
    if(!is.null(spl)) {
        new("LayoutColTree", level = lev, children = kids,
            summary_func = summary_function,
            pos_in_tree = tpos,
            split = spl,
            ## subset = sub,
            ## splitvar = svar,
            label = lab)
    } else {
        new("LayoutColLeaf", level = lev, 
            func = if(is.null(summary_function)) length else summary_function,
            pos_in_tree = tpos,
            label = lab)
    }        
}

LayoutColLeaf = function(lev = 0L, lab = "", tpos#,
                        ## n = NA_integer_,
                         #svar = NA_character_
                         ) {
    new("LayoutColLeaf", level = lev, label = lab,
        pos_in_tree = tpos##, 
        ##subset = sub#,
        ##N_count = n,
        ##splitvar = svar
        )
}

LayoutRowTree = function(lev = 0L, kids = list()) {
    new("LayoutRowTree", level = lev, children = kids)
}

LayoutRowLeaf = function(lev = 0L, lab = "",
                       pos){##, sub, n) {
    new("LayoutRowLeaf", level = lev, label = lab,
        pos_in_tree = pos)##subset = sub, N_count = n)
}

setClass("RTablesLayout", contains="VIRTUAL", representation(col_layout = "LayoutColTree", row_layout = "LayoutRowTree"))

## dominant here means which subset is taken first to define the subset for
## a particular cell, the one associated with the table column or the one associated
## with the table row

setClass("RowDominantLayout", contains = "RTablesLayout")
setClass("ColDominantLayout", contains = "RTablesLayout")

rtables_layout = function(row_dominant = FALSE, rowtree = LayoutRowTree(),
                  coltree = LayoutColTree()) {
    
    if(row_dominant) {
        new("RowDominantLayout", col_layout = coltree, row_layout = rowtree)
    } else {
        new("ColDominantLayout", col_layout = coltree, row_layout = rowtree)
    }
}




### TableTree Core non-virtual Classes

              
setClass("TableRow", contains = "VTableLeafInfo",
         representation(colspans = "integer",
                        pos_in_tree = "TableRowPos"),
         validity = function(object) {
    lcsp = length(object@colspans)
    length(lcsp ==  0) || lcsp == length(object@leaf_value)
})

TableRow = function(val = list(),
                    lev = 1L,
                    lab = "",
                    cspan = rep(1L, length(val)),
                    clayout = LayoutColTree(),
                    tpos = TableRowPos(),
                    var = NA_character_,
                    var_lbl = NA_character_,
                    v_type = NA_character_,
                    fmt = NULL) {
    rw = new("TableRow", leaf_value = val,
        level = lev,
        label = lab,
        colspans = cspan,
        col_layout = clayout,
        pos_in_tree = tpos,
        var_analyzed = var,
        var_label = var_lbl,
        value_type = v_type,
        format = NULL)
    rw = set_fmt_recursive(rw, fmt, FALSE)
    rw
}

setClassUnion("IntegerOrNull", c("integer", "NULL"))
setClass("ElementaryTable", contains = "VTableTree",
         representation(var_analyzed = "character",
                        var_label = "character"),
         validity = function(object) {
    kids = tree_children(object)
    all(sapply(kids, is, "TableRow")) && all(sapply(kids, function(k) identical(k@col_layout, object@col_layout)))
})

ElementaryTable = function(kids = list(),
                           lev = 1L,
                           lab = "",
                           rspans = data.frame(),
                           clayout = NULL,
                           tpos = TableTreePos(),
                           iscontent = NA,
                           var = NA_character_,
                           var_lbl = var,
                           fmt = NULL) {
    if(is.null(clayout)) {
        if(length(kids) > 0)
            clayout = kids[[1]]@col_layout
        else
            clayout = LayoutColTree()
    }
    if(is.na(iscontent))
        iscontent = is_content_pos(tpos)
    else if(iscontent != is_content_pos(tpos)) {
        is_content_pos(tpos) = iscontent
    }
    tab = new("ElementaryTable",
        children = kids,
        level = lev,
        label = lab,
        rowspans = rspans,
        col_layout = clayout,
        pos_in_tree = tpos,
        var_analyzed = var,
        var_label = var_lbl,
        format = NULL)
    tab = set_fmt_recursive(tab, fmt, FALSE)
    tab
}

## under this model, non-leaf nodes can have a content table where rollup
## analyses live 

setClass("TableTree", contains = c("VTableTree"),
         representation(content = "ElementaryTable",
                        split = "Split"),
         validity = function(object) {
    all(sapply(tree_children(object), function(x) is(x, "TableTree") || is(x, "ElementaryTable") || is(x, "TableRow")))
})

TableTree = function(cont = ElementaryTable(),
                     kids = list(),
                     lev = 1L,
                     lab = "",
                     rspans = data.frame(),
                     tpos = TableTreePos(),
                     iscontent = NA,
                     spl = NULL,
                     var = NA_character_,
                     var_lbl = var,
                     clayout = NULL,
                     fmt = NULL) {
    if(is.null(clayout)) {
        if(!is.null(cont)) {
            clayout = clayout(cont)
        } else if(length(kids) > 0) {
            clayout = clayout(kids[[1]])
        } else {
            stop("unable to determine column layout")
        }
    }
    if(is.na(iscontent))
        iscontent = is_content_pos(tpos)
    else if(iscontent != is_content_pos(tpos))
        is_content_pos(tpos) = iscontent
    
    if(iscontent && !is.null(cont) && nrow(cont) > 0)
        stop("Got table tree with content table and content position")
    ## rs_values[sapply(rs_values, function(x) x == nasentinel)] = NA
    if((is.null(cont) || nrow(cont) == 0) && all(sapply(kids, is, "TableRow"))) {
        ## constructor takes care of recursive format application
        ElementaryTable(kids = kids, lev = lev, lab = lab,
                        rspans = rspans,
                        clayout = clayout,
                        tpos = tpos,
                        var = var,
                        var_lbl = var_lbl,
                        fmt = fmt)
    } else {
        tab = new("TableTree", content = cont,
            children = kids,
            level = lev,
            label = lab,
            rowspans = rspans,
            pos_in_tree = tpos,
            split = spl,
            col_layout = clayout,
            format = NULL)## ,
        ## var_label = var_lbl)
        tab = set_fmt_recursive(tab, fmt, FALSE)
        tab
    }
}


###
### Pre-Data Layout Declaration Classes
###
### Notably these are NOT represented as trees
### because without data we cannot know what the
### children should be.
###

##
## Vector (ordered list) of splits.
##
## This is a vector (ordered list) of splits to be
## applied recursively to the data when provided.
##
## For convenience, if this is length 1, it can contain
## a pre-existing TableTree/ElementaryTable.
## This is used for add_existing_table in
## colby_constructors.R
##

setClass("SplitVector", contains="list",
         validity = function(object) {
    if(length(object)  >= 1)
        lst = tail(object, 1)[[1]]
    else
        lst = NULL
    all(sapply(head(object, -1), is, "Split")) && (is.null(lst) || is(lst, "Split") || is(lst, "VTableNodeInfo"))
})

SplitVector = function(x = NULL,
                       ...,
                       lst = list(...)) {
    if(!is.null(x))
        lst = unlist(c(list(x), lst), recursive = FALSE)
    new("SplitVector", lst)
}

avar_noneorlast = function(vec) {
    if(length(vec) == 0)
        return(TRUE)
    isavar = which(sapply(vec, is, "AnalyzeVarSplit"))
    (length(isavar) == 0) || (length(isavar) == 1 && isavar == length(vec))
}

setClass("PreDataAxisLayout", contains = "list",
         representation(root_split = "ANY"),
         validity = function(object) {
     allleafs = unlist(object, recursive = TRUE)
     all(sapply(object, is, "SplitVector")) && all(sapply(allleafs, is, "Split")) && all(sapply(object, avar_noneorlast))
})



setClass("PreDataColLayout", contains = "PreDataAxisLayout")
setClass("PreDataRowLayout", contains = "PreDataAxisLayout")

PreDataColLayout = function(x = SplitVector(),
                            rtsp = RootSplit(),
                            ...,
                            lst = list(x, ...)) {
    ret =  new("PreDataColLayout", lst)
    ret@root_split = rtsp
    ret
}


PreDataRowLayout = function(x = SplitVector(),
                            root = RootSplit(),
                            ...,
                            lst = list(x, ...)) {
    new("PreDataRowLayout", lst, root_split = root)
}


setClass("PreDataTableLayouts",
         representation(row_layout = "PreDataRowLayout",
                        col_layout = "PreDataColLayout"))

PreDataTableLayouts = function(rlayout = PreDataRowLayout(),
                               clayout = PreDataColLayout()) {
    new("PreDataTableLayouts",
        row_layout = rlayout,
        col_layout = clayout)
}
        
