
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
                        
setClassUnion("functionOrNULL", c("function", "NULL"))

setClass("Split", contains = "VIRTUAL",
         representation(
             payload = "character",
             ## type = "character",
             split_label = "character",
             content_fun = "functionOrNULL"))



setClass("VarLevelSplit", contains = "Split",
         representation(value_lbl_var = "character"))

VarLevelSplit = function(var, splbl, valuelblvar = NULL, cfun = NULL) {
    if(is.null(valuelblvar))
        valuelblvar = var
    new("VarLevelSplit", payload = var, split_label = splbl,
        value_lbl_var = valuelblvar,
        content_fun = cfun)
}
setClass("NULLSplit", contains = "Split",
         validity = function(object) {
length(object@payload) == 0 && length(object@split_label) == 0})
 
NULLSplit = function(...) {
    ## only(!!) valid instantiation of NULLSplit class
    new("NULLSplit", payload = character(), split_label = character(), content_fun = NULL)
}

setClass("AllSplit", contains = "Split",
         validity = function(object) length(object@payload) == 0
         )

AllSplit = function(splbl = "", cfun = NULL,  ...) {
    new("AllSplit", split_label = splbl,
        content_fun = cfun)
}


## splits across which variables are being analynzed
setClass("MultiVarSplit", contains = "Split",
         representation(var_labels = "character"),
         validity = function(object) length(object@payload) > 1 && all(!is.na(object@payload)) && length(object@payload) == length(object@var_labels))


MultiVarSplit = function(vars, splbl, varlbls = NULL, cfun = NULL) {
    if(length(vars) == 1 && grepl(":", vars))
        vars = strsplit(vars, ":")[[1]]
    if(length(varlbls) == 0) ## covers NULL and character()
       varlbls = vars
    new("MultiVarSplit", payload = vars, split_label = splbl,
        var_labels = varlbls, content_fun = cfun)
}


setClass("VarStaticCutSplit",
         representation(cuts = "numeric",
                        cut_labels = "character"))

VarStaticCutSplit = function(var, splbl, cuts, cutlbls = NULL, cfun = NULL) {
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
        content_fun = cfun)
}

## cut_funct must take avector and no other arguments
## and return a named vector of cut points
setClass("VarDynCutSplit",
         representation(cut_fun = "function"))
                        
VarDynCutSplit = function(var, splbl, cutfun, cfun = NULL) {
    new("VarDynCutSplit", payload = var,
        split_label = splbl,
        cut_fun = cutfun,
        content_fun = cfun)
}

setClass("AnalyzeVarSplit",
         representation(analysis_fun = "function"))

AnalyzeVarSplit = function(var, splbl, afun, cfun = NULL) {
    new("AnalyzeVarSplit",
        payload = var,
        split_label = splbl,
        content_fun = cfun,
        analysis_fun = afun)
}



        
Split = function(var, type, lbl, cfun = NULL,  extra = NULL) {
    switch(type,
           varlevels = VarLevelSplit(var, lbl, extra, cfun = cfun),
           multivar = MultiVarSplit(var, lbl, extra, cfun = cfun),
           null = NULLSplit(),
           all = AllSplit(splbl = lbl, cfun = cfun),
           staticcut = VarStaticCutSplit(var, lbl, extra, cfun = cfun),
           dyncut = VarDynCutSplit(var, lbl, extra, cfun = cfun),
           stop("Don't know how to make a split of type ", type)
           )
}




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


## convenience
make_child_pos = function(parpos, newspl, newval, newlab = newval) {
    newpos = TreePos(
        spls = c(pos_splits(parpos), newspl),
        svals = c(pos_splvals(parpos), newval),
        svlbls = c(pos_splval_lbls(parpos), newlab),
        sub = .combine_subset_exprs(pos_subset(parpos),
                                    make_subset_expr(newspl, newval)))
}




## TODO in the future 
## setClass("BaselineCompSplit", contains = "Split",
##          representation(
##              factor_var = "character",
##              baseline_level = "character"))



setClass("VNodeInfo", contains = "VIRTUAL", representation(level = "integer", label = "character"))
setClass("VTree", contains = c("VIRTUAL", "VNodeInfo"), representation(children = "list"))
setClass("VLeaf", contains = c("VIRTUAL", "VNodeInfo"),
         representation(leaf_value = "ANY"))
##
## Table Tree classes
##

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

make_rowpos = function(tp, rownum) {
    new("TableRowPos", tp,
        local_rownum = rownum)
}
                        
setClass("VTableNodeInfo", contains = c("VNodeInfo", "VIRTUAL"),
         representation(col_layout = "VLayoutNode",
                        pos_in_tree = "TreePos")) ## ,rowsplit_vars = "character",
##                         rowsplit_var_lbls = "character",
##                         rowsplit_values = "ANY",
##                         rowsplit_value_lbls = "character"))
setClass("VTableTree", contains = c("VIRTUAL", "VTableNodeInfo"),
         representation(children = "list",
                        rowspans = "data.frame"
                        ))
setClass("VTableLeaf", contains = c("VIRTUAL", "VTableNodeInfo"),
         representation(leaf_value = "ANY",
                        var_analyzed = "character",
                        var_label = "character",
                        value_type = "ANY"))


## colspan and rowspan are not modeled here because I think they are layout
## and/or formatting, NOT table structure
setClass("TableRow", contains = "VTableLeaf",
         representation(colspans = "integer",
                        pos_in_tree = "TableRowPos" ),
         validity = function(object) {
    lcsp = length(object@colspans)
    length(lcsp ==  0) || lcsp == length(object@leaf_value)
})

TableRow = function(val = list(),
                    lev = 1L,
                    lab = "",
                    cspan = seq(along = val),
                    clayout = LayoutColTree(),
                    tpos = TableRowPos(),
                    var = NA_character_,
                    var_lbl = NA_character_,
                    v_type = NA_character_) {
    new("TableRow", leaf_value = val,
        level = lev,
        label = lab,
        colspans = cspan,
        col_layout = clayout,
        pos_in_tree = tpos,
        var_analyzed = var,
        var_label = var_lbl,
        value_type = v_type)
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
                           ## rs_vars = NA_character_,
                           ## rs_var_lbls = rs_vars,
                           ## rs_values = NA,
                           ## rs_value_lbls = as.character(rs_values),
                           tpos = TableTreePos(),
                           iscontent = NA,
                           var = NA_character_,
                           var_lbl = var) {
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
    new("ElementaryTable",
        children = kids,
        level = lev,
        label = lab,
        rowspans = rspans,
        col_layout = clayout,
        pos_in_tree = tpos,
        ## rowsplit_vars = rs_vars,
        ## rowsplit_var_lbls = rs_var_lbls,
        ## rowsplit_values = rs_values,
        ## rowsplit_value_lbls = rs_value_lbls,
        
        var_analyzed = var,
        var_label = var_lbl)
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
                     ## rs_vars = NA_character_,
                     ## rs_var_lbls = rs_vars,
                     ## rs_values = NA,
                     ## rs_value_lbls = as.character(rs_values),
                     tpos = TableTreePos(),
                     iscontent = NA,
                     spl = NULL,
                     var = NA_character_,
                     var_lbl = var,
                     clayout = NULL) {
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
    if(is.null(cont) || (nrow(cont) == 0 && all(sapply(kids, is, "TableRow"))))
        ElementaryTable(kids = kids, lev = lev, lab = lab,
                        rspans = rspans,
                        clayout = clayout,
                        ## rs_vars = rs_vars,
                        ## rs_var_lbls = rs_var_lbls,
                        ## rs_values = rs_values,
                        ## rs_value_lbls = rs_value_lbls,
                        tpos = tpos,
                        var = var,
                        var_lbl = var_lbl)
    ## new("ElementaryTable", children = kids, level = lev, label = lab, rowspans = rpsan, col_layout = clayout, rowsplit_vars = rs_vars,
    ##         rowsplit_values = rs_values,)
    else {
        new("TableTree", content = cont,
            children = kids,
            level = lev,
            label = lab,
            rowspans = rspans,
            ## rowsplit_vars = rs_vars,
            ## rowsplit_var_lbls = rs_var_lbls,
            ## rowsplit_values = rs_values,
            ## rowsplit_value_lbls = rs_value_lbls,
            pos_in_tree = tpos,
            split = spl,
            col_layout = clayout)## ,
            ## var_label = var_lbl)
    }
}



##
## LayoutAxisTree classes 
##

setOldClass("function")
setOldClass("NULL")
setClassUnion("FunctionOrNULL", c("function", "NULL"))


setClass("VLayoutNode", contains= c("VIRTUAL", "VNodeInfo"),
         representation(pos_in_tree = "TreePos"))
## Careful here. Is this multiple-inheritence ok?
setClass("VLayoutTree", contains = c("VIRTUAL", "VTree", "VLayoutNode"), representation(split = "Split"))
         
        


setClass("LayoutAxisTree", contains= "VLayoutTree",
         representation(summary_func = "FunctionOrNULL"),
                        
                        ## subset = "LayoutSubsetDef",
                        ## splitvar = "ANY",
                        ## splitlbl = "character"), ## because what about quantile cuts...),
         validity = function(object) all(sapply(object@children, function(x) is(x, "LayoutAxisTree") || is(x, "LayoutAxisLeaf"))))

setClass("LayoutAxisLeaf", contains = "VLayoutNode", ##"VNodeInfo",
         representation(func = "function"))


setClass("LayoutColTree", contains = "LayoutAxisTree")
##setClass("LayoutColBranch", contains = "LayoutAxisBranch")
setClass("LayoutColLeaf", contains = "LayoutAxisLeaf")
setClass("LayoutRowTree", contains = "LayoutAxisTree")
##setClass("LayoutRowBranch", contains = "LayoutAxisBranch")
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

## LayoutColBranch = function(lev = 0L, kids = list(), lab = "") {
##     new("LayoutColBranch", level = lev, children = kids, label = lab)
## }

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

## LayoutRowBranch = function(lev = 0L, kids = list(), lab = "") {
##     new("LayoutRowBranch", level = lev, children = kids, label = lab)
## }

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



setClass("SplitVector", contains="list",
         validity = function(object) {
    all(sapply(object, is, "Split"))
})

SplitVector = function(x = AllSplit(),
                       ...,
                       lst = list(x, ...)) {
    new("SplitVector", lst)
}

avar_noneorlast = function(vec) {
    isavar = which(sapply(vec, is, "AnalyzeVarSplit"))
    (length(isavar) == 0) || (length(isavar) == 1 && isavar == length(vec))
}

setClass("PreDataAxisLayout", contains = "list",
         validity = function(object) {
     allleafs = unlist(object, recursive = TRUE)
     all(sapply(object, is, "SplitVector")) && all(sapply(allleafs, is, "Split")) && all(sapply(object, avar_noneorlast))
})



setClass("PreDataColLayout", contains = "PreDataAxisLayout")
setClass("PreDataRowLayout", contains = "PreDataAxisLayout")

PreDataColLayout = function(x = SplitVector(),
                            ...,
                            lst = list(x, ...)) {
    new("PreDataColLayout", lst)
}


PreDataRowLayout = function(x = SplitVector(),
                            ...,
                            lst = list(x, ...)) {
    new("PreDataRowLayout", lst)
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
        
