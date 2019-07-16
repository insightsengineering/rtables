setClass("VNodeInfo", contains = "VIRTUAL", representation(level = "integer", label = "character"))
setClass("VTree", contains = c("VIRTUAL", "VNodeInfo"), representation(children = "list"))
setClass("VLeaf", contains = c("VIRTUAL", "VNodeInfo"),
         representation(leaf_value = "ANY"))
##
## Table Tree classes
##

setClass("VTableNodeInfo", contains = c("VNodeInfo", "VIRTUAL"),
         representation(col_layout = "LayoutColTree",
                        rowsplit_vars = "character",
                        rowsplit_values = "ANY"))
setClass("VTableTree", contains = c("VIRTUAL", "VTableNodeInfo"),
         representation(children = "list",
                        rowspans = "data.frame"
                        ))
setClass("VTableLeaf", contains = c("VIRTUAL", "VTableNodeInfo"),
         representation(leaf_value = "ANY",
                        var_analyzed = "character"))


## colspan and rowspan are not modeled here because I think they are layout
## and/or formatting, NOT table structure
setClass("TableRow", contains = "VTableLeaf", representation(colspans = "integer"),
         validity = function(object) {
    lcsp = length(object@colspans)
    length(lcsp ==  0) || lcsp == length(object@leaf_value)
})

TableRow = function(val = list(),  lev = 1L, lab = "", cspan = seq(along = val), clayout = LayoutColTree(), rs_vars = NA_character,
                    rs_values = NA,
                    var = NA_character_) {
    new("TableRow", leaf_value = val, level = lev, label = lab, colspans = cspan, col_layout = clayout, var_analyzed = var,
        rowsplit_vars = rs_vars,
        rowsplit_values = rs_values)
}

setClassUnion("IntegerOrNull", c("integer", "NULL"))
setClass("ElementaryTable", contains = "VTableTree",
         validity = function(object) {
    kids = tree_children(object)
    all(sapply(kids, is, "TableRow")) && all(sapply(kids, function(k) identical(k@col_layout, object@col_layout)))
})

ElementaryTable = function(kids = list(), lev = 1L, lab = "", rspans = data.frame(), clayout = NULL, rs_vars = NA_character_,
                           rs_values = NA) {
    if(is.null(clayout)) {
        if(length(kids) > 0)
            clayout = kids[[1]]@col_layout
        else
            clayout = LayoutColTree()
    }
    new("ElementaryTable", children = kids, level = lev, label = lab, rowspans = rspans, col_layout = clayout, rowsplit_vars = rs_vars,
        rowsplit_values = rs_values)
}

## under this model, non-leaf nodes can have a content table where rollup
## analyses live 

setClass("TableTree", contains = c("VTableTree"),
         representation(content = "ElementaryTable"),
         validity = function(object) {
    all(sapply(tree_children(object), function(x) is(x, "TableTree") || is(x, "ElementaryTable") || is(x, "TableRow"))) &&
        all((!is.na(object@rowsplit_vars)) |
            is.na(object@rowsplit_values))
})

TableTree = function(cont = ElementaryTable(), kids = list(), lev = 1L, lab = "", rspan = data.frame(), rs_vars = NA_character_, rs_values = NA,
                     clayout = cont@col_layout) {
    rs_values[sapply(rs_values, function(x) x == nasentinel)] = NA
    if(nrow(cont) == 0 && all(sapply(kids, is, "TableRow")))
        new("ElementaryTable", children = kids, level = lev, label = lab, rowspans = rpsan, col_layout = clayout, rowsplit_vars = rs_vars,
            rowsplit_values = rs_values)
    else 
        new("TableTree", content = cont, children = kids, level = lev, label = lab, rowspans = rspan, rowsplit_vars = rs_vars, rowsplit_values = rs_values,
            col_layout = clayout)
}


setMethod("nrow", "ElementaryTable",
          function(x) length(tree_children(x)))
setMethod("ncol", "ElementaryTable",
          function(x) {
    rs = tree_children(x)
    if(length(rs) > 0)
        max(sapply(tree_children(x),
                   function(x) length(x@leaf_value)))
    else
        0
})



docat = function(obj) {
    if(!is(obj, "ElementaryTable") && nrow(obj@content) > 0 ){
        crows = nrow(obj@content)
        ccols = if(crows == 0) 0 else ncol(obj@content)
        cat(rep("*", obj@level), sprintf(" %s [%d x %d]\n",
                                         obj@content@label,
                                         crows, ccols),
            sep = "")
        
    }
    if(is(obj, "VTableTree") && length(tree_children(obj))) {
        kids = tree_children(obj)
        isr = which(sapply(kids, is, "TableRow"))
        ## can they ever be inteerspersed, I don't think so
        if(length(isr)) {
            r = kids[[isr[1]]]
            lv = r@level
            if(is.na(lv)) lv = 0
            cat(rep("*", lv),
                sprintf(" %s [%d x %d] \n",
                        obj@label,
                        length(kids),
                        length(r@leaf_value)),
                sep="")
            kids = kids[-isr]
        }
        lapply(kids, docat)
    }
}
    
setMethod("show", "TableTree",
          function(object) {
    cat("\nA TableTree object\n")
    docat(object)
    
})

setMethod("show", "ElementaryTable",
          function(object) {
    cat("\nAn ElementaryTableTree object\n")
    docat(object)
    
})





##
## LayoutAxisTree classes 
##

setOldClass("function")
setOldClass("NULL")
setClassUnion("FunctionOrNULL", c("function", "NULL"))

setOldClass("expression")
setClassUnion("LayoutSubsetDef", c("expression", "logical", "integer", "numeric"))


setClass("LayoutAxisTree", contains= "VTree",
         representation(summary_func = "FunctionOrNULL",
                        subset = "LayoutSubsetDef",
                        splitvar = "ANY"), ## because what about quantile cuts...),
         validity = function(object) all(sapply(object@children, function(x) is(x, "LayoutAxisTree") || is(x, "LayoutAxisLeaf"))))

setClass("LayoutAxisLeaf", contains = "VNodeInfo",
         representation(func = "function",
                        subset = "LayoutSubsetDef"))

setClass("LayoutColTree", contains = "LayoutAxisTree")
##setClass("LayoutColBranch", contains = "LayoutAxisBranch")
setClass("LayoutColLeaf", contains = "LayoutAxisLeaf")
setClass("LayoutRowTree", contains = "LayoutAxisTree")
##setClass("LayoutRowBranch", contains = "LayoutAxisBranch")
setClass("LayoutRowLeaf", contains = "LayoutAxisLeaf")
LayoutColTree = function(lev = 0L,
                         lab = "",
                         kids = list(),
                         summary_function = NULL,
                         sub = expression(TRUE),
                         svar = NA_character_) {
    new("LayoutColTree", level = lev, children = kids,
        summary_func = summary_function,
        subset = sub,
        splitvar = svar,
        label = lab)
}

## LayoutColBranch = function(lev = 0L, kids = list(), lab = "") {
##     new("LayoutColBranch", level = lev, children = kids, label = lab)
## }

LayoutColLeaf = function(lev = 0L, lab = "", sub#,
                        ## n = NA_integer_,
                         #svar = NA_character_
                         ) {
    new("LayoutColLeaf", level = lev, label = lab, subset = sub#,
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

LayoutRowLeaf = function(lev = 0L, lab = "", sub, n) {
    new("LayoutRowLeaf", level = lev, label = lab, subset = sub, N_count = n)
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



setGeneric("tree_children", function(x) standardGeneric("tree_children"))
setMethod("tree_children", c(x = "VTree"),
          function(x) x@children)

setMethod("tree_children", c(x = "VTableTree"),
          function(x) x@children)

setGeneric("tree_children<-", function(x, value) standardGeneric("tree_children<-"))
setMethod("tree_children<-", c(x = "VTree"),
          function(x, value){
    x@children = value
    x
})

setMethod("tree_children<-", c(x = "VTableTree"),
          function(x, value){
    x@children = value
    x
})



## not worth the S4 dispatch to do it "right"... probably
n_leaves = function(tree) {
    kids = layout_children(tree)
    length(unlist(lapply(kids, function(x) if(is(x, "VLeaf")) TRUE else n_leaves(x))))
}
