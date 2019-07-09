setClass("VNodeInfo", contains = "VIRTUAL", representation(level = "integer", label = "character"))
setClass("VTree", contains = c("VIRTUAL", "VNodeInfo"), representation(children = "list"))
setClass("VLeaf", contains = c("VIRTUAL", "VNodeInfo"),
         representation(leaf_value = "ANY"))
##
## Table Tree classes
##


## colspan and rowspan are not modeled here because I think they are layout
## and/or formatting, NOT table structure
setClass("TableRow", contains = "VLeaf")
TableRow = function(val = list(),  lev = 1L, lab = "") {
    new("TableRow", leaf_value = val, level = lev, label = lab)
}

setClass("ElementaryTable", contains = "VTree",
         validity = function(object) {
    kids = tree_children(object)
    all(sapply(kids, is, "TableRow"))
})

ElementaryTable = function(kids = list(), lev = 1L, lab = "") {
    new("ElementaryTable", children = kids, level = lev, label = lab)
}

## under this model, non-leaf nodes can have a content table where rollup
## analyses live 

setClass("TableTree", contains = c("VTree"), representation(content = "ElementaryTable"),
         validity = function(object) {
    all(sapply(tree_children(object), function(x) is(x, "TableTree") || is(x, "ElementaryTable") || is(x, "TableRow")))
})

TableTree = function(cont = ElementaryTable(), kids = list(), lev = 1L, lab = "") {
    if(nrow(cont) == 0 && all(sapply(kids, is, "TableRow")))
        new("ElementaryTable", children = kids, level = lev, label = lab)
    else 
        new("TableTree", content = cont, children = kids, level = lev, label = lab)
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
    if(!is(obj, "ElementaryTable") ){
        crows = nrow(obj@content)
        ccols = if(crows == 0) 0 else ncol(obj@content)
        cat(rep("*", obj@level), sprintf(" %s [%d x %d]\n",
                                         obj@content@label,
                                         crows, ccols),
            sep = "")
        
    }
    if(is(obj, "VTree") && length(tree_children(obj))) {
        kids = tree_children(obj)
        isr = which(sapply(kids, is, "TableRow"))
        ## can they ever be inteerspersed, I don't think so
        if(length(isr)) {
            r = kids[[isr[1]]]
            cat(rep("*", r@level),
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
setClassUnion("LeafSubsetDef", c("expression", "logical", "integer", "numeric"))

setClass("LayoutAxisTree", contains= "VTree",
         representation(summary_func = "FunctionOrNULL"),
         validity = function(object) all(lapply(object@children, function(x) is(x, "LayoutAxisTree") || is(x, "LayoutAxisLeaf"))))

setClass("LayoutAxisLeaf", contains = "VNodeInfo",
         representation(func = "function",
                        subset = "LeafSubsetDef"))

setClass("LayoutColTree", contains = "LayoutAxisTree")
##setClass("LayoutColBranch", contains = "LayoutAxisBranch")
setClass("LayoutColLeaf", contains = "LayoutAxisLeaf")
setClass("LayoutRowTree", contains = "LayoutAxisTree")
##setClass("LayoutRowBranch", contains = "LayoutAxisBranch")
setClass("LayoutRowLeaf", contains = "LayoutAxisLeaf")
LayoutColTree = function(lev = 0L, kids = list(), summary_function = NULL) {
    new("LayoutColTree", level = lev, children = kids,
        summary_func = summary_function)
}

## LayoutColBranch = function(lev = 0L, kids = list(), lab = "") {
##     new("LayoutColBranch", level = lev, children = kids, label = lab)
## }

LayoutColLeaf = function(lev = 0L, lab = "", sub, n) {
    new("LayoutColLeaf", level = lev, label = lab, subset = sub, N_count = n)
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

setGeneric("tree_children<-", function(x, value) standardGeneric("tree_children<-"))
setMethod("tree_children<-", c(x = "VTree"),
          function(x, value){
    x@children = value
    x
})



## not worth the S4 dispatch to do it "right"... probably
n_leaves = function(tree) {
    kids = layout_children(tree)
    length(unlist(lapply(kids, function(x) if(is(x, "VLeaf")) TRUE else n_leaves(x))))
}
