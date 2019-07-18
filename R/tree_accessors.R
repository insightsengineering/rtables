

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


setGeneric("rs_vars", function(obj) standardGeneric("rs_vars"))

setMethod("rs_vars", "VTableNodeInfo",
          function(obj) obj@rowsplit_vars)

setGeneric("rs_values", function(obj) standardGeneric("rs_values"))

setMethod("rs_values", "VTableNodeInfo",
          function(obj) obj@rowsplit_values)

setGeneric("rs_var_lbls", function(obj) standardGeneric("rs_var_lbls"))

setMethod("rs_var_lbls", "VTableNodeInfo",
          function(obj) obj@rowsplit_var_lbls)

setGeneric("rs_value_lbls", function(obj) standardGeneric("rs_value_lbls"))

setMethod("rs_value_lbls", "VTableNodeInfo",
          function(obj) obj@rowsplit_value_lbls)


setGeneric("content_table", function(obj) standardGeneric("content_table"))
setMethod("content_table", "TableTree",
          function(obj) obj@content)

setGeneric("clayout", function(obj) standardGeneric("clayout"))
setMethod("clayout", "VTableNodeInfo",
          function(obj) obj@col_layout)


setGeneric("df_datcol_names", function(obj) standardGeneric("df_datcol_names"))
setMethod("df_datcol_names",
          "VTableNodeInfo",
          function(obj) df_datcol_names(clayout(obj)))
setMethod("df_datcol_names", "LayoutColTree",
          function(obj) {
    kids = tree_children(obj)
    ret = unlist(lapply(names(kids),  function(nm) paste(nm, df_datcol_names(kids[[nm]]), sep = "___")))
    inds = grep("([[:alnum:]]+___)+[[:alnum:]]*$", ret)
    ret[inds] = gsub("___$",  "", ret[inds])
    ret
})
setMethod("df_datcol_names", "LayoutColLeaf",
          function(obj) "")
