

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


setGeneric("pos_payloads", function(obj) standardGeneric("pos_payloads"))
setMethod("pos_payloads", "TreePos",
          function(obj) {
    spls = pos_splits(obj)
    lapply(spls, function(x) x@payload)
})

setMethod("pos_payloads", "VNodeInfo",
          function(obj) pos_payloads(tree_pos(obj)))

## setMethod("pos_payloads", "VLayoutNode",
##           function(obj) pos_payloads(tree_pos(obj)))

## XXX this is probably not thee right model for column layouts because
## we don't find ourselves consuming/walking a layout as a tree often
##
setGeneric("clayout_splits", function(obj) standardGeneric("clayout_splits"))
setMethod("clayout_splits", "LayoutColTree", function(obj) {
    ##this is going to descend to the first ("leftmost") leaf
    clayout_splits(tree_children(obj)[[1]])
})

setMethod("clayout_splits", "LayoutColLeaf", function(obj) {
    pos_splits(tree_pos(obj))
})

setMethod("clayout_splits", "VTableNodeInfo",
          function(obj) clayout_splits(clayout(obj)))



## setGeneric("rs_values", function(obj) standardGeneric("rs_values"))

## setMethod("rs_values", "VTableNodeInfo",
##           function(obj) tree_pos(obj)@s_values)

## setGeneric("rs_var_lbls", function(obj) standardGeneric("rs_var_lbls"))
 
## setMethod("rs_var_lbls", "VTableNodeInfo",
##           function(obj) {
##     spls = pos_splits(obj)
## })
## setGeneric("rs_value_lbls", function(obj) standardGeneric("rs_value_lbls"))

## setMethod("rs_value_lbls", "VTableNodeInfo",
##           function(obj) tree_pos(obj)@sval_labels)


setGeneric("content_table", function(obj) standardGeneric("content_table"))
setMethod("content_table", "TableTree",
          function(obj) obj@content)

setGeneric("clayout", function(obj) standardGeneric("clayout"))
setMethod("clayout", "VTableNodeInfo",
          function(obj) obj@col_layout)

setMethod("clayout", "PreDataTableLayouts",
          function(obj) obj@col_layout)


setGeneric("clayout<-", function(object, value) standardGeneric("clayout<-"))
setMethod("clayout<-", "PreDataTableLayouts",
          function(object, value) {
    object@col_layout = value
    object
})



setGeneric("rlayout", function(obj) standardGeneric("rlayout"))

setMethod("rlayout", "PreDataTableLayouts",
          function(obj) obj@row_layout)

setGeneric("rlayout<-", function(object, value) standardGeneric("rlayout<-"))
setMethod("rlayout<-", "PreDataTableLayouts",
          function(object, value) {
    object@row_layout = value
    object
})


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

setGeneric("row_variable", function(obj) standardGeneric("row_variable"))
setMethod("row_variable", "TableTree", function(obj) NA_character_)
setMethod("row_variable", "ElementaryTable", function(obj) obj@var_analyzed)
setMethod("row_variable", "TableRow", function(obj) obj@var_analyzed)

setGeneric("rowvar_label", function(obj) standardGeneric("rowvar_label"))
setMethod("rowvar_label", "TableTree", function(obj) NA_character_)
setMethod("rowvar_label", "ElementaryTable", function(obj) obj@var_label)
setMethod("rowvar_label", "TableRow", function(obj) obj@var_label)


setGeneric("tree_pos", function(obj) standardGeneric("tree_pos"))
setMethod("tree_pos", "VNodeInfo",
          function(obj) obj@pos_in_tree)
## setMethod("tree_pos", "VLayoutNode",
##           function(obj) obj@pos_in_tree)



setGeneric("pos_subset", function(obj) standardGeneric("pos_subset"))
setMethod("pos_subset", "TreePos",
          function(obj) obj@subset)
setMethod("pos_subset", "VNodeInfo",
          function(obj) pos_subset(tree_pos(obj)))
## setMethod("pos_subset", "VLayoutNode",
##           function(obj) pos_subset(tree_pos(obj)))

setGeneric("pos_splits", function(obj) standardGeneric("pos_splits"))
setMethod("pos_splits", "TreePos",
          function(obj) obj@splits)
setMethod("pos_splits", "VNodeInfo",
          function(obj) pos_splits(tree_pos(obj)))
## setMethod("pos_splits", "VLayoutNode",
##           function(obj) pos_splits(tree_pos(obj)))


setGeneric("pos_splvals", function(obj) standardGeneric("pos_splvals"))
setMethod("pos_splvals", "TreePos",
          function(obj) obj@s_values)

setMethod("pos_splvals", "VNodeInfo",
          function(obj) pos_splvals(tree_pos(obj)))
## setMethod("pos_splvals", "VLayoutNode",
##           function(obj) pos_splvals(tree_pos(obj)))


         

setGeneric("pos_split_lbls", function(obj) standardGeneric("pos_split_lbls"))
setMethod("pos_split_lbls", "TreePos",
          function(obj) {
    spls = pos_splits(obj)
    sapply(spls, function(x) x@split_label)
})

setMethod("pos_split_lbls", "VNodeInfo",
           function(obj) pos_split_lbls(tree_pos(obj)))
## setMethod("pos_split_lbls", "VLayoutNode",
##            function(obj) pos_split_lbls(tree_pos(obj)))


setGeneric("split_texttype", function(obj) standardGeneric("split_texttype"))
setMethod("split_texttype", "VarLevelSplit", function(obj) "varlevels")
setMethod("split_texttype", "MultiVarSplit", function(obj) "multivar")
setMethod("split_texttype", "AllSplit", function(obj) "allobs")
setMethod("split_texttype", "RootSplit", function(obj) "root")
setMethod("split_texttype", "NULLSplit", function(obj) "null")
setMethod("split_texttype", "VarStaticCutSplit", function(obj) "scut")
setMethod("split_texttype", "VarDynCutSplit", function(obj) "dyncut")

setMethod("split_texttype", "ANY", function(obj) stop("unknown split type"))

setGeneric("pos_spltypes", function(obj) standardGeneric("pos_spltypes"))
setMethod("pos_spltypes", "TreePos",
          function(obj) {
    spls = pos_splits(obj)
    sapply(spls, split_texttype)
})

setMethod("pos_spltypes", "VNodeInfo",
          function(obj) pos_spltypes(tree_pos(obj)))
## setMethod("pos_spltypes", "VLayoutNode",
##           function(obj) pos_spltypes(tree_pos(obj)))


          

setGeneric("pos_splval_lbls", function(obj) standardGeneric("pos_splval_lbls"))
setMethod("pos_splval_lbls", "TreePos",
          function(obj) obj@sval_labels)
setMethod("pos_splval_lbls", "VNodeInfo",
           function(obj) pos_splval_lbls(tree_pos(obj)))
## setMethod("pos_splval_lbls", "VLayoutNode",
##            function(obj) pos_splval_lbls(tree_pos(obj)))



setGeneric("is_content_pos", function(obj) standardGeneric("is_content_pos"))
## this covers TableRowPos too via inheritence
setMethod("is_content_pos", "TableTreePos",
          function(obj) obj@is_content)

setMethod("is_content_pos", "ElementaryTable",
          function(obj) is_content_pos(tree_pos(obj)))
setMethod("is_content_pos", "TableTree",
          function(obj) FALSE) ## if it were true it'd have to be an ElementaryTable

setMethod("is_content_pos", "TableRow",
          function(obj) is_content_pos(tree_pos(obj))) ## if it were true it'd have to be an Elementar



setGeneric("is_content_pos<-", function(obj, value) standardGeneric("is_content_pos<-"))
## this covers TableRowPos too via inheritence
setMethod("is_content_pos<-", "TableTreePos",
          function(obj, value) {
    obj@is_content = value
    obj
})

setGeneric("spl_payload", function(obj) standardGeneric("spl_payload"))
setMethod("spl_payload", "Split", function(obj) obj@payload)



setGeneric("obj_label", function(obj) standardGeneric("obj_label"))
setMethod("obj_label", "Split", function(obj) obj@split_label)
setMethod("obj_label", "VNodeInfo", function(obj) obj@label)



setGeneric("content_fun", function(obj) standardGeneric("content_fun"))
setMethod("content_fun", "Split", function(obj) obj@content_fun)


setGeneric("content_fun<-", function(object, value) standardGeneric("content_fun<-"))
setMethod("content_fun<-", "Split", function(object, value) {
    object@content_fun = value
    object
})


setGeneric("analysis_fun", function(obj) standardGeneric("analysis_fun"))
setMethod("analysis_fun", "AnalyzeVarSplit", function(obj) obj@analysis_fun)

setGeneric("split_fun", function(obj) standardGeneric("split_fun"))
setMethod("split_fun", "CustomizableSplit", function(obj) obj@split_fun)

setGeneric("spl_lblvar", function(obj) standardGeneric("spl_lblvar"))
setMethod("spl_lblvar", "VarLevelSplit", function(obj) obj@value_lbl_var)

setGeneric("spl_child_order", function(obj) standardGeneric("spl_child_order"))
setMethod("spl_child_order", "VarLevelSplit", function(obj) obj@value_order)

setGeneric("spl_child_order<-",
           function(obj, value) standardGeneric("spl_child_order<-"))
setMethod("spl_child_order<-", "VarLevelSplit",
          function(obj, value) {
    obj@value_order = value
    obj
})





setGeneric("root_spl", function(obj) standardGeneric("root_spl"))
setMethod("root_spl", "PreDataAxisLayout",
          function(obj) obj@root_split)


setGeneric("root_spl<-", function(obj, value) standardGeneric("root_spl<-"))
setMethod("root_spl<-", "PreDataAxisLayout",
          function(obj, value) {
    obj@root_split <- value
    obj
})


setGeneric("row_values", function(obj) standardGeneric("row_values"))
setMethod("row_values", "TableRow", function(obj) obj@leaf_values)


setGeneric("obj_fmt", function(obj) standardGeneric("obj_fmt"))
## this covers rcell, etc
setMethod("obj_fmt", "ANY", function(obj) attr(obj, "format"))
setMethod("obj_fmt", "TableRow", function(obj) obj@format)
setMethod("obj_fmt", "Split", function(obj) obj@format)

setGeneric("obj_fmt<-", function(obj, value) standardGeneric("obj_fmt<-"))
## this covers rcell, etc
setMethod("obj_fmt<-", "ANY", function(obj, value) {
    attr(obj, "format") = value
    obj
})
setMethod("obj_fmt<-", "TableRow", function(obj, value) {
    obj@format = value
    obj
})

setMethod("obj_fmt<-", "Split", function(obj, value) {
    obj@split_format = value
    obj
})




setGeneric("content_fmt", function(obj) standardGeneric("content_fmt"))
setMethod("content_fmt", "Split", function(obj) obj@format)

setGeneric("content_fmt<-", function(obj, value) standardGeneric("content_fmt<-"))

setMethod("content_fmt<-", "Split", function(obj, value) {
    obj@content_format = value
    obj
})



setGeneric("collect_leaves",
           function(ttree, incl.cont = TRUE)
    standardGeneric("collect_leaves"), signature = "ttree")

##handle the ext
setMethod("collect_leaves", "TableTree",
          function(ttree, incl.cont = TRUE) {
    ret = c(if(incl.cont) {tree_children(content_table(ttree))},
      lapply(tree_children(ttree),
             collect_leaves))
    unlist(ret, recursive = TRUE)
})

setMethod("collect_leaves", "VTree",
          function(ttree, incl.cont) {
    ret = lapply(tree_children(ttree),
                 collect_leaves)
    unlist(ret, recursive = TRUE)
})

setMethod("collect_leaves", "VLeaf",
          function(ttree, incl.cont) {
    ttree
})

setMethod("collect_leaves", "ANY",
          function(ttree, incl.cont)
    stop("class ", class(ttree), " does not inherit from VTree or VLeaf"))


