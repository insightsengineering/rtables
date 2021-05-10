## XXX Do we want add.labrows here or no?
## we have to choose one and stick to it.

#' @name internal_methods
#' @title Internal Generics and Methods
#' @rdname int_methods
#' @description These are internal methods that are documented only to satisfy
#' R CMD check. End users should pay no attention to this documentation.
#' @inheritParams gen_args
#' @inheritParams constr_args
#' @inheritParams lyt_args
#' @param x The object.
#' @param obj The object.
NULL

#' @rdname dimensions
#' @return the number of rows (nrow), columns (ncol) or both (dim) of the object.
#' @exportMethod nrow
setMethod("nrow", "VTableTree",
          function(x) length(collect_leaves(x, TRUE ,TRUE)))

#' @rdname dimensions
#' @exportMethod nrow
setMethod("nrow", "TableRow",
          function(x) 1L)

#' Table Dimensions
#' @rdname dimensions
#'
#' @exportMethod ncol
#'
#' @param x `TableTree` or `ElementaryTable` object
#'
#' @examples
#' tbl <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("SEX", "AGE")) %>%
#'   build_table(ex_adsl)
#'
#' dim(tbl)
#' nrow(tbl)
#' ncol(tbl)
#'
#' NROW(tbl)
#' NCOL(tbl)
#'
setMethod("ncol", "VTableNodeInfo",
          function(x) {
    ncol(col_info(x))
})

#' @rdname dimensions
#' @exportMethod ncol
setMethod("ncol", "TableRow",
          function(x) {
    if(!no_colinfo(x))
        ncol(col_info(x))
    else
        length(spanned_values(x))
})

#' @rdname dimensions
#' @exportMethod ncol
setMethod("ncol", "LabelRow",
          function(x) {
    ncol(col_info(x))
})

#' @rdname dimensions
#' @exportMethod ncol
setMethod("ncol", "InstantiatedColumnInfo",
          function(x) {
    length(col_exprs(x))
})

#' @rdname dimensions
#' @exportMethod dim
setMethod("dim", "VTableNodeInfo",
          function(x) c(nrow(x), ncol(x)))

#' Retrieve or set the direct children of a Tree-style object
#'
#' @param x An object with a Tree structure
#' @param value New list of children.
#' @return List of direct children of \code{x}
#' @export
#' @rdname tree_children
setGeneric("tree_children", function(x) standardGeneric("tree_children"))
#' @exportMethod tree_children
#' @rdname tree_children
setMethod("tree_children", c(x = "VTree"),
          function(x) x@children)
#' @exportMethod tree_children
#' @rdname tree_children
setMethod("tree_children", c(x = "VTableTree"),
          function(x) x@children)
#' @exportMethod tree_children
#' @rdname tree_children
 setMethod("tree_children", c(x = "VLeaf"),
          function(x) list())

#' @export
#' @rdname tree_children
setGeneric("tree_children<-", function(x, value) standardGeneric("tree_children<-"))
#' @exportMethod tree_children<-
#' @rdname tree_children
setMethod("tree_children<-", c(x = "VTree"),
          function(x, value){
    x@children = value
    x
})
#' @exportMethod tree_children<-
#' @rdname tree_children
setMethod("tree_children<-", c(x = "VTableTree"),
          function(x, value){
    x@children = value
    x
})


#' Retrieve or set Content Table from a TableTree
#'
#' Returns the content table of \code{obj} if it is a \code{TableTree} object, or \code{NULL} otherwise
#'
#' @param obj TableTree. The TableTree
#' @return the \code{ElementaryTable} containing the (top level) \emph{content rows} of \code{obj} ( or \code{NULL}
#' if \code{obj} is not a formal table object).
#' @export
#' @rdname content_table
setGeneric("content_table", function(obj) standardGeneric("content_table"))
#' @exportMethod content_table
#' @rdname content_table
setMethod("content_table", "TableTree",
          function(obj) obj@content)
#' @exportMethod content_table
#' @rdname content_table
setMethod("content_table", "ANY",
          function(obj) NULL)

#' @export
#' @param value ElementaryTable. The new content table for \code{obj}.
#' @rdname content_table
setGeneric("content_table<-", function(obj, value) standardGeneric("content_table<-"))
#' @exportMethod "content_table<-"
#' @rdname content_table
setMethod("content_table<-", c("TableTree", "ElementaryTable"),
          function(obj, value) {
    obj@content = value
    obj
})

#' @rdname int_methods
#' @param for_analyze logical(1).
setGeneric("next_rpos", function(obj, nested = TRUE, for_analyze = FALSE) standardGeneric("next_rpos"))

#' @rdname int_methods
setMethod("next_rpos", "PreDataTableLayouts",
          function(obj, nested, for_analyze = FALSE) next_rpos(rlayout(obj), nested, for_analyze = for_analyze))

.check_if_nest <- function(obj, nested, for_analyze) {
    if(!nested)
        FALSE
    else
        ## can always nest analyze splits (almost? what about colvars noncolvars mixing? prolly ok?)
        for_analyze ||
            ## If its not an analyze split it can't go under an analyze split
            !(is(last_rowsplit(obj), "VAnalyzeSplit") ||
              is(last_rowsplit(obj), "AnalyzeMultiVars")) ## should this be CompoundSplit?
}
#' @rdname int_methods
setMethod("next_rpos", "PreDataRowLayout",
          function(obj, nested, for_analyze) {
    l = length(obj)
    if(length(obj[[l]]) > 0L &&
       !.check_if_nest(obj, nested, for_analyze)) {
        l = l + 1L
    }
    l
})



#' @rdname int_methods
setMethod("next_rpos", "ANY", function(obj, nested) 1L)
#' @rdname int_methods
setGeneric("next_cpos", function(obj, nested = TRUE) standardGeneric("next_cpos"))
#' @rdname int_methods
setMethod("next_cpos", "PreDataTableLayouts",
          function(obj, nested) next_cpos(clayout(obj), nested))
#' @rdname int_methods
setMethod("next_cpos", "PreDataColLayout",
          function(obj, nested) {
    if(nested || length(obj[[length(obj)]]) == 0)
        length(obj)
    else
        length(obj) + 1L
})
#' @rdname int_methods
setMethod("next_cpos", "ANY", function(obj, nested) 1L)

#' @rdname int_methods
setGeneric("last_rowsplit", function(obj) standardGeneric("last_rowsplit"))
#' @rdname int_methods
setMethod("last_rowsplit", "NULL",
          function(obj) NULL)
#' @rdname int_methods
setMethod("last_rowsplit", "SplitVector",
          function(obj) {
    if(length(obj) == 0)
        NULL
    else
        obj[[length(obj)]]
})
#' @rdname int_methods
setMethod("last_rowsplit", "PreDataRowLayout",
          function(obj) {
    if(length(obj) == 0)
        NULL
    else
        last_rowsplit(obj[[ length( obj ) ]])
})
#' @rdname int_methods
setMethod("last_rowsplit", "PreDataTableLayouts",
          function(obj) last_rowsplit(rlayout(obj)))



## TODO maybe export these?
#' @rdname int_methods
setGeneric("rlayout", function(obj) standardGeneric("rlayout"))
#' @rdname int_methods
setMethod("rlayout", "PreDataTableLayouts",
          function(obj) obj@row_layout)
#' @rdname int_methods
setMethod("rlayout", "ANY", function(obj) PreDataRowLayout())
#' @rdname int_methods
setGeneric("rlayout<-", function(object, value) standardGeneric("rlayout<-"))
#' @rdname int_methods
setMethod("rlayout<-", "PreDataTableLayouts",
          function(object, value) {
    object@row_layout = value
    object
})




#' @rdname int_methods
setGeneric("tree_pos", function(obj) standardGeneric("tree_pos"))
## setMethod("tree_pos", "VNodeInfo",
##           function(obj) obj@pos_in_tree)
#' @rdname int_methods
setMethod("tree_pos", "VLayoutNode",
          function(obj) obj@pos_in_tree)


#' @rdname int_methods
setGeneric("pos_subset", function(obj) standardGeneric("pos_subset"))
#' @rdname int_methods
setMethod("pos_subset", "TreePos",
          function(obj) obj@subset)
## setMethod("pos_subset", "VNodeInfo",
##           function(obj) pos_subset(tree_pos(obj)))
#' @rdname int_methods
setMethod("pos_subset", "VLayoutNode",
          function(obj) pos_subset(tree_pos(obj)))
#' @rdname int_methods
setGeneric("pos_splits", function(obj) standardGeneric("pos_splits"))
#' @rdname int_methods
setMethod("pos_splits", "TreePos",
          function(obj) obj@splits)
## setMethod("pos_splits", "VNodeInfo",
##           function(obj) pos_splits(tree_pos(obj)))
#' @rdname int_methods
setMethod("pos_splits", "VLayoutNode",
          function(obj) pos_splits(tree_pos(obj)))

#' @rdname int_methods
setGeneric("pos_splvals", function(obj) standardGeneric("pos_splvals"))
#' @rdname int_methods
setMethod("pos_splvals", "TreePos",
          function(obj) obj@s_values)

## setMethod("pos_splvals", "VNodeInfo",
##           function(obj) pos_splvals(tree_pos(obj)))
#' @rdname int_methods
setMethod("pos_splvals", "VLayoutNode",
          function(obj) pos_splvals(tree_pos(obj)))

#' @rdname int_methods
setGeneric("pos_split_labels", function(obj) standardGeneric("pos_split_labels"))
#' @rdname int_methods
setMethod("pos_split_labels", "TreePos",
          function(obj) {
    spls = pos_splits(obj)
    sapply(spls, function(x) x@split_label)
})

## setMethod("pos_split_labels", "VNodeInfo",
##            function(obj) pos_split_labels(tree_pos(obj)))
#' @rdname int_methods
setMethod("pos_split_labels", "VLayoutNode",
           function(obj) pos_split_labels(tree_pos(obj)))

#' @rdname int_methods
setGeneric("split_texttype", function(obj) standardGeneric("split_texttype"))
#' @rdname int_methods
setMethod("split_texttype", "VarLevelSplit", function(obj) "varlevels")
#' @rdname int_methods
setMethod("split_texttype", "MultiVarSplit", function(obj) "multivar")
#' @rdname int_methods
setMethod("split_texttype", "AllSplit", function(obj) "allobs")
#' @rdname int_methods
setMethod("split_texttype", "RootSplit", function(obj) "root")
#' @rdname int_methods
setMethod("split_texttype", "NULLSplit", function(obj) "null")
#' @rdname int_methods
setMethod("split_texttype", "VarStaticCutSplit", function(obj) "scut")
#' @rdname int_methods
setMethod("split_texttype", "VarDynCutSplit", function(obj) "dyncut")
#' @rdname int_methods
setMethod("split_texttype", "ManualSplit", function(obj) "manual")
#' @rdname int_methods
setMethod("split_texttype", "ANY", function(obj) stop("unknown split type"))

#' @rdname int_methods
setGeneric("pos_spltypes", function(obj) standardGeneric("pos_spltypes"))
#' @rdname int_methods
setMethod("pos_spltypes", "TreePos",
          function(obj) {
    spls = pos_splits(obj)
    sapply(spls, split_texttype)
})

## setMethod("pos_spltypes", "VNodeInfo",
##           function(obj) pos_spltypes(tree_pos(obj)))
#' @rdname int_methods
setMethod("pos_spltypes", "VLayoutNode",
          function(obj) pos_spltypes(tree_pos(obj)))

#' @rdname int_methods
setGeneric("pos_splval_labels", function(obj) standardGeneric("pos_splval_labels"))
#' @rdname int_methods
setMethod("pos_splval_labels", "TreePos",
          function(obj) obj@sval_labels)
## setMethod("pos_splval_labels", "VNodeInfo",
##            function(obj) pos_splval_labels(tree_pos(obj)))
#' @rdname int_methods
setMethod("pos_splval_labels", "VLayoutNode",
           function(obj) pos_splval_labels(tree_pos(obj)))


#' @rdname int_methods
setGeneric("spl_payload", function(obj) standardGeneric("spl_payload"))
#' @rdname int_methods
setMethod("spl_payload", "Split", function(obj) obj@payload)

#' @rdname int_methods
setGeneric("spl_payload<-", function(obj, value) standardGeneric("spl_payload<-"))
#' @rdname int_methods
setMethod("spl_payload<-", "Split", function(obj, value) {
    obj@payload <- value
    obj
})

#' @rdname int_methods
setGeneric("spl_label_var", function(obj) standardGeneric("spl_label_var"))
#' @rdname int_methods
setMethod("spl_label_var", "VarLevelSplit", function(obj) obj@value_label_var)
## TODO revisit. do we want to do this? used in vars_in_layout, but only
## for convenience.
#' @rdname int_methods
setMethod("spl_label_var", "Split", function(obj) NULL)

### name related things
#' Label and Name accessors
#' @param obj ANY. The object.
#' @rdname lab_name
#' @return the name or label of \code{obj} for getters, or \code{obj} after modification
#' for setters.
#' @export
setGeneric("obj_name", function(obj) standardGeneric("obj_name"))
#' @rdname lab_name
#' @exportMethod obj_name
setMethod("obj_name", "VNodeInfo",
          function(obj) obj@name)

#' @rdname lab_name
#' @exportMethod obj_name
setMethod("obj_name", "Split",
          function(obj) obj@name)
#' @rdname lab_name
#' @param value character(1). The new value
#' @export
setGeneric("obj_name<-", function(obj, value) standardGeneric("obj_name<-"))
#' @rdname lab_name
#' @exportMethod obj_name<-
setMethod("obj_name<-", "VNodeInfo",
          function(obj, value) {
    obj@name = value
    obj
})
#' @rdname lab_name
#' @exportMethod obj_name<-
setMethod("obj_name<-", "Split",
          function(obj, value) {
    obj@name = value
    obj
})


### Label related things
#' @rdname lab_name
#' @export
setGeneric("obj_label", function(obj) standardGeneric("obj_label"))
#' @rdname lab_name
#' @exportMethod obj_label
setMethod("obj_label", "Split", function(obj) obj@split_label)
#' @rdname lab_name
#' @exportMethod obj_label
setMethod("obj_label", "ANY", function(obj) attr(obj, "label"))

#' @rdname lab_name
#' @exportMethod obj_label
setMethod("obj_label", "TableRow", function(obj) obj@label)
## XXX Do we want a convenience for VTableTree that
## grabs the label from the LabelRow or will
## that just muddy the waters?
#' @rdname lab_name
#' @exportMethod obj_label
setMethod("obj_label", "VTableTree",
          function(obj) obj_label(tt_labelrow(obj)))

#' @rdname lab_name
#' @exportMethod obj_label
setMethod("obj_label", "ValueWrapper", function(obj) obj@label)

#' @rdname lab_name
#' @export
setGeneric("obj_label<-", function(obj, value) standardGeneric("obj_label<-"))
#' @rdname lab_name
#' @exportMethod obj_label<-
setMethod("obj_label<-", "Split",
          function(obj, value) {
    obj@split_label <- value
    obj
})

#' @rdname lab_name
#' @exportMethod obj_label<-
setMethod("obj_label<-", "TableRow",
          function(obj, value){
    obj@label = value
    obj
})

#' @rdname lab_name
#' @exportMethod obj_label<-
setMethod("obj_label<-", "ValueWrapper",
          function(obj, value){
    obj@label = value
    obj
})

#' @rdname lab_name
#' @exportMethod obj_label<-
setMethod("obj_label<-", "ANY",
          function(obj, value){
    attr(obj, "label") = value
    obj
})

#' @rdname lab_name
#' @exportMethod obj_label<-
setMethod("obj_label<-", "VTableTree",
          function(obj, value) {
    lr = tt_labelrow(obj)
    obj_label(lr) = value
    if( !is.na(value) && nzchar(value))
        labelrow_visible(lr) = "visible" ## TRUE

    tt_labelrow(obj) = lr
    obj
})

### Label rows.
#' @rdname int_methods
setGeneric("tt_labelrow", function(obj) standardGeneric("tt_labelrow"))
#' @rdname int_methods
setMethod("tt_labelrow", "VTableTree",
          function(obj) obj@labelrow)

#' @rdname int_methods
setGeneric("tt_labelrow<-", function(obj, value) standardGeneric("tt_labelrow<-"))
#' @rdname int_methods
setMethod("tt_labelrow<-", "VTableTree",
          function(obj, value) {
    obj@labelrow = value
    obj
})

#' @rdname int_methods
setGeneric("labelrow_visible", function(obj) standardGeneric("labelrow_visible"))
#' @rdname int_methods
setMethod("labelrow_visible", "VTableTree",
          function(obj) {
    labelrow_visible(tt_labelrow(obj))
})

#' @rdname int_methods
setMethod("labelrow_visible", "LabelRow",
          function(obj) obj@visible)
#' @rdname int_methods
setMethod("labelrow_visible", "VAnalyzeSplit",
          function(obj) .labelkids_helper(obj@var_label_position))

#' @rdname int_methods
setGeneric("labelrow_visible<-", function(obj, value) standardGeneric("labelrow_visible<-"))
#' @rdname int_methods
setMethod("labelrow_visible<-", "VTableTree",
          function(obj, value) {
    lr = tt_labelrow(obj)
    labelrow_visible(lr) = value
    tt_labelrow(obj) = lr
    obj
})

#' @rdname int_methods
setMethod("labelrow_visible<-", "LabelRow",
          function(obj, value) {
    obj@visible = value
    obj
})

#' @rdname int_methods
setMethod("labelrow_visible<-", "VAnalyzeSplit",
          function(obj, value) {
    obj@var_label_position = value
    obj
})


## TRUE is always, FALSE is never, NA is only when no
## content function (or rows in an instantiated table) is present
#' @rdname int_methods
setGeneric("label_kids", function(spl) standardGeneric("label_kids"))
#' @rdname int_methods
setMethod("label_kids", "Split", function(spl) spl@label_children)

#' @rdname int_methods
setGeneric("label_kids<-", function(spl, value) standardGeneric("label_kids<-"))
#' @rdname int_methods
setMethod("label_kids<-", c("Split", "character"), function(spl, value) {
    label_kids(spl) <- .labelkids_helper(value)
    spl
})
#' @rdname int_methods
setMethod("label_kids<-", c("Split", "logical"), function(spl, value) {
    spl@label_children <- value
    spl
})

#' @rdname int_methods
setGeneric("vis_label", function(spl) standardGeneric("vis_label"))
#' @rdname int_methods
setMethod("vis_label", "Split", function(spl) {
    .labelkids_helper(label_position(spl))
})

#' @rdname int_methods
setGeneric("vis_label<-", function(spl, value) standardGeneric("vis_label<-"))
#' @rdname int_methods
setMethod("vis_label<-", "Split", function(spl, value) {
    stop("defunct")
    if(is.na(value))
        stop("split label visibility must be TRUE or FALSE, got NA")
#    spl@split_label_visible <- value
    spl
})



#' @rdname int_methods
setGeneric("label_position", function(spl) standardGeneric("label_position"))
#' @rdname int_methods
setMethod("label_position", "Split", function(spl) spl@split_label_position)

#' @rdname int_methods
setMethod("label_position", "VAnalyzeSplit", function(spl) spl@var_label_position) ##split_label_position)


#' @rdname int_methods
setGeneric("label_position<-", function(spl, value) standardGeneric("label_position<-"))
#' @rdname int_methods
setMethod("label_position<-", "Split", function(spl, value) {
    value <- match.arg(value, valid_lbl_pos)
    spl@split_label_position <- value
    spl
})







### Function acessors (summary, tabulation and split)
#' @rdname int_methods
setGeneric("content_fun", function(obj) standardGeneric("content_fun"))
#' @rdname int_methods
setMethod("content_fun", "Split", function(obj) obj@content_fun)

#' @rdname int_methods
setGeneric("content_fun<-", function(object, value) standardGeneric("content_fun<-"))
#' @rdname int_methods
setMethod("content_fun<-", "Split", function(object, value) {
    object@content_fun = value
    object
})

#' @rdname int_methods
setGeneric("analysis_fun", function(obj) standardGeneric("analysis_fun"))
#' @rdname int_methods
setMethod("analysis_fun", "AnalyzeVarSplit", function(obj) obj@analysis_fun)
#' @rdname int_methods
setMethod("analysis_fun", "AnalyzeColVarSplit", function(obj) obj@analysis_fun)

#' @rdname int_methods
setGeneric("split_fun", function(obj) standardGeneric("split_fun"))
#' @rdname int_methods
setMethod("split_fun", "CustomizableSplit", function(obj) obj@split_fun)

## Only that type of split currently has the slot
## this should probably change? for now  define
## an accessor that just returns NULL
#' @rdname int_methods
setMethod("split_fun", "Split", function(obj) NULL)


## Content specification related accessors
#' @rdname int_methods
setGeneric("content_extra_args", function(obj) standardGeneric("content_extra_args"))
#' @rdname int_methods
setMethod("content_extra_args", "Split", function(obj) obj@content_extra_args)

#' @rdname int_methods
setGeneric("content_extra_args<-", function(object, value) standardGeneric("content_extra_args<-"))
#' @rdname int_methods
setMethod("content_extra_args<-", "Split", function(object, value) {
    object@content_extra_args = value
    object
})


#' @rdname int_methods
setGeneric("content_var", function(obj) standardGeneric("content_var"))
#' @rdname int_methods
setMethod("content_var", "Split", function(obj) obj@content_var)

#' @rdname int_methods
setGeneric("content_var<-", function(object, value) standardGeneric("content_var<-"))
#' @rdname int_methods
setMethod("content_var<-", "Split", function(object, value) {
    object@content_var = value
    object
})





### Miscelaneous accessors
#' @rdname int_methods
setGeneric("avar_inclNAs", function(obj) standardGeneric("avar_inclNAs"))
#' @rdname int_methods
setMethod("avar_inclNAs", "VAnalyzeSplit",
          function(obj) obj@include_NAs)

#' @rdname int_methods
setGeneric("avar_inclNAs<-", function(obj, value) standardGeneric("avar_inclNAs<-"))
#' @rdname int_methods
setMethod("avar_inclNAs<-", "VAnalyzeSplit",
          function(obj, value) {
    obj@include_NAs = value
})

#' @rdname int_methods
setGeneric("spl_labelvar", function(obj) standardGeneric("spl_labelvar"))
#' @rdname int_methods
setMethod("spl_labelvar", "VarLevelSplit", function(obj) obj@value_label_var)

#' @rdname int_methods
setGeneric("spl_child_order", function(obj) standardGeneric("spl_child_order"))
#' @rdname int_methods
setMethod("spl_child_order", "VarLevelSplit", function(obj) obj@value_order)

#' @rdname int_methods
setGeneric("spl_child_order<-",
           function(obj, value) standardGeneric("spl_child_order<-"))
#' @rdname int_methods
setMethod("spl_child_order<-", "VarLevelSplit",
          function(obj, value) {
    obj@value_order = value
    obj
})

#' @rdname int_methods
setMethod("spl_child_order",
          "ManualSplit",
          function(obj) obj@levels)
#' @rdname int_methods
setMethod("spl_child_order",
          "MultiVarSplit",
          function(obj) spl_varnames(obj))
#' @rdname int_methods
setMethod("spl_child_order",
          "AllSplit",
          function(obj) character())
#' @rdname int_methods
setMethod("spl_child_order",
          "VarStaticCutSplit",
          function(obj) spl_cutlabels(obj))

#' @rdname int_methods
setGeneric("root_spl", function(obj) standardGeneric("root_spl"))
#' @rdname int_methods
setMethod("root_spl", "PreDataAxisLayout",
          function(obj) obj@root_split)

#' @rdname int_methods
setGeneric("root_spl<-", function(obj, value) standardGeneric("root_spl<-"))
#' @rdname int_methods
setMethod("root_spl<-", "PreDataAxisLayout",
          function(obj, value) {
    obj@root_split <- value
    obj
})

#' Row attribute accessors
#' @inheritParams gen_args
#' @return various, depending on the accessor called.
#' @rdname row_accessors
#' @export
#'
setGeneric("obj_avar", function(obj) standardGeneric("obj_avar"))
#'@rdname row_accessors
#' @exportMethod obj_avar
setMethod("obj_avar", "TableRow", function(obj) obj@var_analyzed)

#'@rdname row_accessors
#' @exportMethod obj_avar
setMethod("obj_avar", "ElementaryTable", function(obj) obj@var_analyzed)

#' @export
#' @rdname row_accessors
setGeneric("row_cells", function(obj) standardGeneric("row_cells"))
#' @rdname row_accessors
#' @exportMethod row_cells
setMethod("row_cells", "TableRow", function(obj) obj@leaf_value)

#' @rdname row_accessors
setGeneric("row_cells<-", function(obj, value) standardGeneric("row_cells<-"))
#' @rdname row_accessors
#' @exportMethod row_cells
setMethod("row_cells<-", "TableRow", function(obj, value) {
    obj@leaf_value <- value
    obj
})

#' @export
#' @rdname row_accessors
setGeneric("row_values", function(obj) standardGeneric("row_values"))
#' @rdname row_accessors
#' @exportMethod row_values
setMethod("row_values", "TableRow", function(obj) rawvalues(obj@leaf_value))


#' @rdname row_accessors
#' @exportMethod row_values<-
setGeneric("row_values<-", function(obj, value) standardGeneric("row_values<-"))
#' @rdname row_accessors
#' @exportMethod row_values<-
setMethod("row_values<-", "TableRow",
          function(obj, value) {
    obj@leaf_value = lapply(value, rcell)
    obj
})
#' @rdname row_accessors
#' @exportMethod row_values<-
setMethod("row_values<-", "LabelRow",
          function(obj, value) {
    stop("LabelRows cannot have row values.")
})

#' @rdname int_methods
setGeneric("spanned_values", function(obj) standardGeneric("spanned_values"))
#' @rdname int_methods
setMethod("spanned_values", "TableRow",
          function(obj) {
    sp = row_cspans(obj)
    rvals = row_values(obj)
    unlist(mapply(function(v, s) rep(list(v), times = s),
                  v = rvals, s = sp),
           recursive = FALSE)
})

#' @rdname int_methods
setMethod("spanned_values", "LabelRow",
          function(obj) {
    rep(list(NULL), ncol(obj))
})

#' @rdname int_methods
setGeneric("spanned_cells", function(obj) standardGeneric("spanned_cells"))
#' @rdname int_methods
setMethod("spanned_cells", "TableRow",
          function(obj) {
    sp = row_cspans(obj)
    rvals = row_cells(obj)
    unlist(mapply(function(v, s) rep(list(v), times = s)),
           recursive = FALSE)
})
#' @rdname int_methods
setMethod("spanned_cells", "LabelRow",
          function(obj) {
    rep(list(NULL), ncol(obj))
})

#' @rdname int_methods
setGeneric("spanned_values<-", function(obj, value) standardGeneric("spanned_values<-"))
#' @rdname int_methods
setMethod("spanned_values<-", "TableRow",
          function(obj, value) {
    sp = row_cspans(obj)
    ## this is 3 times too clever!!!
    splvec = cumsum(unlist(lapply(sp, function(x) c(1, rep(0, x - 1)))))

    rvals = lapply(split(value, splvec),
                   function(v) {
        if(length(v) == 1)
            return(v)
        stopifnot(length(unique(v)) == 1L)
        rcell(unique(v), colspan= length(v))
    })
    row_values(obj) = rvals
    obj
})
#' @rdname int_methods
setMethod("spanned_values<-", "LabelRow",
          function(obj, value) {
    if(!is.null(value))
        stop("Label rows can't have non-null cell values, got", value)
    obj
})



### Format manipulation
### obj_format<- is not recursive
## TODO export these?
#' @rdname int_methods
setGeneric("obj_format", function(obj) standardGeneric("obj_format"))
## this covers rcell, etc
#' @rdname int_methods
setMethod("obj_format", "ANY", function(obj) attr(obj, "format"))
#' @rdname int_methods
setMethod("obj_format", "VTableNodeInfo", function(obj) obj@format)
##setMethod("obj_format", "CellValue", function(obj) obj@format)
#' @rdname int_methods
setMethod("obj_format", "Split", function(obj) obj@split_format)

#' @rdname int_methods
setGeneric("obj_format<-", function(obj, value) standardGeneric("obj_format<-"))
## this covers rcell, etc
#' @rdname int_methods
setMethod("obj_format<-", "ANY", function(obj, value) {
    attr(obj, "format") = value
    obj
})
#' @rdname int_methods
setMethod("obj_format<-", "VTableNodeInfo", function(obj, value) {
    obj@format = value
    obj
})
#' @rdname int_methods
setMethod("obj_format<-", "Split", function(obj, value) {
    obj@split_format = value
    obj
})

## setMethod("obj_format<-", "CellValue", function(obj, value) {
##     obj@format = value
##     obj
## })



#' @rdname int_methods
setGeneric("set_format_recursive", function(obj, format, override = FALSE) standardGeneric("set_format_recursive"))
#' @rdname int_methods
#' @param override logical(1).
setMethod("set_format_recursive", "TableRow",
          function(obj, format, override = FALSE) {
    if(is.null(format))
        return(obj)
    if(is.null(obj_format(obj)) || override)
        obj_format(obj) = format
    lcells = row_cells(obj)
    lvals = lapply(lcells, function(x) {
        if(!is.null(x) && is.null(obj_format(x)))
            obj_format(x) = obj_format(obj)
        x
    })
    row_values(obj) = lvals
    obj
})
#' @rdname int_methods
setMethod("set_format_recursive", "LabelRow",
          function(obj, format, override = FALSE) obj)
setMethod("set_format_recursive", "VTableTree",
          function(obj, format, override = FALSE) {
    force(format)
    if(is.null(format))
        return(obj)

    if(is.null(obj_format(obj)) || override)
        obj_format(obj) = format

    kids = tree_children(obj)
    kids = lapply(kids, function(x, format2, oride) set_format_recursive(x,
                                                                      format = format2, override = oride),
                  format2 = obj_format(obj), oride = override)
    tree_children(obj) = kids
    obj
})

#' @rdname int_methods
setGeneric("content_format", function(obj) standardGeneric("content_format"))
#' @rdname int_methods
setMethod("content_format", "Split", function(obj) obj@content_format)

#' @rdname int_methods
setGeneric("content_format<-", function(obj, value) standardGeneric("content_format<-"))
#' @rdname int_methods
setMethod("content_format<-", "Split", function(obj, value) {
    obj@content_format = value
    obj
})

## credit: rlang, Henry and Wickham.
## this one tiny utility function is NOT worth a dependency.
## modified it so any length 0 x grabs y
`%||%` = function(L, R) if(length(L) == 0) R else L
#' @rdname int_methods
#' @param default FormatSpec.
setGeneric("value_formats", function(obj, default = obj_format(obj)) standardGeneric("value_formats"))
#' @rdname int_methods
setMethod("value_formats", "ANY",
          function(obj, default) {
    attr(obj, "format") %||% default
})

#' @rdname int_methods
setMethod("value_formats", "TableRow",
          function(obj, default) {
    formats = lapply(row_cells(obj), function(x)
        value_formats(x) %||% default)
    formats
})
#' @rdname int_methods
setMethod("value_formats", "LabelRow",
          function(obj, default) {
    rep(list(NULL), ncol(obj))
})
#' @rdname int_methods
setMethod("value_formats", "VTableTree",
          function(obj, default) {
    rws = collect_leaves(obj, TRUE, TRUE)
    formatrws = lapply(rws, value_formats)
    mat = do.call(rbind, formatrws)
    row.names(mat) = NULL
    mat
})


### Collect all leaves of a current tree
### This is a workhorse function in various
### places
### NB this is written generally enought o
### be used on all tree-based structures in the
### framework.

#' Collect leaves of a table tree
#' @inheritParams gen_args
#' @param incl.cont logical. Include rows from content tables within the tree. Defaults to \code{TRUE}
#' @param add.labrows logical. Include label rows. Defaults to \code{FALSE}
#' @return A list of \code{TableRow} objects for all rows in the table
#' @rdname collect_leaves
#' @export
setGeneric("collect_leaves",
           function(tt, incl.cont = TRUE, add.labrows = FALSE)
    standardGeneric("collect_leaves"), signature = "tt")

#' @rdname collect_leaves
#' @exportMethod collect_leaves

setMethod("collect_leaves", "TableTree",
          function(tt, incl.cont = TRUE, add.labrows = FALSE) {
    ret = c(
        if(add.labrows && labelrow_visible(tt)) {
            tt_labelrow(tt)
        },
        if(incl.cont) {tree_children(content_table(tt))},

        lapply(tree_children(tt),
               collect_leaves, incl.cont = incl.cont, add.labrows = add.labrows))
    unlist(ret, recursive = TRUE)
})

#' @rdname collect_leaves
#' @exportMethod collect_leaves

setMethod("collect_leaves", "ElementaryTable",
          function(tt, incl.cont = TRUE, add.labrows = FALSE) {
    ret = tree_children(tt)
    if(add.labrows && labelrow_visible(tt)) {
        ret = c(tt_labelrow(tt), ret)
    }
    ret
})

#' @rdname collect_leaves
#' @exportMethod collect_leaves

setMethod("collect_leaves", "VTree",
          function(tt, incl.cont, add.labrows) {
    ret = lapply(tree_children(tt),
                 collect_leaves)
    unlist(ret, recursive = TRUE)
})

#' @rdname collect_leaves
#' @exportMethod collect_leaves
setMethod("collect_leaves", "VLeaf",
          function(tt, incl.cont, add.labrows) {
    list(tt)
})

#' @rdname collect_leaves
#' @exportMethod collect_leaves
setMethod("collect_leaves", "NULL",
          function(tt, incl.cont, add.labrows) {
    list()
})


#' @rdname collect_leaves
#' @exportMethod collect_leaves
setMethod("collect_leaves", "ANY",
          function(tt, incl.cont, add.labrows)
    stop("class ", class(tt), " does not inherit from VTree or VLeaf"))


n_leaves <- function(tt, ...) {
    length(collect_leaves(tt, ...))
}
### Spanning information
#' @rdname int_methods
setGeneric("row_cspans", function(obj) standardGeneric("row_cspans"))
#' @rdname int_methods
setMethod("row_cspans", "TableRow", function(obj) obj@colspans)
#' @rdname int_methods
setMethod("row_cspans", "LabelRow",
          function(obj) rep(1L, ncol(obj)))

#' @rdname int_methods
setGeneric("row_cspans<-", function(obj, value) standardGeneric("row_cspans<-"))
#' @rdname int_methods
setMethod("row_cspans<-", "TableRow", function(obj, value) {
    obj@colspans = value
    obj
})
#' @rdname int_methods
setMethod("row_cspans<-", "LabelRow", function(obj, value) {
    stop("attempted to set colspans for LabelRow")
})


## XXX TODO colapse with above?
#' @rdname int_methods
setGeneric("cell_cspan", function(obj) standardGeneric("cell_cspan"))
#' @rdname int_methods
setMethod("cell_cspan", "CellValue", function(obj) attr(obj, "colspan")) ##obj@colspan)

#' @rdname int_methods
setGeneric("cell_cspan<-", function(obj, value) standardGeneric("cell_cspan<-"))
#' @rdname int_methods
setMethod("cell_cspan<-", "CellValue", function(obj, value) {
    ##  obj@colspan = value
    attr(obj, "colspan") <- value
    obj
})


### Level (indent) in tree structure
#' @rdname int_methods
setGeneric("tt_level", function(obj) standardGeneric("tt_level"))
## this will hit everything via inheritence
#' @rdname int_methods
setMethod("tt_level", "VNodeInfo", function(obj) obj@level)

#' @rdname int_methods
setGeneric("tt_level<-", function(obj, value) standardGeneric("tt_level<-"))
## this will hit everyhing via inheritence
#' @rdname int_methods
setMethod("tt_level<-", "VNodeInfo", function(obj, value) {
    obj@level = as.integer(value)
    obj
})
#' @rdname int_methods
setMethod("tt_level<-", "VTableTree",
          function(obj, value) {
    obj@level = as.integer(value)
    tree_children(obj) = lapply(tree_children(obj),
                                `tt_level<-`, value = as.integer(value) + 1L)
    obj
})

#' @rdname int_methods
setGeneric("indent_mod", function(obj) standardGeneric("indent_mod"))
#' @rdname int_methods
setMethod("indent_mod", "Split",
          function(obj) obj@indent_modifier)
#' @rdname int_methods
setMethod("indent_mod", "VTableNodeInfo",
          function(obj) obj@indent_modifier)
#' @rdname int_methods
setMethod("indent_mod", "ANY",
          function(obj) attr(obj, "indent_mod") %||% 0L)
#' @rdname int_methods
setMethod("indent_mod", "RowsVerticalSection",
          ##          function(obj) setNames(obj@indent_mods,names(obj)))
          function(obj) {
    val <- attr(obj, "indent_mods") %||% rep(0L, length(obj))
    setNames(val, names(obj))
})

#' @rdname int_methods
setGeneric("indent_mod<-", function(obj, value) standardGeneric("indent_mod<-"))
#' @rdname int_methods
setMethod("indent_mod<-", "Split",
          function(obj, value) {
    obj@indent_modifier = as.integer(value)
    obj
})
#' @rdname int_methods
setMethod("indent_mod<-", "VTableNodeInfo",
          function(obj, value) {
    obj@indent_modifier = as.integer(value)
    obj
})

setMethod("indent_mod<-", "CellValue",
          function(obj, value) {
    attr(obj, "indent_mod") <- as.integer(value)
    obj
})

setMethod("indent_mod<-", "RowsVerticalSection",
          function(obj, value) {
    if(length(value) != 1 && length(value) != length(obj))
        stop("When setting indent mods on a RowsVerticalSection the value must have length 1 or the number of rows")
    attr(obj, "indent_mods") <- as.integer(value)
    obj

    ## obj@indent_mods <- value
    ## obj
})



#' @rdname int_methods
setGeneric("content_indent_mod", function(obj) standardGeneric("content_indent_mod"))
#' @rdname int_methods
setMethod("content_indent_mod", "Split",
          function(obj) obj@content_indent_modifier)
#' @rdname int_methods
setMethod("content_indent_mod", "VTableNodeInfo",
          function(obj) obj@content_indent_modifier)

#' @rdname int_methods
setGeneric("content_indent_mod<-", function(obj, value) standardGeneric("content_indent_mod<-"))
#' @rdname int_methods
setMethod("content_indent_mod<-", "Split",
          function(obj, value) {
    obj@content_indent_modifier = as.integer(value)
    obj
})
#' @rdname int_methods
setMethod("content_indent_mod<-", "VTableNodeInfo",
          function(obj, value) {
    obj@content_indent_modifier = as.integer(value)
    obj
})

## TODO export these?
#' @rdname int_methods
setGeneric("rawvalues", function(obj) standardGeneric("rawvalues"))
#' @rdname int_methods
setMethod("rawvalues", "ValueWrapper",  function(obj) obj@value)
#' @rdname int_methods
setMethod("rawvalues",  "LevelComboSplitValue",  function(obj) obj@combolevels)
#' @rdname int_methods
setMethod("rawvalues", "list", function(obj) lapply(obj, rawvalues))
#' @rdname int_methods
setMethod("rawvalues", "ANY", function(obj) obj)
#' @rdname int_methods
setMethod("rawvalues", "CellValue", function(obj) obj[[1]])
#' @rdname int_methods
setMethod("rawvalues", "TreePos",
          function(obj) rawvalues(pos_splvals(obj)))
setMethod("rawvalues", "RowsVerticalSection",  function(obj) unlist(obj, recursive = FALSE))

#' @rdname int_methods
setGeneric("value_names", function(obj) standardGeneric("value_names"))
#' @rdname int_methods
setMethod("value_names", "ANY", function(obj) as.character(rawvalues(obj)))
#' @rdname int_methods
setMethod("value_names", "TreePos",
          function(obj) value_names(pos_splvals(obj)))
#' @rdname int_methods
setMethod("value_names", "list", function(obj) lapply(obj, value_names))
#' @rdname int_methods
setMethod("value_names", "ValueWrapper",  function(obj) rawvalues(obj))
#' @rdname int_methods
setMethod("value_names", "LevelComboSplitValue",  function(obj) obj@value) ##obj@comboname)
#' @rdname int_methods
setMethod("value_names", "RowsVerticalSection",  function(obj) attr(obj, "row_names")) ##obj@row_names)

## not sure if I need these anywhere
## XXX
#' @rdname int_methods
setGeneric("value_labels", function(obj) standardGeneric("value_labels"))
#' @rdname int_methods
setMethod("value_labels", "ANY", function(obj) as.character(obj_label(obj)))
#' @rdname int_methods
setMethod("value_labels", "TreePos", function(obj) sapply(pos_splvals(obj), obj_label))
#' @rdname int_methods
setMethod("value_labels", "list", function(obj) {
    ret <- lapply(obj, obj_label)
    if(!is.null(names(obj))) {
        inds <- vapply(ret, function(x) length(x) == 0, NA)
        ret[inds] = names(obj)[inds]
    }
    ret
})

#' @rdname int_methods
setMethod("value_labels", "RowsVerticalSection", function(obj) setNames(attr(obj, "row_labels"), value_names(obj))) ##obj@row_labels, value_names(obj)))


#' @rdname int_methods
setMethod("value_labels", "ValueWrapper",  function(obj) obj_label(obj))
#' @rdname int_methods
setMethod("value_labels", "LevelComboSplitValue",  function(obj) obj_label(obj)) ##obj@comboname)
#' @rdname int_methods
setMethod("value_labels", "MultiVarSplit", function(obj) obj@var_labels)







## These two are similar enough we could probably combine
## them but conceptually they are pretty different
## split_exargs is a list of extra arguments that apply
## to *all the chidlren*,
## while splv_extra is for *child-specific* extra arguments,
## associated with specific values of the split
#' @rdname int_methods
setGeneric("splv_extra", function(obj) standardGeneric("splv_extra"))
#' @rdname int_methods
setMethod("splv_extra", "SplitValue",
          function(obj) obj@extra)

#' @rdname int_methods
setGeneric("splv_extra<-", function(obj, value) standardGeneric("splv_extra<-"))
#' @rdname int_methods
setMethod("splv_extra<-", "SplitValue",
          function(obj, value) {
    obj@extra <- value
    obj
})




#' @rdname int_methods
setGeneric("split_exargs", function(obj) standardGeneric("split_exargs"))
#' @rdname int_methods
setMethod("split_exargs", "Split",
          function(obj) obj@extra_args)

#' @rdname int_methods
setGeneric("split_exargs<-", function(obj, value) standardGeneric("split_exargs<-"))
#' @rdname int_methods
setMethod("split_exargs<-", "Split",
          function(obj, value ) {
    obj@extra_args <- value
    obj
})


is_labrow = function(obj) is(obj, "LabelRow")

spl_ref_group = function(obj) {
    stopifnot(is(obj, "VarLevWBaselineSplit"))
    obj@ref_group_value
}

### column info


## XXX this is probably not thee right model for column layouts because
## we don't find ourselves consuming/walking a layout as a tree often
##
#' @rdname int_methods
setGeneric("clayout_splits", function(obj) standardGeneric("clayout_splits"))
#' @rdname int_methods
setMethod("clayout_splits", "LayoutColTree", function(obj) {
    ##this is going to descend to the first ("leftmost") leaf
    clayout_splits(tree_children(obj)[[1]])
})
#' @rdname int_methods
setMethod("clayout_splits", "LayoutColLeaf", function(obj) {
    pos_splits(tree_pos(obj))
})
#' @rdname int_methods
setMethod("clayout_splits", "VTableNodeInfo",
          function(obj) clayout_splits(clayout(obj)))

## XXX this seems bad. the class that is returned
## depends on whether we are pre or post data.
## should be a different accessor
##
## FIXME

#' Column information/structure accessors
#'
#' @inheritParams gen_args
#' @param df data.frame/NULL. Data to use if the column information is being
#'   generated from a  Pre-Data layout object
#' @param rtpos TreePos. Root position.
#'
#' @return A \code{LayoutColTree} object.
#'
#' @rdname col_accessors
#'
#' @export
setGeneric("clayout", function(obj) standardGeneric("clayout"))
#'@rdname col_accessors
#' @exportMethod clayout
setMethod("clayout", "VTableNodeInfo",
          function(obj) obj@col_info@tree_layout)

#'@rdname col_accessors
#' @exportMethod clayout
setMethod("clayout", "PreDataTableLayouts",
          function(obj) obj@col_layout)

## useful convenience for the cascading methods in colby_constructors
#'@rdname col_accessors
#' @exportMethod clayout
setMethod("clayout", "ANY", function(obj) PreDataColLayout())





#'@rdname col_accessors
#' @export
setGeneric("clayout<-", function(object, value) standardGeneric("clayout<-"))

#'@rdname col_accessors
#' @exportMethod clayout<-
setMethod("clayout<-", "PreDataTableLayouts",
          function(object, value) {
    object@col_layout = value
    object
})


#'@rdname col_accessors
#' @export
setGeneric("col_info", function(obj) standardGeneric("col_info"))

#'@rdname col_accessors
#' @exportMethod col_info
setMethod("col_info", "VTableNodeInfo",
          function(obj) obj@col_info)

### XXX I've made this recursive. Do we ALWAYS want it to be?
###
### I think we do.
#'@rdname col_accessors
#' @export
setGeneric("col_info<-", function(obj, value) standardGeneric("col_info<-"))
#'@rdname col_accessors
#' @return Various column information, depending on the accessor used.
#' @exportMethod col_info<-
setMethod("col_info<-", "TableRow",
          function(obj, value) {
    obj@col_info = value
    obj
})

.set_cinfo_kids = function(obj) {
    kids = lapply(tree_children(obj),
                  function(x) {
        col_info(x) = col_info(obj)
        x
    })
    tree_children(obj) = kids
    obj
}

#'@rdname col_accessors
#' @exportMethod col_info<-
setMethod("col_info<-", "ElementaryTable",
          function(obj, value) {
    obj@col_info = value
    .set_cinfo_kids(obj)

})

#'@rdname col_accessors
#' @exportMethod col_info<-
setMethod("col_info<-", "TableTree",
          function(obj, value) {
    obj@col_info = value
    if(nrow(content_table(obj))) {
        ct = content_table(obj)
        col_info(ct) = value
        content_table(obj) = ct
    }
    .set_cinfo_kids(obj)
})





#' @rdname col_accessors
#' @export
setGeneric("coltree", function(obj, df = NULL, rtpos = TreePos()) standardGeneric("coltree"))


#' @rdname col_accessors
#' @exportMethod coltree
setMethod("coltree", "InstantiatedColumnInfo",
          function(obj, df = NULL, rtpos = TreePos()) {
    if(!is.null(df))
        warning("Ignoring df argument and retrieving already-computed LayoutColTree")
    obj@tree_layout
})

#' @rdname col_accessors
#' @export coltree

setMethod("coltree", "PreDataTableLayouts",
          function(obj, df, rtpos) coltree(clayout(obj), df, rtpos))

#' @rdname col_accessors
#' @export coltree
setMethod("coltree", "PreDataColLayout",
          function(obj, df, rtpos) {
    kids = lapply(obj, function(x) splitvec_to_coltree(df = df, splvec = x, pos = rtpos))
    if(length(kids) == 1)
        res = kids[[1]]
    else
        res = LayoutColTree(lev = 0L,
                  kids = kids,
                  tpos = rtpos,
                  spl = RootSplit())
    disp_ccounts(res) = disp_ccounts(obj)
    res
})


#' @rdname col_accessors
#' @export coltree
setMethod("coltree", "LayoutColTree",
          function(obj, df, rtpos) obj)

#' @rdname col_accessors
#' @export coltree
setMethod("coltree", "VTableTree",
          function(obj, df, rtpos) coltree(col_info(obj)))

#' @rdname col_accessors
#' @export coltree
setMethod("coltree", "TableRow",
          function(obj, df, rtpos) coltree(col_info(obj)))


#' @rdname col_accessors
#' @export
setGeneric("col_exprs", function(obj, df = NULL) standardGeneric("col_exprs"))

#' @rdname col_accessors
#' @export col_exprs
setMethod("col_exprs", "PreDataTableLayouts",
          function(obj, df = NULL) col_exprs(clayout(obj), df))

#' @rdname col_accessors
#' @export col_exprs
setMethod("col_exprs", "PreDataColLayout",
          function(obj, df = NULL) {
    unlist(recursive = FALSE,
           lapply(obj, build_splits_expr,
                  df = df))
})

#' @rdname col_accessors
#' @export col_exprs
setMethod("col_exprs", "InstantiatedColumnInfo",
          function(obj, df = NULL) {
    if(!is.null(df))
        warning("Ignoring df method when extracted precomputed column subsetting expressions.")
    obj@subset_exprs
})

#' @rdname int_methods
setGeneric("col_extra_args", function(obj, df = NULL) standardGeneric("col_extra_args"))
#' @rdname int_methods
setMethod("col_extra_args", "InstantiatedColumnInfo",
          function(obj, df) {
    if(!is.null(df))
        warning("Ignorning df when retrieving already-computed column extra arguments.")
    obj@cextra_args
})
#' @rdname int_methods
setMethod("col_extra_args", "PreDataTableLayouts",
          function(obj, df) col_extra_args(clayout(obj), df))
#' @rdname int_methods
setMethod("col_extra_args", "PreDataColLayout",
          function(obj, df) {
    col_extra_args(coltree(obj, df), NULL)
})
#' @rdname int_methods
setMethod("col_extra_args", "LayoutColTree",
          function(obj, df) {
    if(!is.null(df))
        warning("Ignoring df argument and returning already calculated extra arguments")
    get_col_extras(obj)
})
#' @rdname int_methods
setMethod("col_extra_args", "LayoutColLeaf",
          function(obj, df) {
    if(!is.null(df))
        warning("Ignoring df argument and returning already calculated extra arguments")

    get_pos_extra(pos = tree_pos(obj))
    })






#' @export
#' @rdname col_accessors
setGeneric("col_counts", function(obj) standardGeneric("col_counts"))

#' @export
#' @rdname col_accessors
setMethod("col_counts",  "InstantiatedColumnInfo",
          function(obj) obj@counts)

#' @export
#' @rdname col_accessors
setMethod("col_counts", "VTableNodeInfo",
          function(obj) col_counts(col_info(obj)))

#' @export
#' @rdname col_accessors
setGeneric("col_counts<-", function(obj, value) standardGeneric("col_counts<-"))

#' @export
#' @rdname col_accessors
setMethod("col_counts<-",  "InstantiatedColumnInfo",
          function(obj, value) {
    obj@counts = value
    obj
})

#' @export
#' @rdname col_accessors
setMethod("col_counts<-", "VTableNodeInfo",
          function(obj, value) {
    cinfo = col_info(obj)
    col_counts(cinfo) = value
    col_info(obj) = cinfo
    obj

})



#' @export
#' @rdname col_accessors
setGeneric("col_total", function(obj) standardGeneric("col_total"))

#' @export
#' @rdname col_accessors
setMethod("col_total",  "InstantiatedColumnInfo",
          function(obj) obj@total_count)

#' @export
#' @rdname col_accessors
setMethod("col_total", "VTableNodeInfo",
          function(obj) col_total(col_info(obj)))

#' @export
#' @rdname col_accessors
setGeneric("col_total<-", function(obj, value) standardGeneric("col_total<-"))

#' @export
#' @rdname col_accessors
setMethod("col_total<-",  "InstantiatedColumnInfo",
          function(obj, value) {
    obj@total_count = value
    obj
})

#' @export
#' @rdname col_accessors
setMethod("col_total<-", "VTableNodeInfo",
          function(obj, value) {
    cinfo = col_info(obj)
    col_total(cinfo) = value
    col_info(obj) = cinfo
    obj

})






#' @rdname int_methods
setGeneric("disp_ccounts", function(obj) standardGeneric("disp_ccounts"))
#' @rdname int_methods
setMethod("disp_ccounts", "VTableTree",
          function(obj) disp_ccounts(col_info(obj)))
#' @rdname int_methods
setMethod("disp_ccounts", "InstantiatedColumnInfo",
          function(obj) obj@display_columncounts)
#' @rdname int_methods
setMethod("disp_ccounts", "PreDataTableLayouts",
          function(obj) disp_ccounts(clayout(obj)))
#' @rdname int_methods
setMethod("disp_ccounts", "PreDataColLayout",
          function(obj) obj@display_columncounts)

#' @rdname int_methods
setGeneric("disp_ccounts<-", function(obj, value) standardGeneric("disp_ccounts<-"))
#' @rdname int_methods
setMethod("disp_ccounts<-", "VTableTree",
          function(obj, value) {
    cinfo = col_info(obj)
    disp_ccounts(cinfo) = value
    col_info(obj) = cinfo
    obj
})
#' @rdname int_methods
setMethod("disp_ccounts<-", "InstantiatedColumnInfo",
          function(obj, value) {
    obj@display_columncounts = value
    obj
})
#' @rdname int_methods
setMethod("disp_ccounts<-", "PreDataColLayout",
          function(obj, value) {
    obj@display_columncounts = value
    obj
})
#' @rdname int_methods
setMethod("disp_ccounts<-", "LayoutColTree",
          function(obj, value) {
    obj@display_columncounts = value
    obj
})
#' @rdname int_methods
setMethod("disp_ccounts<-", "PreDataTableLayouts",
          function(obj, value) {
    clyt = clayout(obj)
    disp_ccounts(clyt) = value
    clayout(obj) = clyt
    obj
})

#' @rdname int_methods
setGeneric("colcount_format", function(obj) standardGeneric("colcount_format"))
#' @rdname int_methods
setMethod("colcount_format", "InstantiatedColumnInfo",
          function(obj) obj@columncount_format)
#' @rdname int_methods
setMethod("colcount_format", "VTableNodeInfo",
          function(obj) colcount_format(col_info(obj)))
#' @rdname int_methods
setMethod("colcount_format", "PreDataColLayout",
          function(obj) obj@columncount_format)
#' @rdname int_methods
setMethod("colcount_format", "PreDataTableLayouts",
          function(obj) colcount_format(clayout(obj)))

#' @rdname int_methods
setGeneric("colcount_format<-", function(obj,value) standardGeneric("colcount_format<-"))
#' @rdname int_methods
setMethod("colcount_format<-", "InstantiatedColumnInfo",
          function(obj, value) {
    obj@columncount_format = value
    obj
})
#' @rdname int_methods
setMethod("colcount_format<-", "VTableNodeInfo",
          function(obj, value) {
    cinfo = col_info(obj)
    colcount_format(cinfo) = value
    col_info(obj) = cinfo
    obj
})
#' @rdname int_methods
setMethod("colcount_format<-", "PreDataColLayout",
          function(obj, value) {
    obj@columncount_format = value
    obj
})
#' @rdname int_methods
setMethod("colcount_format<-", "PreDataTableLayouts",
          function(obj, value) {
    clyt = clayout(obj)
    colcount_format(clyt) = value
    clayout(obj) = clyt
    obj
})


#' Exported for use in tern
#'
#' Does the table/row/InstantiatedColumnInfo object contain no column structure information?
#'
#' @inheritParams gen_args
#' @rdname no_info
#' @return \code{TRUE} if the object has no/empty instantiated column information,
#' \code{FALSE} otherwise.
#' @export
setGeneric("no_colinfo", function(obj) standardGeneric("no_colinfo"))

#' @exportMethod no_colinfo
#' @rdname no_info
setMethod("no_colinfo", "VTableNodeInfo",
          function(obj) no_colinfo(col_info(obj)))

#' @exportMethod no_colinfo
#' @rdname no_info
setMethod("no_colinfo", "InstantiatedColumnInfo",
           function(obj) length(obj@subset_exprs) == 0) ##identical(obj, EmptyColInfo))


#' Names of a TableTree
#'
#' @param x the object.
#' @details For TableTrees with more than one level of splitting in columns, the names are defined to be the top-level
#'   split values repped out across the columns that they span.
#' @rdname names
#' @return The column names of \code{x}, as defined in the details above.
#' @exportMethod names
setMethod("names", "VTableNodeInfo",
          function(x) names(col_info(x)))

#' @rdname names
#' @exportMethod names

setMethod("names", "InstantiatedColumnInfo",
          function(x) names(coltree(x)))
#' @rdname names
#' @exportMethod names
setMethod("names", "LayoutColTree",
          function(x) {
    unname(unlist(lapply(tree_children(x),
                  function(obj) {
        nm <- obj_name(obj)
        rep(nm, n_leaves(obj))
    })))
})

#' @rdname names
#' @exportMethod row.names
setMethod("row.names", "VTableTree",
          function(x) {
    unname(sapply(collect_leaves(x, add.labrows = TRUE),
           obj_label, USE.NAMES = FALSE)) ## XXXX this should probably be obj_name???


})




#' convert to a vector
#'
#' @note  This only works for a table with a single row or a row object.
#'
#' @rdname asvec
#' @param x ANY. The object to be converted to a vector
#' @param mode character(1). Passed on to \code{\link[base]{as.vector}}
#' @return a vector of the chosen mode (or an error is raised if more than one row was present).
#' @exportMethod as.vector
#'
setMethod("as.vector", "TableRow", function(x, mode) as.vector(unlist(row_values(x)), mode = mode))
#'@rdname asvec
#' @exportMethod as.vector
setMethod("as.vector", "ElementaryTable", function(x, mode) {
    stopifnot(nrow(x) == 1L)
    as.vector(tree_children(x)[[1]], mode = mode)
})

#'@rdname asvec
#' @exportMethod as.vector
setMethod("as.vector", "VTableTree", function(x, mode) {
    stopifnot(nrow(x) == 1L)
    if(nrow(content_table(x)) == 1L)
        tab = content_table(x)
    else
        tab = x
    as.vector(tree_children(tab)[[1]], mode = mode)
})


## cuts
#' @rdname int_methods
setGeneric("spl_cuts", function(obj) standardGeneric("spl_cuts"))
#' @rdname int_methods
setMethod("spl_cuts", "VarStaticCutSplit",
          function(obj) obj@cuts)

#' @rdname int_methods
setGeneric("spl_cutlabels", function(obj) standardGeneric("spl_cutlabels"))
#' @rdname int_methods
setMethod("spl_cutlabels", "VarStaticCutSplit",
          function(obj) obj@cut_labels)

#' @rdname int_methods
setGeneric("spl_cutfun", function(obj) standardGeneric("spl_cutfun"))
#' @rdname int_methods
setMethod("spl_cutfun", "VarDynCutSplit",
          function(obj) obj@cut_fun)

#' @rdname int_methods
setGeneric("spl_cutlabelfun", function(obj) standardGeneric("spl_cutlabelfun"))
#' @rdname int_methods
setMethod("spl_cutlabelfun", "VarDynCutSplit",
          function(obj) obj@cut_label_fun)

#' @rdname int_methods
setGeneric("spl_is_cmlcuts", function(obj) standardGeneric("spl_is_cmlcuts"))
#' @rdname int_methods
setMethod("spl_is_cmlcuts", "VarDynCutSplit",
          function(obj) obj@cumulative_cuts)

#' @rdname int_methods
setGeneric("spl_varnames",
           function(obj) standardGeneric("spl_varnames"))
#' @rdname int_methods
setMethod("spl_varnames", "MultiVarSplit",
          function(obj) obj@var_names)


#' Top Left Material (Experimental)
#' @inheritParams gen_args
#' @description A TableTree object can have \emph{top left material} which is a sequence
#' of strings which are printed in the area of the table between the column header display
#' and the label of the first row.  These functions acccess and modify that material.
#'
#' @return A character vector representing the top-left material of \code{obj} (or
#' \code{obj} after modification, in the case of the setter).
#' @export
#' @rdname top_left
setGeneric("top_left", function(obj) standardGeneric("top_left"))
#' @export
#' @rdname top_left
setMethod("top_left", "VTableTree", function(obj) top_left(col_info(obj)))
#' @export
#' @rdname top_left
setMethod("top_left", "InstantiatedColumnInfo", function(obj) obj@top_left)
#' @export
#' @rdname top_left
setMethod("top_left", "PreDataTableLayouts", function(obj) obj@top_left)


#' @export
#' @rdname top_left
setGeneric("top_left<-", function(obj, value) standardGeneric("top_left<-"))
#' @export
#' @rdname top_left
setMethod("top_left<-", "VTableTree", function(obj, value) {
    cinfo <- col_info(obj)
    top_left(cinfo) <- value
    col_info(obj) <- cinfo
    obj
})
#' @export
#' @rdname top_left
setMethod("top_left<-", "InstantiatedColumnInfo", function(obj, value) {
    obj@top_left <- value
    obj
})

#' @export
#' @rdname top_left
setMethod("top_left<-", "PreDataTableLayouts", function(obj, value) {
    obj@top_left <- value
    obj
})


vil_collapse <- function(x) {
    x <- unlist(x)
    x <- x[!is.na(x)]
    x <- unique(x)
    x[nzchar(x)]
}

#' List Variables required by a pre-data table layout
#'
#' @param lyt The Layout (or a component thereof)
#'
#' @details This will walk the  layout declaration and return a vector
#'     of the  names of the unique  variables that are used  in any of
#'     the following ways:
#'
#' \itemize{
#' \item{Variable being split on (directly or via cuts)}
#' \item{Element of a Multi-variable column split}
#' \item{Content variable}
#' \item{Value-label variable}
#' }
#'
#' @note This function will not detect dependencies implicit in
#' analysis or summary functions which accept \code{df} and then
#' rely on the existence of particular variables not being split on/
#' analyzed.
#'
#' @note The order these variable names appear within the return vector
#' is undefined and should not be relied upon.
#'
#' @return A character vector containing the unique variables explicitly used in the layout (see Notes).
#'
#' @examples
#' lyt <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_cols_by("SEX") %>%
#'     summarize_row_groups(label_fstr = "Overall (N)") %>%
#'     split_rows_by("RACE", split_label = "Ethnicity", labels_var = "ethn_lab",
#'                   split_fun = drop_split_levels) %>%
#'     summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
#'     analyze("AGE", var_labels = "Age", afun = mean, format = "xx.xx")
#'
#' vars_in_layout(lyt)
#'
#' @export
#' @rdname vil
setGeneric("vars_in_layout", function(lyt) standardGeneric("vars_in_layout"))

#' @rdname vil
setMethod("vars_in_layout", "PreDataTableLayouts",
          function(lyt) {
    vil_collapse(c(vars_in_layout(clayout(lyt)),
             vars_in_layout(rlayout(lyt))))
})

#' @rdname vil
setMethod("vars_in_layout", "PreDataAxisLayout",
          function(lyt) {
    vil_collapse(lapply(lyt, vars_in_layout))
})

#' @rdname vil
setMethod("vars_in_layout", "SplitVector",
          function(lyt) {
    vil_collapse(lapply(lyt, vars_in_layout))
})

#' @rdname vil
setMethod("vars_in_layout", "Split",
          function(lyt) vil_collapse(c(spl_payload(lyt),
                                       ## for an AllSplit/RootSplit
                                       ## doesn't have to be same as payload
                                       content_var(lyt),
                                       spl_label_var(lyt))))

#' @rdname vil
setMethod("vars_in_layout", "CompoundSplit",
          function(lyt) vil_collapse(lapply(spl_payload(lyt), vars_in_layout)))

#' @rdname vil
setMethod("vars_in_layout", "ManualSplit",
          function(lyt) character())





## Titles and footers

setGeneric("main_title", function(obj) standardGeneric("main_title"))
setMethod("main_title", "VTitleFooter",
          function(obj) obj@main_title)

setGeneric("main_title<-", function(obj, value) standardGeneric("main_title<-"))
setMethod("main_title<-", "VTitleFooter",
          function(obj, value) {
    stopifnot(length(value) == 1)
    obj@main_title <- value
    obj
})


setGeneric("subtitles", function(obj) standardGeneric("subtitles"))
setMethod("subtitles", "VTitleFooter",
          function(obj) obj@subtitles)


setGeneric("subtitles<-", function(obj, value) standardGeneric("subtitles<-"))
setMethod("subtitles<-", "VTitleFooter",
          function(obj, value) {
    obj@subtitles <- value
    obj
})

all_titles <- function(obj) c(main_title(obj), subtitles(obj))


setGeneric("main_footer", function(obj) standardGeneric("main_footer"))
setMethod("main_footer", "VTitleFooter",
          function(obj) obj@main_footer)


setGeneric("main_footer<-", function(obj, value) standardGeneric("main_footer<-"))
setMethod("main_footer<-", "VTitleFooter",
          function(obj, value) {
    obj@main_footer <- value
    obj
})



setGeneric("prov_footer", function(obj) standardGeneric("prov_footer"))
setMethod("prov_footer", "VTitleFooter",
          function(obj) obj@provenance_footer)


setGeneric("prov_footer<-", function(obj, value) standardGeneric("prov_footer<-"))
setMethod("prov_footer<-", "VTitleFooter",
          function(obj, value) {
    obj@provenance_footer <- value
    obj
})

all_footers <- function(obj) c(main_footer(obj), prov_footer(obj))


#' Referential Footnote Accessors
#'
#' Get and set referential footnotes on aspects of a built table
#'
#' @inheritParams gen_args
#' @export
#' @rdname ref_fnotes

setGeneric("row_footnotes", function(obj) standardGeneric("row_footnotes"))
#' @export
#' @rdname ref_fnotes
setMethod("row_footnotes", "TableRow",
          function(obj) obj@row_footnotes)
#' @export
#' @rdname ref_fnotes
setMethod("row_footnotes", "RowsVerticalSection",
          function(obj) attr(obj, "row_footnotes") %||% list())

#' @export
#' @rdname ref_fnotes
setGeneric("row_footnotes<-", function(obj, value) standardGeneric("row_footnotes<-"))
#' @export
#' @rdname ref_fnotes
setMethod("row_footnotes<-", "TableRow",
          function(obj, value) {
    obj@row_footnotes <- value
    obj
})

#' @export
#' @rdname ref_fnotes
setMethod("row_footnotes", "ElementaryTable",
          function(obj) {
    rws <- collect_leaves(obj, TRUE, TRUE)
    cells <- lapply(rws, row_footnotes)
    cells
})

#' @export
#' @rdname ref_fnotes
setGeneric("cell_footnotes", function(obj) standardGeneric("cell_footnotes"))
#' @export
#' @rdname ref_fnotes
setMethod("cell_footnotes", "CellValue",
          function(obj) attr(obj, "footnotes") %||% list())
#' @export
#' @rdname ref_fnotes
setMethod("cell_footnotes", "TableRow",
          function(obj) {
    lapply(row_cells(obj), cell_footnotes)
})

#' @export
#' @rdname ref_fnotes
setMethod("cell_footnotes", "LabelRow",
          function(obj) {
    rep(list(list()), ncol(obj))
})

#' @export
#' @rdname ref_fnotes
setMethod("cell_footnotes", "ElementaryTable",
          function(obj) {
    rws <- collect_leaves(obj, TRUE, TRUE)
    cells <- lapply(rws, cell_footnotes)
    do.call(rbind, cells)
})


#' @export
#' @rdname ref_fnotes
setGeneric("cell_footnotes<-", function(obj, value) standardGeneric("cell_footnotes<-"))
#' @export
#' @rdname ref_fnotes
setMethod("cell_footnotes<-", "CellValue",
          function(obj, value) {
    if(is(value, "RefFootnote"))
        value <- list(value)
    else if (!is.list(value))
        value <- lapply(value, RefFootnote)
    attr(obj, "footnotes") <- value
    obj
})
#' @export
#' @rdname ref_fnotes
setMethod("cell_footnotes<-", "DataRow",
          function(obj, value) {
    if(length(value) != ncol(obj))
        stop("Did not get the right number of footnote ref values for cell_footnotes<- on a full row.")

    row_cells(obj) <- mapply(function(cell, fns) {
        if(is.list(fns))
            cell_footnotes(cell) <- lapply(fns, RefFootnote)
        else
            cell_footnotes(cell) <- list(RefFootnote(fns))
        cell
    },
    cell = row_cells(obj),
    fns = value, SIMPLIFY=FALSE)
    obj
})



#' @export
#' @rdname ref_fnotes
setGeneric("ref_index", function(obj) standardGeneric("ref_index"))
#' @export
#' @rdname ref_fnotes
setMethod("ref_index", "RefFootnote",
          function(obj) obj@index)

#' @export
#' @rdname ref_fnotes
setGeneric("ref_index<-", function(obj, value) standardGeneric("ref_index<-"))
#' @export
#' @rdname ref_fnotes
setMethod("ref_index<-", "RefFootnote",
          function(obj, value) {
    obj@index <- value
    obj
})

