## XXX Do we want add.labrows here or no?
## we have to choose one and stick to it.
setMethod("nrow", "VTableTree",
          function(x) length(collect_leaves(x, TRUE ,TRUE)))

setMethod("nrow", "TableRow",
          function(x) 1L)
setMethod("ncol", "VTableNodeInfo",
          function(x) {
    ncol(col_info(x))
})

setMethod("ncol", "InstantiatedColumnInfo",
          function(x) {
    length(col_exprs(x))
})

#' Dim of a TableTree-related Object
#' @param x The Object
#' @exportMethod dim
setMethod("dim", "VTableNodeInfo",
          function(x) c(nrow(x), ncol(x)))
#' Retrieve or set tthe direct children of a Tree-style objecgt
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
#' @rdname content_table
setGeneric("content_table<-", function(obj, value) standardGeneric("content_table<-"))
#' @exportMethod "content_table<-"
#' @rdname content_table
setMethod("content_table<-", c("TableTree", "ElementaryTable"),
          function(obj, value) {
    obj@content = value
    obj
})


setGeneric("next_rpos", function(obj, nested = TRUE, for_analyze = FALSE) standardGeneric("next_rpos"))

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
setMethod("next_rpos", "PreDataRowLayout",
          function(obj, nested, for_analyze) {
    l = length(obj)
    if(length(obj[[l]]) > 0L &&
       !.check_if_nest(obj, nested, for_analyze)) {
        l = l + 1L
    }
    l
})




setMethod("next_rpos", "ANY", function(obj, nested) 1L)

setGeneric("next_cpos", function(obj, nested = TRUE) standardGeneric("next_cpos"))

setMethod("next_cpos", "PreDataTableLayouts",
          function(obj, nested) next_cpos(clayout(obj), nested))

setMethod("next_cpos", "PreDataColLayout",
          function(obj, nested) {
    if(!nested)
        length(obj) + 1L
    else
        length(obj)
})

setMethod("next_cpos", "ANY", function(obj, nested) 1L)


setGeneric("last_rowsplit", function(obj) standardGeneric("last_rowsplit"))
setMethod("last_rowsplit", "NULL",
          function(obj) NULL)

setMethod("last_rowsplit", "SplitVector",
          function(obj) {
    if(length(obj) == 0)
        NULL
    else
        obj[[length(obj)]]
})
setMethod("last_rowsplit", "PreDataRowLayout",
          function(obj) {
    if(length(obj) == 0)
        NULL
    else
        last_rowsplit(obj[[ length( obj ) ]])
})

setMethod("last_rowsplit", "PreDataTableLayouts",
          function(obj) last_rowsplit(rlayout(obj)))




setGeneric("rlayout", function(obj) standardGeneric("rlayout"))

setMethod("rlayout", "PreDataTableLayouts",
          function(obj) obj@row_layout)

setMethod("rlayout", "ANY", function(obj) PreDataRowLayout())

setGeneric("rlayout<-", function(object, value) standardGeneric("rlayout<-"))
setMethod("rlayout<-", "PreDataTableLayouts",
          function(object, value) {
    object@row_layout = value
    object
})





setGeneric("tree_pos", function(obj) standardGeneric("tree_pos"))
## setMethod("tree_pos", "VNodeInfo",
##           function(obj) obj@pos_in_tree)
setMethod("tree_pos", "VLayoutNode",
          function(obj) obj@pos_in_tree)



setGeneric("pos_subset", function(obj) standardGeneric("pos_subset"))
setMethod("pos_subset", "TreePos",
          function(obj) obj@subset)
## setMethod("pos_subset", "VNodeInfo",
##           function(obj) pos_subset(tree_pos(obj)))
setMethod("pos_subset", "VLayoutNode",
          function(obj) pos_subset(tree_pos(obj)))

setGeneric("pos_splits", function(obj) standardGeneric("pos_splits"))
setMethod("pos_splits", "TreePos",
          function(obj) obj@splits)
## setMethod("pos_splits", "VNodeInfo",
##           function(obj) pos_splits(tree_pos(obj)))
setMethod("pos_splits", "VLayoutNode",
          function(obj) pos_splits(tree_pos(obj)))


setGeneric("pos_splvals", function(obj) standardGeneric("pos_splvals"))
setMethod("pos_splvals", "TreePos",
          function(obj) obj@s_values)

## setMethod("pos_splvals", "VNodeInfo",
##           function(obj) pos_splvals(tree_pos(obj)))
setMethod("pos_splvals", "VLayoutNode",
          function(obj) pos_splvals(tree_pos(obj)))




setGeneric("pos_split_labels", function(obj) standardGeneric("pos_split_labels"))
setMethod("pos_split_labels", "TreePos",
          function(obj) {
    spls = pos_splits(obj)
    sapply(spls, function(x) x@split_label)
})

## setMethod("pos_split_labels", "VNodeInfo",
##            function(obj) pos_split_labels(tree_pos(obj)))
setMethod("pos_split_labels", "VLayoutNode",
           function(obj) pos_split_labels(tree_pos(obj)))


setGeneric("split_texttype", function(obj) standardGeneric("split_texttype"))
setMethod("split_texttype", "VarLevelSplit", function(obj) "varlevels")
setMethod("split_texttype", "MultiVarSplit", function(obj) "multivar")
setMethod("split_texttype", "AllSplit", function(obj) "allobs")
setMethod("split_texttype", "RootSplit", function(obj) "root")
setMethod("split_texttype", "NULLSplit", function(obj) "null")
setMethod("split_texttype", "VarStaticCutSplit", function(obj) "scut")
setMethod("split_texttype", "VarDynCutSplit", function(obj) "dyncut")
setMethod("split_texttype", "ManualSplit", function(obj) "manual")

setMethod("split_texttype", "ANY", function(obj) stop("unknown split type"))

setGeneric("pos_spltypes", function(obj) standardGeneric("pos_spltypes"))
setMethod("pos_spltypes", "TreePos",
          function(obj) {
    spls = pos_splits(obj)
    sapply(spls, split_texttype)
})

## setMethod("pos_spltypes", "VNodeInfo",
##           function(obj) pos_spltypes(tree_pos(obj)))
setMethod("pos_spltypes", "VLayoutNode",
          function(obj) pos_spltypes(tree_pos(obj)))

setGeneric("pos_splval_labels", function(obj) standardGeneric("pos_splval_labels"))
setMethod("pos_splval_labels", "TreePos",
          function(obj) obj@sval_labels)
## setMethod("pos_splval_labels", "VNodeInfo",
##            function(obj) pos_splval_labels(tree_pos(obj)))
setMethod("pos_splval_labels", "VLayoutNode",
           function(obj) pos_splval_labels(tree_pos(obj)))



setGeneric("spl_payload", function(obj) standardGeneric("spl_payload"))
setMethod("spl_payload", "Split", function(obj) obj@payload)

setGeneric("spl_payload<-", function(obj, value) standardGeneric("spl_payload<-"))
setMethod("spl_payload<-", "Split", function(obj, value) {
    obj@payload <- value
    obj
})


setGeneric("spl_label_var", function(obj) standardGeneric("spl_label_var"))
setMethod("spl_label_var", "VarLevelSplit", function(obj) obj@value_label_var)

### name related things
#' Label and Name accessors
#' @param obj ANY. The object.
#' @rdname lab_name
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
        labelrow_visible(lr) = TRUE

    tt_labelrow(obj) = lr
    obj
})

### Label rows.
setGeneric("tt_labelrow", function(obj) standardGeneric("tt_labelrow"))
setMethod("tt_labelrow", "VTableTree",
          function(obj) obj@labelrow)

setGeneric("tt_labelrow<-", function(obj, value) standardGeneric("tt_labelrow<-"))
setMethod("tt_labelrow<-", "VTableTree",
          function(obj, value) {
    obj@labelrow = value
    obj
})

setGeneric("labelrow_visible", function(obj) standardGeneric("labelrow_visible"))
setMethod("labelrow_visible", "VTableTree",
          function(obj) {
    labelrow_visible(tt_labelrow(obj))
})
setMethod("labelrow_visible", "LabelRow",
          function(obj) obj@visible)

setMethod("labelrow_visible", "VAnalyzeSplit",
          function(obj) obj@var_label_visible)



setGeneric("labelrow_visible<-", function(obj, value) standardGeneric("labelrow_visible<-"))
setMethod("labelrow_visible<-", "VTableTree",
          function(obj, value) {
    lr = tt_labelrow(obj)
    labelrow_visible(lr) = value
    tt_labelrow(obj) = lr
    obj
})
setMethod("labelrow_visible<-", "LabelRow",
          function(obj, value) {
    obj@visible = value
    obj
})

setMethod("labelrow_visible<-", "VAnalyzeSplit",
          function(obj, value) {
    obj@var_label_visible = value
    obj
})


## TRUE is always, FALSE is never, NA is only when no
## content function (or rows in an instantiated table) is present
setGeneric("label_kids", function(spl) standardGeneric("label_kids"))
setMethod("label_kids", "Split", function(spl) spl@label_children)

setGeneric("label_kids<-", function(spl, value) standardGeneric("label_kids<-"))
setMethod("label_kids<-", c("Split", "character"), function(spl, value) {
    label_kids(spl) <- .labelkids_helper(value)
    spl
})

setMethod("label_kids<-", c("Split", "logical"), function(spl, value) {
    spl@label_children <- value
    spl
})


setGeneric("vis_label", function(spl) standardGeneric("vis_label"))
setMethod("vis_label", "Split", function(spl) spl@split_label_visible)

setGeneric("vis_label<-", function(spl, value) standardGeneric("vis_label<-"))
setMethod("vis_label<-", "Split", function(spl, value) {
    if(is.na(value))
        stop("split label visibility must be TRUE or FALSE, got NA")
    spl@split_label_visible <- value
    spl
})


### Function acessors (summary, tabulation and split)

setGeneric("content_fun", function(obj) standardGeneric("content_fun"))
setMethod("content_fun", "Split", function(obj) obj@content_fun)


setGeneric("content_fun<-", function(object, value) standardGeneric("content_fun<-"))
setMethod("content_fun<-", "Split", function(object, value) {
    object@content_fun = value
    object
})


setGeneric("content_var", function(obj) standardGeneric("content_var"))
setMethod("content_var", "Split", function(obj) obj@content_var)


setGeneric("content_var<-", function(object, value) standardGeneric("content_var<-"))
setMethod("content_var<-", "Split", function(object, value) {
    object@content_var = value
    object
})

setGeneric("analysis_fun", function(obj) standardGeneric("analysis_fun"))
setMethod("analysis_fun", "AnalyzeVarSplit", function(obj) obj@analysis_fun)
setMethod("analysis_fun", "AnalyzeColVarSplit", function(obj) obj@analysis_fun)




setGeneric("split_fun", function(obj) standardGeneric("split_fun"))
setMethod("split_fun", "CustomizableSplit", function(obj) obj@split_fun)

## Only that type of split currently has the slot
## this should probably change? for now  define
## an accessor that just returns NULL
setMethod("split_fun", "Split", function(obj) NULL)



### Miscelaneous accessors

setGeneric("avar_inclNAs", function(obj) standardGeneric("avar_inclNAs"))
setMethod("avar_inclNAs", "VAnalyzeSplit",
          function(obj) obj@include_NAs)

setGeneric("avar_inclNAs<-", function(obj, value) standardGeneric("avar_inclNAs<-"))
setMethod("avar_inclNAs<-", "VAnalyzeSplit",
          function(obj, value) {
    obj@include_NAs = value
})


setGeneric("spl_labelvar", function(obj) standardGeneric("spl_labelvar"))
setMethod("spl_labelvar", "VarLevelSplit", function(obj) obj@value_label_var)

setGeneric("spl_child_order", function(obj) standardGeneric("spl_child_order"))
setMethod("spl_child_order", "VarLevelSplit", function(obj) obj@value_order)

setGeneric("spl_child_order<-",
           function(obj, value) standardGeneric("spl_child_order<-"))
setMethod("spl_child_order<-", "VarLevelSplit",
          function(obj, value) {
    obj@value_order = value
    obj
})

setMethod("spl_child_order",
          "ManualSplit",
          function(obj) obj@levels)

setMethod("spl_child_order",
          "MultiVarSplit",
          function(obj) spl_payload(obj))

setMethod("spl_child_order",
          "AllSplit",
          function(obj) character())

setMethod("spl_child_order",
          "VarStaticCutSplit",
          function(obj) spl_cutlabels(obj))





setGeneric("root_spl", function(obj) standardGeneric("root_spl"))
setMethod("root_spl", "PreDataAxisLayout",
          function(obj) obj@root_split)


setGeneric("root_spl<-", function(obj, value) standardGeneric("root_spl<-"))
setMethod("root_spl<-", "PreDataAxisLayout",
          function(obj, value) {
    obj@root_split <- value
    obj
})

#' Row attribute accessors
#' @inheritParams gen_args
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

setGeneric("spanned_values", function(obj) standardGeneric("spanned_values"))
setMethod("spanned_values", "TableRow",
          function(obj) {
    sp = row_cspans(obj)
    rvals = row_values(obj)
    unlist(mapply(function(v, s) rep(list(v), times = s),
                  v = rvals, s = sp),
           recursive = FALSE)
})

setMethod("spanned_values", "LabelRow",
          function(obj) {
    rep(list(NULL), ncol(obj))
})


setGeneric("spanned_cells", function(obj) standardGeneric("spanned_cells"))
setMethod("spanned_cells", "TableRow",
          function(obj) {
    sp = row_cspans(obj)
    rvals = row_cells(obj)
    unlist(mapply(function(v, s) rep(list(v), times = s)),
           recursive = FALSE)
})

setMethod("spanned_cells", "LabelRow",
          function(obj) {
    rep(list(NULL), ncol(obj))
})


setGeneric("spanned_values<-", function(obj, value) standardGeneric("spanned_values<-"))
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

setMethod("spanned_values<-", "LabelRow",
          function(obj, value) {
    if(!is.null(value))
        stop("Label rows can't have non-null cell values, got", value)
    obj
})



### Format manipulation
### obj_format<- is not recursive

setGeneric("obj_format", function(obj) standardGeneric("obj_format"))
## this covers rcell, etc
setMethod("obj_format", "ANY", function(obj) attr(obj, "format"))
setMethod("obj_format", "VTableNodeInfo", function(obj) obj@format)
##setMethod("obj_format", "CellValue", function(obj) obj@format)
setMethod("obj_format", "Split", function(obj) obj@split_format)

setGeneric("obj_format<-", function(obj, value) standardGeneric("obj_format<-"))
## this covers rcell, etc
setMethod("obj_format<-", "ANY", function(obj, value) {
    attr(obj, "format") = value
    obj
})
setMethod("obj_format<-", "VTableNodeInfo", function(obj, value) {
    obj@format = value
    obj
})

setMethod("obj_format<-", "Split", function(obj, value) {
    obj@split_format = value
    obj
})

## setMethod("obj_format<-", "CellValue", function(obj, value) {
##     obj@format = value
##     obj
## })




setGeneric("set_format_recursive", function(obj, format, override = FALSE) standardGeneric("set_format_recursive"))
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

setGeneric("content_format", function(obj) standardGeneric("content_format"))
setMethod("content_format", "Split", function(obj) obj@content_format)

setGeneric("content_format<-", function(obj, value) standardGeneric("content_format<-"))

setMethod("content_format<-", "Split", function(obj, value) {
    obj@content_format = value
    obj
})

## credit: rlang, Henry and Wickham.
## this one tiny utility function is NOT worth a dependency.
## modified it so any length 0 x grabs y
`%||%` = function(L, R) if(length(L) == 0) R else L
setGeneric("value_formats", function(obj, default = obj_format(obj)) standardGeneric("value_formats"))
setMethod("value_formats", "ANY",
          function(obj, default) {
    attr(obj, "format") %||% default
})

setMethod("value_formats", "TableRow",
          function(obj, default) {
    formats = lapply(row_values(obj), function(x)
        value_formats(x) %||% default)
    formats
})

setMethod("value_formats", "LabelRow",
          function(obj, default) {
    rep(list(NULL), ncol(obj))
})



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
    tt
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

setGeneric("row_cspans", function(obj) standardGeneric("row_cspans"))
setMethod("row_cspans", "TableRow", function(obj) obj@colspans)
setMethod("row_cspans", "LabelRow",
          function(obj) rep(1L, ncol(obj)))

setGeneric("row_cspans<-", function(obj, value) standardGeneric("row_cspans<-"))
setMethod("row_cspans<-", "TableRow", function(obj, value) {
    obj@colspans = value
    obj
})
setMethod("row_cspans<-", "LabelRow", function(obj, value) {
    stop("attempted to set colspans for LabelRow")
})


## XXX TODO colapse with above?

setGeneric("cell_cspan", function(obj) standardGeneric("cell_cspan"))
setMethod("cell_cspan", "CellValue", function(obj) attr(obj, "colspan")) ##obj@colspan)

setGeneric("cell_cspan<-", function(obj, value) standardGeneric("cell_cspan<-"))
setMethod("cell_cspan<-", "CellValue", function(obj, value) {
    ##  obj@colspan = value
    attr(obj, "colspan") <- value
    obj
})



### Level (indent) in tree structure
setGeneric("tt_level", function(obj) standardGeneric("tt_level"))
## this will hit everything via inheritence
setMethod("tt_level", "VNodeInfo", function(obj) obj@level)


setGeneric("tt_level<-", function(obj, value) standardGeneric("tt_level<-"))
## this will hit everything via inheritence
setMethod("tt_level<-", "VNodeInfo", function(obj, value) {
    obj@level = as.integer(value)
    obj
})

setMethod("tt_level<-", "VTableTree",
          function(obj, value) {
    obj@level = as.integer(value)
    tree_children(obj) = lapply(tree_children(obj),
                                `tt_level<-`, value = as.integer(value) + 1L)
    obj
})

setGeneric("indent_mod", function(obj) standardGeneric("indent_mod"))
setMethod("indent_mod", "Split",
          function(obj) obj@indent_modifier)

setMethod("indent_mod", "VTableNodeInfo",
          function(obj) obj@indent_modifier)

setGeneric("indent_mod<-", function(obj, value) standardGeneric("indent_mod<-"))
setMethod("indent_mod<-", "Split",
          function(obj, value) {
    obj@indent_modifier = as.integer(value)
    obj
})
setMethod("indent_mod<-", "VTableNodeInfo",
          function(obj, value) {
    obj@indent_modifier = as.integer(value)
    obj
})



setGeneric("content_indent_mod", function(obj) standardGeneric("content_indent_mod"))
setMethod("content_indent_mod", "Split",
          function(obj) obj@content_indent_modifier)

setMethod("content_indent_mod", "VTableNodeInfo",
          function(obj) obj@content_indent_modifier)

setGeneric("content_indent_mod<-", function(obj, value) standardGeneric("content_indent_mod<-"))
setMethod("content_indent_mod<-", "Split",
          function(obj, value) {
    obj@content_indent_modifier = as.integer(value)
    obj
})
setMethod("content_indent_mod<-", "VTableNodeInfo",
          function(obj, value) {
    obj@content_indent_modifier = as.integer(value)
    obj
})


setGeneric("rawvalues", function(obj) standardGeneric("rawvalues"))
setMethod("rawvalues", "ValueWrapper",  function(obj) obj@value)
setMethod("rawvalues", "list", function(obj) lapply(obj, rawvalues))
setMethod("rawvalues", "ANY", function(obj) obj)
setMethod("rawvalues", "CellValue", function(obj) obj[[1]])
setMethod("rawvalues", "TreePos",
          function(obj) rawvalues(pos_splvals(obj)))

setGeneric("value_names", function(obj) standardGeneric("value_names"))
setMethod("value_names", "ANY", function(obj) as.character(rawvalues(obj)))
setMethod("value_names", "list", function(obj) lapply(obj, value_names))
setMethod("value_names", "ValueWrapper",  function(obj) rawvalues(obj))
setMethod("value_names", "LevelComboSplitValue",  function(obj) obj@comboname)

## not sure if I need these anywhere
## XXX
setGeneric("value_labels", function(obj) standardGeneric("value_labels"))
setMethod("value_labels", "ANY", function(obj) as.character(obj_label(obj)))
setMethod("value_labels", "TreePos", function(obj) sapply(pos_splvals(obj), obj_label))
setMethod("value_labels", "list", function(obj) lapply(obj, value_labels))
setMethod("value_labels", "ValueWrapper",  function(obj) obj_label(obj))
setMethod("value_labels", "LevelComboSplitValue",  function(obj) obj@comboname)
setMethod("value_labels", "MultiVarSplit", function(obj) obj@var_labels)



## These two are similar enough we could probably combine
## them but conceptually they are pretty different
## split_exargs is a list of extra arguments that apply
## to *all the chidlren*,
## while splv_extra is for *child-specific* extra arguments,
## associated with specific values of the split
setGeneric("splv_extra", function(obj) standardGeneric("splv_extra"))
setMethod("splv_extra", "SplitValue",
          function(obj) obj@extra)

setGeneric("split_exargs", function(obj) standardGeneric("split_exargs"))
setMethod("split_exargs", "Split",
          function(obj) obj@extra_args)

setGeneric("split_exargs<-", function(obj, value) standardGeneric("split_exargs<-"))
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
    ## ## XXX this [[1]] is WRONG!!
    ## ## XXXXXXXX
    ## if(length(obj) == 1L)
    ##     splitvec_to_coltree(df, obj[[1]], rtpos)
    ## else
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

setGeneric("cextra_args", function(obj, df = NULL) standardGeneric("cextra_args"))

setMethod("cextra_args", "InstantiatedColumnInfo",
          function(obj, df) {
    if(!is.null(df))
        warning("Ignorning df when retrieving already-computed column extra arguments.")
    obj@cextra_args
})


setMethod("cextra_args", "PreDataTableLayouts",
          function(obj, df) cextra_args(clayout(obj), df))

setMethod("cextra_args", "PreDataColLayout",
          function(obj, df) {
    cextra_args(coltree(obj, df), NULL)
})


setMethod("cextra_args", "LayoutColTree",
          function(obj, df) {
    if(!is.null(df))
        warning("Ignoring df argument and returning already calculated extra arguments")
    get_col_extras(obj)
})

setMethod("cextra_args", "LayoutColLeaf",
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



setGeneric("disp_ccounts", function(obj) standardGeneric("disp_ccounts"))

setMethod("disp_ccounts", "VTableTree",
          function(obj) disp_ccounts(col_info(obj)))

setMethod("disp_ccounts", "InstantiatedColumnInfo",
          function(obj) obj@display_columncounts)

setMethod("disp_ccounts", "PreDataTableLayouts",
          function(obj) disp_ccounts(clayout(obj)))

setMethod("disp_ccounts", "PreDataColLayout",
          function(obj) obj@display_columncounts)


setGeneric("disp_ccounts<-", function(obj, value) standardGeneric("disp_ccounts<-"))

setMethod("disp_ccounts<-", "VTableTree",
          function(obj, value) {
    cinfo = col_info(obj)
    disp_ccounts(cinfo) = value
    col_info(obj) = cinfo
    obj
})

setMethod("disp_ccounts<-", "InstantiatedColumnInfo",
          function(obj, value) {
    obj@display_columncounts = value
    obj
})

setMethod("disp_ccounts<-", "PreDataColLayout",
          function(obj, value) {
    obj@display_columncounts = value
    obj
})

setMethod("disp_ccounts<-", "LayoutColTree",
          function(obj, value) {
    obj@display_columncounts = value
    obj
})



setMethod("disp_ccounts<-", "PreDataTableLayouts",
          function(obj, value) {
    clyt = clayout(obj)
    disp_ccounts(clyt) = value
    clayout(obj) = clyt
    obj
})


setGeneric("colcount_format", function(obj) standardGeneric("colcount_format"))

setMethod("colcount_format", "InstantiatedColumnInfo",
          function(obj) obj@columncount_format)

setMethod("colcount_format", "VTableNodeInfo",
          function(obj) colcount_format(col_info(obj)))


setMethod("colcount_format", "PreDataColLayout",
          function(obj) obj@columncount_format)

setMethod("colcount_format", "PreDataTableLayouts",
          function(obj) colcount_format(clayout(obj)))



setGeneric("colcount_format<-", function(obj,value) standardGeneric("colcount_format<-"))

setMethod("colcount_format<-", "InstantiatedColumnInfo",
          function(obj, value) {
    obj@columncount_format = value
    obj
})

setMethod("colcount_format<-", "VTableNodeInfo",
          function(obj, value) {
    cinfo = col_info(obj)
    colcount_format(cinfo) = value
    col_info(obj) = cinfo
    obj
})


setMethod("colcount_format<-", "PreDataColLayout",
          function(obj, value) {
    obj@columncount_format = value
    obj
})

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
#'
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
#' @rdname asvec
#' @param x ANY. The object to be converted to a vector
#' @param mode character(1). Passed on to \code{\link[base]{as.vector}}
#' @exportMethod as.vector
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

setGeneric("spl_cuts", function(obj) standardGeneric("spl_cuts"))
setMethod("spl_cuts", "VarStaticCutSplit",
          function(obj) obj@cuts)

setGeneric("spl_cutlabels", function(obj) standardGeneric("spl_cutlabels"))
setMethod("spl_cutlabels", "VarStaticCutSplit",
          function(obj) obj@cut_labels)


setGeneric("spl_cutfun", function(obj) standardGeneric("spl_cutfun"))
setMethod("spl_cutfun", "VarDynCutSplit",
          function(obj) obj@cut_fun)

setGeneric("spl_cutlabelfun", function(obj) standardGeneric("spl_cutlabelfun"))
setMethod("spl_cutlabelfun", "VarDynCutSplit",
          function(obj) obj@cut_label_fun)


setGeneric("spl_is_cmlcuts", function(obj) standardGeneric("spl_is_cmlcuts"))
setMethod("spl_is_cmlcuts", "VarDynCutSplit",
          function(obj) obj@cumulative_cuts)
