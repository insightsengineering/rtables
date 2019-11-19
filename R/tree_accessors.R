

setMethod("nrow", "ElementaryTable",
          function(x) length(tree_children(x)))
## XXX Do we want add.labrows here or no?
## we have to choose one and stick to it.
setMethod("nrow", "VTableTree",
          function(x) length(collect_leaves(x)))

setMethod("ncol", "VTableNodeInfo",
          function(x) {
    ci = col_info(x)
    length(col_exprs(ci))
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

setGeneric("content_table<-", function(obj, value) standardGeneric("content_table<-"))
setMethod("content_table<-", c("TableTree", "ElementaryTable"),
          function(obj, value) {
    obj@content = value
    obj
})


setGeneric("next_rpos", function(obj, newtree = FALSE) standardGeneric("next_rpos"))

setMethod("next_rpos", "PreDataTableLayouts",
          function(obj, newtree) next_rpos(rlayout(obj), newtree))

setMethod("next_rpos", "PreDataRowLayout",
          function(obj, newtree) {
    if(newtree)
        length(obj) + 1L
    else
        length(obj)
})

## setMethod("next_rpos", "PreDataColLayout", function(obj, newtree) stop("can't get next row position from a column layout object"))

setMethod("next_rpos", "ANY", function(obj, newtree) 1L)

setGeneric("next_cpos", function(obj, newtree = FALSE) standardGeneric("next_cpos"))

setMethod("next_cpos", "PreDataTableLayouts",
          function(obj, newtree) next_cpos(rlayout(obj), newtree))

setMethod("next_cpos", "PreDataColLayout",
          function(obj, newtree) {
    if(newtree)
        length(obj) + 1L
    else
        length(obj)
})

setMethod("next_cpos", "ANY", function(obj, newtree) 1L)

## setMethod("next_cpos", "PreDataRowLayout", function(obj, newtree) stop("can't get next column position from a row layout object"))





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
setMethod("obj_label", "TableTree", function(obj) obj_label(obj@split))



setGeneric("obj_label<-", function(obj, value) standardGeneric("obj_label<-"))
setMethod("obj_label<-", "Split", function(obj, value) {
    obj@split_label <- value
    obj
})
setMethod("obj_label<-", "VNodeInfo", function(obj, value){
    obj@label = value
    obj
})

setMethod("obj_label<-", "TableTree", function(obj, value) {
    spl = obj@split
    obj_label(spl) = value
    obj@split = spl
    obj
})
    



setGeneric("content_fun", function(obj) standardGeneric("content_fun"))
setMethod("content_fun", "Split", function(obj) obj@content_fun)


setGeneric("content_fun<-", function(object, value) standardGeneric("content_fun<-"))
setMethod("content_fun<-", "Split", function(object, value) {
    object@content_fun = value
    object
})


setGeneric("analysis_fun", function(obj) standardGeneric("analysis_fun"))
setMethod("analysis_fun", "AnalyzeVarSplit", function(obj) obj@analysis_fun)

setGeneric("avar_inclNAs", function(obj) standardGeneric("avar_inclNAs"))
setMethod("avar_inclNAs", "AnalyzeVarSplit",
          function(obj) obj@include_NAs)

setGeneric("avar_inclNAs<-", function(obj, value) standardGeneric("avar_inclNAs<-"))
setMethod("avar_inclNAs<-", "AnalyzeVarSplit",
          function(obj, value) {
    obj@include_NAs = value
})



setGeneric("split_fun", function(obj) standardGeneric("split_fun"))
setMethod("split_fun", "CustomizableSplit", function(obj) obj@split_fun)

## Only that type of split currently has the slot
## this should probably change? for now  define
## an accessor that just returns NULL
setMethod("split_fun", "Split", function(obj) NULL)

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

setMethod("spl_child_order",
          "ManualSplit",
          function(obj) obj@levels)





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
setMethod("row_values", "TableRow", function(obj) obj@leaf_value)

setGeneric("row_values<-", function(obj, value) standardGeneric("row_values<-"))
setMethod("row_values<-", "TableRow",
          function(obj, value) {
    obj@leaf_value = value
    obj
})


setGeneric("obj_fmt", function(obj) standardGeneric("obj_fmt"))
## this covers rcell, etc
setMethod("obj_fmt", "ANY", function(obj) attr(obj, "format"))
setMethod("obj_fmt", "VTableNodeInfo", function(obj) obj@format)
setMethod("obj_fmt", "Split", function(obj) obj@split_format)

setGeneric("obj_fmt<-", function(obj, value) standardGeneric("obj_fmt<-"))
## this covers rcell, etc
setMethod("obj_fmt<-", "ANY", function(obj, value) {
    attr(obj, "format") = value
    obj
})
setMethod("obj_fmt<-", "VTableNodeInfo", function(obj, value) {
    obj@format = value
    obj
})

setMethod("obj_fmt<-", "Split", function(obj, value) {
    obj@split_format = value
    obj
})

setGeneric("set_fmt_recursive", function(obj, fmt, override = FALSE) standardGeneric("set_fmt_recursive"))
setMethod("set_fmt_recursive", "TableRow",
          function(obj, fmt, override = FALSE) {
    if(is.null(fmt))
        return(obj)
    if(is.null(obj_fmt(obj)) || override)
        obj_fmt(obj) = fmt
    lvals = row_values(obj)
    lvals = lapply(lvals, function(x) {
        if(!is.null(x) && is.null(attr(x, "format")))
            attr(x, "format") = obj_fmt(obj)
        x
    })
    row_values(obj) = lvals
    obj
})

setMethod("set_fmt_recursive", "VTableTree",
          function(obj, fmt, override = FALSE) {
    force(fmt)
    if(is.null(fmt))
        return(obj)
    
    if(is.null(obj_fmt(obj)) || override)
        obj_fmt(obj) = fmt
    
    kids = tree_children(obj)
    kids = lapply(kids, function(x, fmt2, oride) set_fmt_recursive(x,
                                                                      fmt = fmt2, override = oride),
                  fmt2 = obj_fmt(obj), oride = override)
    tree_children(obj) = kids
    obj
})



setGeneric("content_fmt", function(obj) standardGeneric("content_fmt"))
setMethod("content_fmt", "Split", function(obj) obj@content_format)

setGeneric("content_fmt<-", function(obj, value) standardGeneric("content_fmt<-"))

setMethod("content_fmt<-", "Split", function(obj, value) {
    obj@content_format = value
    obj
})

setGeneric("current_spl", function(obj) standardGeneric("current_spl"))
setMethod("current_spl", "VTableTree", function(obj) obj@split)


setGeneric("current_spl<-", function(obj, value) standardGeneric("current_spl<-"))
setMethod("current_spl<-", "VTableTree", function(obj, value)  {
    obj@split = value
    obj
})


setGeneric("collect_leaves",
           function(ttree, incl.cont = TRUE, add.labrows = FALSE)
    standardGeneric("collect_leaves"), signature = "ttree")


.controws = function(tr, add.labrows) {
    rows = tree_children(content_table(tr))
    if(add.labrows) {
        lab = obj_label(tr@split)
        if(length(lab) > 0 && !is.na(lab) && nzchar(lab)) {
            ## rows is first because the label is
            ## for the split at the next level of nesting
            lrowpos = if(length(rows)) tree_pos(rows[[1]]) else TableRowPos(iscontent = TRUE, islabel = TRUE)
            is_labrow(lrowpos) = TRUE
            rows = c( rows, TTLabelRow(lab = lab, lev = tt_level(tr),
                                     tpos = lrowpos, cinfo = col_info(tr)))
        }
    }
    rows
}
##handle the ext
setMethod("collect_leaves", "TableTree",
          function(ttree, incl.cont = TRUE, add.labrows = FALSE) {
    ret = c(#if(incl.cont) {tree_children(content_table(ttree))},
        if(incl.cont) {.controws(ttree, add.labrows)},
      lapply(tree_children(ttree),
             collect_leaves, incl.cont = incl.cont, add.labrows = add.labrows))
    unlist(ret, recursive = TRUE)
})


setMethod("collect_leaves", "ElementaryTable",
          function(ttree, incl.cont = TRUE, add.labrows = FALSE) {
    ret = tree_children(ttree)
    if(add.labrows) {
        rl = obj_label(ttree)
        if(length(rl) >0 && !is.na(rl) && nzchar(rl))
            ret = c(TTLabelRow(lev = max(0L,length(pos_splits(ttree)) - 2L), lab = rl, cinfo = col_info(ttree)), ret)
    }
    ret
})
setMethod("collect_leaves", "VTree",
          function(ttree, incl.cont, add.labrows) {
    ret = lapply(tree_children(ttree),
                 collect_leaves)
    unlist(ret, recursive = TRUE)
})

setMethod("collect_leaves", "VLeaf",
          function(ttree, incl.cont, add.labrows) {
    ttree
})

setMethod("collect_leaves", "ANY",
          function(ttree, incl.cont, add.labrows)
    stop("class ", class(ttree), " does not inherit from VTree or VLeaf"))


setGeneric("row_cspans", function(obj) standardGeneric("row_cspans"))
setMethod("row_cspans", "TableRow", function(obj) obj@colspans)

setGeneric("row_cspans<-", function(obj, value) standardGeneric("row_cspans<-"))
setMethod("row_cspans<-", "TableRow", function(obj, value) {
    obj@colspans = value
    obj
})

setGeneric("tt_level", function(obj) standardGeneric("tt_level"))
## this will hit everything via inheritence
setMethod("tt_level", "VNodeInfo", function(obj) obj@level)

setGeneric("splv_rawvalues", function(obj) standardGeneric("splv_rawvalues"))
setMethod("splv_rawvalues", "SplitValue",
          function(obj) obj@value)
setMethod("splv_rawvalues", "list",
          function(obj) lapply(obj, splv_rawvalues))

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






setGeneric("is_labrow", function(obj) standardGeneric("is_labrow"))

setMethod("is_labrow", "TableRowPos",
          function(obj) obj@is_label_row)

setMethod("is_labrow", "TableRow",
          function(obj) is_labrow(tree_pos(obj)))


setGeneric("is_labrow<-", function(obj, value) standardGeneric("is_labrow<-"))

setMethod("is_labrow<-", "TableRowPos",
          function(obj, value) {
    obj@is_label_row = value
    obj
})

spl_baseline = function(obj) {
    stopifnot(is(obj, "VarLevWBaselineSplit"))
    obj@baseline_value
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
setGeneric("clayout", function(obj) standardGeneric("clayout"))
setMethod("clayout", "VTableNodeInfo",
          function(obj) obj@col_info@tree_layout)

setMethod("clayout", "PreDataTableLayouts",
          function(obj) obj@col_layout)

## useful convenience for the cascading methods in colby_constructors
setMethod("clayout", "ANY", function(obj) PreDataColLayout())



setGeneric("clayout<-", function(object, value) standardGeneric("clayout<-"))
setMethod("clayout<-", "PreDataTableLayouts",
          function(object, value) {
    object@col_layout = value
    object
})


setGeneric("col_info", function(obj) standardGeneric("col_info"))
setMethod("col_info", "VTableNodeInfo",
          function(obj) obj@col_info)

### XXX I've made this recursive. Do we ALWAYS want it to be?
###
### I think we do.
setGeneric("col_info<-", function(obj, value) standardGeneric("col_info<-"))
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
setMethod("col_info<-", "ElementaryTable",
          function(obj, value) {
    obj@col_info = value
    .set_cinfo_kids(obj)
    
})

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







setGeneric("coltree", function(obj, df = NULL, rtpos = TreePos()) standardGeneric("coltree"))
setMethod("coltree", "InstantiatedColumnInfo",
          function(obj, df = NULL, rtpos = TreePos()) {
    if(!is.null(df))
        warning("Ignoring df argument and retrieving already-computed LayoutColTree")
    obj@tree_layout
})

setMethod("coltree", "PreDataTableLayouts",
          function(obj, df, rtpos) coltree(clayout(obj), df, rtpos))

setMethod("coltree", "PreDataColLayout",
          function(obj, df, rtpos) {
    ## ## XXX this [[1]] is WRONG!!
    ## ## XXXXXXXX
    ## if(length(obj) == 1L)
    ##     splitvec_to_coltree(df, obj[[1]], rtpos)
    ## else
    kids = lapply(obj, function(x) splitvec_to_coltree(df = df, splvec = x, pos = rtpos))
    if(length(kids) == 1)
        kids[[1]]
    else
        LayoutColTree(lev = 0L,
                  kids = kids,
                  tpos = rtpos,
                  spl = RootSplit())
})


setMethod("coltree", "LayoutColTree",
          function(obj, df, rtpos) obj)

setMethod("coltree", "VTableTree",
          function(obj, df, rtpos) coltree(col_info(obj)))

setGeneric("col_exprs", function(obj, df = NULL) standardGeneric("col_exprs"))

setMethod("col_exprs", "PreDataTableLayouts",
          function(obj, df = NULL) col_exprs(clayout(obj), df))

setMethod("col_exprs", "PreDataColLayout",
          function(obj, df = NULL) {
    unlist(recursive = FALSE,
           lapply(obj, build_splits_expr,
                  df = df))
})

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


setGeneric("col_counts", function(obj) standardGeneric("col_counts"))

setMethod("col_counts",  "InstantiatedColumnInfo",
          function(obj) obj@counts)

setMethod("col_counts", "VTableNodeInfo",
          function(obj) col_counts(col_info(obj)))

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



setMethod("disp_ccounts<-", "PreDataTableLayouts",
          function(obj, value) {
    clyt = clayout(obj)
    disp_ccounts(clyt) = value
    clayout(obj) = clyt
    obj
})


setGeneric("colcount_fmt", function(obj) standardGeneric("colcount_fmt"))

setMethod("colcount_fmt", "InstantiatedColumnInfo",
          function(obj) obj@columncount_format)

setMethod("colcount_fmt", "VTableNodeInfo",
          function(obj) colcount_fmt(col_info(obj)))


setMethod("colcount_fmt", "PreDataColLayout",
          function(obj) obj@columncount_format)

setMethod("colcount_fmt", "PreDataTableLayouts",
          function(obj) colcount_fmt(clayout(obj)))



setGeneric("colcount_fmt<-", function(obj,value) standardGeneric("colcount_fmt<-"))

setMethod("colcount_fmt<-", "InstantiatedColumnInfo",
          function(obj, value) {
    obj@columncount_formatvalue
    obj
})

setMethod("colcount_fmt<-", "VTableNodeInfo",
          function(obj, value) {
    cinfo = col_info(obj)
    colcount_fmt(cinfo) = value
    col_info(obj) = cinfo
    obj
})


setMethod("colcount_fmt<-", "PreDataColLayout",
          function(obj, value) {
    obj@columncount_format = value
    obj
})

setMethod("colcount_fmt<-", "PreDataTableLayouts",
          function(obj, value) {
    clyt = clayout(obj)
    colcount_fmt(clyt) = value
    clayout(obj) = clyt
    obj
})


setGeneric("no_colinfo", function(obj) standardGeneric("no_colinfo"))
setMethod("no_colinfo", "VTableNodeInfo",
          function(obj) no_colinfo(col_info(obj)))

setMethod("no_colinfo", "InstantiatedColumnInfo",
           function(obj) identical(obj, InstantiatedColumnInfo()))
