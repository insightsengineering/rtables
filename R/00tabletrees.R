
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

## heavier-weight than I'd like but I think we need
## this to carry around thee subsets for
## comparison-based splits

setClass("SplitValue",
         representation(
             value = "ANY",
             extra = "list"))

SplitValue = function(val, extr =list()) {
    if(is(val, "SplitValue"))
        stop("SplitValue  object passed to SplitValue constructor")
    ## this might happen if its coming out of the
    ## data.frame form.
    if(is(extr, "AsIs"))
        extr = unclass(extr)
    if(is(val, "AsIs"))
        val = unclass(val)
    
    new("SplitValue", value = val,
        extra = extr)
}


setClass("Split", contains = "VIRTUAL",
         representation(
             payload = "ANY",
             name = "character",
             split_label = "character",
             split_format = "FormatSpec",
             ## NB this is the function which is applied to
             ## get the content rows for the CHILDREN of this
             ## split!!!
             content_fun = "functionOrNULL",
             content_format = "FormatSpec",
             extra_args = "list"))


setClass("CustomizableSplit", contains = "Split",
         representation( split_fun = "functionOrNULL"))

setClass("VarLevelSplit", contains = "CustomizableSplit",
         representation(value_lbl_var = "character",
                        value_order = "ANY"))

VarLevelSplit = function(var, splbl, valuelblvar = NULL, cfun = NULL, cfmt = NULL, splfun = NULL, splfmt = NULL, valorder = NULL, splname = var ) {
    if(is.null(valuelblvar))
        valuelblvar = var
    new("VarLevelSplit", payload = var,
        split_label = splbl,
        name = splname,
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
    new("NULLSplit", payload = character(), split_label = character(), content_fun = NULL, content_format= NULL, split_format = NULL, name  = "")
}

setClass("AllSplit", contains = "Split",
         validity = function(object) length(object@payload) == 0
         )

AllSplit = function(splbl = "", cfun = NULL, cfmt = NULL, splfmt = NULL, splname = "ALL", ...) {
    new("AllSplit", split_label = splbl,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt,
        name = splname)
}

setClass("RootSplit", contains = "AllSplit")

RootSplit = function(splbl = "", cfun = NULL, cfmt = NULL, splfmt= NULL, ...) {
    new("RootSplit", split_label = splbl,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt,
        name = "root")
}

setClass("ManualSplit", contains = "AllSplit",
         representation(levels = "character"))


ManualSplit = function(levs, lbl, name = "manual") {
    new("ManualSplit",
        split_label = lbl,
        levels = levs,
        name = name)
    
}


## splits across which variables are being analynzed
setClass("MultiVarSplit", contains = "Split",
         representation(var_labels = "character"),
         validity = function(object) {
    length(object@payload) > 1 &&
        all(!is.na(object@payload)) &&
        length(object@payload) == length(object@var_labels)
})


MultiVarSplit = function(vars, splbl, varlbls = NULL, cfun = NULL, cfmt = NULL, splfmt = NULL, splname = paste(vars, collapse = ":")) {
    if(length(vars) == 1 && grepl(":", vars))
        vars = strsplit(vars, ":")[[1]]
    if(length(varlbls) == 0) ## covers NULL and character()
       varlbls = vars
    new("MultiVarSplit", payload = vars,
        split_label = splbl,
        var_labels = varlbls,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt)
}


setClass("VarStaticCutSplit", contains = "Split",
         representation(cuts = "numeric",
                        cut_labels = "character"))

VarStaticCutSplit = function(var, splbl, cuts, cutlbls = NULL, cfun = NULL, cfmt = NULL, splfmt = NULL, splname = var) {
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
        split_format = splfmt,
        name = splname)
}


## do we want this to be a CustomizableSplit instead of
## taking cut_fun?
## cut_funct must take avector and no other arguments
## and return a named vector of cut points
setClass("VarDynCutSplit", contains = "Split",
         representation(cut_fun = "function"))
                        
VarDynCutSplit = function(var, splbl, cutfun, cfun = NULL, cfmt = NULL, splfmt = NULL, splname = var) {
    new("VarDynCutSplit", payload = var,
        split_label = splbl,
        cut_fun = cutfun,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt,
        name = splname)
}

setClass("AnalyzeVarSplit", contains = "Split",
         representation(analysis_fun = "function",
                        default_rowlabel = "character",
                        include_NAs = "logical"))


AnalyzeVarSplit = function(var,
                           splbl,
                           afun,
                           defrowlab = "",
                           cfun = NULL,
                           cfmt = NULL,
                           splfmt = NULL,
                           inclNAs = FALSE,
                           splname = var) {
    if(!any(nzchar(defrowlab)))
        defrowlab = as.character(substitute(afun))
    new("AnalyzeVarSplit",
        payload = var,
        split_label = splbl,
        content_fun = cfun,
        analysis_fun = afun,
        content_format = cfmt,
        split_format = splfmt,
        default_rowlabel = defrowlab,
        include_NAs = inclNAs,
        name = splname)
}

setClass("CompoundSplit", contains = "Split",
         validity = function(object) are(object@payload, "Split"))


setClass("AnalyzeMultiVars", contains = "CompoundSplit")
.repoutlst = function(x, nv) {
    if(!is.function(x) && length(x) == nv)
        return(x)
    if(!is(x, "list"))
        x = list(x)
    rep(x, length.out = nv)
}

AnalyzeMultiVars = function(var, splbl ="", afun, defrowlab = "", cfun = NULL, cfmt = NULL, splfmt = NULL, inclNAs = FALSE,
                            .payload = NULL,
                            splname = NULL) {
    if(is.null(.payload)) {
        nv = length(var)
        defrowlab = .repoutlst(defrowlab, nv)
        afun = .repoutlst(afun, nv)
        splbl = .repoutlst(splbl, nv)
        cfun = .repoutlst(cfun, nv)
        cfmt = .repoutlst(cfmt, nv)
        splfmt = .repoutlst(splfmt, nv)
        inclNAs = .repoutlst(inclNAs, nv)
        pld = mapply(AnalyzeVarSplit,
                     var = var,
                     splbl =splbl,
                     afun = afun,
                     defrowlab = defrowlab,
                     cfun = cfun,
                     cfmt = cfmt,
                     splfmt = splfmt,
                     inclNAs = inclNAs,
                     SIMPLIFY = FALSE)
    } else {
        .payload = unlist(lapply(.payload,
                                 function(x) {
            if(is(x, "CompoundSplit")) {
                spl_payload(x)
            } else {
                x
            }
        }))
        pld = .payload
    }
     if(length(pld) == 1) {
            return(pld[[1]])
     } else {
         if(is.null(splname))
             splname = paste(.payload, collapse = ":")
         
         new("AnalyzeMultiVars",
             payload = pld,
             split_label = "",
             split_format = NULL,
             content_fun = NULL,
             content_format = NULL)
     }
}

setClass("VAnalyzeVarComp", contains = c("VIRTUAL", "AnalyzeVarSplit"),
         representation(comparison_fun = "function"))

## this is just a sentinel class so we hit different methods
setClass("AVarBaselineComp", contains = "VAnalyzeVarComp")

AVarBaselineComp = function(var,
                            splbl,
                            afun,
                            compfun = `-`,
                            valuelblvar = NULL,
                            cfun = NULL,
                            cfmt = NULL,
                            splfun = NULL,
                            splfmt = NULL,
                            valorder = NULL,
                            splname = var) {
    if(is.null(valuelblvar))
        valuelblvar = var
    new("AVarBaselineComp", payload = var, split_label = splbl,
        value_lbl_var = valuelblvar,
        content_fun = cfun,
        content_format = cfmt,
        split_fun = splfun,
        split_format = splfmt,
        value_order = valorder,
        comparison_fun = compfun,
        name = splname
        )
}




## A comparison split is one where the analysis value (e.g., mean)
## will neeed to be calculated on two different subsets and further
## computed on (by calling comparison_func on them, default is just
## `-` meaning subtraction) to get the final single cell value
##
## Note that traditional, side-by-side displays such as
##
##      A          B
##  BEP  ALL   BEP   ALL
##
## are NOT comparisons, each of the individual cell values here
## is derived from applying the analysis function to only a
## single subset.
##
##   A Table that IS an actual comparison split is
##
##           A                                    B
## Baseline    (Followup - Baseline)    Baseline     (Followup - Baseline)
##


setClass("VCompSplit", contains = c("VIRTUAL", "Split"),
         representation(comparison_func = "function",
                        label_format = "character"))
## This is what will bee used to generate the parallel
## lists of subsets to be pair/element-wise compared
## function should take the data.frame and return a
## list of subsets,
## character should name a categorical column in
## the dataframe that it should be split on
setClassUnion("VarOrFactory", c("function", "character"))

## this contains "AllSplit" because wee need all
## of the data to be accessible  from all of the children
## so that they can perform the comparison

setClass("MultiSubsetCompSplit", contains = "VCompSplit",
         representation(subset1_gen = "VarOrFactory",
                        subset2_gen = "VarOrFactory"))

MultiSubsetCompSplit = function(factory1, factory2, lbl_fmt = "%s - %s",
                           splbl = "",
                           comparison = `-`,
                           ## not needed i think...
                           cfun = NULL, cfmt = NULL, splfmt = NULL) {
    new("MultiSubsetCompSplit",
        subset1_gen = factory1,
        subset2_gen = factory2,
        label_format = lbl_fmt,
        split_label = splbl,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt,
        comparison_func = comparison)
}

setClass("SubsetSplit", contains = "Split",
         representation(subset_gen = "VarOrFactory",
                        vs_all = "logical",
                        vs_non = "logical",
                        child_order = "character"),
         validity = function(object) object@vs_all || object@vs_non,
         prototype = prototype(vs_all = TRUE,
                               vs_non = FALSE))

SubsetSplit = function(subset, vall = TRUE, vnon = FALSE,
                       order = c("subset", if(vnon) "non", if(vall) "all"),
                       splbl,
                       cfun = NULL, cfmt = NULL, splfmt = NULL) {
    new("SubsetSplit",
        split_label = splbl,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt,
        subset_gen = subset,
        vs_all = vall,
        vs_non = vnon,
        child_order = order)
}

### This is HARD. do we want this to inherit from VCompSplit
### or VarLevelSplit????
###
### Is it safe to have it inherit from both? I think no?


setClass("VarLevWBaselineSplit", contains = "VarLevelSplit",
         representation(var = "character",
                        baseline_value = "character",
                        incl_allcategory = "logical"))

VarLevWBaselineSplit = function(var, baseline, valuelblvar= var, incl_all = FALSE,
                                splbl,
#                             comparison = `-`,
                             lbl_fmt = "%s - %s",
                             ## not needed I Think...
                             cfun =  NULL, cfmt = NULL, splfmt = NULL,
                             valorder = NULL,
                             splname = var) {
    new("VarLevWBaselineSplit",
        payload = var,
        baseline_value = baseline,
        incl_allcategory = incl_all,
        ## This will occur ata theee row level not on the column split, for now
        ## TODO revisit this to confirm its right
        ##        comparison_func = comparison,
  #      label_format = lbl_fmt,
        split_label = splbl,
        content_fun = cfun,
        content_format = cfmt,
        split_format = splfmt,
        name = splname)
}
                        


setClass("CompSubsetVectors",
         representation(subset1 = "list",
                        subset2 = "list"),
         validity = function(object) length(object@subset1) == length(object@subset2))










        
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
    svals = make_splvalue_vec(vals = svals)
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


## setClass("TableTreePos", contains = "TreePos",
##          representation(is_content="logical"),
##          prototype = list(is_content = FALSE))

## TableTreePos = function(iscontent = FALSE, ...) {
##     tp = TreePos(...)
##     new("TableTreePos", tp, is_content = iscontent)
## }

## setClass("TableRowPos", contains = "TableTreePos",
##          representation(local_rownum = "numeric",
##                         is_label_row = "logical"),
##          prototype = list(is_content = FALSE,
##                           is_label_row = FALSE))


## TableRowPos = function(localrow = NA_real_, iscontent = FALSE, islabel = FALSE,...) {
##     ttpos = TableTreePos(iscontent, ...)
##     new("TableRowPos", ttpos, local_rownum = localrow, is_label_row = islabel)
## }


##
## Tree position convenience functions
##

make_child_pos = function(parpos, newspl, newval, newlab = newval, newextra = list()) {
    if(!is(newval, "SplitValue"))
        nsplitval = SplitValue(newval, newextra)
    else
        nsplitval = newval
    
    newpos = TreePos(
        spls = c(pos_splits(parpos), newspl),
        svals = c(pos_splvals(parpos), nsplitval),
        svlbls = c(pos_splval_lbls(parpos), newlab),
        sub = .combine_subset_exprs(pos_subset(parpos),
                                    make_subset_expr(newspl, nsplitval)))
}

## make_tablepos= function(treepos, iscontent) {
##     ttpos = new("TableTreePos", treepos,
##                 is_content = iscontent)
## }


## make_rowpos = function(tp, rownum) {
##     new("TableRowPos", tp,
##         local_rownum = rownum)
## }





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
                        name = "character"##,
                        ##label = "character"
                        ))
setClass("VTree", contains = c("VIRTUAL", "VNodeInfo"), representation(children = "list"))
setClass("VLeaf", contains = c("VIRTUAL", "VNodeInfo"))


## Layout trees
#setClass("VLayoutNode", contains= c("VIRTUAL", "VNodeInfo"))
setClass("VLayoutLeaf", contains = c("VIRTUAL", "VLeaf"),
         representation(pos_in_tree = "TreePos",
                        label = "character"))
setClass("VLayoutTree", contains = c("VIRTUAL", "VTree"),
         representation(split = "Split",
                        pos_in_tree = "TreePos",
                        label = "character"))
setClassUnion("VLayoutNode", c("VLayoutLeaf", "VLayoutTree"))




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


setClass("LayoutColTree", contains = "LayoutAxisTree",
         representation(display_columncounts = "logical",
                        columncount_format = "character"))

setClass("LayoutColLeaf", contains = "LayoutAxisLeaf")
setClass("LayoutRowTree", contains = "LayoutAxisTree")
setClass("LayoutRowLeaf", contains = "LayoutAxisLeaf")
LayoutColTree = function(lev = 0L,
                         lab = "",
                         kids = list(),
                         spl = if(length(kids) == 0) NULL else AllSplit(), 
                         tpos = TreePos(), 
                         summary_function = NULL,
                         vis_colcounts = FALSE,
                         colcount_fmt = "(N=xx)"){## ,
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
            label = lab,
            display_columncounts = vis_colcounts,
            columncount_format = colcount_fmt)
    } else {
        LayoutColLeaf(lev = lev, lab = lab, tpos = tpos)
    }        
}

LayoutColLeaf = function(lev = 0L, lab = "", tpos = TreePos()#,
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


## Instantiated column info class
##
## This is so we don't need multiple arguments
## in the recursive functions that track
## various aspects of the column layout
## once its applied to the data.

setClass("InstantiatedColumnInfo",
         representation(tree_layout = "VLayoutNode", ##LayoutColTree",
                        subset_exprs = "list",
                        cextra_args = "list",
                        counts = "integer",
                        display_columncounts = "logical",
                        columncount_format = "FormatSpec"),
         validity = function(object) {
    nleaves = length(collect_leaves(object@tree_layout))
    counts = object@counts
    length(object@subset_exprs) == nleaves &&
        length(object@cextra_args) == nleaves &&
        length(counts) == nleaves &&
        (all(is.na(counts)) || all(!is.na(counts) & counts > 0))
})

InstantiatedColumnInfo = function(treelyt = LayoutColTree(),
                                  csubs = list(expression(TRUE)),
                                  extras = list(list()),
                                  cnts = NA_integer_,
                                  dispcounts = FALSE,
                                  countfmt = "(N=xx)") {
    leaves = collect_leaves(treelyt)
    nl = length(leaves)
    extras = rep(extras, length.out = nl)
    cnts = rep(cnts, length.out = nl)
    csubs = rep(csubs, length.out = nl)
    
    new("InstantiatedColumnInfo",
        tree_layout = treelyt,
        subset_exprs = csubs,
        cextra_args = extras,
        counts = cnts,
        display_columncounts = dispcounts,
        columncount_format = countfmt)
}





## TableTrees
## XXX Rowspans as implemented dont really work
## they're aren't attached to the right data structures
## during conversions.

## FIXME: if we ever actually need row spanning
setClass("VTableNodeInfo", contains = c("VNodeInfo", "VIRTUAL"),
         representation(
             ##col_layout = "VLayoutNode",
             col_info = "InstantiatedColumnInfo",
             format = "FormatSpec"))

setClass("VTableTree", contains = c("VIRTUAL", "VTableNodeInfo", "VTree"),
         representation(children = "list",
                        rowspans = "data.frame",
                        labelrow = "LabelRow"
                        ))
setClass("TableRow", contains = c("VIRTUAL", "VLeaf", "VTableNodeInfo"), 
         representation(leaf_value = "ANY",
                        var_analyzed = "character",
               ##         var_label = "character",
                        label = "character"))




### TableTree Core non-virtual Classes

              
setClass("DataRow", contains = "TableRow",
         representation(colspans = "integer"),
                        ##pos_in_tree = "TableRowPos"),
         validity = function(object) {
    lcsp = length(object@colspans)
    length(lcsp ==  0) || lcsp == length(object@leaf_value)
})

setClass("ContentRow", contains = "TableRow",
         representation(colspans = "integer"),
                        ##pos_in_tree = "TableRowPos"),
         validity = function(object) {
    lcsp = length(object@colspans)
    length(lcsp ==  0) || lcsp == length(object@leaf_value)
})

setClass("LabelRow", contains = "TableRow",
         representation(visible = "logical"),
         validity = function(object) {
    identical(object@leaf_value, list()) &&
        (length(object@var_analyzed) == 0 || is.na(object@var_analyzed) || nchar(object@var_analyzed) == 0)
})


LabelRow = function(lev = 1L,
                      lab = "",
                      vis = nzchar(lab),
                      cinfo = InstantiatedColumnInfo()) {
    new("LabelRow",
        leaf_value = list(),
        level = lev,
        label = lab,
        col_info = cinfo,
        visible = vis)
}
                      

.tablerow = function(val = list(),
                    name = "",
                    lev = 1L,
                    lab = name,
                    cspan = rep(1L, length(val)),
                    ##clayout = LayoutColTree(),
                    cinfo = InstantiatedColumnInfo(),
                    tpos = TableRowPos(),
                    var = NA_character_,
 ##                   var_lbl = NA_character_,
 ##                   v_type = NA_character_,
                    fmt = NULL,
                    klass) {
    if((missing(name) || nchar(name) == 0) &&
       !missing(lab))
        name = lab
    rw = new(klass, leaf_value = val,
             name = name,        level = lev,
        label = lab,
        colspans = cspan,
        col_info = cinfo,
        ##  col_layout = clayout,
        ##     pos_in_tree = tpos,
        var_analyzed = var,
##        var_label = var_lbl,
##        value_type = v_type,
        format = NULL)
    rw = set_fmt_recursive(rw, fmt, FALSE)
    rw
}

DataRow = function(...) .tablerow(..., klass = "DataRow")
ContentRow = function(...) .tablerow(..., klass = "ContentRow")


setClassUnion("IntegerOrNull", c("integer", "NULL"))
setClass("ElementaryTable", contains = "VTableTree",
         representation(var_analyzed = "character"),
                        ##var_label = "character"),
         validity = function(object) {
    kids = tree_children(object)
    all(sapply(kids,
               function(k) {
                   (is(k, "DataRow") || is(k, "ContentRow")) &&
                       identical(k@col_info, object@col_info)
    }))
})

.enforce_valid_kids = function(lst, colinfo) {
    ## colinfo
    if(!no_colinfo(colinfo)) {
        lst = lapply(lst,
                 function(x) {
            if(no_colinfo(x))
                col_info(x) = colinfo
            else if(!identical(colinfo, col_info(x)))
                stop("attempted to add child with non-matching, non-empty column info to an existing table")
            x
        })
    }

    ## names
    realnames = sapply(lst, obj_name)
    lstnames = names(lst)
    if(is.null(lstnames)) {
        names(lst) = realnames
    } else if(!identical(realnames, lstnames)) {
        warning("non-null names of child list didn't match names of child objects. overriding list names")
        names(lst) = realnames
    }
    lst

}
            
ElementaryTable = function(kids = list(),
                           name = "",
                           lev = 1L,
                           lab = "",
                           labrow = LabelRow(lev = lev,
                                             lab = lab,
                                             vis = !isTRUE(iscontent)),
                           rspans = data.frame(),
                           cinfo = NULL,
                           tpos = TableTreePos(),
                           iscontent = NA,
                           var = NA_character_,
##                           var_lbl = var,
                           fmt = NULL) {
    if(is.null(cinfo)) {
        if(length(kids) > 0)
            cinfo = col_info(kids[[1]])
        else
            cinfo = InstantiatedColumnInfo()
    }

    if(no_colinfo(labrow))
        col_info(labrow) = cinfo
    kids = .enforce_valid_kids(kids, cinfo)
    tab = new("ElementaryTable",
              children = kids,
              name = name,
              level = lev,
              labelrow = labrow,
              rowspans = rspans,
              col_info = cinfo,
              var_analyzed = var,
##              var_label = var_lbl,
              format = NULL)
    tab = set_fmt_recursive(tab, fmt, FALSE)
    tab
}

## under this model, non-leaf nodes can have a content table where rollup
## analyses live 

setClass("TableTree", contains = c("VTableTree"),
         representation(content = "ElementaryTable"##,
                     ##   split = "Split"
                        ),
         validity = function(object) {
    all(sapply(tree_children(object), function(x) is(x, "TableTree") || is(x, "ElementaryTable") || is(x, "TableRow")))
})

TableTree = function(kids = list(),
                     name = if(!is.na(var)) var else "",
                     cont = ElementaryTable(),
                     lev = 1L,
                     lab = name,
                     labrow = LabelRow(lev = lev,
                                       lab = lab,
                                       vis = nrow(cont)== 0),
                     rspans = data.frame(),
                     tpos = TableTreePos(),
                     iscontent = NA,
                     spl = NULL,
                     var = NA_character_,
##                     var_lbl = var,
                     cinfo = NULL,
                     fmt = NULL) {
    if(is.null(cinfo)) {
        if(!is.null(cont)) {
            cinfo = col_info(cont)
        } else if(length(kids) > 0) {
            cinfo = col_info(kids[[1]])
        } else {
            cinfo = InstantiatedColumnInfo()
        }
    } 
    kids = .enforce_valid_kids(kids, cinfo)
    if(iscontent && !is.null(cont) && nrow(cont) > 0)
        stop("Got table tree with content table and content position")
    if(no_colinfo(labrow))
        col_info(labrow) = cinfo
    if((is.null(cont) || nrow(cont) == 0) && all(sapply(kids, is, "DataRow"))) {
        ## constructor takes care of recursive format application
        ElementaryTable(kids = kids,
                        name = name,
                        lev = lev,
                        labrow = labrow,
                        rspans = rspans,
                        cinfo = cinfo,
    ##                    tpos = tpos,
                        var = var,
##                        var_lbl = var_lbl,
                        fmt = fmt)
    } else {
        tab = new("TableTree", content = cont,
                  children = kids,
                  name = name,
                  level = lev,
                  labelrow = labrow,
                  rowspans = rspans,
                  ##         pos_in_tree = tpos,
       ##           split = spl,
                  col_info = cinfo,
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



setClass("PreDataColLayout", contains = "PreDataAxisLayout",
         representation(display_columncounts = "logical",
                        columncount_format = "character"))
setClass("PreDataRowLayout", contains = "PreDataAxisLayout")

PreDataColLayout = function(x = SplitVector(),
                            rtsp = RootSplit(),
                            ...,
                            lst = list(x, ...),
                            disp_colcounts = FALSE,
                            colcount_fmt = "(N=xx)") {
    ret =  new("PreDataColLayout", lst, display_columncounts = disp_colcounts,
               columncount_format = colcount_fmt)
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
        
