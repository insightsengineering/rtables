

## Split types
## -----------
## variable: split on distinct values of a variable
## all: include all observations (root 'split')
## rawcut: cut on static values of a variable
## quantilecut: cut on quantiles of observed values for a variable
## missing: split obs based on missingness of a variable/observation. This could be used for compare to ref_group??
## multicolumn: each child analyzes a different column
## arbitrary: children are not related to eachother in any systematic fashion.

.labelkids_helper = function(charval) {
    ret = switch(charval,
                 "default" = NA,
                 "visible" = TRUE,
                 "hidden" = FALSE,
                 stop("unrecognized charval in .labelkids_helper. this shouldn't ever happen"))
}

setOldClass("expression")
setClassUnion("SubsetDef", c("expression", "logical", "integer", "numeric"))



setClassUnion("integerOrNULL", c("NULL", "integer"))
setClassUnion("characterOrNULL", c("NULL", "character"))





## should XXX [splits, s_values, sval_labels, subset(?)] be a data.frame?
setClass("TreePos", representation(splits = "list",
                                   s_values = "list",
                                   sval_labels = "character",
                                   subset = "SubsetDef"),
         validity = function(object) {
    nspl = length(object@splits)
    length(object@s_values) == nspl && length(object@sval_labels) == nspl
})



setClassUnion("functionOrNULL", c("NULL", "function"))
## TODO (?) make "list" more specific, e.g FormatList, or FunctionList?
setClassUnion("FormatSpec",c("NULL", "character", "function", "list"))

setClass("ValueWrapper", representation(value = "ANY",
                                        label = "characterOrNULL"),
         contains = "VIRTUAL")
## heavier-weight than I'd like but I think we need
## this to carry around thee subsets for
## comparison-based splits

setClass("SplitValue",
         contains = "ValueWrapper",
         representation(extra = "list"))

SplitValue = function(val, extr =list(), label = val) {
    if(is(val, "SplitValue")) {
        if(length(extr) >0 && !identical(extr, splv_extra(val)))
            stop("SplitValue  object passed to SplitValue constructor along with non-identical extra")
        return(val)
    }
    if(!is(extr, "list"))
        extr <- list(extr)
    if(!is(label, "character"))
        label = as.character(label)

    new("SplitValue", value = val,
        extra = extr, label = label)
}

setClass("LevelComboSplitValue",
         contains = "SplitValue",
         representation(comboname = "character"))

LevelComboSplitValue = function(val, extr, comboname) {
    new("LevelComboSplitValue",
        value = val,
        extra = extr,
        comboname = comboname)
}


setClass("Split", contains = "VIRTUAL",
         representation(
             payload = "ANY",
             name = "character",
             split_label = "character",
             split_format = "FormatSpec",
             split_label_visible = "logical",
             ## NB this is the function which is applied to
             ## get the content rows for the CHILDREN of this
             ## split!!!
             content_fun = "functionOrNULL",
             content_format = "FormatSpec",
             content_var = "character",
             label_children = "logical",
             extra_args = "list",
             indent_modifier = "integer",
             content_indent_modifier = "integer"))


setClass("CustomizableSplit", contains = "Split",
         representation( split_fun = "functionOrNULL"))

#' @exportClass VarLevelSplit
#' @rdname VarLevelSplit
#' @author Gabriel Becker
setClass("VarLevelSplit", contains = "CustomizableSplit",
         representation(value_label_var = "character",
                        value_order = "ANY"))
#' Split on levels within a variable
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @export
VarLevelSplit = function(var,
                         split_label,
                         labels_var = NULL,
                         cfun = NULL,
                         cformat = NULL,
                         split_fun = NULL,
                         split_format = NULL,
                         valorder = NULL,
                         split_name = var,
                         child_labels = c("default", "visible", "hidden"),
                         extra_args = list(),
                         indent_mod = 0L,
                         visible_label = FALSE,
                         cindent_mod = 0L,
                         cvar = ""
                         ) {
    child_labels = match.arg(child_labels)
    if(is.null(labels_var))
        labels_var = var
    new("VarLevelSplit", payload = var,
        split_label = split_label,
        name = split_name,
        value_label_var = labels_var,
        content_fun = cfun,
        content_format = cformat,
        split_fun = split_fun,
        split_format = split_format,
        value_order = NULL,
        label_children = .labelkids_helper(child_labels),
        extra_args = extra_args,
        indent_modifier = as.integer(indent_mod),
        content_indent_modifier = as.integer(cindent_mod),
        content_var = cvar,
        split_label_visible = visible_label
        )
}
setClass("NULLSplit", contains = "Split")

NULLSplit = function(...) {
    ## only(!!) valid instantiation of NULLSplit class
    new("NULLSplit", payload = character(),
        split_label = character(),
        content_fun = NULL,
        content_format= NULL,
        split_format = NULL,
        name  = "",
        indent_modifier = 0L,
        content_indent_modifier = 0L,
        content_var = "",
        visible_label = FALSE)
}

setClass("AllSplit", contains = "Split")
## ,
##          validity = function(object) length(object@payload) == 0
##          )

AllSplit = function(split_label = "",
                    cfun = NULL,
                    cformat = NULL,
                    split_format = NULL,
                    split_name = if(!missing(split_label) && nzchar(split_label)) split_label else "all obs",
                    extra_args = list(),
                    indent_mod = 0L,
                    cindent_mod = 0L,
                    cvar = "",
                    ...) {
    new("AllSplit", split_label = split_label,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
        name = split_name,
        label_children = FALSE,
        extra_args = extra_args,
        indent_modifier = as.integer(indent_mod),
        content_indent_modifier = as.integer(cindent_mod),
        content_var = cvar,
        split_label_visible = FALSE)
}

setClass("RootSplit", contains = "AllSplit")

RootSplit = function(split_label = "", cfun = NULL, cformat = NULL, cvar = "", split_format= NULL, ...) {
    new("RootSplit", split_label = split_label,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
        name = "root",
        label_children = FALSE,
        indent_modifier = 0L,
        content_indent_modifier = 0L,
        content_var = cvar,
        split_label_visible = FALSE)
}

setClass("ManualSplit", contains = "AllSplit",
         representation(levels = "character"))

#' Manually defined split
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @inheritParams gen_args
#' @param levels character. Levels of the split (ie the children of the manual split)
#' @author Gabriel Becker
#' @export
ManualSplit = function(levels, label, name = "manual",
                       extra_args = list(),
                       indent_mod = 0L,
                       cindent_mod = 0L,
                       cvar = "",
                       visible_label = FALSE) {
    new("ManualSplit",
        split_label = label,
        levels = levels,
        name = name,
        label_children = FALSE,
        extra_args = extra_args,
        indent_modifier = 0L,
        content_indent_modifier = as.integer(cindent_mod),
        content_var = cvar,
        split_label_visible = visible_label)

}


## splits across which variables are being analynzed
setClass("MultiVarSplit", contains = "Split",
         representation(var_labels = "character"),
         validity = function(object) {
    length(object@payload) > 1 &&
        all(!is.na(object@payload)) &&
        (length(object@var_labels) == 0 || length(object@payload) == length(object@var_labels))
})

#' Split between two or more different variables
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @author Gabriel Becker
#' @export
MultiVarSplit = function(vars,
                         split_label = "",
                         varlabels = NULL,
                         cfun = NULL,
                         cformat = NULL,
                         split_format = NULL,
                         split_name = paste(vars, collapse = ":"),
                         child_labels = c("default", "visible", "hidden"),
                         extra_args = list(),
                         indent_mod = 0L,
                         cindent_mod = 0L,
                         cvar = "",
                         visible_label = FALSE) {
    child_labels = match.arg(child_labels)
    if(length(vars) == 1 && grepl(":", vars))
        vars = strsplit(vars, ":")[[1]]
    if(length(varlabels) == 0) ## covers NULL and character()
       varlabels = vars
    new("MultiVarSplit", payload = vars,
        split_label = split_label,
        var_labels = varlabels,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
        label_children = .labelkids_helper(child_labels),
        extra_args = extra_args,
        indent_modifier = as.integer(indent_mod),
        content_indent_modifier = as.integer(cindent_mod),
        content_var = cvar,
        split_label_visible = visible_label)
}


#' Splits for cutting by values of a numeric variable
#' @rdname cutsplits
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @exportClass VarStaticCutSplit
setClass("VarStaticCutSplit", contains = "Split",
         representation(cuts = "numeric",
                        cut_labels = "character"))
#' @rdname cutsplits
#' @export
VarStaticCutSplit = function(var,
                             split_label = var,
                             cuts,
                             cutlabels = NULL,
                             cfun = NULL,
                             cformat = NULL,
                             split_format = NULL,
                             split_name = var,
                             child_labels = c("default", "visible", "hidden"),
                             extra_args = list(),
                             indent_mod = 0L,
                             cindent_mod = 0L,
                             cvar = "",
                             visible_label = FALSE) {
    child_labels = match.arg(child_labels)
    if(is.list(cuts) && is.numeric(cuts[[1]]) &&
       is.character(cuts[[2]]) &&
       length(cuts[[1]]) == length(cuts[[2]])) {
        cutlabels = cuts[[2]]
        cuts = cuts[[1]]
    }
    if(is.unsorted(cuts, strictly = TRUE))
        stop("invalid cuts vector. not sorted unique values.")

    if(is.null(cutlabels) && !is.null(names(cuts)))
        cutlabels = names(cuts)[-1] ## XXX is this always right?
    new("VarStaticCutSplit", payload = var,
        split_label = split_label,
        cuts = cuts,
        cut_labels = cutlabels,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
        name = split_name,
        label_children = .labelkids_helper(child_labels),
        extra_args = extra_args,
        indent_modifier = as.integer(indent_mod),
        content_indent_modifier = as.integer(cindent_mod),
        content_var = cvar,
        split_label_visible = visible_label)
}



#' @rdname cutsplits
#' @exportClass CumulativeCutSplit

setClass("CumulativeCutSplit", contains = "VarStaticCutSplit")

#' @rdname cutsplits
#' @export
CumulativeCutSplit = function(var,
                             split_label,
                             cuts,
                             cutlabels = NULL,
                             cfun = NULL,
                             cformat = NULL,
                             split_format = NULL,
                             split_name = var,
                             child_labels = c("default", "visible", "hidden"),
                             extra_args = list(),
                             indent_mod = 0L,
                             cindent_mod = 0L,
                             cvar = "",
                             visible_label = FALSE) {
    child_labels = match.arg(child_labels)
    if(is.list(cuts) && is.numeric(cuts[[1]]) &&
       is.character(cuts[[2]]) &&
       length(cuts[[1]]) == length(cuts[[2]])) {
        cutlabels = cuts[[2]]
        cuts = cuts[[1]]
    }
    if(is.unsorted(cuts, strictly = TRUE))
        stop("invalid cuts vector. not sorted unique values.")

    if(is.null(cutlabels) && !is.null(names(cuts)))
        cutlabels = names(cuts)[-1] ## XXX is this always right?
    new("CumulativeCutSplit", payload = var,
        split_label = split_label,
        cuts = cuts,
        cut_labels = cutlabels,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
        name = split_name,
        label_children = .labelkids_helper(child_labels),
        extra_args = extra_args,
        indent_modifier = as.integer(indent_mod),
        content_indent_modifier = as.integer(cindent_mod),
        content_var = cvar,
        split_label_visible = visible_label)
}




## do we want this to be a CustomizableSplit instead of
## taking cut_fun?
## cut_funct must take avector and no other arguments
## and return a named vector of cut points
#' @rdname cutsplits
#' @exportClass VarDynCutSplit
setClass("VarDynCutSplit", contains = "Split",
         representation(cut_fun = "function",
                        cut_label_fun = "function",
                        cumulative_cuts = "logical"))
#' @rdname cutsplits
#' @export
VarDynCutSplit = function(var,
                          split_label,
                          cutfun,
                          cutlabelfun = function(x) NULL,
                          cfun = NULL,
                          cformat = NULL,
                          split_format = NULL,
                          split_name = var,
                          child_labels = c("default", "visible", "hidden"),
                          extra_args = list(),
                          cumulative = FALSE,
                          indent_mod = 0L,
                          cindent_mod = 0L,
                          cvar = "",
                          visible_label = FALSE) {
    child_labels = match.arg(child_labels)
    new("VarDynCutSplit", payload = var,
        split_label = split_label,
        cut_fun = cutfun,
        cumulative_cuts = cumulative,
        cut_label_fun = cutlabelfun,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
        name = split_name,
        label_children = .labelkids_helper(child_labels),
        extra_args = extra_args,
        indent_modifier = as.integer(indent_mod),
        content_indent_modifier = as.integer(cindent_mod),
        content_var = cvar,
        split_label_visible = visible_label)
}


setClass("VAnalyzeSplit", contains = "Split",
         representation(default_rowlabel = "character",
                        include_NAs = "logical",
                        var_label_visible = "logical"))

setClass("AnalyzeVarSplit", contains = "VAnalyzeSplit",
         representation(analysis_fun = "function"))

setClass("AnalyzeColVarSplit", contains = "VAnalyzeSplit",
         representation(analysis_fun = "list"))

#' Define a subset tabulation/analysis
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @param defrowlab character. Default row labels if they are not specified by the return value of \code{afun}
#' @rdname avarspl
#' @author Gabriel Becker
#' @export
AnalyzeVarSplit = function(var,
                           split_label = var,
                           afun,
                           defrowlab = "",
                           cfun = NULL,
                           cformat = NULL,
                           split_format = NULL,
                           inclNAs = FALSE,
                           split_name = var,
                           extra_args = list(),
                           indent_mod = 0L,
                           visible_label = NA,
                           cvar = ""
                           ) {
    if(!any(nzchar(defrowlab))) {
        defrowlab = as.character(substitute(afun))
        if(length(defrowlab) > 1 || startsWith(defrowlab, "function("))
            defrowlab = ""
    }
    new("AnalyzeVarSplit",
        payload = var,
        split_label = split_label,
        content_fun = cfun,
        analysis_fun = afun,
        content_format = cformat,
        split_format = split_format,
        default_rowlabel = defrowlab,
        include_NAs = inclNAs,
        name = split_name,
        label_children = FALSE,
        extra_args = extra_args,
        indent_modifier = as.integer(indent_mod),
        content_indent_modifier = 0L,
        var_label_visible = visible_label,
        content_var = cvar)
}

#' Define a subset tabulation/analysis
#'
#' @inheritParams lyt_args
#' @inheritParams constr_args
#' @rdname avarspl
#' @author Gabriel Becker
#' @export
AnalyzeColVarSplit = function(afun,
                              defrowlab = "",
                              cfun = NULL,
                              cformat = NULL,
                              split_format = NULL,
                              inclNAs = FALSE,
                              split_name = "",
                              extra_args = list(),
                              indent_mod = 0L,
                              visible_label = NA,
                              cvar = "") {
    ## if(is.function(afun)) {
    ##     if(!any(nzchar(defrowlab))) {
    ##         defrowlab = as.character(substitute(afun))
    ##         if(length(defrowlab) > 1 || startsWith(defrowlab, "function(") || startsWith(defrowlab, "list("))
    ##             defrowlab = ""
    ##     }
    ##     afun = lapply(var, function(x) afun)
    ## }

    new("AnalyzeColVarSplit",
        payload = NA_character_,
        split_label = "",
        content_fun = cfun,
        analysis_fun = afun,
        content_format = cformat,
        split_format = split_format,
        default_rowlabel = defrowlab,
        include_NAs = inclNAs,
        name = split_name,
        label_children = FALSE,
        extra_args = extra_args,
        indent_modifier = as.integer(indent_mod),
        content_indent_modifier = 0L,
        var_label_visible = visible_label,
        content_var = cvar)
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


.uncompound <- function(csplit) {
    if(is(csplit, "list"))
        return(unlist(lapply(csplit, .uncompound)))

    if(!is(csplit, "CompoundSplit"))
        return(csplit)

    pld = spl_payload(csplit)
    done = all(!vapply(pld, is, TRUE, class2 = "CompoundSplit"))
    if(done)
        pld
    else
        unlist(lapply(pld, .uncompound))
}


#' @rdname avarspl
#' @param .payload Used internally, not intended to be set by end users.
#' @export
AnalyzeMultiVars = function(var,
                            split_label = "",
                            afun,
                            defrowlab = "",
                            cfun = NULL,
                            cformat = NULL,
                            split_format = NULL,
                            inclNAs = FALSE,
                            .payload = NULL,
                            split_name = NULL,
                            extra_args = list(),
                            indent_mod = 0L,
                            child_labels = c("default", "visible", "hidden"),
                            cvar = ""
                            ) {
    ## NB we used to resolve to strict TRUE/FALSE for label visibillity
    ## in this function but that was too greedy for repeated
    ## analyze calls, so that now occurs in the tabulation machinery
    ## when the table is actually being built.
    show_kidlabs = .labelkids_helper(match.arg(child_labels))
    if(is.null(.payload)) {
        nv = length(var)
        defrowlab = .repoutlst(defrowlab, nv)
        afun = .repoutlst(afun, nv)
        split_label = .repoutlst(split_label, nv)
        cfun = .repoutlst(cfun, nv)
        cformat = .repoutlst(cformat, nv)
##        split_format = .repoutlst(split_format, nv)
        inclNAs = .repoutlst(inclNAs, nv)
        pld = mapply(AnalyzeVarSplit,
                     var = var,
                     split_label =split_label,
                     afun = afun,
                     defrowlab = defrowlab,
                     cfun = cfun,
                     cformat = cformat,
##                     split_format = split_format,
                     inclNAs = inclNAs,
                     MoreArgs = list(extra_args = extra_args,
                                     indent_mod = indent_mod,
                                     visible_label = show_kidlabs,
                                     split_format = split_format
                                     ),##rvis),
                     SIMPLIFY = FALSE)
    } else {
        ## we're combining existing splits here
        pld = unlist(lapply(.payload, .uncompound))

        ## only override the childen being combined if the constructor
        ## was passed a non-default value for child_labels
        ## and the child was at NA before
        pld = lapply(pld,
                     function(x) {
            rvis = labelrow_visible(x)
            if(!is.na(show_kidlabs)) {
                if(is.na(rvis))
                    rvis = show_kidlabs
            }
            labelrow_visible(x) = rvis
            x
        })
    }
    if(length(pld) == 1) {
        ret = pld[[1]]
     } else {
         if(is.null(split_name))
             split_name = paste(.payload, collapse = ":")
         ## pld = lapply(pld,
         ##              function(pldspl) {
         ##     labelrow_visible(pldspl) <- !identical(show_kidlabs, FALSE)
         ##     pldspl
         ## })

         ret = new("AnalyzeMultiVars",
                   payload = pld,
                   split_label = "",
                   split_format = NULL,
                   content_fun = NULL,
                   content_format = NULL,
                   ## I beleive this is superfluous now
                   ## the payloads carry aroudn the real instructions
                   ## XXX
                   label_children = show_kidlabs,
                   extra_args = extra_args,
                   ## XXX is this right? modifier applied ton
                   ## splits in payload
                   indent_modifier = 0L,
                   content_indent_modifier = 0L,
                   content_var = cvar)
     }
    ret
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

MultiSubsetCompSplit = function(factory1, factory2, label_fstr = "%s - %s",
                           split_label = "",
                           comparison = `-`,
                           ## not needed i think...
                           cfun = NULL, cformat = NULL, split_format = NULL) {
    new("MultiSubsetCompSplit",
        subset1_gen = factory1,
        subset2_gen = factory2,
        label_format = label_fstr,
        split_label = split_label,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
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
                       split_label,
                       cfun = NULL, cformat = NULL, split_format = NULL) {
    new("SubsetSplit",
        split_label = split_label,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
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
                        ref_group_value = "character",
                        incl_allcategory = "logical"))
#' @rdname VarLevelSplit
#' @export
VarLevWBaselineSplit = function(var, ref_group, labels_var= var, incl_all = FALSE,
                                split_label,
                                split_fun = NULL,
#                             comparison = `-`,
                             label_fstr = "%s - %s",
                             ## not needed I Think...
                             cfun =  NULL, cformat = NULL, cvar = "", split_format = NULL,
                             valorder = NULL,
                             split_name = var,
                             extra_args = list()) {
    new("VarLevWBaselineSplit",
        payload = var,
        ref_group_value = ref_group,
        incl_allcategory = incl_all,
        ## This will occur at the row level not on the column split, for now
        ## TODO revisit this to confirm its right
        ##        comparison_func = comparison,
                                        #      label_format = label_fstr,
        value_label_var = labels_var,
        split_label = split_label,
        content_fun = cfun,
        content_format = cformat,
        split_format = split_format,
        split_fun = split_fun,
        name = split_name,
        label_children = FALSE,
        extra_args = extra_args,
        ## this is always a column split
        indent_modifier = 0L,
        content_indent_modifier = 0L,
        content_var = cvar)
}



setClass("CompSubsetVectors",
         representation(subset1 = "list",
                        subset2 = "list"),
         validity = function(object) length(object@subset1) == length(object@subset2))




.chkname = function(nm) {
    if(is.null(nm))
        nm = ""
    if(length(nm) != 1) {
        stop("name is not of length one")
    } else if( is.na(nm)) {
        warning("Got missing value for name, converting to characters '<NA>'")
        nm = "<NA>"
    }
    nm
}




###
### Tree Position Representation
###
### Class(es) that represent position with in a
### tree as parallel vectors of Split objects and
### values chosen at that split, plus labeling info

TreePos = function(spls = list(), svals = list(), svlabels =  character(), sub = NULL) {
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
        sval_labels = svlabels,
        subset = sub)
}

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
        svlabels = c(pos_splval_labels(parpos), newlab),
        sub = .combine_subset_exprs(pos_subset(parpos),
                                    make_subset_expr(newspl, nsplitval)))
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
                         name = label,
                         label = "",
                         kids = list(),
                         spl = EmptyAllSplit,
                         tpos = TreePos(),
                         summary_function = NULL,
                         disp_colcounts = FALSE,
                         colcount_format = "(N=xx)"){## ,
                         ## sub = expression(TRUE),
                         ## svar = NA_character_,
    ## slab = NA_character_) {
    if(!is.null(spl)) {
        new("LayoutColTree", level = lev, children = kids,
            name = .chkname(name),
            summary_func = summary_function,
            pos_in_tree = tpos,
            split = spl,
            ## subset = sub,
            ## splitvar = svar,
            label = label,
            display_columncounts = disp_colcounts,
            columncount_format = colcount_format)
    } else {
        stop("problema my manitar")
        LayoutColLeaf(lev = lev, label = label, tpos = tpos,
                      name = .chkname(name))
    }
}

LayoutColLeaf = function(lev = 0L, name = label, label = "", tpos = TreePos()#,
                        ## n = NA_integer_,
                         #svar = NA_character_
                         ) {
    new("LayoutColLeaf", level = lev, name = .chkname(name), label = label,
        pos_in_tree = tpos##,
        ##subset = sub#,
        ##N_count = n,
        ##splitvar = svar
        )
}

LayoutRowTree = function(lev = 0L, kids = list()) {
    new("LayoutRowTree", level = lev, children = kids)
}

LayoutRowLeaf = function(lev = 0L, label = "",
                       pos){##, sub, n) {
    new("LayoutRowLeaf", level = lev, label = label,
        pos_in_tree = pos)##subset = sub, N_count = n)
}


## Instantiated column info class
##
## This is so we don't need multiple arguments
## in the recursive functions that track
## various aspects of the column layout
## once its applied to the data.

#' InstantiatedColumnInfo
#' @exportClass InstantiatedColumnInfo
#' @rdname cinfo
setClass("InstantiatedColumnInfo",
         representation(tree_layout = "VLayoutNode", ##LayoutColTree",
                        subset_exprs = "list",
                        cextra_args = "list",
                        counts = "integer",
                        display_columncounts = "logical",
                        columncount_format = "FormatSpec")##,
    ##      validity = function(object) {
    ## nleaves = length(collect_leaves(object@tree_layout))
    ## counts = object@counts
    ## length(object@subset_exprs) == nleaves &&
    ##     length(object@cextra_args) == nleaves &&
    ##     length(counts) == nleaves &&
    ##     (all(is.na(counts)) || all(!is.na(counts)))
    ## }
)

#' @rdname cinfo
#' @export
#' @param treelyt LayoutColTree.
#' @param csubs list. List of subsetting expressions
#' @param extras list. Extra arguments associated with the columns
#' @param cnts integer. Counts.
#' @param dispcounts logical. Should the counts be displayed as header info when the associated table is printed.
#' @param countformat string. Format for the counts if thtey are displayed
InstantiatedColumnInfo = function(treelyt = LayoutColTree(),
                                  csubs = list(expression(TRUE)),
                                  extras = list(list()),
                                  cnts = NA_integer_,
                                  dispcounts = FALSE,
                                  countformat = "(N=xx)") {
    leaves = collect_leaves(treelyt)
    nl = length(leaves)
    extras = rep(extras, length.out = nl)
    cnts = rep(cnts, length.out = nl)
    csubs = rep(csubs, length.out = nl)



    nleaves = length(leaves)
    snas = sum(is.na(cnts))
    if(length(csubs) != nleaves ||
       length(extras) != nleaves ||
       length(cnts) != nleaves ||
       (snas != 0  && snas != nleaves))
        stop("attempted to create invalid InstatiedColumnInfo object. Please contact the maintainer(s).")

    new("InstantiatedColumnInfo",
        tree_layout = treelyt,
        subset_exprs = csubs,
        cextra_args = extras,
        counts = cnts,
        display_columncounts = dispcounts,
        columncount_format = countformat)
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
             format = "FormatSpec",
             indent_modifier = "integer"))

setClass("TableRow", contains = c("VIRTUAL", "VLeaf", "VTableNodeInfo"),
         representation(leaf_value = "ANY",
                        var_analyzed = "character",
               ##         var_label = "character",
                        label = "character"))




### TableTree Core non-virtual Classes
#' Row classes and constructors
#' @rdname rowclasses
#' @inheritParams constr_args
#' @inheritParams lyt_args
#' @param vis logical. Should the row be visible (\code{LabelRow} only).
#' @author Gabriel Becker
#' @export
LabelRow = function(lev = 1L,
                    label = "",
                    name = label,
                    vis = !is.na(label) && nzchar(label),
                    cinfo = EmptyColInfo,
                    indent_mod = 0L) {
    new("LabelRow",
        leaf_value = list(),
        level = lev,
        label = label,
        ## XXX this means that a label row and its talbe can have the same name....that is bad but how bad remains to be seen
        ## XXX
        name = .chkname(name),
        col_info = cinfo,
        visible = vis,
        indent_modifier = as.integer(indent_mod))
}

#' Row constructors and Classes
#' @rdname rowclasses
#' @exportClass DataRow
setClass("DataRow", contains = "TableRow",
         representation(colspans = "integer"),
                        ##pos_in_tree = "TableRowPos"),
         validity = function(object) {
    lcsp = length(object@colspans)
    length(lcsp ==  0) || lcsp == length(object@leaf_value)
})

#' @rdname rowclasses
#' @exportClass ContentRow
setClass("ContentRow", contains = "TableRow",
         representation(colspans = "integer"),
                        ##pos_in_tree = "TableRowPos"),
         validity = function(object) {
    lcsp = length(object@colspans)
    length(lcsp ==  0) || lcsp == length(object@leaf_value)
})

#' @rdname rowclasses
#' @exportClass LabelRow
setClass("LabelRow", contains = "TableRow",
         representation(visible = "logical"),
         validity = function(object) {
    identical(object@leaf_value, list()) &&
        (length(object@var_analyzed) == 0 || is.na(object@var_analyzed) || nchar(object@var_analyzed) == 0)
})



#' @rdname rowclasses
#' @param klass Internal detail.
#'
#' @export
.tablerow = function(vals = list(),
                    name = "",
                    lev = 1L,
                    label = name,
                    cspan = rep(1L, length(vals)),
                    cinfo = EmptyColInfo,
                    var = NA_character_,
                    format = NULL,
                    klass,
                    indent_mod = 0L) {
    if((missing(name) || is.null(name) || is.na(name) ||  nchar(name) == 0) &&
       !missing(label))
        name = label
    vals = lapply(vals, rcell)
    rlabels = unique(unlist(lapply(vals, obj_label)))
    if((missing(label) || is.null(label) || identical(label, "")) &&
       sum(nzchar(rlabels)) == 1)
        label = rlabels[nzchar(rlabels)]

    rw = new(klass,
             leaf_value = vals,
             name = .chkname(name),
             level = lev,
             label = .chkname(label),
             colspans = cspan,
             col_info = cinfo,
             var_analyzed = var,
             format = NULL,
             indent_modifier = indent_mod
             )
    rw = set_format_recursive(rw, format, FALSE)
    rw
}

#' @rdname rowclasses
#' @param \dots passed to shared constructor (\code{.tablerow}).
#' @export
DataRow = function(...) .tablerow(..., klass = "DataRow")
#' @rdname rowclasses
#' @export
ContentRow = function(...) .tablerow(..., klass = "ContentRow")

setClass("VTableTree", contains = c("VIRTUAL", "VTableNodeInfo", "VTree"),
         representation(children = "list",
                        rowspans = "data.frame",
                        labelrow = "LabelRow"
                        ))

setClassUnion("IntegerOrNull", c("integer", "NULL"))
#' TableTree classes
#' @exportClass ElementaryTable
#' @author Gabriel Becker
#' @rdname tabclasses
setClass("ElementaryTable", contains = "VTableTree",
         representation(var_analyzed = "character"),
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

    if(are(lst, "ElementaryTable") &&
       all(sapply(lst, function(tb) {
           nrow(tb) <= 1 && identical(obj_name(tb), "")
       }))) {
        lst = unlist(lapply(lst, function(tb) tree_children(tb)[[1]]))
    }
    if(length(lst) == 0)
        return(list())
    ## names
    realnames = sapply(lst, obj_name)
    lstnames = names(lst)
    if(is.null(lstnames)) {
        names(lst) = realnames
    } else if(!identical(realnames, lstnames)) {
        ##        warning("non-null names of child list didn't match names of child objects. overriding list names")
        names(lst) = realnames
    }

    lst

}

#' Table Constructors and Classes
#' @inheritParams constr_args
#' @inheritParams lyt_args
#' @param rspans data.frame. Currently stored but otherwise ignored.
#' @rdname tabclasses
#' @author Gabriel Becker
#' @export
ElementaryTable = function(kids = list(),
                           name = "",
                           lev = 1L,
                           label = "",
                           labelrow = LabelRow(lev = lev,
                                             label = label,
                                             vis = !isTRUE(iscontent) && !is.na(label) && nzchar(label)),
                           rspans = data.frame(),
                           cinfo = NULL,
                           iscontent = NA,
                           var = NA_character_,
                           format = NULL,
                           indent_mod = 0L) {
    if(is.null(cinfo)) {
        if(length(kids) > 0)
            cinfo = col_info(kids[[1]])
        else
            cinfo = EmptyColInfo
    }

    if(no_colinfo(labelrow))
        col_info(labelrow) = cinfo
    kids = .enforce_valid_kids(kids, cinfo)
    tab = new("ElementaryTable",
              children = kids,
              name = .chkname(name),
              level = lev,
              labelrow = labelrow,
              rowspans = rspans,
              col_info = cinfo,
              var_analyzed = var,
              format = NULL,
              indent_modifier = as.integer(indent_mod))
    tab = set_format_recursive(tab, format, FALSE)
    tab
}

## under this model, non-leaf nodes can have a content table where rollup
## analyses live
#' @rdname tabclasses
#' @exportClass TableTree
setClass("TableTree", contains = c("VTableTree"),
         representation(content = "ElementaryTable"
                        ),
         validity = function(object) {
    all(sapply(tree_children(object), function(x) is(x, "TableTree") || is(x, "ElementaryTable") || is(x, "TableRow")))
})

#' @rdname tabclasses
#' @export
TableTree = function(kids = list(),
                     name = if(!is.na(var)) var else "",
                     cont = EmptyElTable,
                     lev = 1L,
                     label= name,
                     labelrow = LabelRow(lev = lev,
                                       label = label,
                                       vis = nrow(cont)== 0 && !is.na(label) && nzchar(label)),
                     rspans = data.frame(),
                     iscontent = NA,
                     var = NA_character_,
                     cinfo = NULL,
                     format = NULL,
                     indent_mod = 0L) {
    if(is.null(cinfo)) {
        if(!is.null(cont)) {
            cinfo = col_info(cont)
        } else if(length(kids) > 0) {
            cinfo = col_info(kids[[1]])
        } else {
            cinfo = EmptyColInfo
        }
    }

    kids = .enforce_valid_kids(kids, cinfo)
    if(isTRUE(iscontent) && !is.null(cont) && nrow(cont) > 0)
        stop("Got table tree with content table and content position")
    if(no_colinfo(labelrow))
        col_info(labelrow) = cinfo
    if((is.null(cont) || nrow(cont) == 0) && all(sapply(kids, is, "DataRow"))) {
        ## constructor takes care of recursive format application
        ElementaryTable(kids = kids,
                        name = .chkname(name),
                        lev = lev,
                        labelrow = labelrow,
                        rspans = rspans,
                        cinfo = cinfo,
                        var = var,
                        format = format,
                        indent_mod = indent_mod)
    } else {
        tab = new("TableTree", content = cont,
                  children = kids,
                  name = .chkname(name),
                  level = lev,
                  labelrow = labelrow,
                  rowspans = rspans,
                  col_info = cinfo,
                  format = NULL,
                  indent_modifier = as.integer(indent_mod))
        tab = set_format_recursive(tab, format, FALSE)
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
    if(!is(vec, "SplitVector"))
        return(FALSE)
    if(length(vec) == 0)
        return(TRUE)
    isavar = which(sapply(vec, is, "AnalyzeVarSplit"))
    (length(isavar) == 0) || (length(isavar) == 1 && isavar == length(vec))
}



setClass("PreDataAxisLayout", contains = "list",
         representation(root_split = "ANY"),
         validity = function(object) {
    allleafs = unlist(object, recursive = TRUE)
    all(sapply(object, avar_noneorlast)) &&
        all(sapply(allleafs,
                   ## remember existing table trees can be added to layouts
                   ## for now...
                   function(x) is(x, "Split") || is(x, "VTableTree")))
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
                            colcount_format = "(N=xx)") {
    ret =  new("PreDataColLayout", lst, display_columncounts = disp_colcounts,
               columncount_format = colcount_format)
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



## setClass("CellValue", contains = "ValueWrapper",
##          representation(format = "FormatSpec",
##                         colspan = "integerOrNULL",
##                         label = "characterOrNULL"),
##          prototype = list(label ="", colspan = NULL, format = NULL))

setOldClass("CellValue")

#' Length of a Cell value
#'
#' @exportMethod length
#' @param x x.
setMethod("length", "CellValue",
          function(x) 1L)


#' Cell Value constructor
#'
#' @inheritParams lyt_args
#' @param val ANY. value in the cell exactly as it should be passed to a formatter or returned when extracted
#' @param colspan integer. Generally ignored currently.
#' @param label used as row name if the row name is not specified by `in_rows`
#'
#' @export
#'
## CellValue = function(val, format = NULL, colspan = 1L, label = NULL)  {
##     if(is.null(colspan))
##         colspan <- 1L
##     if(!is.null(colspan) && !is(colspan, "integer"))
##         colspan <- as.integer(colspan)
##     new("CellValue", value = val, format = format, colspan = colspan, label = label)
## }


## Class definition
## [[1]] list: cell value
## format : format for cell
## colspan: column span info for cell
## label: row label to be used for parent row
## indent_mod: indent modifier to be used for parent row
CellValue = function(val, format = NULL, colspan = 1L, label = NULL, indent_mod = NULL)  {

    if(is.null(colspan))
        colspan <- 1L
    if(!is.null(colspan) && !is(colspan, "integer"))
        colspan <- as.integer(colspan)
    ## if we're not given a label but the value has one associated with
    ## it we use that.
    ## NB: we need to be able to override a non-empty label with an empty one
    ## so we can't have "" mean "not given a label" here
    if((is.null(label) || is.na(label)) &&
       !is.null(obj_label(val)))
        label <- obj_label(val)
    ret = structure(list(val), format = format, colspan = colspan, label = label,
              indent_mod = indent_mod, class = "CellValue")
}


#' @method print CellValue
print.CellValue <- function(x, ...) {
    cat(paste("rcell:", format_rcell(x), "\n"))
}


## ## Empty default objects to avoid repeated calls
## EmptyColInfo <- InstantiatedColumnInfo()
## EmptyElTable <- ElementaryTable()
## EmptyRootSplit <- RootSplit()
## EmptyAllSplit <- AllSplit()
