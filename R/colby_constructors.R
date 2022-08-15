
label_pos_values <- c("hidden", "visible", "topleft")

#' @name internal_methods
#' @rdname int_methods
NULL

#' combine SplitVector objects
#' @param x SplitVecttor
#' @param ... Splits or SplitVector objects
#' @exportMethod c
#' @return Various, but should be considered implementation details.
#' @rdname int_methods
setMethod("c", "SplitVector", function(x, ...) {
    arglst = list(...)
    stopifnot(all(sapply(arglst, is, "Split")))
    tmp = c(unclass(x), arglst)
    SplitVector(lst = tmp)
})

## split_rows and split_cols are "recursive method stacks" which follow
## the general pattern of accept object -> call add_*_split on slot of object ->
## update object with value returned from slot method, return object.
##
## Thus each of the methods is idempotent, returning an updated object of the
## same class it was passed. The exception for idempotency is the NULL method
## which constructs a PreDataTableLayouts object with the specified split in the
## correct place.

## The cascading (by class) in this case is as follows for the row case:
## PreDataTableLayouts -> PreDataRowLayout -> SplitVector
#' @param cmpnd_fun function. Intended for internal use.
#' @param pos numeric(1). Intended for internal use.
#' @param spl Split. The split.
#' @rdname int_methods
setGeneric("split_rows", function(lyt = NULL, spl, pos,
                                  cmpnd_fun = AnalyzeMultiVars) standardGeneric("split_rows"))
#' @rdname int_methods
setMethod("split_rows", "NULL", function(lyt, spl, pos, cmpnd_fun = AnalyzeMultiVars) {
    .Deprecated(msg = "Initializing layouts via NULL is deprecated, please use basic_table() instead")
    rl = PreDataRowLayout(SplitVector(spl))
    cl = PreDataColLayout()
    PreDataTableLayouts(rlayout = rl, clayout = cl)
})
#' @rdname int_methods
setMethod("split_rows", "PreDataRowLayout",
          function(lyt, spl, pos, cmpnd_fun = AnalyzeMultiVars) {
    stopifnot(pos >0 && pos <= length(lyt) + 1)
    tmp  = if (pos <= length(lyt)) {
               split_rows(lyt[[pos]], spl, pos, cmpnd_fun)
           } else {
               if(pos != 1 && has_force_pag(spl))
                   stop("page_by splits cannot have top-level siblings",
                        call. = FALSE)
               SplitVector(spl)
           }
    lyt[[pos]] = tmp
    lyt
})
is_analysis_spl = function(spl) is(spl, "VAnalyzeSplit") || is(spl, "AnalyzeMultiVars")
## note "pos" is ignored here because it is for which nest-chain
## spl should be placed in, NOIT for where in that chain it should go
#' @rdname int_methods
setMethod("split_rows", "SplitVector",
          function(lyt, spl, pos, cmpnd_fun = AnalyzeMultiVars) {
    ## if(is_analysis_spl(spl) &&
    ##    is_analysis_spl(last_rowsplit(lyt))) {
    ##     return(cmpnd_last_rowsplit(lyt, spl, cmpnd_fun))
    ## }

    if(has_force_pag(spl) && length(lyt) > 0 && !has_force_pag(lyt[[length(lyt)]]))
        stop("page_by splits cannot be nested within non-page_by splits",
             call. = FALSE)
    tmp = c(unclass(lyt), spl)
    SplitVector(lst = tmp)
})

#' @rdname int_methods
setMethod("split_rows", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt <- rlayout(lyt)
    addtl <- FALSE
    split_label <- obj_label(spl)
    if(is(spl, "Split") && ## exclude existing tables that are being tacked in
       identical(label_position(spl), "topleft") &&
       length(split_label) == 1  && nzchar(split_label)) {
        addtl <- TRUE
        label_position(spl) <- "hidden"
    }

    rlyt <- split_rows(rlyt, spl, pos)
    rlayout(lyt) <- rlyt
    if(addtl) {
        lyt <- append_topleft(lyt, indent_string(split_label, .tl_indent(lyt)))
    }
    lyt
})
#' @rdname int_methods
setMethod("split_rows", "ANY",
          function(lyt, spl, pos) stop("nope. can't add a row split to that (", class(lyt), "). contact the maintaner.")
          )

#' @rdname int_methods
#' @param constructor function.
setGeneric("cmpnd_last_rowsplit", function(lyt, spl, constructor) standardGeneric("cmpnd_last_rowsplit"))
#' @rdname int_methods
setMethod("cmpnd_last_rowsplit", "NULL", function(lyt, spl, constructor) {
    stop("no existing splits to compound with. contact the maintainer") # nocov
})
#' @rdname int_methods
setMethod("cmpnd_last_rowsplit", "PreDataRowLayout",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    tmp = cmpnd_last_rowsplit(lyt[[pos]], spl, constructor)
    lyt[[pos]] = tmp
    lyt
})
#' @rdname int_methods
setMethod("cmpnd_last_rowsplit", "SplitVector",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    lst = lyt[[pos]]
    tmp = if(is(lst, "CompoundSplit")) {
              spl_payload(lst) = c(.uncompound(spl_payload(lst)), .uncompound(spl))
              obj_name(lst) <- make_ma_name(spl = lst)
              lst
          } else { ## XXX never reached because AnalzyeMultiVars inherits from CompoundSplit???
              constructor(.payload = list(lst, spl))
          }
    lyt[[pos]] = tmp
    lyt
})

#' @rdname int_methods
setMethod("cmpnd_last_rowsplit", "PreDataTableLayouts",
          function(lyt, spl, constructor){
    rlyt = rlayout(lyt)
    rlyt = cmpnd_last_rowsplit(rlyt, spl, constructor)
    rlayout(lyt)= rlyt
    lyt
})
#' @rdname int_methods
setMethod("cmpnd_last_rowsplit", "ANY",
          function(lyt, spl, constructor) stop("nope. can't do cmpnd_last_rowsplit to that (", class(lyt), "). contact the maintaner.")
          )


#' @rdname int_methods
setGeneric("split_cols", function(lyt = NULL, spl, pos) standardGeneric("split_cols"))
#' @rdname int_methods
setMethod("split_cols", "NULL", function(lyt, spl, pos) {
    .Deprecated(msg = "Initializing layouts via NULL is deprecated, please use basic_table() instead")
    cl = PreDataColLayout(SplitVector(spl))
    rl = PreDataRowLayout()
    PreDataTableLayouts(rlayout = rl, clayout = cl)
})
#' @rdname int_methods
setMethod("split_cols", "PreDataColLayout",
          function(lyt, spl, pos) {
    stopifnot(pos > 0 && pos <= length(lyt) + 1)
    tmp  = if (pos <= length(lyt)) {
               split_cols(lyt[[pos]], spl, pos)
           } else {
               SplitVector(spl)
           }

    lyt[[pos]] = tmp
    lyt
})
#' @rdname int_methods
setMethod("split_cols", "SplitVector",
          function(lyt, spl, pos) {
    tmp = c(lyt, spl)
    SplitVector(lst = tmp)
})
#' @rdname int_methods
setMethod("split_cols", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt = lyt@col_layout
    rlyt = split_cols(rlyt, spl, pos)
    lyt@col_layout = rlyt
    lyt
})
#' @rdname int_methods
setMethod("split_cols", "ANY",
          function(lyt, spl, pos) stop("nope. can't add a col split to that (", class(lyt), "). contact the maintaner.")
          )

## no longer needed (if it ever was) AFAICT

## #' @rdname int_methods
## setGeneric("cmpnd_last_colsplit", function(lyt, spl, constructor) standardGeneric("cmpnd_last_colsplit"))
## #' @rdname int_methods
## setMethod("cmpnd_last_colsplit", "NULL", function(lyt, spl, constructor) {
##     stop("no existing splits to compound with. contact the maintainer") # nocov
## })
## #' @rdname int_methods
## setMethod("cmpnd_last_colsplit", "PreDataColLayout",
##           function(lyt, spl, constructor) {
##     pos = length(lyt)
##     tmp = cmpnd_last_colsplit(lyt[[pos]], spl, constructor)
##     lyt[[pos]] = tmp
##     lyt
## })
## #' @rdname int_methods
## setMethod("cmpnd_last_colsplit", "SplitVector",
##           function(lyt, spl, constructor) {
##     pos = length(lyt)
##     lst = lyt[[pos]]
##     tmp = if(is(lst, "CompoundSplit")) {
##               spl_payload(lst) = c(spl_payload(lst), spl)
##               lst
##           } else {
##               constructor(.payload = list(lst, spl))
##           }
##     lyt[[pos]] = tmp
##     lyt
## })

## #' @rdname int_methods
## setMethod("cmpnd_last_colsplit", "PreDataTableLayouts",
##           function(lyt, spl, constructor){
##     clyt = clayout(lyt)
##     clyt = cmpnd_last_colsplit(clyt, spl, constructor)
##     clayout(lyt)= clyt
##     lyt
## })
## #' @rdname int_methods
## setMethod("cmpnd_last_colsplit", "ANY",
##           function(lyt, spl, constructor) stop("nope. can't do cmpnd_last_colsplit to that (", class(lyt), "). contact the maintaner.")
##           )



# constructors ----



## Pipe-able functions to add the various types of splits to the current layout for both
## row and column.  These all act as wrappers to the split_cols and split_rows
## method stacks.


#' Declaring a column-split based on levels of a variable
#'
#' Will generate children for each subset of a categorical variable
#'
#'
#' @inheritSection custom_split_funs Custom Splitting Function Details
#'
#' @inheritParams lyt_args
#'
#' @param ref_group character(1) or `NULL`. Level of `var` which should be considered ref_group/reference
#'
#' @export
#'
#' @author Gabriel Becker
#' @return A \code{PreDataTableLayouts} object suitable for passing to further layouting functions, and to \code{build_table}.
#' @examples
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2"))
#'
#' build_table(lyt, ex_adsl)
#'
#' # Let's look at the splits in more detail
#'
#' l <- basic_table() %>% split_cols_by("ARM")
#' l
#'
#' # add an analysis (summary)
#' l2 <- l %>%
#'     analyze(c("AGE", "COUNTRY"), afun = list_wrap_x(summary) , format = "xx.xx")
#' l2
#'
#' build_table(l2, DM)
#'
#' # By default sequentially adding layouts results in nesting
#' library(dplyr)
#' DM_MF <- DM %>% filter(SEX %in% c("M", "F")) %>% mutate(SEX = droplevels(SEX))
#'
#' l3 <- basic_table() %>% split_cols_by("ARM") %>%
#'   split_cols_by("SEX") %>%
#'   analyze(c("AGE", "COUNTRY"), afun = list_wrap_x(summary), format = "xx.xx")
#' l3
#'
#'  build_table(l3, DM_MF)
#'
#' # nested=TRUE vs not
#' l4 <- basic_table() %>% split_cols_by("ARM") %>%
#'  split_rows_by("SEX", split_fun = drop_split_levels) %>%
#'  split_rows_by("RACE", split_fun = drop_split_levels) %>%
#'  analyze("AGE")
#'
#' l4
#' build_table(l4, DM)
#'
#' l5 <- basic_table() %>% split_cols_by("ARM") %>%
#'  split_rows_by("SEX", split_fun= drop_split_levels) %>%
#'  analyze("AGE") %>%
#'  split_rows_by("RACE", nested=FALSE, split_fun = drop_split_levels) %>%
#'  analyze("AGE")
#'
#' l5
#' build_table(l5, DM)
#'
#'
split_cols_by = function(lyt,
                         var,
                         labels_var = var,
                         split_label = var,
                         split_fun = NULL,
                         format = NULL,
                         nested = TRUE,
                         child_labels = c("default", "visible", "hidden"),
                         extra_args = list(),
                         ref_group = NULL) {##,
    if(is.null(ref_group)) {
        spl = VarLevelSplit(var = var,
                            split_label = split_label,
                            labels_var = labels_var,
                            split_format = format,
                            child_labels = child_labels,
                            split_fun = split_fun,
                            extra_args = extra_args)
    } else {
        spl = VarLevWBaselineSplit(var = var,
                                   ref_group = ref_group,
                                   split_label = split_label,
                                   split_fun = split_fun,
                                   labels_var = labels_var,
                                   split_format = format)

    }
    pos = next_cpos(lyt, nested)
    split_cols(lyt, spl, pos)
}


setGeneric(".tl_indent_inner", function(lyt) standardGeneric(".tl_indent_inner"))
setMethod(".tl_indent_inner", "PreDataTableLayouts",
          function(lyt) .tl_indent_inner(rlayout(lyt)))
setMethod(".tl_indent_inner", "PreDataRowLayout",
          function(lyt) {
    if(length(lyt) == 0 || length(lyt[[1]]) == 0)
        0L
    else
        .tl_indent_inner(lyt[[length(lyt)]])
})

setMethod(".tl_indent_inner", "SplitVector",
          function(lyt) length(lyt)  - 1L)


.tl_indent <- function(lyt, nested = TRUE) {
    if(!nested)
        0L
    else
        .tl_indent_inner(lyt)
}


#' Add Rows according to levels of a variable
#'
#'
#' @inheritSection custom_split_funs Custom Splitting Function Details
#'
#'
#' @inheritParams lyt_args
#'
#' @note
#' If \code{var} is a factor with empty unobserved levels and
#' \code{labels_var} is specified, it must also be a factor
#' with the same number of levels as \code{var}. Currently the
#' error that occurs when this is not hte case is not very informative,
#' but that will change in the future.
#'
#' @export
#' @author Gabriel Becker
#' @inherit split_cols_by return
#' @examples
#'
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_rows_by("RACE", split_fun = drop_split_levels) %>%
#'     analyze("AGE", mean, var_labels = "Age", format = "xx.xx")
#'
#' build_table(l, DM)
#'
#'
#' basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_rows_by("RACE") %>%
#'     analyze("AGE", mean, var_labels = "Age", format = "xx.xx") %>%
#'     build_table(DM)
#'
#'
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_cols_by("SEX") %>%
#'     summarize_row_groups(label_fstr = "Overall (N)") %>%
#'     split_rows_by("RACE", split_label = "Ethnicity", labels_var = "ethn_lab",
#'                   split_fun = drop_split_levels) %>%
#'     summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
#'     analyze("AGE", var_labels = "Age", afun = mean, format = "xx.xx")
#'
#' l
#'
#' library(dplyr)
#' DM2 <- DM %>%
#'     filter(SEX %in% c("M", "F")) %>%
#'     mutate(
#'         SEX = droplevels(SEX),
#'         gender_lab = c("F" = "Female", "M" = "Male",
#'                        "U" = "Unknown", "UNDIFFERENTIATED" = "Undifferentiated")[SEX],
#'         ethn_lab = c(
#'             "ASIAN" = "Asian",
#'             "BLACK OR AFRICAN AMERICAN" = "Black or African American",
#'             "WHITE" = "White",
#'             "AMERICAN INDIAN OR ALASKA NATIVE" = "American Indian or Alaska Native",
#'              "MULTIPLE" = "Multiple",
#'              "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" =
#'                  "Native Hawaiian or Other Pacific Islander",
#'              "OTHER" = "Other", "UNKNOWN" = "Unknown"
#'         )[RACE]
#'     )
#'
#' build_table(l, DM2)
#'
split_rows_by = function(lyt,
                         var,
                         labels_var = var,
                         split_label = var,
                         split_fun = NULL,
                         format = NULL,
                         nested = TRUE,
                         child_labels = c("default", "visible", "hidden"),
                         label_pos = "hidden",
                         indent_mod = 0L,
                         page_by = FALSE,
                         page_prefix = split_label,
                         section_div = NA_character_) {
    label_pos <- match.arg(label_pos, label_pos_values)
    child_labels = match.arg(child_labels)
    spl = VarLevelSplit(var = var,
                        split_label = split_label,
                        label_pos = label_pos,
                        labels_var = labels_var,
                        split_fun = split_fun,
                        split_format = format,
                        child_labels = child_labels,
                        indent_mod = indent_mod,
                        page_prefix = if(page_by) page_prefix else NA_character_,
                        section_div = section_div)
    addtl <- identical(label_pos, "topleft")

    pos <- next_rpos(lyt, nested)
    ret <- split_rows(lyt, spl, pos)

    ret
}


#' Associate Multiple Variables with Columns
#'
#' In some cases, the variable to be ultimately analyzed is most naturally defined on a column, not a row basis. When we
#' need columns to reflect different variables entirely, rather than different levels of a single variable, we use
#' `split_cols_by_multivar`
#'
#' @inheritParams lyt_args
#'
#' @export
#'
#' @author Gabriel Becker
#'
#' @seealso \code{\link{analyze_colvars}}
#' @inherit split_cols_by return
#' @examples
#'
#' library(dplyr)
#' ANL <- DM %>% mutate(value = rnorm(n()), pctdiff = runif(n()))
#'
#' ## toy example where we take the mean of the first variable and the
#' ## count of >.5 for the second.
#' colfuns <- list(function(x) in_rows(mean = mean(x), .formats = "xx.x"),
#'                 function(x) in_rows("# x > 5" = sum(x > .5), .formats = "xx"))
#'
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_cols_by_multivar(c("value", "pctdiff")) %>%
#'     split_rows_by("RACE", split_label = "ethnicity", split_fun = drop_split_levels) %>%
#'     summarize_row_groups() %>%
#'     analyze_colvars(afun = colfuns)
#'
#' l
#'
#' build_table(l, ANL)
#'
split_cols_by_multivar = function(lyt,
                                  vars,
                                  split_fun = NULL,
                                  varlabels = vars,
                                  varnames = NULL,
                                  nested = TRUE) {
    spl = MultiVarSplit(vars = vars, split_label = "",##split_label,
                        varlabels =varlabels,
                        varnames = varnames,
                        split_fun = split_fun)
    pos = next_cpos(lyt, nested)
    split_cols(lyt, spl, pos)
}


split_rows_by_multivar = function(lyt,
                                  vars,
                                  split_fun = NULL,
                                  varlabels = vars,
                                  format = NULL,
                                  nested = TRUE,
                                  child_labels = c("default", "visible", "hidden"),
                                  indent_mod = 0L,
                                  section_div = NA_character_) {
    child_labels = match.arg(child_labels)
    spl = MultiVarSplit(vars = vars, split_label = "", varlabels,
                        split_format = format,
                        child_labels = child_labels,
                        indent_mod = indent_mod,
                        split_fun = split_fun,
                        section_div = section_div)
    pos = next_rpos(lyt, nested)
    split_rows(lyt, spl, pos)
}

#' Split on static or dynamic cuts of the data
#'
#' Create columns (or row splits) based on values (such as quartiles) of \code{var}.
#'
#' @inheritParams lyt_args
#' @param cuts numeric. Cuts to use
#' @param cutlabels character (or NULL). Labels for the cutst
#' @param cumulative logical. Should the cuts be treated as cumulative. Defaults to \code{FALSE}
#' @param cutfun function. Function which accepts the full vector of \code{var} values and returns cut points to be passed to \code{cut}.
#'
#'
#' @details
#' For dynamic cuts, the cut is transformed into a static cut by \code{\link{build_table}} \emph{based on the full
#' dataset}, before proceeding. Thus even when nested within another split in column/row space, the resulting split will
#' reflect the overall vaalues (e.g., quartiles) in the dataset, NOT the values for subset  it is nested under.
#'
#' @export
#'
#' @rdname varcuts
#' @inherit split_cols_by return
#' @author Gabriel Becker
#'
#' @examples
#' library(dplyr)
#'
#' # split_cols_by_cuts
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_cols_by_cuts("AGE", split_label = "Age",
#'                        cuts = c(0, 25, 35, 1000),
#'                        cutlabels = c("young", "medium", "old")) %>%
#'     analyze(c("BMRKR2", "STRATA2")) %>%
#'     append_topleft("counts")
#'
#' build_table(l, ex_adsl)
#'
#'
#' # split_rows_by_cuts
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_rows_by_cuts("AGE", split_label = "Age",
#'                   cuts = c(0, 25, 35, 1000),
#'                   cutlabels = c("young", "medium", "old")) %>%
#'     analyze(c("BMRKR2", "STRATA2")) %>%
#'     append_topleft("counts")
#'
#'
#' build_table(l, ex_adsl)
#'
#'
#' # split_cols_by_quartiles
#'
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_cols_by_quartiles("AGE", split_label = "Age") %>%
#'     analyze(c("BMRKR2", "STRATA2")) %>%
#'     append_topleft("counts")
#'
#' build_table(l, ex_adsl)
#'
#' # split_rows_by_quartiles
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     add_colcounts() %>%
#'     split_rows_by_quartiles("AGE", split_label = "Age") %>%
#'     analyze("BMRKR2") %>%
#'     append_topleft(c("Age Quartiles", " Counts BMRKR2"))
#'
#' build_table(l, ex_adsl)
#'
#'
split_cols_by_cuts = function(lyt, var, cuts,
                              cutlabels = NULL,
                              split_label = var,
                              nested = TRUE,
                              cumulative = FALSE) {
    spl<- make_static_cut_split(var = var, split_label = split_label, cuts = cuts,
                                cutlabels = cutlabels, cumulative = cumulative) ##= VarStaticCutSplit(var, split_label, cuts, cutlabels)
    ## if(cumulative)
    ##     spl = as(spl, "CumulativeCutSplit")
    pos = next_cpos(lyt, nested)
    split_cols(lyt, spl, pos)
}

#' @export
#' @rdname varcuts
split_rows_by_cuts = function(lyt, var, cuts,
                              cutlabels = NULL,
                              split_label = var,
                              nested = TRUE,
                              cumulative = FALSE,
                              label_pos = "hidden",
                              section_div = NA_character_) {
    label_pos <- match.arg(label_pos, label_pos_values)
##    VarStaticCutSplit(
    spl <- make_static_cut_split(var, split_label, cuts = cuts,
                            cutlabels = cutlabels,
                            label_pos = label_pos,
                            cumulative = cumulative,
                            section_div = section_div)
    ## if(cumulative)
    ##     spl = as(spl, "CumulativeCutSplit")
    pos = next_rpos(lyt, nested)
    split_rows(lyt, spl, pos)
}

## #' @export
## #' @rdname varcuts
## split_rows_by_dyncut = function(lyt, var, cut,
##                                 cutlabels = NULL,
##                                 split_label = var,
##                                 format = NULL,
##                                 nested = TRUE,
##                                 child_labels = c("default", "visible", "hidden"),
##                                 cumulative = FALSE,
##                                 indent_mod = 0L) {
##     child_labels = match.arg(child_labels)
##     spl = VarStaticCutSplit(var, split_label, cuts, cutlabels,
##                             split_format = format,
##                             child_labels = child_labels,
##                             indent_mod = indent_mod)
##     if(cumulative)
##         spl = as(spl, "CumulativeCutSplit")

##     pos = next_rpos(lyt, nested)
##     split_rows(lyt, spl, pos)
## }

#' @export
#' @rdname varcuts
split_cols_by_cutfun = function(lyt, var,
                                cutfun = qtile_cuts,
                                cutlabelfun = function(x) NULL,
                                split_label = var,
                                format = NULL,
                                nested = TRUE,
                                extra_args = list(),
                                cumulative = FALSE
                                ) {
    spl = VarDynCutSplit(var, split_label,
                         cutfun = cutfun,
                         cutlabelfun = cutlabelfun,
                         split_format = format,
                         extra_args = extra_args,
                         cumulative = cumulative,
                         label_pos = "hidden")
    pos = next_cpos(lyt, nested)
    split_cols(lyt, spl, pos)
}

#' @export
#' @rdname varcuts
split_cols_by_quartiles = function(lyt, var, split_label = var,
                             format = NULL,
                             nested = TRUE,
                             extra_args = list(),
                             cumulative = FALSE) {
      split_cols_by_cutfun(lyt = lyt,
                         var = var,
                         split_label = split_label,
                         format = format,
                         cutfun = qtile_cuts,
                         cutlabelfun = function(x) c("[min, Q1]",
                                                   "(Q1, Q2]",
                                                   "(Q2, Q3]",
                                                   "(Q3, max]"),
                         nested = nested,
                         extra_args = extra_args,
                         cumulative = cumulative)
    ## spl = VarDynCutSplit(var, split_label, cutfun = qtile_cuts,
    ##                      cutlabelfun = function(x) c("[min, Q1]",
    ##                                                "(Q1, Q2]",
    ##                                                "(Q2, Q3]",
    ##                                                "(Q3, max]"),
    ##                      split_format = format,
    ##                      extra_args = extra_args,
    ##                      cumulative = cumulative,
    ##                      label_pos = "hidden")
    ## pos = next_cpos(lyt, nested)
    ## split_cols(lyt, spl, pos)
}


#' @export
#' @rdname varcuts
split_rows_by_quartiles = function(lyt, var, split_label = var,
                             format = NULL,
                             nested = TRUE,
                             child_labels = c("default", "visible", "hidden"),
                             extra_args = list(),
                             cumulative= FALSE,
                             indent_mod = 0L,
                             label_pos = "hidden",
                             section_div = NA_character_) {
    split_rows_by_cutfun(lyt = lyt,
                         var = var,
                         split_label = split_label,
                         format = format,
                         cutfun = qtile_cuts,
                         cutlabelfun = function(x) c("[min, Q1]",
                                                   "(Q1, Q2]",
                                                   "(Q2, Q3]",
                                                   "(Q3, max]"),
                         nested = nested,
                         child_labels = child_labels,
                         extra_args = extra_args,
                         cumulative = cumulative,
                         indent_mod = indent_mod,
                         label_pos = label_pos,
                         section_div = section_div)

    ## label_pos <- match.arg(label_pos, label_pos_values)
    ## spl = VarDynCutSplit(var, split_label, cutfun = qtile_cuts,
    ##                      cutlabelfun = ,
    ##                      split_format = format,
    ##                      child_labels = child_labels,
    ##                      extra_args = extra_args,
    ##                      cumulative = cumulative,
    ##                      indent_mod = indent_mod,
    ##                      label_pos = label_pos)
    ## pos = next_rpos(lyt, nested)
    ## split_rows(lyt, spl, pos)
}



qtile_cuts = function(x) {
    ret = quantile(x)
    names(ret) = c("",
                   "1st qrtile",
                    "2nd qrtile",
                    "3rd qrtile",
                    "4th qrtile")
    ret
}

#' @export
#' @rdname varcuts
split_rows_by_cutfun = function(lyt, var,
                                cutfun = qtile_cuts,
                                cutlabelfun = function(x) NULL,
                                split_label = var,
                                format = NULL,
                                nested = TRUE,
                                child_labels = c("default", "visible", "hidden"),
                                extra_args = list(),
                                cumulative = FALSE,
                                indent_mod = 0L,
                                label_pos = "hidden",
                                section_div = NA_character_) {
    label_pos <- match.arg(label_pos, label_pos_values)
    child_labels = match.arg(child_labels)
    spl = VarDynCutSplit(var, split_label, cutfun = cutfun,
                         cutlabelfun = cutlabelfun,
                         split_format = format,
                         child_labels = child_labels,
                         extra_args = extra_args,
                         cumulative = cumulative,
                         indent_mod = indent_mod,
                         label_pos = label_pos,
                         section_div = section_div)
    pos = next_rpos(lyt, nested)
    split_rows(lyt, spl, pos)
}


#' @title .spl_context within analysis and split functions
#'
#' @name spl_context
#' @rdname spl_context
#'
#' @section .spl_context Details:
#' The `.spl_context` `data.frame` gives information about the subsets of data corresponding to the
#' splits within-which the current `analyze` action is nested. Taken together, these correspond to the
#' path that the resulting (set of) rows the analysis function is creating, although the information is
#' in a slighlyt different form. Each split (which correspond to groups of rows in the resulting table) is
#' represented via the following columns:
#' \describe{
#'   \item{split}{The name of the split (often the variable being split in the simple case)}
#'   \item{value}{The string representation of the value at that split}
#'   \item{full_parent_df}{a dataframe containing the full data (ie across all columns) corresponding to the path
#' defined by the combination of `split` and `value` of this row \emph{and all rows above this row}}
#'   \item{all_cols_n}{the number of observations  corresponding to this row grouping (union of all columns)}
#'   \item{\emph{(row-split and analyze contexts only)} <1 column for each column in the table structure}{ These
#' list columns (named the same as \code{names(col_exprs(tab))}) contain logical vectors corresponding to the
#' subset of this row's `full_parent_df` corresponding to that column}
#'   \item{cur_col_subset}{List column containing logical vectors indicating the subset of that row's `full_parent_df` for the column currently being created by the analysis function}
#'   \item{cur_col_n}{integer column containing the observation counts for that split}
#' }
#'
#' \emph{note Within analysis functions that accept `.spl_context`, the `all_cols_n` and `cur_col_n` columns of
#' the dataframe will contain the 'true' observation counts corresponding to the row-group and
#' row-group x column subsets of the data. These numbers will not, and currently cannot, reflect alternate
#' column observation counts provided by the `alt_counts_df`, `col_counts` or `col_total` arguments
#' to \code{\link{build_table}}}
NULL





#' Generate Rows Analyzing Variables Across Columns
#'
#' Adding /analyzed variables/ to our table layout defines the primary tabulation to be performed. We do this by adding
#' calls to \code{analyze} and/or \code{\link{analyze_colvars}} into our layout pipeline. As with
#' adding further splitting, the tabulation will occur at the current/next level of nesting by default.
#'
#' @inheritParams lyt_args
#' @inherit split_cols_by return
#'
#' @details
#'
#' When non-NULL \code{format} is used to specify formats for all generated rows, and can be a character vector, a function, or a list of functions. It will be repped out to the number of rows once this is known during the tabulation process, but will be overridden by formats specified within \code{rcell} calls in \code{afun}.
#'
#' The analysis function (\code{afun}) should take as its first parameter either \code{x} or \code{df}. Which of these the
#'   function accepts changes the behavior when tabulation is performed.
#'
#' \itemize{
#'   \item{
#'   If \code{afun}'s first parameter is x, it will receive the corresponding subset \emph{vector} of data from the
#'   relevant column (from \code{var} here) of the raw data being used to build the table.
#'   }
#'
#'   \item{
#'   If \code{afun}'s first parameter is \code{df}, it will receive the corresponding subset \emph{data.frame} (i.e. all
#'   columns) of the raw data being tabulated
#'   }
#' }
#'
#' In addition to differentiation on the first argument, the analysis function can optionally accept a number of other
#' parameters which, \emph{if and only if} present in the formals will be passed to the function by the tabulation
#' machinery. These are as follows:
#'
#' \describe{
#'   \item{.N_col}{column-wise N (column count) for the full column being tabulated within}
#'   \item{.N_total}{overall N (all observation count, defined as sum of column counts) for the tabulation}
#'   \item{.N_row}{row-wise N (row group count) for the group of observations being analyzed (ie with no column-based subsetting)}
#'   \item{.df_row}{ data.frame for observations in the row group being analyzed (ie with no column-based subsetting)}
#'   \item{.var}{variable that is analyzed}
#'   \item{.ref_group}{data.frame or vector of subset corresponding to the `ref_group` column including subsetting
#'   defined by row-splitting. Optional and only required/meaningful if a `ref_group` column has been defined}
#'   \item{.ref_full}{data.frame or vector of subset corresponding to the `ref_group` column without subsetting
#'   defined by row-splitting. Optional and only required/meaningful if a `ref_group` column has been defined}
#'   \item{.in_ref_col}{boolean indicates if calculation is done for cells withing the reference column}
#'   \item{.spl_context}{data.frame, each row gives information about a previous/'ancestor' split state. see below}
#' }
#'
#' @inheritSection spl_context .spl_context Details
#'
#'
#' @note None of the arguments described in the Details section
#' can be overridden via extra_args or when calling
#' \code{\link{make_afun}}. \code{.N_col} and \code{.N_total} can
#' be overridden via the \code{col_counts} argument to
#' \code{\link{build_table}}. Alternative values for the others
#' must be calculated within \code{afun} based on a combination
#' of extra arguments and the unmodified values provided by the
#' tabulation framework.
#'
#' @inherit split_cols_by return
#'
#' @export
#'
#' @author Gabriel Becker
#'
#'
#' @examples
#'
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     analyze("AGE", afun = list_wrap_x(summary) , format = "xx.xx")
#' l
#' build_table(l, DM)
#'
#'
#' l <- basic_table() %>%
#'     split_cols_by("Species") %>%
#'     analyze(head(names(iris), -1), afun = function(x) {
#'         list(
#'             "mean / sd" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
#'             "range" = rcell(diff(range(x)), format = "xx.xx")
#'         )
#'     })
#' l
#' build_table(l, iris)
#'
analyze = function(lyt,
                   vars,
                   afun = simple_analysis,
                   var_labels = vars,
                   table_names = vars,
                   format = NULL,
                   nested = TRUE,
                   ##can we name this na_rm? symbol conflict with possible afuns!!!
                   inclNAs = FALSE,
                   extra_args = list(),
                   show_labels = c("default", "visible", "hidden"),
                   indent_mod = 0L,
                   section_div = NA_character_) {
    show_labels = match.arg(show_labels)
    subafun = substitute(afun)
    if(is.name(subafun) &&
       is.function(afun) &&
       ## this is gross. basically testing
       ## if the symbol we have corresponds
       ## in some meaningful way to the function
       ## we will be calling.
       identical(mget(as.character(subafun),
                      mode = "function",
                      ifnotfound = list(NULL),
                      inherits = TRUE
                      )[[1]], afun)) {
        defrowlab = as.character(subafun)
    } else {
        defrowlab = var_labels
    }

    spl = AnalyzeMultiVars(vars, var_labels,
                          afun = afun,
                          split_format = format,
                          defrowlab = defrowlab,
                          inclNAs = inclNAs,
                          extra_args = extra_args,
                          indent_mod = indent_mod,
                          child_names = table_names,
                          child_labels = show_labels,
                          section_div = section_div)

    if(nested &&
       (is(last_rowsplit(lyt), "VAnalyzeSplit") ||
        is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
        cmpnd_last_rowsplit(lyt, spl, AnalyzeMultiVars)
    } else {
    ## analysis compounding now done in split_rows
        pos = next_rpos(lyt, nested)
        split_rows(lyt, spl, pos)
    }
}



get_acolvar_name  <- function(lyt) {
    ## clyt <- clayout(lyt)
    ## stopifnot(length(clyt) == 1L)
    ## vec = clyt[[1]]
    ## vcls = vapply(vec, class, "")
    ## pos = max(which(vcls ==  "MultiVarSplit"))
    paste(c("ac", get_acolvar_vars(lyt)), collapse = "_")
}


get_acolvar_vars <- function(lyt) {
    clyt <- clayout(lyt)
    stopifnot(length(clyt) == 1L)
    vec = clyt[[1]]
    vcls = vapply(vec, class, "")
    pos = which(vcls ==  "MultiVarSplit")
    if(length(pos) > 0)
        spl_payload(vec[[pos]])
    else
        "non_multivar"
}


#' Generate Rows Analyzing Different Variables Across Columns
#'
#' @inheritParams  lyt_args
#'
#' @param afun function or list. Function(s) to be used to calculate the values in each column.  the list will be repped out as needed and matched by position with the columns during tabulation.
#'
#' @export
#'
#' @inherit split_cols_by return
#'
#' @seealso \code{\link{split_cols_by_multivar}}
#'
#' @author Gabriel Becker
#'
#' @examples
#'
#' library(dplyr)
#' ANL <- DM %>% mutate(value = rnorm(n()), pctdiff = runif(n()))
#'
#' ## toy example where we take the mean of the first variable and the
#' ## count of >.5 for the second.
#' colfuns <- list(function(x) rcell(mean(x), format = "xx.x"),
#'                 function(x) rcell(sum(x > .5), format = "xx"))
#'
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_cols_by_multivar(c("value", "pctdiff")) %>%
#'     split_rows_by("RACE", split_label = "ethnicity", split_fun = drop_split_levels) %>%
#'     summarize_row_groups() %>%
#'     analyze_colvars(afun = colfuns)
#'
#' l
#'
#' build_table(l, ANL)
#'
#'
#' basic_table() %>% split_cols_by("ARM") %>%
#'     split_cols_by_multivar(c("value", "pctdiff"), varlabels = c("Measurement", "Pct Diff")) %>%
#'     split_rows_by("RACE", split_label = "ethnicity", split_fun = drop_split_levels) %>%
#'     summarize_row_groups() %>%
#'     analyze_colvars(afun = mean, format = "xx.xx") %>%
#'     build_table(ANL)
#'
analyze_colvars = function(lyt, afun,
                           format = NULL,
                           nested = TRUE,
                           extra_args = list(),
                           indent_mod = 0L,
                           inclNAs = FALSE) {
    if(is.function(afun)) {
        subafun = substitute(afun)
        if(is.name(subafun) &&
           is.function(afun) &&
           ## this is gross. basically testing
           ## if the symbol we have corresponds
           ## in some meaningful way to the function
           ## we will be calling.
           identical(mget(as.character(subafun),
                          mode = "function",
                          ifnotfound = list(NULL),
                          inherits = TRUE
                          )[[1]], afun)) {
            defrowlab = as.character(subafun)
        } else {
            defrowlab = ""
        }
        afun = lapply(get_acolvar_vars(lyt),
                      function(x) afun)
    } else {
        defrowlab = ""
    }
    spl = AnalyzeColVarSplit(afun = afun,
                          defrowlab = defrowlab,
                          split_format = format,
                          split_name = get_acolvar_name(lyt),
                          indent_mod = indent_mod,
                          extra_args = extra_args,
                          inclNAs = inclNAs)
    pos = next_rpos(lyt, nested, for_analyze = TRUE)
    split_rows(lyt, spl, pos)
}

## Add a total column at the next **top level** spot in
## the column layout.

#' Add Overall Column
#'
#' @description This function will \emph{only} add an overall
#' column at the \emph{top} level of splitting, NOT within
#' existing column splits.
#' See \code{\link{add_overall_level}} for the recommended
#' way to add overall columns more generally within existing splits.
#'
#' @inheritParams lyt_args
#'
#' @inherit split_cols_by return
#'
#' @export
#'
#' @seealso \code{\link{add_overall_level}}
#'
#' @examples
#' l <- basic_table() %>%
#'    split_cols_by("ARM") %>%
#'    add_overall_col("All Patients") %>%
#'    analyze("AGE")
#'
#' l
#'
#' build_table(l, DM)
#'
add_overall_col = function(lyt, label) {
    spl = AllSplit(label)
    split_cols(lyt,
                  spl,
                  next_cpos(lyt, FALSE))
}


#'
#' @inheritParams lyt_args
#' @export
#'
#' @rdname int_methods
setGeneric(".add_row_summary",
           function(lyt,
                    label,
                    cfun,
                    child_labels = c("default", "visible", "hidden"),
                    cformat = NULL,
                    indent_mod = 0L,
                    cvar = "",
                    extra_args = list()) standardGeneric(".add_row_summary"))
#' @rdname int_methods
setMethod(".add_row_summary", "PreDataTableLayouts",
          function(lyt,
                   label,
                   cfun,
                   child_labels = c("default", "visible", "hidden"),
                   cformat = NULL,
                   indent_mod = 0L,
                   cvar = "",
                   extra_args = list()) {
    child_labels = match.arg(child_labels)
    tmp = .add_row_summary(rlayout(lyt), label, cfun,
                      child_labels = child_labels,
                      cformat = cformat,
                      indent_mod = indent_mod,
                      cvar = cvar,
                      extra_args = extra_args)
    rlayout(lyt) = tmp
    lyt
})
#' @rdname int_methods
setMethod(".add_row_summary", "PreDataRowLayout",
          function(lyt,
                   label,
                   cfun,
                   child_labels = c("default", "visible", "hidden"),
                   cformat = NULL,
                   indent_mod = 0L,
                   cvar = "",
                   extra_args = list()) {
    child_labels = match.arg(child_labels)
    if(length(lyt) == 0 ||
       (length(lyt) == 1 && length(lyt[[1]]) == 0)) {
        ## XXX ignoring indent mod here
        rt = root_spl(lyt)
        rt = .add_row_summary(rt,
                              label,
                              cfun,
                              child_labels = child_labels,
                              cformat = cformat,
                              cvar = cvar,
                              extra_args = extra_args)
        root_spl(lyt) = rt
    } else {
        ind = length(lyt)
        tmp = .add_row_summary(lyt[[ind]], label, cfun,
                          child_labels = child_labels,
                          cformat = cformat,
                          indent_mod = indent_mod,
                          cvar = cvar,
                          extra_args = extra_args)
        lyt[[ind]] = tmp
    }
    lyt
})
#' @rdname int_methods
setMethod(".add_row_summary", "SplitVector",
          function(lyt,
                   label,
                   cfun,
                   child_labels = c("default", "visible", "hidden"),
                   cformat = NULL,
                   indent_mod = 0L,
                   cvar = "",
                   extra_args = list()) {
    child_labels = match.arg(child_labels)
    ind = length(lyt)
    if(ind == 0) stop("no split to add content rows at")
    spl = lyt[[ind]]
    ## if(is(spl, "AnalyzeVarSplit")) stop("can't add content rows to analyze variable split")
    tmp = .add_row_summary(spl,
                           label,
                           cfun,
                           child_labels = child_labels,
                           cformat = cformat,
                           indent_mod = indent_mod,
                           cvar = cvar,
                           extra_args = extra_args)
    lyt[[ind]] = tmp
    lyt
})
#' @rdname int_methods
setMethod(".add_row_summary", "Split",
          function(lyt,
                   label,
                   cfun,
                   child_labels = c("default", "visible", "hidden"),
                   cformat = NULL,
                   indent_mod = 0L,
                   cvar = "",
                   extra_args = list()) {
    child_labels = match.arg(child_labels)
 #   lbl_kids = .labelkids_helper(child_labels)
    content_fun(lyt) = cfun
    content_indent_mod(lyt) = indent_mod
    content_var(lyt) = cvar
    ##obj_format(lyt) = cformat
    content_format(lyt) <- cformat
    if(!identical(child_labels, "default") && !identical(child_labels, label_kids(lyt)))
        label_kids(lyt) = child_labels
    content_extra_args(lyt) = extra_args
    lyt
})
## #' @rdname int_methods
## setMethod(".add_row_summary", "NULL",
##           function(lyt,
##                    label,
##                    cfun,
##                    child_labels = c("default", "visible", "hidden"),
##                    cformat = NULL,
##                    indent_mod = 0L,
##                    cvar = "",
##                    extra_args = list()) {

##     rlyt <- PreDataRowLayout()
##     rtspl <- root_spl(rlyt)

##     rtspl <- .add_row_summary(lyt = rtspl,
##                      label = label,
##                      cfun = cfun,
##                      child_labels = child_labels,
##                      cformat = cformat,
##                      indent_mod = indent_mod,
##                      cvar = cvar,
##                      extra_args = extra_args)
##     root_spl(rlyt) <- rtspl
##     PreDataTableLayouts(rlayout = rlyt)
## })

.count_raw_constr = function(var, format, label_fstr) {
    function(df, labelstr = "") {
        if(grepl("%s", label_fstr, fixed = TRUE))
            label <- sprintf(label_fstr, labelstr)
        else
            label <- label_fstr
        if(is(df, "data.frame")) {
            if(!is.null(var) && nzchar(var))
                cnt <- sum(!is.na(df[[var]]))
            else
                cnt <- nrow(df)
        } else { # df is the data column vector
            cnt <- sum(!is.na(df))
        }
        ret <- rcell(cnt, format = format,
                    label = label)
        ret
    }
}

.count_wpcts_constr = function(var, format, label_fstr) {
    function(df, labelstr = "", .N_col) {
        if(grepl("%s", label_fstr, fixed = TRUE))
            label <- sprintf(label_fstr, labelstr)
        else
            label <- label_fstr
        if(is(df, "data.frame")) {
            if(!is.null(var) && nzchar(var))
                cnt <- sum(!is.na(df[[var]]))
            else
                cnt <- nrow(df)
        } else { # df is the data column vector
            cnt <- sum(!is.na(df))
        }
        ## the formatter does the *100 so we don't here.
        ret <- rcell(c(cnt, cnt/.N_col),
                    format = format,
                    label = label)
        ret
    }
}

.validate_cfuns <- function(fun) {
    if(is.list(fun))
        return(unlist(lapply(fun, .validate_cfuns)))

    frmls <- formals(fun)
    ls_pos <- match("labelstr", names(frmls))
    if(is.na(ls_pos))
        stop("content functions must explicitly accept a 'labelstr' argument")

    ## if(is.na(ls_pos)) {
    ##     ls_pos <- grep("lbl_{0,1}str", names(frmls))
    ##     if(length(ls_pos) == 0)
    ##         stop("Invalid content function - does not accept the required labelstr argument")
    ##     .Deprecated(old = "Use of content functions which do not accept a named 'labelstr' argument", new = "content functions which explicitly accept 'labelstr'")
    ##     names(formals(fun))[ls_pos] <- "labelstr"
    ## }
    list(fun)
}


#' Add a content row of summary counts
#'
#' @inheritParams lyt_args
#'
#' @inherit split_cols_by return
#'
#' @details If `format` expects 1 value (i.e. it is specified as a format string and `xx` appears  `xx` apepars  values (i.e. `xx` appears twice in the format string) or is specified as a function,
#' then both raw and percent of column total counts are calculated. If `format` is a format string where `xx` appears
#' only one time, only raw counts are used.
#'
#' `cfun` must accept `df` as its first argument and will receive the subset `data.frame` corresponding
#' with the row- and column-splitting for the cell being calculated. Must accept `labelstr` as the second
#' parameter, which accepts the `label` of the level of the parent split currently being summarized. Can additionally take any optional argument supported by analysis functions. (see \code{\link{analyze}}).
#'
#' @export
#'
#' @author Gabriel Becker
#'
#' @examples
#'
#' DM2 <- subset(DM, COUNTRY %in% c("USA", "CAN", "CHN"))
#'
#' l <- basic_table() %>% split_cols_by("ARM") %>%
#'     split_rows_by("COUNTRY", split_fun = drop_split_levels) %>%
#'     summarize_row_groups(label_fstr = "%s (n)") %>%
#'     analyze("AGE", afun = list_wrap_x(summary) , format = "xx.xx")
#' l
#'
#' tbl <- build_table(l, DM2)
#'
#' tbl
#'
#' row_paths_summary(tbl) # summary count is a content table
#'
#'
#' ## use a cfun and extra_args to customize summarization
#' ## behavior
#' sfun <- function(x, labelstr, trim) {
#'     in_rows(
#'         c(mean(x, trim = trim), trim),
#'         .formats = "xx.x (xx.x%)",
#'         .labels = sprintf("%s (Trimmed mean and trim %%)",
#'                               labelstr)
#'     )
#' }
#'
#' l2 <- basic_table() %>% split_cols_by("ARM") %>%
#'     split_rows_by("COUNTRY", split_fun = drop_split_levels) %>%
#'     add_colcounts() %>%
#'     summarize_row_groups("AGE", cfun = sfun,
#'                          extra_args = list(trim = .2)) %>%
#'     analyze("AGE", afun = list_wrap_x(summary) , format = "xx.xx") %>%
#'     append_topleft(c("Country", "  Age"))
#'
#' tbl2 <- build_table(l2, DM2)
#' tbl2
#'
summarize_row_groups = function(lyt,
                                var = "",
                                label_fstr = "%s",
                                format = "xx (xx.x%)",
                                cfun = NULL,
                                indent_mod = 0L,
                                extra_args = list()){

    if(is.null(cfun)) {
        if(is.character(format) && length(gregexpr("xx(\\.x*){0,1}", format)[[1]]) == 1)
            cfun = .count_raw_constr(var,format, label_fstr)
        else
            cfun = .count_wpcts_constr(var, format, label_fstr)
    }
    cfun <- .validate_cfuns(cfun)
    .add_row_summary(lyt,
                     cfun = cfun,
                     cformat = format,
                     indent_mod = indent_mod,
                     cvar = var,
                     extra_args = extra_args)
}


#' Add the column population counts to the header
#'
#' Add the data derived column counts.
#'
#' @details It is often the case that the the column counts derived from the
#'   input data to `build_table` is not representative of the population counts.
#'   For example, if events are counted in the table and the header should
#'   display the number of subjects and not the total number of events. In that
#'   case use the `col_count` argument in `build_table` to control the counts
#'   displayed in the table header.
#'
#' @inheritParams lyt_args
#'
#' @inherit split_cols_by return
#'
#' @export
#'
#' @author Gabriel Becker
#'
#' @examples
#' l <- basic_table() %>% split_cols_by("ARM") %>%
#'     add_colcounts() %>%
#'     split_rows_by("RACE", split_fun = drop_split_levels) %>%
#'     analyze("AGE", afun = function(x) list(min = min(x), max = max(x)))
#' l
#'
#' build_table(l, DM)
#'
add_colcounts = function(lyt, format = "(N=xx)") {
    if(is.null(lyt))
        lyt <- PreDataTableLayouts()
    disp_ccounts(lyt) <- TRUE
    colcount_format(lyt) <- format
    lyt
}

## Currently existing tables can ONLY be added
## as new entries at the top level, never at any
## level of nesting.
#' Add an already calculated table to the layout
#' @inheritParams lyt_args
#' @inheritParams gen_args
#'
#' @inherit split_cols_by return
#' @export
#' @author Gabriel Becker
#'
#' @examples
#' tbl1 <- basic_table() %>%
#'    split_cols_by("ARM") %>%
#'    analyze("AGE", afun = mean, format = "xx.xx") %>%
#'    build_table(DM)
#'
#' tbl1
#'
#' tbl2 <- basic_table() %>% split_cols_by("ARM") %>%
#'    analyze("AGE", afun = sd, format = "xx.xx") %>%
#'    add_existing_table(tbl1) %>%
#'    build_table(DM)
#'
#' tbl2
#'
#' table_structure(tbl2)
#'
#' row_paths_summary(tbl2)
add_existing_table = function(lyt, tt, indent_mod = 0) {
    indent_mod(tt) = indent_mod
    lyt = split_rows(lyt,
                        tt,
                        next_rpos(lyt, nested = FALSE))
    lyt
}


## takes_coln = function(f) {
##     stopifnot(is(f, "function"))
##     forms = names(formals(f))
##     res = ".N_col" %in% forms
##     res
## }

## takes_totn = function(f) {
##     stopifnot(is(f, "function"))
##     forms = names(formals(f))
##     res = ".N_total" %in% forms
##     res
## }


## use data to transform dynamic cuts to static cuts
#' @rdname int_methods
setGeneric("fix_dyncuts", function(spl, df) standardGeneric("fix_dyncuts"))
#' @rdname int_methods
setMethod("fix_dyncuts", "Split", function(spl, df) spl)
#' @rdname int_methods
setMethod("fix_dyncuts", "VarDynCutSplit",
          function(spl, df) {

    var = spl_payload(spl)
    varvec = df[[var]]

    cfun = spl_cutfun(spl)
    cuts = cfun(varvec)
    cutlabels <- spl_cutlabelfun(spl)(cuts)
    if(length(cutlabels) != length(cuts) - 1  &&
       !is.null(names(cuts))) {
        cutlabels <- names(cuts)[-1]
    }

    ret <- make_static_cut_split(var = var, split_label = obj_label(spl),
                                 cuts = cuts, cutlabels = cutlabels,
                                 cumulative = spl_is_cmlcuts(spl))
    ## ret = VarStaticCutSplit(var = var, split_label = obj_label(spl),
    ##                   cuts = cuts, cutlabels = cutlabels)
    ## ## classes are tthe same structurally CumulativeCutSplit
    ## ## is just a sentinal so it can hit different make_subset_expr
    ## ## method
    ## if(spl_is_cmlcuts(spl))
    ##     ret = as(ret, "CumulativeCutSplit")
    ret
})
#' @rdname int_methods
setMethod("fix_dyncuts", "VTableTree",
          function(spl, df) spl)


.fd_helper = function(spl, df) {
    lst = lapply(spl, fix_dyncuts, df = df)
    spl@.Data = lst
    spl

}
#' @rdname int_methods
setMethod("fix_dyncuts", "PreDataRowLayout",
          function(spl, df) {
 #   rt = root_spl(spl)
    ret = .fd_helper(spl, df)
#    root_spl(ret) = rt
    ret
})
#' @rdname int_methods
setMethod("fix_dyncuts", "PreDataColLayout",
          function(spl, df) {
 #   rt = root_spl(spl)
    ret = .fd_helper(spl, df)
 #   root_spl(ret) = rt
 #   disp_ccounts(ret) = disp_ccounts(spl)
 #   colcount_format(ret) = colcount_format(spl)
    ret
})
#' @rdname int_methods
setMethod("fix_dyncuts", "SplitVector",
          function(spl, df) {
    .fd_helper(spl, df)
})
#' @rdname int_methods
setMethod("fix_dyncuts", "PreDataTableLayouts",
          function(spl, df) {
    rlayout(spl) = fix_dyncuts(rlayout(spl), df)
    clayout(spl) = fix_dyncuts(clayout(spl), df)
    spl
})




## Manual column construction in a simple (seeming
## to the user) way.
#' Manual column declaration
#' @param \dots One or more vectors of levels to appear
#' in the column splace. If more than one set of levels is given, the values of the second are nested within each value of the first, and so on.
#' @param .lst A list of sets of levels, by default populated via \code{list(...)}.
#' @return An InstantiatedColumnInfo object, suitable for use declaring the column structure for a manually constructed table.
#' @author Gabriel Becker
#'
#' @export
#'
#' @examples
#' # simple one level column space
#' rows = lapply(1:5, function(i) {
#'    DataRow(rep(i, times  = 3))})
#' tab = TableTree(kids = rows, cinfo = manual_cols(split = c("a", "b", "c")))
#' tab
#'
#' # manually declared nesting
#' tab2 = TableTree(kids = list(DataRow(as.list(1:4))),
#'                  cinfo = manual_cols(Arm = c("Arm A", "Arm B"),
#'                                      Gender = c("M", "F")))
#'
#' tab2
#'
manual_cols = function(..., .lst = list(...)) {
    if(is.null(names(.lst)))
        names(.lst) = paste("colsplit", seq_along(.lst))

    splvec = SplitVector(lst = mapply(ManualSplit, levels = .lst, label = names(.lst)))
    ctree = splitvec_to_coltree(data.frame(), splvec=splvec, pos = TreePos())
    InstantiatedColumnInfo(treelyt = ctree)
}


#' Returns a function that coerces the return values of f to a list
#'
#' @param f The function to wrap.
#' @export
#'
#' @details \code{list_wrap_x} generates a wrapper which takes \code{x} as its first argument, while \code{list_wrap_df}
#'   generates an otherwise identical wrapper function whose first argument is named \code{df}.
#'
#'   We provide both because when using the functions as tabulation in
#'   \code{\link{analyze}}, functions which take \code{df} as their first argument are passed the full subset dataframe,
#'   while those which accept anything else {notably including \code{x}} are passed only the relevant subset of the
#'   variable being analyzed.
#'
#' @rdname list_wrap
#' @return A function which calls \code{f} and converts the result to a list of \code{CellValue} objects.
#' @author Gabriel Becker
#' @examples
#'
#' summary(iris$Sepal.Length)
#'
#' f <- list_wrap_x(summary)
#' f(x = iris$Sepal.Length)
#'
#' f2 <- list_wrap_df(summary)
#' f2(df = iris$Sepal.Length)
#'
list_wrap_x = function(f) {
    function(x,...) {
        vs = as.list(f(x, ...))
        ret = mapply(function(v, nm) {
            rcell(v, label = nm)
        },
        v = vs,
        nm = names(vs))
        ret

    }
}

#' @rdname list_wrap
#' @export
list_wrap_df = function(f) {
    function(df,...) {
        vs = as.list(f(df,...))
        ret = mapply(function(v, nm) {
            rcell(v, label = nm)
        },
        v = vs,
        nm = names(vs))
        ret
    }
}


#' Layout with 1 column and zero rows
#'
#' Every layout must start with a basic table.
#'
#' @export
#' @inheritParams constr_args
#' @param show_colcounts logical(1). Should column counts be displayed in the resulting table
#' when this layout is applied to data
#'
#' @inherit split_cols_by return
#'
#' @examples
#'
#' lyt <- basic_table() %>%
#'   analyze("AGE", afun = mean)
#'
#' build_table(lyt, DM)
#'
#'
#' lyt <- basic_table(title = "Title of table", subtitles = c("a number", "of subtitles"),
#'                    main_footer = "test footer",
#'                    prov_footer = paste("test.R program, executed at", Sys.time())) %>%
#'   split_cols_by("ARM") %>%
#'   analyze("AGE", mean)
#'
#' build_table(lyt, DM)
#'
basic_table <- function(title = "",
                        subtitles = character(),
                        main_footer = character(),
                        prov_footer = character(),
                        show_colcounts = FALSE) {
    ret <- PreDataTableLayouts(title = title,
                        subtitles = subtitles,
                        main_footer = main_footer,
                        prov_footer = prov_footer)
    if(show_colcounts)
        ret <- add_colcounts(ret)
    ret
}


#' Append a description to the 'top-left' materials for the layout
#'
#' @description This function \emph{adds} \code{newlines} to the current
#' set of "top-left materials".
#' @details
#'
#' Adds \code{newlines} to the set of strings representing the 'top-left'
#' materials declared in the layout (the content displayed to the left of
#' the column labels when the resulting tables are printed).
#'
#' Top-left material strings are stored and then displayed \emph{exactly as is},
#' no structure or indenting is applied to them either wheyn they are added
#' or when they are displayed.
#' @inheritParams lyt_args
#'
#' @inherit split_cols_by return
#'
#' @param newlines character. The new line(s) to be added to the materials
#' @note Currently, where in the construction of the layout this is called
#' makes no difference, as it is indepenedent of the actual splitting keywords.
#' This may change in the future.
#' @note This function is experimental, its name and the details of
#' its behavior are subject to change in future versions.
#'
#' @export
#' @seealso top_left
#'
#' @examples
#' library(dplyr)
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_cols_by("SEX") %>%
#'   split_rows_by("RACE") %>%
#'   append_topleft("Ethnicity") %>%
#'   analyze("AGE") %>%
#'   append_topleft("  Age")
#'
#' DM2 <- DM %>% mutate(RACE = factor(RACE), SEX = factor(SEX))
#'
#' build_table(lyt, DM2)
append_topleft <- function(lyt, newlines) {
    stopifnot(is(lyt, "PreDataTableLayouts"),
              is(newlines, "character"))
    lyt@top_left <- c(lyt@top_left, newlines)
    lyt
}
