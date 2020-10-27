
#' combine SplitVector objects
#' @param x SplitVecttor
#' @param ... Splits or SplitVector objects
#' @exportMethod c
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

    tmp = c(unclass(lyt), spl)
    SplitVector(lst = tmp)
})

#' @rdname int_methods
setMethod("split_rows", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt = lyt@row_layout
    rlyt = split_rows(rlyt, spl, pos)
    lyt@row_layout = rlyt
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
    stop("no existing splits to compound with. contact the maintainer")
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
              lst
          } else {
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

#' @rdname int_methods
setGeneric("cmpnd_last_colsplit", function(lyt, spl, constructor) standardGeneric("cmpnd_last_colsplit"))
#' @rdname int_methods
setMethod("cmpnd_last_colsplit", "NULL", function(lyt, spl, constructor) {
    stop("no existing splits to compound with. contact the maintainer")
})
#' @rdname int_methods
setMethod("cmpnd_last_colsplit", "PreDataColLayout",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    tmp = cmpnd_last_colsplit(lyt[[pos]], spl, constructor)
    lyt[[pos]] = tmp
    lyt
})
#' @rdname int_methods
setMethod("cmpnd_last_colsplit", "SplitVector",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    lst = lyt[[pos]]
    tmp = if(is(lst, "CompoundSplit")) {
              spl_payload(lst) = c(spl_payload(lst), spl)
              lst
          } else {
              constructor(.payload = list(lst, spl))
          }
    lyt[[pos]] = tmp
    lyt
})

#' @rdname int_methods
setMethod("cmpnd_last_colsplit", "PreDataTableLayouts",
          function(lyt, spl, constructor){
    clyt = clayout(lyt)
    clyt = cmpnd_last_colsplit(clyt, spl, constructor)
    clayout(lyt)= clyt
    lyt
})
#' @rdname int_methods
setMethod("cmpnd_last_colsplit", "ANY",
          function(lyt, spl, constructor) stop("nope. can't do cmpnd_last_colsplit to that (", class(lyt), "). contact the maintaner.")
          )



# constructors ----



## Pipe-able functions to add the various types of splits to the current layout for both
## row and column.  These all act as wrappers to the split_cols and split_rows
## method stacks.


#' Declaring a column-split based on levels of a variable
#'
#' Will generate children for each subset of a categorical variable
#'
#' @inheritParams lyt_args
#'
#' @param ref_group character(1) or NULL. Level of \code{var} which should be considered ref_group/reference
#' @param incl_all logical(1). Should a column representing all observations at this level of nesting be added. defaults to \code{FALSE}
#'
#' @export
#'
#' @author Gabriel Becker
#' @examples
#' l <- basic_table() %>% split_cols_by("ARM")
#' l
#'
#' # add an analysis (summary)
#' l2 <- l %>%
#'     analyze("AGE", afun = list_wrap_x(summary) , format = "xx.xx")
#' l2
#'
#' build_table(l2, DM)
#'
#' # By default sequentially adding layouts results in nesting
#' l3 <- basic_table() %>% split_cols_by("ARM") %>%
#'   split_cols_by("SEX") %>%
#'   analyze("AGE", afun = list_wrap_x(summary), format = "xx.xx")
#' l3
#'
#'  build_table(l3, DM)
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
                         ref_group = NULL,
                         incl_all = FALSE) {
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
                                   incl_all = incl_all,
                                   split_label = split_label,
                                   split_fun = split_fun,
                                   labels_var = labels_var,
                                   split_format = format)

    }
    pos = next_cpos(lyt, nested)
    split_cols(lyt, spl, pos)
}


#' Add Rows according to levels of a variable
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
#' @examples
#'
#' l <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("RACE", split_fun = drop_split_levels) %>%
#'   analyze("AGE", mean, var_labels = "Age", format = "xx.xx")
#'
#' build_table(l, DM)
#'
#'
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("RACE") %>%
#'   analyze("AGE", mean, var_labels = "Age", format = "xx.xx") %>%
#'   build_table(DM)
#'
#'
#'  l <- basic_table() %>%
#'    split_cols_by("ARM") %>%
#'    split_cols_by("SEX") %>%
#'    summarize_row_groups(label_fstr = "Overall (N)") %>%
#'      split_rows_by("RACE", split_label = "Ethnicity", labels_var = "ethn_lab",
#'                    split_fun = drop_split_levels) %>%
#'    summarize_row_groups("RACE", label_fstr = "%s (n)") %>%
#'    analyze("AGE", var_labels = "Age", afun = mean, format = "xx.xx")
#'
#'  l
#'
#'  DM2 <- DM
#'  DM2$gender_lab <- DM2$SEX
#' levels(DM2$gender_lab) <- c("Female", "Male", "Unknown", "Undifferentiated")
#' DM2$ethn_lab <- c("Asian", "African American", "Caucasian", "Am Indian or Alaska Native",
#'                   "Multiple", "Pacific Islander", "Other", "Unknown")[DM2$RACE]
#'
#' build_table(l, DM2)
#'
#'
#'
split_rows_by = function(lyt,
                         var,
                         labels_var = var,
                         split_label = var,
                         split_fun = NULL,
                         format = NULL,
                         nested = TRUE,
                         child_labels = c("default", "visible", "hidden"),
                         visible_label = FALSE,
                         indent_mod = 0L) {
    child_labels = match.arg(child_labels)
    spl = VarLevelSplit(var = var,
                        split_label = split_label,
                        visible_label = visible_label,
                        labels_var = labels_var,
                        split_fun = split_fun,
                        split_format = format,
                        child_labels = child_labels,
                        indent_mod = indent_mod)
    pos = next_rpos(lyt, nested)
    split_rows(lyt, spl, pos)
}



#' Associate Multiple Variables with Columns
#'
#' In some cases, the variable to be ultimately analyzed is most naturally defined on a column, not a row basis. When we
#' need columns to reflect different variables entirely, rather than different levels of a single variable, we use
#' \code{split_cols_by_multivar}
#'
#' @inheritParams lyt_args
#'
#' @export
#'
#' @seealso \code{\link{analyze_colvars}}
#' @author Gabriel Becker

split_cols_by_multivar = function(lyt,
                                  vars,
                                  varlabels = vars,
                                  nested = TRUE) {
    spl = MultiVarSplit(vars = vars, split_label = "",##split_label,
                        varlabels =varlabels)
    pos = next_cpos(lyt, nested)
    split_cols(lyt, spl, pos)
}


split_rows_by_multivar = function(lyt, vars, split_label, varlabels,
                              format = NULL,
                              nested = TRUE,
                              child_labels = c("default", "visible", "hidden"),
                              indent_mod = 0L) {
    child_labels = match.arg(child_labels)
    spl = MultiVarSplit(vars = vars, split_label = split_label, varlabels,
                        split_format = format,
                        child_labels = child_labels,
                        indent_mod = indent_mod)
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
#'
#' @author Gabriel Becker
#'
#' @examples
#' l <- basic_table() %>%
#'     split_cols_by_cuts("AGE", split_label = "Age",
#'                        cuts = c(0, 25, 35, 1000),
#'                        cutlabels = c("young", "medium", "old")) %>%
#'     analyze("RACE", afun = length)
#'
#' build_table(l, DM)
#'
split_cols_by_cuts = function(lyt, var, cuts,
                              cutlabels = NULL,
                              split_label = var,
                              nested = TRUE,
                              cumulative = FALSE) {
    spl = VarStaticCutSplit(var, split_label, cuts, cutlabels)
    if(cumulative)
        spl = as(spl, "CumulativeCutSplit")
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
                              visible_label = FALSE) {
    spl = VarStaticCutSplit(var, split_label, cuts = cuts,
                            cutlabels = cutlabels,
                            visible_label = visible_label)
    if(cumulative)
        spl = as(spl, "CumulativeCutSplit")
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
                                cumulative = FALSE,
                                indent_mod = 0L) {
    spl = VarDynCutSplit(var, split_label,
                         cutfun = cutfun,
                         cutlabelfun = cutlabelfun,
                         split_format = format,
                         extra_args = extra_args,
                         cumulative = cumulative,
                         indent_mod = indent_mod,
                         visible_label = FALSE)
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
    spl = VarDynCutSplit(var, split_label, cutfun = qtile_cuts,
                         cutlabelfun = function(x) c("[min, Q1]",
                                                   "(Q1, Q2]",
                                                   "(Q2, Q3]",
                                                   "(Q3, max]"),
                         split_format = format,
                         extra_args = extra_args,
                         cumulative = cumulative,
                         visible_label = FALSE)
    pos = next_cpos(lyt, nested)
    split_cols(lyt, spl, pos)
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
                             visible_label = FALSE) {
    spl = VarDynCutSplit(var, split_label, cutfun = qtile_cuts,
                         cutlabelfun = function(x) c("[min, Q1]",
                                                   "(Q1, Q2]",
                                                   "(Q2, Q3]",
                                                   "(Q3, max]"),
                         split_format = format,
                         child_labels = child_labels,
                         extra_args = extra_args,
                         cumulative = cumulative,
                         indent_mod = indent_mod,
                         visible_label = visible_label)
    pos = next_rpos(lyt, nested)
    split_rows(lyt, spl, pos)
}



qtile_cuts = function(x) {
    ret = quantile(x)
    levels(ret) = c("1st qrtile",
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
                                visible_label = FALSE) {
    child_labels = match.arg(child_labels)
    spl = VarDynCutSplit(var, split_label, cutfun = cutfun,
                         cutlabelfun = cutlabelfun,
                         split_format = format,
                         child_labels = child_labels,
                         extra_args = extra_args,
                         cumulative = cumulative,
                         indent_mod = indent_mod,
                         visible_label = visible_label)
    pos = next_rpos(lyt, nested)
    split_rows(lyt, spl, pos)
}


#' Generate Rows Analyzing Variables Across Columns
#'
#' Adding /analyzed variables/ to our table layout defines the primary tabulation to be performed. We do this by adding
#' calls to \code{analyze} and/or \code{\link{analyze_colvars}} into our layout pipeline. As with
#' adding further splitting, the tabulation will occur at the current/next level of nesting by default.
#'
#' @inheritParams lyt_args
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
#' }
#'
#' @note None of the arguments described in the Details section
#' can be overridden via extra_args or when calling
#' \code{\link{make_afun}}. \code{.N_col} and \code{.N_total} can
#' be overridden via the \code{col_counts} argument to
#' \code{\link{build_table}}. Alternative values for the others
#' must be calculated within \code{afun} based on a combination
#' of extra arguments and the unmodified values provided by the
#' tabulation framework.
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
#'
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
                   afun = rtab_inner,
                   var_labels = vars,
                   table_names = vars,
                   format = NULL,
                   nested = TRUE,
                   ##can we name this na_rm? symbol conflict with possible afuns!!!
                   inclNAs = FALSE,
                   extra_args = list(),
                   show_labels = c("default", "visible", "hidden"),
                   indent_mod = 0L) {

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
                          child_labels = show_labels)

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
    clyt <- clayout(lyt)
    stopifnot(length(clyt) == 1L)
    vec = clyt[[1]]
    vcls = vapply(vec, class, "")
    pos = max(which(vcls ==  "MultiVarSplit"))
    paste(c("ac", get_acolvar_vars(lyt)), collapse = "_")
}


get_acolvar_vars <- function(lyt) {
    clyt <- clayout(lyt)
    stopifnot(length(clyt) == 1L)
    vec = clyt[[1]]
    vcls = vapply(vec, class, "")
    pos = max(which(vcls ==  "MultiVarSplit"))
    spl_payload(vec[[pos]])
}


#' Generate Rows Analyzing Different Variables Across Columns
#'
#' @inheritParams  lyt_args
#'
#' @param afun function or list. Function(s) to be used to calculate the values in each column.  the list will be repped out as needed and matched by position with the columns during tabulation.
#'
#' @export
#'
#' @seealso \code{\link{split_cols_by_multivar}}
#'
#' @author Gabriel Becker
#'
#' @examples
#' ## toy example where we take the mean of the first variable and the
#' ## count of >.5 for the second.
#' colfuns <- list(function(x) rcell(mean(x), format = "xx.x"),
#'                 function(x) rcell(sum(x > .5), format = "xx"))
#'
#' l <- basic_table() %>% split_cols_by("ARM") %>%
#'     split_cols_by_multivar(c("value", "pctdiff")) %>%
#'     split_rows_by("RACE", split_label = "ethnicity", split_fun = drop_split_levels) %>%
#'     summarize_row_groups() %>%
#'     analyze_colvars(afun = colfuns)
#'
#' l
#'
#' library(dplyr)
#' ANL <- DM %>% mutate(value = rnorm(n()), pctdiff = runif(n()))
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

#' Add ref_group comparison analysis recipe
#'
#' @inheritParams lyt_args
#' @author Gabriel Becker
#' @export
#' @rdname bline_analyses
analyze_against_ref_group = function(lyt, var = NA_character_,
                                     afun,
                                     label = if(is.na(var)) "" else var,
                                     compfun = `-`,
                                     format = NULL,
                                     nested = TRUE,
                                     indent_mod = 0L,
                                     show_labels = c("default", "hidden", "visible")) {
    .Deprecated("analyze", msg = "use analyze with a function that takes .ref_group and .in_ref_col params instead.")

    show_labels = match.arg(show_labels)
    if(is.character(afun)) {
        afnm = afun
        afun = get(afun, mode = "function")
    } else {
        afnm = as.character(substitute(afun))
    }
    defrowlab = "Diff from Baseline"
    if(is.character(compfun))
        compfun = get(compfun, mode = "function")
    afun2 = function(x, .ref_group = NULL, .in_ref_col, .N_col, .N_total, ...) {
        if(is.null(.ref_group))
            stop("did not receive ref_group aggregataion value required for comparison")
        if(!is.na(var) && !.takes_df(afun))
            blinevardat = .ref_group[[var]]
        else
            blinevardat = .ref_group
        if(.in_ref_col)
            return(NULL) ## we are in the ref_group

        args = list()
        if(takes_coln(afun))
            args = c(args, list(.N_col = .N_col))
        if(takes_totn(afun))
            args = c(args, list(.N_total = .N_total))
        ##XXX totdo extras
        datvalue = do.call(afun, c(list(x), args))
        blvalue = do.call(afun, c(list(blinevardat), args))
        ## TODO(?) compfun cmight need .N_col or .N_total??
        compargs = list(datvalue, blvalue)
        ret = do.call(compfun, compargs)
        if(length(ret) == 1 && is.null(names(ret)))
            names(ret) = afnm
        ret
    }

    spl = AnalyzeVarSplit(var,
                          label,
                          afun = afun2,
                          split_format = format,
                          defrowlab = defrowlab,
                          indent_mod = indent_mod,
                          visible_label = .labelkids_helper(show_labels))
    if(nested &&
       (is(last_rowsplit(lyt), "VAnalyzeSplit") ||
        is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
        cmpnd_last_rowsplit(lyt, spl, AnalyzeMultiVars)
    } else {

        pos = next_rpos(lyt, nested, for_analyze = TRUE)
        split_rows(lyt, spl, pos)
    }
}


## Add a total column at the next **top level** spot in
## the column layout.
## TODO remove this???
#' Add Overall Column (deprecated?)
#' @description This function will \emph{only} add an overall
#' column at the \emph{top} level of splitting, NOT within
#' existing column splits.
#' See \code{\link{add_overall_level}} for the recommended
#' way to add overall columns more generally within existing splits.
#' @inheritParams lyt_args
#' @export
add_overall_col = function(lyt, label) {
    spl = AllSplit(label)
    split_cols(lyt,
                  spl,
                  next_cpos(lyt, FALSE))
}

#' Add Row Summary
#'
#' @inheritParams lyt_args
#' @rdname dot_add_row_summary
#' @export
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
#' @rdname dot_add_row_summary
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
#' @rdname dot_add_row_summary
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
#' @rdname dot_add_row_summary
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
    lbl_kids = .labelkids_helper(child_labels)
    content_fun(lyt) = cfun
    content_indent_mod(lyt) = indent_mod
    content_var(lyt) = cvar
    ##obj_format(lyt) = cformat
    content_format(lyt) <- cformat
    if(!is.na(lbl_kids) && !identical(lbl_kids, label_kids(lyt)))
        label_kids(lyt) = lbl_kids
    content_extra_args(lyt) = extra_args
    lyt
})
#' @rdname dot_add_row_summary
setMethod(".add_row_summary", "NULL",
          function(lyt,
                   label,
                   cfun,
                   child_labels = c("default", "visible", "hidden"),
                   cformat = NULL,
                   indent_mod = 0L,
                   cvar = "",
                   extra_args = list()) {

    rlyt <- PreDataRowLayout()
    rtspl <- root_spl(rlyt)

    rtspl <- .add_row_summary(lyt = rtspl,
                     label = label,
                     cfun = cfun,
                     child_labels = child_labels,
                     cformat = cformat,
                     indent_mod = indent_mod,
                     cvar = cvar,
                     extra_args = extra_args)
    root_spl(rlyt) <- rtspl
    PreDataTableLayouts(rlayout = rlyt)
})

.count_raw_constr = function(var, format, label_fstr) {
    function(df, labelstr = "") {
        label = sprintf(label_fstr, labelstr)
        if(is(df, "data.frame")) {
            if(!is.null(var) && nzchar(var))
                cnt = sum(!is.na(df[[var]]))
            else
                cnt = nrow(df)
        } else { # df is the data column vector
            cnt = sum(!is.na(df))
        }
        ret = rcell(cnt, format = format,
                    label = label)

        ## attr(ret, "format") = format
        ## names(ret) = label
        ret
    }
}

.count_wpcts_constr = function(var, format, label_fstr) {
    function(df, labelstr = "", .N_col) {
        label = sprintf(label_fstr, labelstr)
        if(is(df, "data.frame")) {
            if(!is.null(var) && nzchar(var))
                cnt = sum(!is.na(df[[var]]))
            else
                cnt = nrow(df)
        } else { # df is the data column vector
            cnt = sum(!is.na(df))
        }
        ## the formatter does the *100 so we don't here.
        ret = rcell(c(cnt, cnt/.N_col),
                    format = format,
                    label = label)

        ## attr(ret, "format") = format
        ## names(ret) = label
        ret
    }
}

.validate_cfuns <- function(fun) {
    if(is.list(fun))
        return(unlist(lapply(fun, .validate_cfuns)))

    frmls <- formals(fun)
    ls_pos <- match("labelstr", names(frmls))
    if(is.na(ls_pos)) {
        ls_pos <- grep("lbl_{0,1}str", names(frmls))
        if(length(ls_pos) == 0)
            stop("Invalid content function - does not accept the required labelstr argument")
        .Deprecated(old = "Use of content functions which do not accept a named 'labelstr' argument", new = "content functions which explicitly accept 'labelstr'")
        names(formals(fun))[ls_pos] <- "labelstr"
    }
    list(fun)
}


#' Add a content row of summary counts
#'
#' @inheritParams lyt_args
#'
#' @details if \code{format} expects 2 values (ie \code{xx} appears twice in the format string, then both raw and percent of column total counts are calculated. Otherwise only raw counts are used.
#'
#' \code{cfun} must accept \code{df} as its first argument and will receive the subset \emph{data.frame} corresponding with the row- and column-splitting for the cell being calculated. Must accept \code{labelstr} as the second parameter, which accepts the \emph{label} of the level of the parent split currently being summarized. Optionally can accept \code{.N_col} or \code{.N_total} (see \code{\link{analyze}}).
#'
#' @export
#' @author Gabriel Becker
#'
#' @examples
#' l <- basic_table() %>% split_cols_by("ARM") %>%
#'     split_rows_by("RACE") %>%
#'     summarize_row_groups(label_fstr = "%s (n)") %>%
#'     analyze("AGE", afun = list_wrap_x(summary) , format = "xx.xx")
#' l
#'
#' tbl <- build_table(l, DM)
#'
#' tbl
#'
#' summary(tbl) # summary count is a content table
#'
#'
#' ## use a cfun and extra_args to customize summarization
#' ## behavior
#' sfun <- function(x, labelstr, trim) {
#'     in_rows(
#'         c(mean(x, trim = trim), trim),
#'         .formats = "xx (xx.x%)",
#'         .labels = sprintf("%s (Trimmed mean and trim %%)",
#'                               labelstr)
#'     )
#' }
#' l2 <- basic_table() %>% split_cols_by("ARM") %>%
#'     split_rows_by("RACE") %>%
#'     summarize_row_groups("AGE", cfun = sfun,
#'                          extra_args = list(trim = .2)) %>%
#'     analyze("AGE", afun = list_wrap_x(summary) , format = "xx.xx")
#' tbl2 <- build_table(l2, DM)
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
        if(length(gregexpr("xx", format)[[1]]) == 2)
            cfun = .count_wpcts_constr(var, format, label_fstr)
        else
            cfun = .count_raw_constr(var,format, label_fstr)
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
#' summary(tbl2)
#'
add_existing_table = function(lyt, tt, indent_mod = 0) {
    indent_mod(tt) = indent_mod
    lyt = split_rows(lyt,
                        tt,
                        next_rpos(lyt, nested = FALSE))
    lyt
}


takes_coln = function(f) {
    stopifnot(is(f, "function"))
    forms = names(formals(f))
    res = ".N_col" %in% forms
    res
}

takes_totn = function(f) {
    stopifnot(is(f, "function"))
    forms = names(formals(f))
    res = ".N_total" %in% forms
    res
}


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

    ret = VarStaticCutSplit(var = var, split_label = obj_label(spl),
                      cuts = cuts, cutlabels = cutlabels)
    ## classes are tthe same structurally CumulativeCutSplit
    ## is just a sentinal so it can hit different make_subset_expr
    ## method
    if(spl_is_cmlcuts(spl))
        ret = as(ret, "CumulativeCutSplit")
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
#'   We provide both because when using the functions as tabulation functions via \code{\link{rtabulate}} or
#'   \code{\link{analyze}}, functions which take \code{df} as their first argument are passed the full subset dataframe,
#'   while those which accept anything else {notably including \code{x}} are passed only the relevant subset of the
#'   variable being analyzed.
#'
#' @rdname list_wrap
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


#' Basic starting table layout with 1 column and zero rows
#'
#'
#' @note this is represented by \code{NULL} currently
#'
#' @export
#'
#' @examples
#'
#' basic_table() %>%
#'   analyze("AGE", afun = mean) %>%
#'   build_table(DM)
#'
basic_table <- function() NULL
