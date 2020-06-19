## These functions will build up a "layout" object.
##  S4

## e.g.
##
## add_colby_tota() %>%>
##   add_colby("colname") %>%
##   add_colby_cumulcuts("colname", cuts) %>%
##   add_colby_collapse_levs("colname",
##                           list(c("lev1a", "lev1b"),
##                                c("lev2a", "lev2b", "lev2c")) %>%


##
## add_rowby_total() %>%>
##   add_rowby("colname") %>%
##   add_rowby_cumulcuts("colname", cuts) %>%
##   add_rowby_collapse_levs("colname",
##                           list(c("lev1a", "lev1b"),
##                                c("lev2a", "lev2b", "lev2c")) 

## empty_dominant_axis = function(layout) {
##     stopifnot(is(layout, "RTablesLayout"))
##     ((is(layout, "RowDominantLayout") && n_leaves(rowtree(layout)) == 0L) ||
##      (is(layout, "ColDominantLayout") && n_leaves(coltree(layout)) == 0L))

## }

# Method Definitions ----

## pre-data layouts:
## structured as lists of lists of splits
## e.g. reference table 1 would have column  proto-layout
##  list(list(VarSplit("ARM"), VarSplit("SEX")))
## and row layout
##  list(list(AllSplit()),
##       list(VarSplit("RACE"),  VarSplit("Factor2")),
##       list(AllSplit()) ## remember var3  is the variable analyzed

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
setGeneric("split_rows", function(lyt = NULL, spl, pos, cmpnd_fun = AnalyzeMultiVars) standardGeneric("split_rows"))

setMethod("split_rows", "NULL", function(lyt, spl, pos, cmpnd_fun = AnalyzeMultiVars) {
    rl = PreDataRowLayout(SplitVector(spl))
    cl = PreDataColLayout()
    PreDataTableLayouts(rlayout = rl, clayout = cl)
})

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
is_analysis_spl = function(spl) is(spl, "AnalyzeVarSplit") || is(spl, "AnalyzeMultiVars")
## note "pos" is ignored here because it is for which nest-chain
## spl should be placed in, NOIT for where in that chain it should go
setMethod("split_rows", "SplitVector",
          function(lyt, spl, pos, cmpnd_fun = AnalyzeMultiVars) {
    ## if(is_analysis_spl(spl) &&
    ##    is_analysis_spl(last_rowsplit(lyt))) {
    ##     return(cmpnd_last_rowsplit(lyt, spl, cmpnd_fun))
    ## }
    
    tmp = c(unclass(lyt), spl)
    SplitVector(lst = tmp)
})
    

setMethod("split_rows", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt = lyt@row_layout
    rlyt = split_rows(rlyt, spl, pos)
    lyt@row_layout = rlyt
    lyt
})

setMethod("split_rows", "ANY",
          function(lyt, spl, pos) stop("nope. can't add a row split to that (", class(lyt), "). contact the maintaner.")
          )


setGeneric("cmpnd_last_rowsplit", function(lyt, spl, constructor) standardGeneric("cmpnd_last_rowsplit"))

setMethod("cmpnd_last_rowsplit", "NULL", function(lyt, spl, constructor) {
    stop("no existing splits to compound with. contact the maintainer")
})

setMethod("cmpnd_last_rowsplit", "PreDataRowLayout",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    tmp = cmpnd_last_rowsplit(lyt[[pos]], spl, constructor)
    lyt[[pos]] = tmp
    lyt
})
setMethod("cmpnd_last_rowsplit", "SplitVector",
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
    

setMethod("cmpnd_last_rowsplit", "PreDataTableLayouts",
          function(lyt, spl, constructor){
    rlyt = rlayout(lyt)
    rlyt = cmpnd_last_rowsplit(rlyt, spl, constructor)
    rlayout(lyt)= rlyt
    lyt
})

setMethod("cmpnd_last_rowsplit", "ANY",
          function(lyt, spl, constructor) stop("nope. can't do cmpnd_last_rowsplit to that (", class(lyt), "). contact the maintaner.")
          )



setGeneric("split_cols", function(lyt = NULL, spl, pos) standardGeneric("split_cols"))

setMethod("split_cols", "NULL", function(lyt, spl, pos) {
    cl = PreDataColLayout(SplitVector(spl))
    rl = PreDataRowLayout()
    PreDataTableLayouts(rlayout = rl, clayout = cl)
})

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

setMethod("split_cols", "SplitVector",
          function(lyt, spl, pos) {
    tmp = c(lyt, spl)
    SplitVector(lst = tmp)
})

setMethod("split_cols", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt = lyt@col_layout
    rlyt = split_cols(rlyt, spl, pos)
    lyt@col_layout = rlyt
    lyt
})

setMethod("split_cols", "ANY",
          function(lyt, spl, pos) stop("nope. can't add a col split to that (", class(lyt), "). contact the maintaner.")
          )



setGeneric("cmpnd_last_colsplit", function(lyt, spl, constructor) standardGeneric("cmpnd_last_colsplit"))

setMethod("cmpnd_last_colsplit", "NULL", function(lyt, spl, constructor) {
    stop("no existing splits to compound with. contact the maintainer")
})

setMethod("cmpnd_last_colsplit", "PreDataColLayout",
          function(lyt, spl, constructor) {
    pos = length(lyt)
    tmp = cmpnd_last_colsplit(lyt[[pos]], spl, constructor)
    lyt[[pos]] = tmp
    lyt
})
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
    

setMethod("cmpnd_last_colsplit", "PreDataTableLayouts",
          function(lyt, spl, constructor){
    clyt = clayout(lyt)
    clyt = cmpnd_last_colsplit(clyt, spl, constructor)
    clayout(lyt)= clyt
    lyt
})

setMethod("cmpnd_last_colsplit", "ANY",
          function(lyt, spl, constructor) stop("nope. can't do cmpnd_last_colsplit to that (", class(lyt), "). contact the maintaner.")
          )



# constructors ----


# TODO: consider to remove
add_new_rowtree = function(lyt, spl) {
    split_rows(lyt, spl, next_rpos(lyt, TRUE))
}

# TODO: consider to remove
add_new_coltree = function(lyt, spl) {
    split_cols(lyt, spl, next_cpos(lyt, TRUE))
}


## Pipe-able functions to add the various types of splits to the current layout for both
## row and column.  These all act as wrappers to the split_cols and split_rows
## method stacks.


#' Declaring a column-split based on levels of a variable
#' 
#' Will generate children for each subset of a categorical variable
#' 
#' @inheritParams lyt_args
#'
#' @param baseline character(1) or NULL. Level of \code{var} which should be considered baseline/reference
#' @param incl_all logical(1). Should a column representing all observations at this level of nesting be added. defaults to \code{FALSE}
#' 
#' @export
#'
#' @author Gabriel Becker
#' @examples 
#' l <- NULL %>% split_cols_by("ARM") 
#' l
#' 
#' # add an analysis (summary)
#' l2 <- l %>% 
#'     analyze("AGE", afun = lstwrapx(summary) , fmt = "xx.xx")
#' l2
#' 
#' build_table(l2, DM)
#' 
#' # By default sequentially adding layouts results in nesting
#' l3 <- NULL %>% split_cols_by("ARM") %>%
#'   split_cols_by("SEX") %>%
#'   analyze("AGE", afun = lstwrapx(summary), fmt = "xx.xx")
#' l3
#' 
#'  build_table(l3, DM)


split_cols_by = function(lyt, var, lbl = var, vlblvar = var, splfmt = NULL, newtoplev = FALSE, lblkids = FALSE,
                         extrargs = list(), baseline = NULL, incl_all = FALSE) {
    if(is.null(baseline)) {
        spl = VarLevelSplit(var = var, splbl = lbl, vlblvar = vlblvar, splfmt = splfmt, lblkids = lblkids,
                            extrargs = extrargs)
    } else {
        spl = VarLevWBaselineSplit(var = var,
                                   baseline = baseline,
                                   incl_all = incl_all,
                                   splbl = lbl,
                                   vlblvar = vlblvar,
                                   splfmt = splfmt)
        
    }
    pos = next_cpos(lyt, newtoplev)
    split_cols(lyt, spl, pos)
}


#' Add Rows according to levels of a variable
#' 
#' 
#' @inheritParams lyt_args
#' 
#' @export
#' @author Gabriel Becker 
#' @examples 
#' 
#' # TODO: this needs to give a compressed output, the mean information would 
#' #       either go into the table header or table title
#' l <- NULL %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("RACE") %>%
#'   analyze("AGE", "Age", afun = mean, fmt = "xx.xx")
#'  
#' build_table(l, DM)
#' 
#' 
#' l <- NULL %>% split_cols_by("ARM", "Arm") %>%
#'   split_cols_by("SEX", "Gender") %>%
#'   summarize_row_groups(lbl_fstr = "Overall (N)") %>%
#'   split_rows_by("RACE", "Ethnicity") %>%
#'   summarize_row_groups("RACE", lbl_fstr = "%s (n)") %>%
#'   analyze("AGE", "Age", afun = mean, fmt = "xx.xx")
#' l
#' 
#' build_table(l, DM)
#' 
split_rows_by = function(lyt,  var, lbl = var,  vlblvar = var, splfun = NULL, fmt = NULL, newtoplev = FALSE, lblkids = NA) {
    spl = VarLevelSplit(var = var,
                        splbl = lbl,
                        vlblvar = vlblvar,
                        splfun = splfun,
                        splfmt = fmt,
                        lblkids = lblkids)
    pos = next_rpos(lyt, newtoplev)
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
#' @examples 
#' l <- NULL %>% split_cols_by("ARM", "Arm") %>%
#'   split_cols_by_multivar(c("value", "pctdiff"), "TODO Multiple Variables") %>%
#'   split_rows_by("RACE", "ethnicity") %>%
#'   analyze_colvars("", afun = mean, fmt = "xx.xx")
#' 
#' l
#'
#' library(dplyr)
#' ANL <- DM %>% mutate(value = rnorm(n()), pctdiff = runif(n()))
#' 
#'  build_table(l, ANL)
#'   
#'  
split_cols_by_multivar = function(lyt, vars, lbl, varlbls = vars,
                              newtoplev = FALSE) {
    spl = MultiVarSplit(vars = vars, splbl = lbl, varlbls)
    pos = next_cpos(lyt, newtoplev)
    split_cols(lyt, spl, pos)
}


split_rows_by_multivar = function(lyt, vars, lbl, varlbls,
                              splfmt = NULL,
                              newtoplev = FALSE,
                              lblkids = NA) {
    spl = MultiVarSplit(vars = vars, splbl = lbl, varlbls,
                        splfmt = splfmt,
                        lblkids = lblkids)
    pos = next_rpos(lyt, newtoplev)
    split_rows(lyt, spl, pos)
}

#' Split on static or dynamic cuts of the data
#'
#' Create columns (or row splits) based on values (such as quartiles) of \code{var}.
#'
#' @inheritParams lyt_args
#' @param cuts numeric. Cuts to use
#' @param cutlbls character (or NULL). Labels for the cutst
#' @param cumulative logical. Should the cuts be treated as cumulative. Defaults to \code{FALSE}
#' @param cutfun function. Function which accepts the full vector of \code{var} values and returns cut points to be passed to \code{cut}.
#' 
#'
#' @details
#' For dynamic cuts, the cut is transformed into a static cut by
#' \code{\link{build_table}} \emph{based on the full dataset}, before proceeding. Thus even when nested within another split in column/row space, the resulting split will reflect the overall vaalues (e.g., quartiles) in the dataset, NOT the values for subset  it is nested under.
#' @export
#' @rdname varcuts
#' @author Gabriel Becker
#' @examples
#' l <- NULL %>%
#' split_cols_by_cuts("AGE", lbl = "Age", cuts = c(0,25, 35, 1000), cutlbls = c("young", "medium", "old")) %>%
#' analyze("RACE", lbl ="", defrowlab="count", afun = length)
#'
#' build_table(l, DM)

split_cols_by_cuts = function(lyt, var, lbl, cuts,
                            cutlbls = NULL,
                            newtoplev = FALSE,
                            cumulative = FALSE) {
    spl = VarStaticCutSplit(var, lbl, cuts, cutlbls)
    if(cumulative)
        spl = as(spl, "CumulativeCutSplit")
    pos = next_cpos(lyt, newtoplev)
    split_cols(lyt, spl, pos)
}

#' @export
#' @rdname varcuts
split_rows_by_dyncut = function(lyt, var, lbl, cuts,
                               cutlbls = NULL,
                               splfmt = NULL,
                               newtoplev = FALSE,
                               lblkids = NA,
                               cumulative = FALSE) {
    spl = VarStaticCutSplit(var, lbl, cuts, cutlbls,
                            splfmt = splfmt,
                            lblkids = lblkids)
    if(cumulative)
        spl = as(spl, "CumulativeCutSplit")

    pos = next_rpos(lyt, newtoplev)
    split_rows(lyt, spl, pos)
}

#' @export
#' @rdname varcuts
split_cols_by_cutfun = function(lyt, var, lbl = var,
                            cutfun = qtile_cuts,
                            cutlblfun = function(x) NULL,
                            splfmt = NULL,
                            newtoplev = FALSE,
                            extrargs = list(),
                            cumulative = FALSE) {
    spl = VarDynCutSplit(var, lbl,
                         cutfun = cutfun,
                         cutlblfun = cutlblfun,
                         splfmt = splfmt,
                         extrargs = extrargs,
                         cumulative = cumulative)
    pos = next_cpos(lyt, newtoplev)
    split_cols(lyt, spl, pos)
}

#' @export
#' @rdname varcuts
split_cols_by_quartiles = function(lyt, var, lbl = var,
                             splfmt = NULL,
                             newtoplev = FALSE,
                             extrargs = list(),
                             cumulative = FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun = qtile_cuts,
                         cutlblfun = function(x) c("[min, Q1]",
                                                   "(Q1, Q2]",
                                                   "(Q2, Q3]",
                                                   "(Q3, max]"),
                         splfmt = splfmt,
                         extrargs = extrargs,
                         cumulative = cumulative)
    pos = next_cpos(lyt, newtoplev)
    split_cols(lyt, spl, pos)
}


#' @export
#' @rdname varcuts
split_rows_by_quartiles = function(lyt, var, lbl = var,
                             splfmt = NULL,
                             newtoplev = FALSE,
                             extrargs = list(),
                             cumulative= FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun = qtile_cuts,
                         cutlblfun = function(x) c("[min, Q1]",
                                                   "(Q1, Q2]",
                                                   "(Q2, Q3]",
                                                   "(Q3, max]"),
                         splfmt = splfmt,
                         extrargs = extrargs,
                         cumulative = cumulative)
    pos = next_rpos(lyt, newtoplev)
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
split_rows_by_cutfun = function(lyt, var, lbl = var,
                            cutfun = qtile_cuts,
                            splfmt = NULL,
                            newtoplev = FALSE,
                            lblkids = NA,
                            cumulative = FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun = cutfun,
                         splfmt = splfmt,
                         lblkids = lblkids,
                         cumulative = cumulative)
    pos = next_rpos(lyt, newtoplev)
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
#' @export
#' @author Gabriel Becker
#' @examples 
#' 
#' l <- NULL %>% split_cols_by("ARM") %>% 
#'     analyze("AGE", afun = lstwrapx(summary) , fmt = "xx.xx")
#' l
#' 
#' build_table(l, DM)
#' 
#' 
#' l <- NULL %>% split_cols_by("Species") %>%
#'     analyze(head(names(iris), -1), afun = function(x) {
#'        list(
#'            "mean / sd" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
#'            "range" = rcell(diff(range(x)), format = "xx.xx")
#'        )
#'     })
#' l
#' build_table(l, iris)
#' 
analyze = function(lyt,
                             var,
                             lbl = var,
                             afun,
                             fmt = NULL,
                             defrowlab = "",
                             newtoplev = FALSE,
                             inclNAs = FALSE,
                             extrargs = list(),
                             lblkids = TRUE) {

    subafun = substitute(afun)
    if(is.name(subafun) &&
       missing(defrowlab) &&
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
    }
    spl = AnalyzeMultiVars(var, lbl,
                          afun = afun,
                          splfmt = fmt,
                          defrowlab = defrowlab,
                          inclNAs = inclNAs,
                          extrargs = extrargs)
 
    if(!newtoplev &&
       (is(last_rowsplit(lyt), "AnalyzeVarSplit") ||
        is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
        cmpnd_last_rowsplit(lyt, spl, AnalyzeMultiVars)
    } else {
    ## analysis compounding now done in split_rows
        pos = next_rpos(lyt, newtoplev)
        split_rows(lyt, spl, pos)
    }
}



get_acolvar_name  <- function(lyt) {
    clyt <- clayout(lyt)
    stopifnot(length(clyt) == 1L)
    vec = clyt[[1]]
    vcls = vapply(vec, class, "")
    pos = max(which(vcls ==  "MultiVarSplit"))
    paste(c("ac", spl_payload(vec[[pos]])), collapse = "_")
}

#' Generate Rows Analyzing Different Variables Across Columns
#' 
#' TODO here indicates that the variables whose data are ultimately processed by afun are specified at the highest-depth
#' level of nesting in the column structure, rather than at the row level.
#' 
#' @inheritParams  lyt_args
#' 
#' @export
#' 
#' @seealso \code{\link{split_cols_by_multivar}}
#' 
#' @author Gabriel Becker
#' @examples 
#' 
#' l <- NULL %>% split_cols_by("ARM", "Arm") %>%
#'   split_cols_by_multivar(c("value", "pctdiff"), "TODO Multiple Variables") %>%
#'   split_rows_by("RACE", "ethnicity") %>%
#'   analyze_colvars("", afun = mean, fmt = "xx.xx")
#' 
#' l
#'
#' library(dplyr)
#' ANL <- DM %>% mutate(value = rnorm(n()), pctdiff = runif(n()))
#' 
#' # TODO: fix
#' build_table(l, ANL)
#' 
analyze_colvars = function(lyt, lbl, afun,
                                fmt = NULL,
                                newtoplev = FALSE) {
    spl = AnalyzeVarSplit(NA_character_, lbl, afun = afun,
                          defrowlab = lbl,
                          splfmt = fmt,
                          splname = get_acolvar_name(lyt))
    pos = next_rpos(lyt, newtoplev)
    split_rows(lyt, spl, pos)


}

#' Add baseline comparison analysis recipe
#'
#' @inheritParams lyt_args
#' @author Gabriel Becker
#' @export
#' @rdname bline_analyses
analyze_against_baseline = function(lyt, var = NA_character_, lbl = "", afun,
                                   compfun = `-`,
                                  fmt = NULL,
                                  defrowlab = "Diff from Baseline",
                                  newtoplev = FALSE) {
    if(is.character(afun)) {
        afnm = afun
        afun = get(afun, mode = "function")
    } else {
        afnm = as.character(substitute(afun))
    }
    
    if(is.character(compfun))
        compfun = get(compfun, mode = "function")
    afun2 = function(x, .baseline_data = NULL, .N_col, .N_total, ...) {
        if(is.null(.baseline_data))
            stop("did not receive baseline aggregataion value required for comparison")
        if(!is.na(var) && !.takes_df(afun))
            blinevardat = .baseline_data[[var]]
        else
            blinevardat = .baseline_data
        if(identical(x, blinevardat))
            return(NULL) ## we are in the baseline
        
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

    spl = AnalyzeVarSplit(var, lbl, afun = afun2,
                          splfmt = fmt, defrowlab = defrowlab)
    if(!newtoplev &&
       (is(last_rowsplit(lyt), "AnalyzeVarSplit") ||
        is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
        cmpnd_last_rowsplit(lyt, spl, AnalyzeMultiVars)
    } else {
        
        pos = next_rpos(lyt, newtoplev)
        split_rows(lyt, spl, pos)
    }
}

#' @export
#' @rdname bline_analyses
analyze_against_baseline_2dtable = function(lyt,
                                 var = NA_character_,
                                 lbl = var,
                                 compfun,
                                 fmt = NULL,
                                 newtoplev = FALSE) {

    if(is.character(compfun)) {
        cfnm = compfun
        compfun = get(compfun, mode = "function")
    } else {
        cfnm = as.character(substitute(compfun))
    }
    

    
    compfun2 = function(colvardat, blinevardat) {
        if(identical(colvardat, blinevardat))
            return(NULL)
            
        ## TODO(?) compfun cmight need .N_col or .N_total??
        tab = .make_2xk_tab(colvardat, blinevardat)
        ret = compfun(tab)
        if(length(ret) == 1 && is.null(names(ret)))
            names(ret) = cfnm
        ret
    }
    analyze_against_baseline(lyt = lyt, var = var, lbl = lbl,
                           afun = function(x) x,
                           compfun = compfun2,
                           fmt = fmt,
                           newtoplev = newtoplev)
}


## Add a total column at the next **top level** spot in
## the column layout.
#' @export
add_overall_col = function(lyt, lbl) {
    spl = AllSplit(lbl)
    split_cols(lyt,
                  spl,
                  next_cpos(lyt, TRUE))
}

#' @rdname .add_row_summary
#' @export
setGeneric(".add_row_summary",
           function(lyt, lbl, cfun, lblkids = NA, cfmt = NULL) standardGeneric(".add_row_summary"))
setMethod(".add_row_summary", "PreDataTableLayouts",
          function(lyt, lbl, cfun, lblkids = NA, cfmt = NULL) {
    tmp = .add_row_summary(rlayout(lyt), lbl, cfun,
                      lblkids = lblkids,
                      cfmt = cfmt)
    rlayout(lyt) = tmp
    lyt
})

setMethod(".add_row_summary", "PreDataRowLayout",
          function(lyt, lbl, cfun, lblkids = NA, cfmt = NULL) {
    if(length(lyt) == 0 ||
       (length(lyt) == 1 && length(lyt[[1]]) == 0)) {
        rt = root_spl(lyt)
        rt = .add_row_summary(rt, lbl, cfun, lblkids = lblkids, cfmt = cfmt)
        root_spl(lyt) = rt
    } else {
        ind = length(lyt)
        tmp = .add_row_summary(lyt[[ind]], lbl, cfun,
                          lblkids = lblkids,
                          cfmt = cfmt)
        lyt[[ind]] = tmp
    }
    lyt
})

setMethod(".add_row_summary", "SplitVector",
          function(lyt, lbl, cfun, lblkids = NA, cfmt = NULL) {
    ind = length(lyt)
    if(ind == 0) stop("no split to add content rows at")
    spl = lyt[[ind]]
    ## if(is(spl, "AnalyzeVarSplit")) stop("can't add content rows to analyze variable split")
    tmp = .add_row_summary(spl, lbl, cfun, lblkids = lblkids, cfmt = cfmt)
    lyt[[ind]] = tmp
    lyt
})

setMethod(".add_row_summary", "Split",
          function(lyt, lbl, cfun, lblkids = NA, cfmt = NULL) {
    content_fun(lyt) = cfun
    obj_fmt(lyt) = cfmt
    if(!is.na(lblkids) && !identical(lblkids, label_kids(lyt)))
        label_kids(lyt) = lblkids
    lyt
})

.count_raw_constr = function(var, fmt, lbl_fstr) {
    function(df, lblstr = "") {
        lbl = sprintf(lbl_fstr, lblstr)
        if(!is.null(var))
            cnt = sum(!is.na(df[[var]]))
        else
            cnt = nrow(df)

        ret = rcell(cnt, format = fmt,
                    lbl = lbl)
        
        ## attr(ret, "format") = fmt
        ## names(ret) = lbl
        ret
    }
}

.count_wpcts_constr = function(var, fmt, lbl_fstr) {
    function(df, lblstr = "", .N_col) {
        lbl = sprintf(lbl_fstr, lblstr)
        if(!is.null(var))
            cnt = sum(!is.na(df[[var]]))
        else
            cnt = nrow(df)

        ## the formatter does the *100 so we don't here.
        ret = rcell(c(cnt, cnt/.N_col),
                    format = fmt,
                    lbl = lbl)
        
        ## attr(ret, "format") = fmt
        ## names(ret) = lbl
        ret
    }
}




#' Add a content row of summary counts
#' 
#' @inheritParams lyt_args
#' 
#' @details if \code{fmt} expects 2 values (ie \code{xx} appears twice in the format string, then both raw and percent of column total counts are calculated. Otherwise only raw counts are used.
#' 
#' @export
#' @author Gabriel Becker
#' 
#' @examples 
#' l <- NULL %>% split_cols_by("ARM") %>% 
#'     split_rows_by("RACE") %>% 
#'     summarize_row_groups(lbl_fstr = "%s (n)") %>% 
#'     analyze("AGE", afun = lstwrapx(summary) , fmt = "xx.xx")
#' l
#' 
#' tbl <- build_table(l, DM)
#' 
#' tbl
#' 
#' summary(tbl) # summary count is a content table
#' 
summarize_row_groups = function(lyt, var = NULL, lbl_fstr = "%s", fmt = "xx (xx.x%)", cfun = NULL ){

    if(is.null(cfun)) {
        if(length(gregexpr("xx", fmt)[[1]]) == 2)
            cfun = .count_wpcts_constr(var, fmt, lbl_fstr)
        else
            cfun = .count_raw_constr(var,fmt, lbl_fstr)
    }
    .add_row_summary(lyt,# lbl = lbl,
                cfun = cfun, cfmt = fmt)
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
#' l <- NULL %>% split_cols_by("ARM") %>% 
#'     add_colcounts() %>% 
#'     split_rows_by("RACE") %>% 
#'     analyze("AGE", afun = function(x) list(min = min(x), max = max(x)))
#' l
#' 
#' build_table(l, DM)
#' 
add_colcounts = function(lyt, fmt = "(N=xx)") {
    disp_ccounts(lyt) = TRUE
    colcount_fmt(lyt) = fmt
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
#' tbl1 <- NULL %>%
#'    split_cols_by("ARM") %>%
#'    analyze("AGE", afun = mean, fmt = "xx.xx") %>%
#'    build_table(DM)
#' 
#' tbl1
#' 
#' tbl2 <- NULL %>% split_cols_by("ARM") %>%
#'    analyze("AGE", afun = sd, fmt = "xx.xx") %>%
#'    add_existing_table(tbl1) %>%
#'    build_table(DM)
#' 
#' summary(tbl2)
#' 
add_existing_table = function(lyt, tt) {
    lyt = split_rows(lyt,
                        tt,
                        next_rpos(lyt, TRUE))
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
setGeneric("fix_dyncuts", function(spl, df) standardGeneric("fix_dyncuts"))

setMethod("fix_dyncuts", "Split", function(spl, df) spl)
setMethod("fix_dyncuts", "VarDynCutSplit",
          function(spl, df) {

    var = spl_payload(spl)
    varvec = df[[var]]

    cfun = spl_cutfun(spl)
    cuts = cfun(varvec)
    cutlbls <- spl_cutlblfun(spl)(cuts)
    if(length(cutlbls) != length(cuts) - 1  &&
       !is.null(names(cuts))) {
        cutlbls <- names(cuts)[-1]
    }
    
    ret = VarStaticCutSplit(var = var, splbl = obj_label(spl),
                      cuts = cuts, cutlbls = cutlbls)
    ## classes are tthe same structurally CumulativeCutSplit
    ## is just a sentinal so it can hit different make_subset_expr
    ## method
    if(spl_is_cmlcuts(spl))
        ret = as(ret, "CumulativeCutSplit")
    ret
})

setMethod("fix_dyncuts", "VTableTree",
          function(spl, df) spl)


.fd_helper = function(spl, df) {
    lst = lapply(spl, fix_dyncuts, df = df)
    as(lst, class(spl))
}
       
setMethod("fix_dyncuts", "PreDataRowLayout",
          function(spl, df) {
    rt = root_spl(spl)
    ret = .fd_helper(spl, df)
    root_spl(ret) = rt
    ret
})

setMethod("fix_dyncuts", "PreDataColLayout",
          function(spl, df) {
    rt = root_spl(spl)
    ret = .fd_helper(spl, df)
    root_spl(ret) = rt
    disp_ccounts(ret) = disp_ccounts(spl)
    ret
})

setMethod("fix_dyncuts", "SplitVector",
          function(spl, df) {
    .fd_helper(spl, df)
})


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
    
    splvec = SplitVector(lst = mapply(ManualSplit, levels = .lst, lbl = names(.lst)))
    ctree = splitvec_to_coltree(data.frame(), splvec=splvec, pos = TreePos())
    InstantiatedColumnInfo(treelyt = ctree)
}


#' Returns a function that coerces the return values of f to a list
#'
#' @param f The function to wrap.
#' @export
#'
#' @details \code{lstwrapx} generates a wrapper which takes \code{x} as its first argument, while \code{lstwrapdf} generates an otherwise identical wrapper function whose first argument is named \code{df}.
#'
#' We provide both because when using the functions as tabulation functions via \code{\link{rtabulate}} or \code{\link{analyze}}, functions which take \code{df} as their first argument are passed the full subset dataframe, while those which accept anything else {notably including \code{x}} are passed only the relevant subset of the variable being analyzed.
#' 
#' @rdname lstwrap
#' @author Gabriel Becker
#' @examples 
#' 
#' summary(iris$Sepal.Length)
#' 
#' f <- lstwrapx(summary)
#' f(iris$Sepal.Length)
lstwrapx = function(f) {
    function(x,...) {
        vs = as.list(f(x,...))
        ret = mapply(function(v, nm) {
            rcell(v, lbl = nm)
        },
        v = vs,
        nm = names(vs))
        ret
        
    }
}

#' @rdname lstwrap
#' @export
lstwrapdf = function(f) {
    function(df,...) {
        vs = as.list(f(df,...))
        ret = mapply(function(v, nm) {
            rcell(v, lbl = nm)
        },
        v = vs,
        nm = names(vs))
        ret
    }
}

