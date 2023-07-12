## Generics and how they are used directly

## check_validsplit - Check if the split is valid for the data, error if not

## .apply_spl_extras - Generate Extras

## .apply_spl_datapart - generate data partition

## .apply_spl_rawvals - Generate raw (i.e. non SplitValue object) partition values



setGeneric(".applysplit_rawvals",
           function(spl, df) standardGeneric(".applysplit_rawvals"))

setGeneric(".applysplit_datapart",
           function(spl, df, vals) standardGeneric(".applysplit_datapart"))

setGeneric(".applysplit_extras",
           function(spl, df, vals) standardGeneric(".applysplit_extras"))

setGeneric(".applysplit_partlabels",
           function(spl, df, vals, labels) standardGeneric(".applysplit_partlabels"))

setGeneric("check_validsplit",
           function(spl, df) standardGeneric("check_validsplit"))

setGeneric(".applysplit_ref_vals",
          function(spl, df, vals) standardGeneric(".applysplit_ref_vals"))

#' @name custom_split_funs
#' @rdname custom_split_funs
#' @title Custom Split Functions
#'
#' @description Split functions provide the work-horse for `rtables`'s
#'   generalized partitioning. These functions accept a (sub)set of incoming
#'   data, a split object, and return 'splits' of that data.
#'
#' @section Custom Splitting Function Details:
#'
#' User-defined custom split functions can perform any type of computation on
#' the incoming data provided that they meet the contract for generating
#' 'splits' of the incoming data 'based on' the split object.
#'
#' Split functions are functions that accept:
#' \describe{
#' \item{df}{data.frame of incoming data to be split}
#' \item{spl}{a Split object. this is largely an internal detail custom
#' functions will not need to worry about, but  \code{obj_name(spl)}, for
#' example, will give the name of the split as it will appear in paths in the
#' resulting table}
#' \item{vals}{Any pre-calculated values. If given non-null values, the values
#' returned should match these. Should be NULL in most cases and can likely be
#' ignored}
#' \item{labels}{Any pre-calculated value labels. Same as above for
#' \code{values}}
#' \item{trim}{If \code{TRUE}, resulting splits that are empty should be
#' removed}
#' \item{(Optional) .spl_context}{a data.frame describing previously performed
#' splits which collectively arrived at \code{df}}
#' }
#'
#' The function must then output a \code{named list} with the following
#' elements:
#'
#' \describe{
#' \item{values}{The vector of all values corresponding to the splits of
#' \code{df}}
#' \item{datasplit}{a list of data.frames representing the groupings of the
#' actual observations from \code{df}.}
#' \item{labels}{a character vector giving a string label for each value listed
#' in the \code{values} element above}
#' \item{(Optional) extras}{If present, extra arguments are to be passed to summary
#' and analysis functions whenever they are executed on the corresponding
#' element of \code{datasplit} or a subset thereof}
#' }
#'
#' One way to generate custom splitting functions is to wrap existing split
#' functions and modify either the incoming data before they are called or
#' their outputs.
#'
#' @seealso [make_split_fun()] for the API for creating custom split functions,
#' and [split_funcs] for a variety of pre-defined split functions.
#'
#' @examples
#' # Example of a picky split function. The number of values in the column variable
#' # var decrees if we are going to print also the column with all observation
#' # or not.
#'
#' picky_splitter <- function(var) {
#'   # Main layout function
#'   function(df, spl, vals, labels, trim) {
#'     orig_vals <- vals
#'
#'     # Check for number of levels if all are selected
#'     if (is.null(vals)) {
#'       vec <- df[[var]]
#'       vals <- unique(vec)
#'     }
#'
#'     # Do a split with or without All obs
#'     if (length(vals) == 1) {
#'       do_base_split(spl = spl, df = df, vals = vals, labels = labels, trim = trim)
#'     } else {
#'       fnc_tmp <- add_overall_level("Overall", label = "All Obs", first = FALSE)
#'       fnc_tmp(df = df, spl = spl, vals = orig_vals, trim = trim)
#'     }
#'   }
#' }
#'
#' # Data sub-set
#' d1 <- subset(ex_adsl, ARM == "A: Drug X" | (ARM == "B: Placebo" & SEX == "F"))
#' d1 <- subset(d1, SEX %in% c("M", "F"))
#' d1$SEX <- factor(d1$SEX)
#'
#' # This table uses the number of values in the SEX column to add the overall col or not
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM", split_fun = drop_split_levels) %>%
#'   split_cols_by("SEX", split_fun = picky_splitter("SEX")) %>%
#'   analyze("AGE", show_labels = "visible")
#' tbl <- build_table(lyt, d1)
#' tbl
#'
NULL


## do various cleaning, and naming, plus
## ensure partinfo$values contains SplitValue objects only
.fixupvals <- function(partinfo) {
    if(is.factor(partinfo$labels))
        partinfo$labels <- as.character(partinfo$labels)

    vals <- partinfo$values
    if(is.factor(vals))
        vals <- levels(vals)[vals]
    extr <- partinfo$extras
    dpart <- partinfo$datasplit
    labels <- partinfo$labels
    if(is.null(labels)) {
        if(!is.null(names(vals)))
            labels <- names(vals)
        else if(!is.null(names(dpart)))
            labels <- names(dpart)
        else if (!is.null(names(extr)))
            labels <- names(extr)
    }

    if(is.null(vals) && !is.null(extr))
        vals <- seq_along(extr)

    if(length(vals) == 0) {
        stopifnot(length(extr) == 0)
        return(partinfo)
    }
    ## length(vals) > 0 from here down

    if(are(vals, "SplitValue") && !are(vals, "LevelComboSplitValue")) {
        if(!is.null(extr)) {
            warning("Got a partinfo list with values that are ",
                    "already SplitValue objects and non-null extras ",
                    "element. This shouldn't happen")
        }
    } else {
        if(is.null(extr))
            extr <- rep(list(list()), length(vals))
        vals <- make_splvalue_vec(vals, extr, labels = labels)
    }
    ## we're done with this so take it off
    partinfo$extras <- NULL

    vnames <- value_names(vals)
    names(vals) <- vnames
    partinfo$values <- vals

    if(!identical(names(dpart), vnames)) {
        names(dpart) <- vnames
        partinfo$datasplit <- dpart
    }


    partinfo$labels <- labels

    stopifnot(length(unique(sapply(partinfo, NROW))) == 1)
    partinfo
}

.add_ref_extras <- function(spl, df, partinfo) {
    ## this is only the .in_ref_col booleans
    refvals <- .applysplit_ref_vals(spl, df, partinfo$values)
    ref_ind <- which(unlist(refvals))
    stopifnot(length(ref_ind) == 1)

    vnames <- value_names(partinfo$values)
    if(is.null(partinfo$extras)) {
        names(refvals) <- vnames
        partinfo$extras <- refvals
    } else {
        newextras <- mapply(function(old, incol, ref_full) {
            c(old, list(.in_ref_col = incol,
                        .ref_full = ref_full))
            },
            old = partinfo$extras,
            incol = unlist(refvals),
            MoreArgs = list(ref_full = partinfo$datasplit[[ref_ind]]),
                            SIMPLIFY = FALSE)
        names(newextras) <- vnames
        partinfo$extras <- newextras
    }
    partinfo
}

func_takes <- function(fun, argname, truefordots = FALSE) {
    fnames <- names(formals(fun))
    argname %in% fnames || (truefordots && "..." %in% fnames)
}

#' Apply Basic Split (For Use In Custom Split Functions)
#'
#' This function is intended for use inside custom split functions. It applies
#' the current split \emph{as if it had no custom splitting function} so that
#' those default splits can be further manipulated.
#'
#' @inheritParams gen_args
#' @param vals ANY. Already calculated/known values of the split. Generally
#'   should be left as \code{NULL}.
#' @param labels character. Labels associated with \code{vals}. Should be
#'   \code{NULL} when \code{vals} is, which should almost always be the case.
#' @param trim logical(1). Should groups corresponding to empty data subsets be
#'   removed. Defaults to \code{FALSE}.
#'
#' @return the result of the split being applied as if it had no custom split
#'   function, see \code{\link{custom_split_funs}}
#'
#' @export
#' @examples
#'
#' uneven_splfun <- function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
#'     ret <- do_base_split(spl, df, vals, labels, trim)
#'     if(NROW(df) == 0)
#'         ret <- lapply(ret, function(x) x[1])
#'     ret
#' }
#'
#' lyt <- basic_table() %>%
#'     split_cols_by("ARM") %>%
#'     split_cols_by_multivar(c("USUBJID", "AESEQ", "BMRKR1"),
#'                            varlabels = c("N", "E", "BMR1"),
#'                            split_fun = uneven_splfun) %>%
#'     analyze_colvars(list(USUBJID = function(x, ...) length(unique(x)),
#'                          AESEQ = max,
#'                          BMRKR1 = mean))
#'
#' tbl <- build_table(lyt, subset(ex_adae, as.numeric(ARM) <= 2))
#' tbl
do_base_split <- function(spl, df, vals = NULL, labels = NULL, trim = FALSE) {
    spl2 <- spl
    split_fun(spl2) <- NULL
    do_split(spl2, df = df, vals = vals, labels = labels, trim = trim,
             spl_context = NULL)
}


### NB This is called at EACH level of recursive splitting
do_split <- function(spl,
                     df,
                     vals = NULL,
                     labels = NULL,
                     trim = FALSE,
                     spl_context) {
    ## this will error if, e.g., df doesn't have columns
    ## required by spl, or generally any time the spl
    ## can't be applied to df
    check_validsplit(spl, df)
    ## note the <- here!!!
    if(!is.null(splfun <- split_fun(spl))) {
        ## Currently the contract is that split_functions take df, vals, labels and
        ## return list(values=., datasplit=., labels = .), optionally with
        ## an additional extras element
        if(func_takes(splfun, ".spl_context")) {
            ret <- tryCatch(splfun(df, spl, vals, labels, trim = trim,
                                   .spl_context = spl_context),
                            error = function(e) e) ## rawvalues(spl_context ))
        } else {
            ret <- tryCatch(splfun(df, spl, vals, labels, trim = trim),
                            error = function(e) e)
        }
        if(is(ret, "error")) {
            stop("Error applying custom split function: ", ret$message, "\n\tsplit: ",
                 class(spl), " (", payloadmsg(spl), ")\n",
                 "\toccured at path: ",
                 spl_context_to_disp_path(spl_context), "\n")
        }
    } else {
        ret <- .apply_split_inner(df = df, spl = spl, vals = vals, labels = labels, trim = trim)
    }

    ## this adds .ref_full and .in_ref_col
    if(is(spl, "VarLevWBaselineSplit"))
        ret <- .add_ref_extras(spl, df, ret)

    ## this:
    ## - guarantees that ret$values contains SplitValue objects
    ## - removes the extras element since its redundant after the above
    ## - Ensures datasplit and values lists are named according to labels
    ## - ensures labels are character not factor
    ret <- .fixupvals(ret)

    ret
}

.apply_split_inner <- function(spl, df, vals = NULL, labels = NULL, trim = FALSE) {

    if(is.null(vals))
        vals <- .applysplit_rawvals(spl, df)
    extr <- .applysplit_extras(spl, df, vals)

    if(is.null(vals)) {
        return(list(values = list(),
                    datasplit = list(),
                    labels = list(),
                    extras = list()))
    }

    dpart <- .applysplit_datapart(spl, df, vals)

    if(is.null(labels))
        labels <- .applysplit_partlabels(spl, df, vals, labels)
    else
        stopifnot(names(labels) == names(vals))
    ## get rid of columns that would not have any
    ## observations.
    ##
    ## But only if there were any rows to start with
    ## if not we're in a manually constructed table
    ## column tree
    if(trim) {
        hasdata <- sapply(dpart, function(x) nrow(x) > 0)
        if(nrow(df) > 0 && length(dpart) > sum(hasdata)) { #some empties
            dpart <- dpart[hasdata]
            vals <- vals[hasdata]
            extr <- extr[hasdata]
            labels <- labels[hasdata]
        }
    }

    if(is.null(spl_child_order(spl)) || is(spl, "AllSplit")) {
        vord <- seq_along(vals)
    } else {
        vord <- match(spl_child_order(spl),
                     vals)
        vord <- vord[!is.na(vord)]
    }


    ## FIXME: should be an S4 object, not a list
    ret <- list(values = vals[vord],
               datasplit = dpart[vord],
               labels = labels[vord],
               extras = extr[vord])
    ret
}


.checkvarsok <- function(spl, df) {

    vars <- spl_payload(spl)
    ## could be multiple vars in the future?
    ## no reason not to make that work here now.
    if(!all(vars %in% names(df)))
        stop(" variable(s) [",
             paste(setdiff(vars, names(df)),
                   collapse = ", "),
             "] not present in data. (",
             class(spl), ")")
    invisible(NULL)

}

### Methods to verify a split appears to be valid, applicable
### to the ***current subset*** of the df.
###
### This is called at each level of recursive splitting so
### do NOT make it check, e.g., if the ref_group level of
### a factor is present in the data, because it may not be.



setMethod("check_validsplit", "VarLevelSplit",
          function(spl, df) {
    .checkvarsok(spl, df)
})


setMethod("check_validsplit", "MultiVarSplit",

          function(spl, df) {
    .checkvarsok(spl, df)
})

setMethod("check_validsplit", "VAnalyzeSplit",

          function(spl, df) {
    if(!is.na(spl_payload(spl))) {
        .checkvarsok(spl, df)
    } else {
        TRUE
    }
})

setMethod("check_validsplit", "CompoundSplit",
          function(spl, df) {
    all(sapply(spl_payload(spl), df))
})




## default does nothing, add methods as they become
## required
setMethod("check_validsplit", "Split",
          function(spl, df)
    invisible(NULL))



setMethod(".applysplit_rawvals", "VarLevelSplit",
          function(spl, df) {
    varvec <- df[[spl_payload(spl)]]
    if(is.factor(varvec))
        levels(varvec)
    else
        unique(varvec)
})

setMethod(".applysplit_rawvals", "MultiVarSplit",
          function(spl, df) {
##    spl_payload(spl)
    spl_varnames(spl)
})

setMethod(".applysplit_rawvals", "AllSplit",
          function(spl, df) obj_name(spl)) #"all obs")

setMethod(".applysplit_rawvals", "ManualSplit",
          function(spl, df) spl@levels)


## setMethod(".applysplit_rawvals", "NULLSplit",
##           function(spl, df) "")

setMethod(".applysplit_rawvals", "VAnalyzeSplit",
          function(spl, df) spl_payload(spl))


## formfactor here is gross we're gonna have ot do this
## all again in tthe data split part :-/
setMethod(".applysplit_rawvals", "VarStaticCutSplit",
          function(spl, df) {
    spl_cutlabels(spl)
})


setMethod(".applysplit_datapart", "VarLevelSplit",
          function(spl, df, vals) {
    if(!(spl_payload(spl) %in% names(df))) {
        stop("Attempted to split on values of column (", spl_payload(spl),
             ") not present in the data")
    }
    ret <- lapply(seq_along(vals), function(i) {
        df[df[[spl_payload(spl)]] == vals[[i]], ]
    })
    names(ret) <- as.character(vals)
    ret
})


setMethod(".applysplit_datapart", "MultiVarSplit",
          function(spl, df, vals) {
    allvnms <- spl_varnames(spl)
    if(!is.null(vals) && !identical(allvnms, vals)) {
        incl <- match(vals, allvnms)
    } else {
        incl <- seq_along(allvnms)
    }
    vars <- spl_payload(spl)[incl]
    ## don't remove  nas
    ## ret = lapply(vars, function(cl) {
    ##     df[!is.na(df[[cl]]),]
    ## })
    ret <- rep(list(df), length(vars))
    names(ret) <- vals
    ret
})

setMethod(".applysplit_datapart", "AllSplit",
          function(spl, df, vals) list(df))

## ## not sure I need this
setMethod(".applysplit_datapart", "ManualSplit",
          function(spl, df, vals) rep(list(df), times = length(vals)))



## setMethod(".applysplit_datapart", "NULLSplit",
##           function(spl, df, vals) list(df[FALSE,]))


setMethod(".applysplit_datapart", "VarStaticCutSplit",
          function(spl, df, vals) {
  #  lbs = spl_cutlabels(spl)
    var <- spl_payload(spl)
    varvec <- df[[var]]
    cts <- spl_cuts(spl)
    cfct <- cut(varvec, cts, include.lowest = TRUE)#, labels = lbs)
    split(df, cfct, drop = FALSE)
})


setMethod(".applysplit_datapart", "CumulativeCutSplit",
          function(spl, df, vals) {
  #  lbs = spl_cutlabels(spl)
    var <- spl_payload(spl)
    varvec <- df[[var]]
    cts <- spl_cuts(spl)
    cfct <- cut(varvec, cts, include.lowest = TRUE)#, labels = lbs)
    ret <- lapply(seq_len(length(levels(cfct))),
                  function(i) df[as.integer(cfct) <= i, ])
    names(ret) <- levels(cfct)
    ret
})

## XXX TODO *CutSplit Methods


setClass("NullSentinel", contains = "NULL")
nullsentinel <- new("NullSentinel")
noarg <- function() nullsentinel

## Extras generation methods
setMethod(".applysplit_extras", "Split",
          function(spl, df, vals) {
    splex <- split_exargs(spl)
    nvals <- length(vals)
    lapply(seq_len(nvals), function(vpos) {
        one_ex <- lapply(splex, function(arg) {
            if(length(arg) >= vpos)
                arg[[vpos]]
            else
                noarg()
            })
        names(one_ex) <- names(splex)
        one_ex <- one_ex[!sapply(one_ex, is, "NullSentinel")]
        one_ex
    })

})



setMethod(".applysplit_ref_vals", "Split",
          function(spl, df, vals) rep(list(NULL), length(vals)))

setMethod(".applysplit_ref_vals", "VarLevWBaselineSplit",
          function(spl, df, vals) {
    bl_level <- spl@ref_group_value #XXX XXX
    vnames <- value_names(vals)
    ret <- lapply(vnames, function(vl) {
        list(.in_ref_col = vl == bl_level)
    })
    names(ret) <- vnames
    ret
})

## XXX TODO FIXME
setMethod(".applysplit_partlabels", "Split",
          function(spl, df, vals, labels) as.character(vals))

setMethod(".applysplit_partlabels", "VarLevelSplit",
          function(spl, df, vals, labels) {

    varname <- spl_payload(spl)
    vlabelname <- spl_labelvar(spl)
    varvec <- df[[varname]]
    ## we used to check if vals was NULL but
    ## this is called after a short-circuit return in .apply_split_inner in that
    ## case
    ## so vals is guaranteed to be non-null here
    if(is.null(labels)) {
        if(varname == vlabelname) {
            labels <- vals
        } else {
            labfact <- is.factor(df[[vlabelname]])
            lablevs <- if(labfact) levels(df[[vlabelname]]) else NULL
            labels <- sapply(vals, function(v) {
                vlabel <- unique(df[varvec == v,
                                   vlabelname, drop = TRUE])
                ## TODO remove this once 1-to-1 value-label map is enforced
                ## elsewhere.
                stopifnot(length(vlabel) < 2)
                if(length(vlabel) == 0)
                vlabel <- ""
                else if(labfact)
                    vlabel <- lablevs[vlabel]
                vlabel
            })
        }
    }
    names(labels) <- as.character(vals)
    labels
})

setMethod(".applysplit_partlabels", "MultiVarSplit",
          function(spl, df, vals, labels) value_labels(spl))


make_splvalue_vec <- function(vals, extrs = list(list()), labels = vals) {
    if(length(vals) == 0)
        return(vals)

    if(is(extrs, "AsIs"))
        extrs <- unclass(extrs)
    ## if(are(vals, "SplitValue")) {

    ##     return(vals)
    ## }

    mapply(SplitValue, val = vals, extr = extrs,
           label = labels,
           SIMPLIFY = FALSE)
}


#' Split functions
#'
#'
#' @inheritSection custom_split_funs Custom Splitting Function Details
#'
#' @inheritParams sf_args
#' @inheritParams gen_args
#' @param vals ANY. For internal use only.
#' @param labels character. Labels to use for the remaining levels instead of
#'   the existing ones.
#' @param excl character. Levels to be excluded (they will not be reflected in
#'   the resulting table structure regardless of presence in the data).
#'
#' @name split_funcs
#' @inherit add_overall_level return
NULL

#' @rdname split_funcs
#' @export
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("COUNTRY",
#'                 split_fun = remove_split_levels(c("USA", "CAN",
#'                                                   "CHE", "BRA"))) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
remove_split_levels <- function(excl) {
    stopifnot(is.character(excl))
    function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
        var <- spl_payload(spl)
        df2 <- df[!(df[[var]] %in% excl), ]
        if(is.factor(df2[[var]])) {
          levels <- levels(df2[[var]])
          levels <- levels[!(levels %in% excl)]
          df2[[var]] <- factor(df2[[var]], levels = levels)
        }
        .apply_split_inner(spl, df2, vals = vals,
                           labels = labels,
                           trim = trim)
    }
}

#' @rdname split_funcs
#' @param only character. Levels to retain (all others will be dropped).
#' @param reorder logical(1). Should the order of \code{only} be used as the
#'   order of the children of the split. defaults to \code{TRUE}
#' @export
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("COUNTRY",
#'                 split_fun = keep_split_levels(c("USA", "CAN", "BRA"))) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
keep_split_levels <- function(only, reorder = TRUE) {
    function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
        var <- spl_payload(spl)
        varvec <- df[[var]]
        if(is.factor(varvec) && !all(only %in% levels(varvec)))
            stop("Attempted to keep invalid factor level(s) in split ",
                 setdiff(only, levels(varvec)))
        df2 <- df[df[[var]] %in% only, ]
        if(reorder)
            df2[[var]] <- factor(df2[[var]], levels = only)
        spl_child_order(spl) <- only
        .apply_split_inner(spl, df2, vals = only,
                           labels = labels,
                           trim = trim)
    }
}

#' @rdname split_funcs
#' @export
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("SEX", split_fun = drop_split_levels) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
drop_split_levels <- function(df,
                              spl,
                              vals = NULL,
                              labels = NULL,
                              trim = FALSE) {
    var <- spl_payload(spl)
    df2 <- df
    df2[[var]] <- factor(df[[var]])
    lblvar <- spl_label_var(spl)
    if(!is.null(lblvar)) {
        df2[[lblvar]] <- factor(df[[lblvar]])
    }

    .apply_split_inner(spl, df2, vals = vals,
                       labels = labels,
                       trim = trim)
}

#' @rdname split_funcs
#' @export
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("SEX", split_fun = drop_and_remove_levels(c("M", "U"))) %>%
#'   analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
drop_and_remove_levels <- function(excl) {
  stopifnot(is.character(excl))
  function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
    var <- spl_payload(spl)
    df2 <- df[!(df[[var]] %in% excl), ]
    df2[[var]] <- factor(df2[[var]])
    .apply_split_inner(
      spl,
      df2,
      vals = vals,
      labels = labels,
      trim = trim
    )
  }
}


#' @rdname split_funcs
#' @param neworder character. New order or factor levels.
#' @param newlabels character. Labels for (new order of) factor levels
#' @param drlevels logical(1). Should levels in the data which do not appear in
#'   \code{neworder} be dropped. Defaults to \code{TRUE}
#' @export
#'
reorder_split_levels <- function(neworder,
                                 newlabels = neworder,
                                 drlevels = TRUE) {
    if(length(neworder) != length(newlabels)) {
        stop("Got mismatching lengths for neworder and newlabels.")
    }
    function(df, spl,  trim, ...) {
        df2 <- df
        valvec <- df2[[spl_payload(spl)]]
        vals <- if(is.factor(valvec)) levels(valvec) else unique(valvec)
        if(!drlevels)
            neworder <- c(neworder, setdiff(vals, neworder))
        df2[[spl_payload(spl)]] <- factor(valvec, levels = neworder)
        if(drlevels) {
            orig_order <- neworder
            df2[[spl_payload(spl)]] <- droplevels(df2[[spl_payload(spl)]])
            neworder <- levels(df2[[spl_payload(spl)]])
            newlabels <- newlabels[orig_order %in% neworder]
        }
        spl_child_order(spl) <- neworder
        .apply_split_inner(spl, df2, vals = neworder, labels = newlabels, trim = trim)
    }
}


#' @rdname split_funcs
#' @param innervar character(1). Variable whose factor levels should be trimmed
#'   (e.g., empty levels dropped) \emph{separately within each grouping defined
#'   at this point in the structure}
#' @param drop_outlevs logical(1). Should empty levels in the variable being
#'   split on (i.e. the 'outer' variable, not \code{innervar}) be dropped?
#'   Defaults to \code{TRUE}
#' @export
trim_levels_in_group <- function(innervar, drop_outlevs = TRUE) {
    myfun <- function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
        if(!drop_outlevs)
            ret <- .apply_split_inner(spl, df, vals = vals,
                                      labels = labels, trim = trim)
        else
            ret <- drop_split_levels(df = df, spl = spl, vals = vals,
                                     labels = labels, trim = trim)

        ret$datasplit <- lapply(ret$datasplit, function(x) {
            coldat <- x[[innervar]]
            if(is(coldat, "character")) {
                if(!is.null(vals))
                    lvs <- vals
                else
                    lvs <- unique(coldat)
                coldat <- factor(coldat, levels = lvs) ## otherwise
            } else {
                coldat <- droplevels(coldat)
            }
            x[[innervar]] <- coldat
            x
        })
        ret$labels <- as.character(ret$labels) # TODO
        ret
    }
    myfun
}

.add_combo_part_info <- function(part,
                                 df,
                                 valuename,
                                 levels,
                                 label,
                                 extras,
                                 first = TRUE) {

    value <- LevelComboSplitValue(valuename, extras, combolevels = levels,
                                  label = label)
    newdat <- setNames(list(df), valuename)
    newval <- setNames(list(value), valuename)
    newextra <- setNames(list(extras), valuename)
    if(first) {
        part$datasplit <- c(newdat, part$datasplit)
        part$values <- c(newval, part$values)
        part$labels <- c(setNames(label, valuename), part$labels)
        part$extras <- c(newextra, part$extras)
    } else {
        part$datasplit <- c(part$datasplit, newdat)
        part$values <- c(part$values, newval)
        part$labels <- c(part$labels, setNames(label, valuename))
        part$extras <- c(part$extras, newextra)
    }
    ## not needed even in custom split function case.
    ##   part = .fixupvals(part)
    part
}

#' Add an virtual 'overall' level to split
#'
#' @inheritParams lyt_args
#' @inheritParams sf_args
#' @param valname character(1). 'Value' to be assigned to the implicit
#'   all-observations split level. Defaults to \code{"Overall"}
#' @param first logical(1). Should the implicit level appear first (\code{TRUE})
#'   or last \code{FALSE}. Defaults to \code{TRUE}.
#'
#' @return a closure suitable for use as a splitting function (\code{splfun})
#'   when creating a table layout
#'
#' @export
#'
#' @examples
#'
#' lyt <- basic_table() %>%
#'    split_cols_by("ARM", split_fun = add_overall_level("All Patients",
#'                                                       first = FALSE)) %>%
#'    analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' lyt2 <- basic_table() %>%
#'    split_cols_by("ARM") %>%
#'    split_rows_by("RACE",
#'                  split_fun = add_overall_level("All Ethnicities")) %>%
#'    summarize_row_groups(label_fstr = "%s (n)") %>%
#'    analyze("AGE")
#' lyt2
#'
#' tbl2 <- build_table(lyt2, DM)
#' tbl2
#'
add_overall_level <- function(valname = "Overall",
                              label = valname,
                              extra_args = list(),
                              first = TRUE,
                              trim = FALSE) {
    combodf <- data.frame(valname = valname,
                          label = label,
                          levelcombo = I(list(select_all_levels)),
                          exargs = I(list(extra_args)),
                          stringsAsFactors = FALSE)
    add_combo_levels(combodf,
                     trim = trim, first = first)
    }

setClass("AllLevelsSentinel", contains = "character")

# nocov start
#' @export
#' @rdname add_combo_levels
select_all_levels <- new("AllLevelsSentinel")
# nocov end

#' Add Combination Levels to split
#' @inheritParams sf_args
#' @inherit add_overall_level return
#' @param combosdf `data.frame`/`tbl_df`. Columns `valname`, `label`, `levelcombo`,
#'   `exargs`. Of which `levelcombo` and `exargs` are list columns. Passing the
#'   \code{select_all_levels} object as a value in the \code{comblevels} column
#'   indicates that an overall/all-observations level should be created.
#' @param keep_levels character or NULL. If non-NULL, the levels to retain
#'   across both combination and individual levels.
#' @note Analysis or summary functions for which the order matters should never
#'   be used within the tabulation framework.
#' @export
#' @examples
#' library(tibble)
#' combodf <- tribble(
#'     ~valname, ~label, ~levelcombo, ~exargs,
#'     "A_B", "Arms A+B", c("A: Drug X", "B: Placebo"), list(),
#'     "A_C", "Arms A+C", c("A: Drug X", "C: Combination"), list())
#'
#' lyt <- basic_table(show_colcounts = TRUE) %>%
#'     split_cols_by("ARM", split_fun = add_combo_levels(combodf)) %>%
#'     analyze("AGE")
#'
#' tbl <- build_table(lyt, DM)
#' tbl
#'
#' lyt1 <- basic_table(show_colcounts = TRUE) %>%
#'     split_cols_by("ARM",
#'                   split_fun = add_combo_levels(combodf,
#'                                                keep_levels = c("A_B",
#'                                                                "A_C"))) %>%
#'     analyze("AGE")
#'
#' tbl1 <- build_table(lyt1, DM)
#' tbl1
#'
#' smallerDM <- droplevels(subset(DM, SEX %in% c("M", "F") &
#'                         grepl("^(A|B)", ARM)))
#' lyt2 <- basic_table(show_colcounts = TRUE) %>%
#'     split_cols_by("ARM", split_fun = add_combo_levels(combodf[1,])) %>%
#'     split_cols_by("SEX",
#'                   split_fun = add_overall_level("SEX_ALL", "All Genders")) %>%
#'     analyze("AGE")
#'
#' lyt3 <-  basic_table(show_colcounts = TRUE) %>%
#'     split_cols_by("ARM", split_fun = add_combo_levels(combodf)) %>%
#'     split_rows_by("SEX",
#'                   split_fun = add_overall_level("SEX_ALL", "All Genders")) %>%
#'     summarize_row_groups() %>%
#'     analyze("AGE")
#'
#' tbl3 <- build_table(lyt3, smallerDM)
#' tbl3
add_combo_levels <- function(combosdf,
                             trim = FALSE,
                             first = FALSE,
                             keep_levels = NULL) {
    myfun <- function(df, spl, vals = NULL, labels = NULL, ...) {
        if(is(spl, "MultiVarSplit"))
            stop("Combining levels of a MultiVarSplit does not make sense.",
                 call. = FALSE) # nocov
        ret <- .apply_split_inner(spl, df, vals = vals,
                                  labels = labels, trim = trim)
        for(i in seq_len(nrow(combosdf))) {
            lcombo <- combosdf[i, "levelcombo", drop = TRUE][[1]]
            spld <- spl_payload(spl)
            if(is(lcombo, "AllLevelsSentinel"))
                subdf <- df
            else if (is(spl, "VarLevelSplit")) {
                 subdf <- df[df[[spld]] %in% lcombo, ]
            } else { ## this covers non-var splits, e.g. Cut-based splits
                stopifnot(all(lcombo %in% c(ret$labels, ret$vals)))
                subdf <- do.call(rbind,
                                 ret$datasplit[names(ret$datasplit) %in% lcombo |
                                                     ret$vals %in% lcombo])
            }
            ret <- .add_combo_part_info(ret, subdf,
                                       combosdf[i, "valname", drop = TRUE],
                                       lcombo,
                                       combosdf[i, "label", drop = TRUE],
                                       combosdf[i, "exargs", drop = TRUE][[1]],
                                       first)
        }
        if(!is.null(keep_levels)) {
            keep_inds <- value_names(ret$values) %in% keep_levels
            ret <- lapply(ret, function(x) x[keep_inds])
        }

        ret
    }
  myfun
}


#' Trim Levels to map
#'
#' This split function constructor creates a split function which trims
#' levels of a variable to reflect restrictions on the possible
#' combinations of two or more variables which are split by
#' (along the same axis) within a layout.
#'
#' @details When splitting occurs, the map is subset to the values of all
#'   previously performed splits. The levels of the variable being split are
#'   then pruned to only those still present within this subset of the map
#'   representing the current hierarchical splitting context.
#'
#'   Splitting is then performed via the \code{\link{keep_split_levels}} split
#'   function.
#'
#'   Each resulting element of the partition is then further trimmed by pruning
#'   values of any remaining variables specified in the map to those values
#'   allowed under the combination of the previous and current split.
#' @param map data.frame. A data.frame defining allowed combinations of
#'   variables. Any combination at the level of this split not present in the
#'   map will be removed from the data, both for the variable being split and
#'   those present in the data but not associated with this split or any parents
#'   of it.
#' @return a fun
#'
#' @seealso [trim_levels_in_group()]
#' @export
#' @examples
#' map <- data.frame(
#'     LBCAT = c("CHEMISTRY", "CHEMISTRY", "CHEMISTRY", "IMMUNOLOGY"),
#'     PARAMCD = c("ALT", "CRP", "CRP", "IGA"),
#'     ANRIND = c("LOW", "LOW", "HIGH", "HIGH"),
#'     stringsAsFactors = FALSE
#' )
#'
#' lyt <- basic_table() %>%
#'     split_rows_by("LBCAT") %>%
#'     split_rows_by("PARAMCD", split_fun = trim_levels_to_map(map = map)) %>%
#'     analyze("ANRIND")
#' tbl <- build_table(lyt, ex_adlb)
trim_levels_to_map <- function(map = NULL) {

    if (is.null(map) || any(sapply(map, class) != "character"))
        stop("No map dataframe was provided or not all of the columns are of ",
             "type character.")

    myfun <- function(df,
                      spl,
                      vals = NULL,
                      labels = NULL,
                      trim = FALSE,
                      .spl_context) {

        allvars <- colnames(map)
        splvar <- spl_payload(spl)

        allvmatches <- match(.spl_context$split, allvars)
        outvars <- allvars[na.omit(allvmatches)]
        ## invars are variables present in data, but not in
        ## previous or current splits
        invars <- intersect(setdiff(allvars, c(outvars, splvar)),
                            names(df))
        ## allvarord <- c(na.omit(allvmatches), ## appear in prior splits
        ##                which(allvars == splvar), ## this split
        ##                allvars[-1*na.omit(allvmatches)]) ## "outvars"

        ## allvars <- allvars[allvarord]
        ## outvars <- allvars[-(which(allvars == splvar):length(allvars))]
        if(length(outvars) > 0) {
            indfilters <- vapply(outvars, function(ivar) {
                obsval <- .spl_context$value[match(ivar, .spl_context$split)]
                sprintf("%s == '%s'", ivar, obsval)
            }, "")

            allfilters <- paste(indfilters, collapse = " & ")
            map <- map[eval(parse(text = allfilters), envir = map), ]
        }
        map_splvarpos <- which(names(map) == splvar)
        nondup <- !duplicated(map[[splvar]])
        ksl_fun <- keep_split_levels(only = map[[splvar]][nondup],
                                     reorder = TRUE)
        ret <- ksl_fun(df, spl, vals, labels, trim = trim)

        if(length(ret$datasplit) == 0) {
            msg <- paste(sprintf("%s[%s]", .spl_context$split, .spl_context$value),
                         collapse = "->")
            stop("map does not allow any values present in data for split ",
                 "variable ", splvar,
                 " under the following parent splits:\n\t", msg)
        }

        ## keep non-split (inner) variables levels
        ret$datasplit <- lapply(ret$values, function(splvar_lev) {
            df3 <- ret$datasplit[[splvar_lev]]
            curmap <- map[map[[map_splvarpos]] == splvar_lev, ]
            ## loop through inner variables
            for (iv in invars) { ##setdiff(colnames(map), splvar)) {
                iv_lev <- df3[[iv]]
                levkeep <- as.character(unique(curmap[[iv]]))
                if (is.factor(iv_lev) && !all(levkeep %in% levels(iv_lev)))
                    stop("Attempted to keep invalid factor level(s) in split ",
                         setdiff(levkeep, levels(iv_lev)))

                df3 <- df3[iv_lev %in% levkeep, , drop = FALSE]

                if (is.factor(iv_lev))
                    df3[[iv]] <- factor(as.character(df3[[iv]]),
                                        levels = levkeep)
            }

            df3
        })
        names(ret$datasplit) <- ret$values
        ret
    }

    myfun
}
