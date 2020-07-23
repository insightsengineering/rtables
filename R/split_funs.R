## Generics and how they are used directly

## check_validsplit - Check if the split is valid for the data, error if not

## .apply_spl_extras - Generate Extras

## .apply_spl_datapart - generate data partition

## .apply_spl_rawvals - Generate raw (ie non SplitValue object) partition values



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





## do various cleaning, and naming, plus
## ensure partinfo$values contains SplitValue objects only
.fixupvals = function(partinfo) {
    if(is.factor(partinfo$labels))
        partinfo$labels = as.character(partinfo$labels)

    vals = partinfo$values
    if(is.factor(vals))
        vals = levels(vals)[vals]
    extr = partinfo$extras
    dpart = partinfo$datasplit
    labels = partinfo$labels
    if(is.null(labels)) {
        if(!is.null(names(vals)))
            labels = names(vals)
        else if(!is.null(names(dpart)))
            labels = names(dpart)
        else if (!is.null(names(extr)))
            labels = names(extr)
    }
        
    if(is.null(vals) && !is.null(extr))
        vals = seq_along(extr)

    if(length(vals) == 0) {
        stopifnot(length(extr) == 0)
        return(partinfo)
    }
    ## length(vals) > 0 from here down
    
    if(are(vals, "SplitValue")) {
        if(!is.null(extr)) {
            warning("Got a partinfo list with values that are ",
                    "already SplitValue objects and non-null extras ",
                    "element. This shouldn't happen")
        }
    } else {
        if(is.null(extr))
            extr = rep(list(list()), length(vals))
        ## strict is FALSE here cause fixupvals might be called repeatedly
        vals = make_splvalue_vec(vals, extr)
    }
    ## we're done with this so take it off
    partinfo$extras = NULL

    names(vals) = labels
    partinfo$values = vals
    
    if(!identical(names(dpart), labels)) {
        names(dpart) = labels
        partinfo$datasplit = dpart
    }

    partinfo$labels = labels

    stopifnot(length(unique(sapply(partinfo, NROW))) == 1)
    partinfo
}


### NB This is called at EACH level of recursive splitting 
do_split = function(spl, df, vals = NULL, labels = NULL, trim = FALSE) {
    ## this will error if, e.g., df doesn't have columns
    ## required by spl, or generally any time the spl
    ## can't be applied to df
    check_validsplit(spl, df)
    ## note the <- here!!!
    if(!is.null(splfun<-split_fun(spl))) {
        ## Currently the contract is that split_functions take df, vals, labels and
        ## return list(values=., datasplit=., labels = .), optionally with
        ## an additional extras element
        ret = splfun(df, spl, vals, labels, trim = trim)
    } else {
        ret = .apply_split_inner(df = df, spl = spl, vals = vals, labels = labels, trim = trim)
    }
    ## this:
    ## - guarantees that ret$values contains SplitValue objects
    ## - removes the extras element since its redundant after the above
    ## - Ensures datasplit and values lists are named according to labels
    ## - ensures labels are character not factor
    ret = .fixupvals(ret)
    ret
}


.apply_split_inner = function(spl, df, vals = NULL, labels = NULL, trim = FALSE) {

    ## try to calculate values first. Most of the time we can
    if(is.null(vals)) 
        vals = .applysplit_rawvals(spl, df)
    extr = .applysplit_extras(spl, df, vals)
    
    ## in some cases, currently ComparisonSplit, we don't know the
    ## values until after we know the extra args, since the values
    ## themselves are meaningless. If this is the case, fix them
    ## before generating the data partition
    if(is.null(vals) && length(extr) > 0) {
        vals = seq_along(extr)
        names(vals) = names(extr)
    }
    
    dpart = .applysplit_datapart(spl, df, vals)
    
    if(is.null(labels))
        labels = .applysplit_partlabels(spl, df, vals, labels)
    else
        stopifnot(names(labels)== names(vals))
    ## get rid of columns that would not have any
    ## observations.
    ##
    ## But only if there were any rows to start with
    ## if not we're in a manually constructed table
    ## column tree
    if(trim) {
        hasdata = sapply(dpart, function(x) nrow(x) >0)
        if(nrow(df) > 0 && length(dpart) > sum(hasdata)) { #some empties
            dpart = dpart[hasdata]
            vals = vals[hasdata]
            extr = extr[hasdata]
            labels = labels[hasdata]
        }
    }
    
    if(is.null(spl_child_order(spl)) || is(spl, "AllSplit")) {
        vord = seq_along(vals)
    } else {
        vord = match(spl_child_order(spl),
                     vals)
        vord = vord[!is.na(vord)]
    } 

    
    ## FIXME: should be an S4 object, not a list
    ret = list(values = vals[vord],
               datasplit = dpart[vord],
               labels = labels[vord],
               extras = extr[vord])
    ret
    ## varvec = df[[spl_payload(spl)]]
    ## labelvec = df[[spl_labelvar(spl)]]
    ## if(is.null(vals)) {
    ##     vals = if(is.factor(varvec))
    ##                levels(varvec)
    ##            else
    ##                unique(varvec)
    ## }
 
    ## if(is.null(labels)) {
    ##     ##XXX dangerous
    ##     if(is.factor(varvec))
    ##         labels = vals
    ##     else
    ##         labels = sapply(vals, function(v) {
    ##             vlabel = unique(df[df[[spl_payload(spl)]] == v,
    ##                              spl_labelvar(spl), drop = TRUE])
    ##             if(length(vlabel) == 0)
    ##                 vlabel = ""
    ##             vlabel
    ##         })
    ## }
    ## fct = factor(varvec, levels = vals)
    ## spl = split(df, fct)
    ## ## should always be true or the above would have broken
    ## if(!are(vals, "SplitValue"))
    ##     vals = make_splvalue_vec(vals)
    
    ## list(values = vals, datasplit = spl,
    ##      labels = labels)
}


.checkvarsok = function(spl, df) {

    vars = spl_payload(spl)
    ## could be multiple vars in the future?
    ## no reason not to make that work here now.
    if(!all(vars %in% names(df)))
        stop( " variable(s) [",
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

setMethod("check_validsplit", "AnalyzeVarSplit",
          
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
    varvec = df[[spl_payload(spl)]]
    if(is.factor(varvec))
        levels(varvec)
    else
        unique(varvec)
})

setMethod(".applysplit_rawvals", "MultiVarSplit",
          function(spl, df) {
    spl_payload(spl)
})

setMethod(".applysplit_rawvals", "AllSplit",
          function(spl, df) obj_name(spl)) #"all obs")

setMethod(".applysplit_rawvals", "ManualSplit",
          function(spl, df) spl@levels)


setMethod(".applysplit_rawvals", "NULLSplit",
          function(spl, df) "")

setMethod(".applysplit_rawvals", "AnalyzeVarSplit",
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
        stop("Attempted to split on values of column (", spl_payload(spl), ") not present in the data")
    }
    ret = lapply(seq_along(vals), function(i) {
        df[df[[spl_payload(spl)]] == vals[[i]],]
    })
    names(ret) = as.character(vals)
    ret
})


setMethod(".applysplit_datapart", "MultiVarSplit",
          function(spl, df, vals) {
    ret = lapply(vals, function(cl) {
        df[!is.na(df[[cl]]),]
    })
    names(ret) = vals
    ret
})

setMethod(".applysplit_datapart", "AllSplit",
          function(spl, df, vals) list(df))

## not sure I need this
setMethod(".applysplit_datapart", "ManualSplit",
          function(spl, df, vals) rep(list(df), times = length(vals)))



setMethod(".applysplit_datapart", "NULLSplit",
          function(spl, df, vals) list(df[FALSE,]))

## XXX should this be mandatorily excluding NAs???
setMethod(".applysplit_datapart", "AnalyzeVarSplit",
          function(spl, df, vals) {
    ## for now, this will work later
    stopifnot(length(vals) == 1L)
    if(!is.na(vals) && !all(vals %in% names(df))) {
        badcols = setdiff(vals, names(df))
        stop("Specified analysis vars (", paste(badcols, collapse = ", "), ") not present in data")
    }
    ret = df
    if(!is.na(vals) && avar_inclNAs(spl))
        ret = df[!is.na(df[[vals]]),] 
    list(ret)
})

setMethod(".applysplit_datapart", "VarStaticCutSplit",
          function(spl, df, vals) {
  #  lbs = spl_cutlabels(spl)
    var = spl_payload(spl)
    varvec = df[[var]]
    cts = spl_cuts(spl)
    cfct = cut(varvec, cts, include.lowest = TRUE)#, labels = lbs)
    split(df, cfct, drop = FALSE)

})
## XXX TODO *CutSplit Methods


setClass("NullSentinel", contains = "NULL")
nullsentinel = new("NullSentinel")
noarg = function() nullsentinel

## Extras generation methods
setMethod(".applysplit_extras", "Split",
          function(spl, df, vals) {
    splex <- split_exargs(spl)
    nextr <- length(splex)
    nvals <- length(vals)
    stopifnot(nvals > 0,
              nextr <= nvals)
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

setMethod(".applysplit_extras", "VarLevWBaselineSplit",
          function(spl, df, vals) {
    var = spl_payload(spl)
    bl_level = spl@ref_group_value #XXX XXX
    bldata = df[df[[var]] == bl_level,]
    lapply(vals, function(vl) {
        list(.ref_full = bldata, .in_ref_col = vl == bl_level)
    })
})

## XXX TODO FIXME
setMethod(".applysplit_partlabels", "Split",
          function(spl, df, vals, labels) as.character(vals))

setMethod(".applysplit_partlabels", "VarLevelSplit",
          function(spl, df, vals, labels) {
    
    
    varvec = df[[spl_payload(spl)]]
    labelvec = df[[spl_labelvar(spl)]]
    if(is.null(vals)) {
        vals = if(is.factor(varvec))
                   levels(varvec)
               else
                   unique(varvec)
    }
    if(is.null(labels)) {
        ##XXX dangerous
        if(is.factor(varvec))
            labels = vals
        else
            labels = sapply(vals, function(v) {
                vlabel = unique(df[df[[spl_payload(spl)]] == v,
                                 spl_labelvar(spl), drop = TRUE])
                if(length(vlabel) == 0)
                    vlabel = ""
                vlabel
            })
    }
    names(labels) = as.character(vals)
    labels
})



subsets_from_factory = function(df, fact) {
   if(is.character(fact)) {
       tmpvals = unique(df[[fact]])
       fctor = factor(df[[fact]], levels = tmpvals)
       ssets = split(df, fctor)
       ## I think split already does this...
       names(ssets) = tmpvals
   } else {
       ssets = fact(df)
   }

   ssets
}


make_splvalue_vec = function(vals, extrs = list(list())) {
    if(is(extrs, "AsIs"))
        extrs = unclass(extrs)
    if(are(vals, "SplitValue")) {
        return(vals)
    }
    
    mapply(SplitValue, val = vals, extr = extrs,
           SIMPLIFY=FALSE)
}


#' Split functions
#' 
#' @param excl character. Levels to be excluded (they will not be reflected in the resulting table structure regardless
#'   of presence in the data).
#'
#' @rdname split_funcs
#' @export
remove_split_levels = function(excl) {
    function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
        var = spl_payload(spl)
        df2 = df[!(df[[var]] %in% excl),]
        .apply_split_inner(spl, df2, vals = vals,
                           labels = labels,
                           trim = trim)
    }
}

#' @rdname split_funcs
#' @param only character. Levels to retain (all others will be dropped).
#' @param reorder logical(1). Should the order of \code{only} be used as the order of the children of the split. defaults to \code{TRUE}
#' @export
keep_split_levels = function(only, reorder = TRUE) {
    function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
        var = spl_payload(spl)
        df2 = df[df[[var]] %in% only,]
        if(reorder)
            df2[[var]] = factor(df2[[var]], levels = only)
        .apply_split_inner(spl, df2, vals = vals,
                           labels = labels,
                           trim = trim)
    }
}

#' @rdname split_funcs
#' @export
drop_split_levels <- function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
        var = spl_payload(spl)
        df2 = df
        df2[[var]] = factor(df[[var]])
        .apply_split_inner(spl, df2, vals = vals,
                           labels = labels,
                           trim = trim)
}

#' @rdname split_funcs
#' @param neworder character. New order or factor levels.
#' @param newlabels character. Labels for (new order of) factor levels
#' @param drlevels logical(1). Should levels in the data which do not appear in \code{neworder} be dropped. Defaults to \code{TRUE}
#' @export
reorder_split_levels = function(neworder, newlabels = neworder, drlevels = TRUE) {
    function(df, spl,  trim, ...) {
        df2 = df
        df2[[spl_payload(spl)]] = factor(df[[spl_payload(spl)]], levels = neworder)
        if(drlevels) {
            df2[[spl_payload(spl)]] = droplevels(df2[[spl_payload(spl)]] )
            neworder = levels(df2[[spl_payload(spl)]])
            newlabels = newlabels[newlabels %in% neworder]
        }
        .apply_split_inner(spl, df2, vals = neworder, labels = newlabels, trim = trim)
    }
}

#' @rdname split_funcs
#' @param innervar character(1). Variable whose factor levels should be trimmed (ie empty levels dropped) \emph{separately within each grouping defined at this point in the structure}
#' @export 
trim_levels_in_group = function(innervar) {
  myfun = function(df, spl, vals = NULL, labels = NULL, trim = FALSE) {
    ret = .apply_split_inner(spl, df, vals = vals, labels = labels, trim = trim)
    
    ret$datasplit = lapply(ret$datasplit, function(x) {
        coldat = x[[innervar]]
        if(is(coldat, "character")) {
            if(!is.null(vals))
                lvs = vals
            else
                lvs = unique(coldat)
            coldat = factor(coldat, levels = lvs) ## otherwise
        } else {
            coldat = droplevels(coldat)
        }
      x[[innervar]] = coldat
      
      x
    })
    
    ret$labels <- as.character(ret$labels) # TODO
    ret
  }
  myfun
  
}

.add_combo_part_info = function(part, df, valuename, levels, label, extras, first = TRUE) {

    value = LevelComboSplitValue(levels, extras, comboname = valuename)
    newdat = setNames(list(df), label)
    newval = setNames(list(value), label)
    newextra = setNames(list(extras), label)
    if(first) {
        part$datasplit = c(newdat, part$datasplit)
        part$values = c(newval, part$values)
        part$labels = c(label, part$labels)
        part$extras = c(newextra, part$extras)
    } else {
        part$datasplit = c(part$datasplit, newdat)
        part$values = c(part$values, newval)
        part$labels = c(part$labels, label)
        part$extras = c(part$extras, newextra)
    }
    ## not needed even in custom split function case.
    ##   part = .fixupvals(part)
    part
}

#' Add an implicit 'overall' level to split
#' 
#' @inheritParams lyt_args
#' @param valname character(1). 'Value' to be assigned to the implicit all-observations split level. Defaults to \code{"Overall"}
#' @param first logical(1). Should the implicit level appear first (\code{TRUE}) or last \code{FALSE}. Defaults to \code{TRUE}.
#' 
#' @return a closure suitable for use as a splitting function (\code{splfun}) when creating a table layout
#' 
#' @export
#' 
#' @examples
#' l <- NULL %>%
#'    split_cols_by("ARM") %>% 
#'    split_rows_by("RACE", split_fun = add_overall_level("All Ethnicities")) %>% 
#'    summarize_row_groups(label_fstr = "%s (n)") %>% 
#'    analyze("AGE", afun = list_wrap_x(summary) , format = "xx.xx")
#'    
#' l
#' 
#' tbl <- build_table(l, DM)
#' 
#' tbl
#' 
add_overall_level = function(valname = "Overall", label = valname, extra_args = list(), first = TRUE, trim = FALSE) {
    combodf <- data.frame(valname = valname,
                          label = label,
                          levelcombo = I(list(select_all_levels)),
                          exargs = I(list(extra_args)),
                          stringsAsFactors = FALSE)
    add_combo_levels(combodf,
                     trim = trim, first = first)
    }

setClass("AllLevelsSentinel", contains = "character")
#' @export
select_all_levels = new("AllLevelsSentinel")

#' Add Combination Levels to split
#' @param combosdf data.frame/tbl_df. Columns valname, label, levelcombo, exargs. Of which levelcombo and exargs are list columns. Passing the \code{select_all_levels} object as a value in the \code{comblevels} column indicates that an overall/all-observations level should be created.
#' @note Analysis or summary functions for which the order matters should never be used within the tabulation framework.
#' @export
#' @examples
#' library(tibble)
#' combodf <- tribble(
#'     ~valname, ~label, ~levelcombo, ~exargs,
#'     "A_B", "Arms A+B", c("A: Drug X", "B: Placebo"), list(),
#'     "A_C", "Arms A+C", c("A: Drug X", "C: Combination"), list())
#'
#' l <- basic_table() %>%
#'     split_cols_by("ARM", split_fun = add_combo_levels(combodf)) %>%
#'     add_colcounts() %>%
#'     analyze("AGE")
#'
#' build_table(l, DM)
#' smallerDM <- droplevels(subset(DM, SEX %in% c("M", "F") &
#'                         grepl("^(A|B)", ARM)))
#' l2 <- basic_table() %>%
#'     split_cols_by("ARM", split_fun = add_combo_levels(combodf[1,])) %>%
#'     split_cols_by("SEX", split_fun = add_overall_level("SEX_ALL", "All Genders")) %>%
#'     add_colcounts() %>%
#'     analyze("AGE")
#' 
#' l3 <-  basic_table() %>%
#'     split_cols_by("ARM", split_fun = add_combo_levels(combodf)) %>%
#'     add_colcounts() %>%
#'     split_rows_by("SEX", split_fun = add_overall_level("SEX_ALL", "All Genders")) %>%
#'     summarize_row_groups() %>%
#'     analyze("AGE")
#' 
#' build_table(l3, smallerDM)

add_combo_levels = function(combosdf, trim = FALSE, first = FALSE) {
    myfun = function(df, spl, vals = NULL, labels = NULL, ...) {
        ret = .apply_split_inner(spl, df, vals = vals, labels = labels, trim = trim)
        for(i in 1:nrow(combosdf)) {
            lcombo = combosdf[i, "levelcombo"][[1]]
            spld = spl_payload(spl)
            if(is(lcombo, "AllLevelsSentinel"))
                subdf = df
            else if (is(spl, "VarLevelSplit")) {
                 subdf = df[df[[spld]] %in% lcombo,]
            } else {
                stopifnot(all(lcombo %in% c(ret$labels, ret$vals)))
                subdf = do.call(rbind, ret$datasplit[names(ret$datasplit) %in% lcombo |
                                                     ret$vals %in% lcombo])
            }
            ret = .add_combo_part_info(ret, subdf,
                                       combosdf[i, "valname", drop=TRUE],
                                       lcombo,
                                       combosdf[i,"label"],
                                       combosdf[i, "exargs"][[1]],
                                       first)
        }
        ret
    }
  myfun
}
