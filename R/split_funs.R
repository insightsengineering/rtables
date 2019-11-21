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

setGeneric(".applysplit_partlbls",
           function(spl, df, vals, lbls) standardGeneric(".applysplit_partlbls"))

setGeneric("check_validsplit",
           function(spl, df) standardGeneric("check_validsplit"))





## do various cleaning, and naming, plus
## ensure partinfo$values contains SplitValue objects only
.fixupvals = function(partinfo) {
    vals = partinfo$values
    if(is.factor(vals))
        vals = levels(vals)[vals]
    extr = partinfo$extras
    dpart = partinfo$datasplit
    lbls = partinfo$labels
    if(is.null(lbls)) {
        if(!is.null(names(vals)))
            lbls = names(vals)
        else if(!is.null(names(dpart)))
            lbls = names(dpart)
        else if (!is.null(names(extr)))
            lbls = names(extr)
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
            partinfo$extras = NULL
        }
        return(partinfo)
    }

    ## vals elements are not  SplitValue objects from here down
    if(is.null(extr))
        extr = rep(list(list()), length(vals))
    vals = make_splvalue_vec(vals, extr)
    names(vals) = lbls
    partinfo$values = vals
    
    if(!identical(names(dpart), lbls)) {
        names(dpart) = lbls
        partinfo$datasplit = dpart
    }
    
    ## we're done with this so take it off
    partinfo$extras = NULL
    partinfo
}


### NB This is called at EACH level of recursive splitting 
do_split = function(spl, df, vals = NULL, lbls = NULL) {
    ## this will error if, e.g., df doesn't have columns
    ## required by spl, or generally any time the spl
    ## can't be applied to df
    check_validsplit(spl, df)
    ## note the <- here!!!
    if(!is.null(splfun<-split_fun(spl))) {
        ## Currently the contract is that split_functions take df, vals, lbls and
        ## return list(values=., datasplit=., labels = .), optionally with
        ## an additional extras element
        ret = splfun(df, spl, vals, lbls)
    } else {
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

        if(is.null(lbls))
            lbls = .applysplit_partlbls(spl, df, vals, lbls)
        else
            stopifnot(names(lbls)== names(vals))

        ## get rid of columns that would not have any
        ## observations.
        ##
        ## But only if there were any rows to start with
        ## if not we're in a manually constructed table
        ## column tree
        hasdata = sapply(dpart, function(x) nrow(x) >0)
        if(nrow(df) > 0 && length(dpart) > sum(hasdata)) { #some empties
            dpart = dpart[hasdata]
            vals = vals[hasdata]
            extr = extr[hasdata]
            lbls = lbls[hasdata]
        }

        if(is.null(spl_child_order(spl)))
            vord = seq_along(vals)
        else {
            vord = match(spl_child_order(spl),
                         vals)
            vord = vord[!is.na(vord)]
        }
                
        ## FIXME: should be an S4 object, not a list
        ret = list(values = vals[vord],
                   datasplit = dpart[vord],
                   labels = lbls[vord],
                   extras = extr[vord])
    }
    ## this:
    ## - guarantees that ret$values contains SplitValue objects
    ## - removes the extras element since its redundant after the above
    ## - Ensures datasplit and values lists are named according to labels
    ret = .fixupvals(ret)
    ret
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
### do NOT make it check, e.g., if the baseline level of
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
    .checkvarsok(spl, df)
})




## default does nothing, add methods as they become
## required
setMethod("check_validsplit", "Split",
          function(spl, df)
    invisible(NULL))



setMethod(".applysplit_rawvals", "VarLevelSplit",
          function(spl, df) {
    varvec = df[[spl_payload(spl)]]
    unique(varvec)
})

setMethod(".applysplit_rawvals", "MultiVarSplit",
          function(spl, df) {
    spl_payload(spl)
})

setMethod(".applysplit_rawvals", "AllSplit",
          function(spl, df) TRUE)

setMethod(".applysplit_rawvals", "ManualSplit",
          function(spl, df) spl@levels)


setMethod(".applysplit_rawvals", "NULLSplit",
          function(spl, df) FALSE)

## this returns null because we actually use
## the extra arguments to know how many splits
## there need to be. .fixupvals catches and takes
## care of this case.
setMethod(".applysplit_rawvals", "ComparisonSplit",
          function(spl, df) NULL)

setMethod(".applysplit_rawvals", "AnalyzeVarSplit",
          function(spl, df) spl_payload(spl))


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
    if(!is.na(vals) && avar_inclNA(spl))
        ret = df[!is.na(df[[vals]]),] 
    list(ret)
})

## setMethod(".applysplit_datapart", "ComparisonSplit",
##           function(spl, df, vals) {
##     stopifnot(length(vals) > 0)
##     rep(list(df), times = length(vals))
## })

## XXX TODO *CutSplit Methods

## Extras generation methods
setMethod(".applysplit_extras", "Split",
          function(spl, df, vals) {
    stopifnot(length(vals) > 0)
    replicate(list(split_exargs(spl)), n = length(vals))
})

## setMethod(".applysplit_extras", "ComparisonSplit",
##           function(spl, df, vals) {
##     make_comp_extargs(spl, df)
## })


setMethod(".applysplit_extras", "VarLevWBaselineSplit",
          function(spl, df, vals) {
    var = spl_payload(spl)
    bl_level = spl@baseline_value #XXX XXX
    bldata = df[df[[var]] == bl_level,]
    replicate(c(list(.baseline_data = bldata), split_exargs(spl)), n = length(vals), simplify = FALSE)
})

## XXX TODO FIXME
setMethod(".applysplit_partlbls", "Split",
          function(spl, df, vals, lbls) as.character(vals))



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


make_comp_extargs = function(spl, df) {
    stopifnot(is(spl, "ComparisonSplit"))
    ssets1 = subsets_from_factory(df, spl@subset1_gen)
    ssets2 = subsets_from_factory(df, spl@subset2_gen)
    if(is.null(names(ssets2)))
        names(ssets2) = names(ssets1)
    
    mapply(function(s1, s2, s1name, s2name) c( list(subset1 = s1, subset1name = s1name, subset2 = s2, subset2name), split_exargs(spl)),
           s1 = ssets1,
           s2 = ssets2,
           s1name = names(ssets1),
           s2name = names(ssets2),
           SIMPLIFY=FALSE)
}


make_blinecomp_extargs = function(spl, df) {
    incall = blsplit_incall(spl)
    var = blsplit_var(spl)
    blvalue = blsplit_baseline(spl)
    
    


}



## setGeneric("make_extras", function(spl, partinfo) standardGeneric("make_extras"))

## setMethod("make_extras", "Split",
##           function(spl, partinfo) {
##     rep(list(list()), times = length(partinfo$values))
## })

## ## currently only ComparisonSplit
## setMethod("add_extras", "ComparisonSplit",
##           function(spl, partinfo){
##     ## the partitions are all the whole df here so this is ok
##     make_comp_extargs(spl, df = partinfo$datasplit[[1]])
## })



make_splvalue_vec = function(vals, extrs = list(list())) {
    if(is(extrs, "AsIs"))
        extrs = unclass(extrs)
    if(are(vals, "SplitValue")) {
        return(vals)
    }
    mapply(SplitValue, val = vals, extr = extrs,
           SIMPLIFY=FALSE)
}

.apply_split_inner = function(spl, df, vals = NULL, lbls = NULL) {
    varvec = df[[spl_payload(spl)]]
    lblvec = df[[spl_lblvar(spl)]]
    if(is.null(vals)) {

        vals = unique(varvec)
    }
 
    if(is.null(lbls))
        lbls = sapply(vals, function(v) {
            vlbl = unique(df[df[[spl_payload(spl)]] == v,
                             spl_lblvar(spl), drop = TRUE])
            if(length(vlbl) == 0)
                vlbl = ""
            vlbl
        })
    fct = factor(varvec, levels = vals)
    spl = split(df, fct)
    ## should always be true or the above would have broken
    if(!are(vals, "SplitValue"))
        vals = make_splvalue_vec(vals)
    list(values = vals, datasplit = spl,
         labels = lbls)
}

excl_levs_sfun = function(excl) {
    function(df, spl, vals = NULL, lbls = NULL) {
        var = spl_payload(spl)
        df2 = df[!(df[[var]] %in% excl),]
        .apply_split_inner(spl, df2, vals = vals,
                           lbls = lbls)
    }
}


only_levs_sfun = function(only) {
    function(df, spl, vals = NULL, lbls = NULL) {
        var = spl_payload(spl)
        df2 = df[df[[var]] %in% only,]
        .apply_split_inner(spl, df2, vals = vals,
                           lbls = lbls)
    }
}

reord_levs_sfun = function(neworder, newlbls = neworder, drlevels = TRUE) {
    function(df, spl,  ...) {
        df2 = df
        df2[[spl_payload(spl)]] = factor(df[[spl_payload(spl)]], levels = neworder)
        if(drlevels) {
            df2[[spl_payload(spl)]] = droplevels(df2[[spl_payload(spl)]] )
            neworder = levels(df2[[spl_payload(spl)]])
            newlbls = newlbls[newlbls %in% neworder]
        }
        .apply_split_inner(spl, df2, vals = neworder, lbls = newlbls)
    }
}

.raw_to_splvals = function(partinfo, extrs) {
    if(are(partinfo$rawvalues, "SplitValue")) {
        if(any(sapply(extrs, length) > 0))
            warning(" values are already SplitValues but extrs contained non-empty arguments. please contact the maintainer")
        partinfo$values = partinfo$rawvalues
        
        return(partinfo)
    }
    partinfo$values = make_splvalue_vec(partinfo$rawvalues,
                                        extrs)
    partinfo

}



## setGeneric("apply_split", 
##            function(spl, df, vals = NULL, lbls = NULL) standardGeneric("apply_split"))

## setMethod("apply_split", "VarLevelSplit",
##           function(spl, df, vals = NULL, lbls = NULL) {
##     if(!is.null(split_fun(spl))) 
##         ret = split_fun(spl)(df, spl,
##             vals = vals, lbls = lbls)
##     else
##         ret = .apply_split_inner(spl, df, vals = vals, lbls = lbls)
##     if(!is.null(spl_child_order(spl))) {
##         if(are(ret$values, "SplitValue"))
##             retrawvals = lapply(ret$values, splv_rawvalues)
##         else
##             retrawvals = ret$values
##         vord = spl_child_order(spl)
##         ## this trick is a bit brain hurty but it works
##         ## y[match(x,y)] will return only the values of y
##         ## which appeared in x, in the order they appear in
##         ## y
##         neword = match(vord, retrawvals)
##         neword = neword[!is.na(neword)]
##         ret$values = ret$values[neword] ##as.character(reorder(ret$values, vord))
##     }
##     if(!are(ret$values, "SplitValue"))
##         ret$values = make_splvalue_vec(ret$values)
##     ret
    
## })

## setMethod("apply_split", "MultiVarSplit",
##           function(spl, df, vals = NULL, lbls = NULL) {
##     vars = spl_payload(spl)
##     lst = lapply(vars, function(v) {
##         df[!is.na(df[[v]]),]
##     })
##     names(lst) = vars
##     list(values = make_splvalue_vec(vars),
##          datasplit = lst,
##          labels = spl_varlbls(spl))
## })

## setMethod("apply_split", "AllSplit",
##           function(spl, df,
##                    vals = NULL,
##                    lbls = NULL) list(values = SplitValue(TRUE),
##                                  datasplit = list(df),
##                                  labels = obj_label(spl)))

## setMethod("apply_split", "NULLSplit",
##           function(spl, df,
##                    vals = NULL,
##                    lbls = NULL) list(values = SplitValue(FALSE),
##                                  datasplits = list(df[0,]),
##                                  labels = ""))
    

## setMethod("apply_split", "AnalyzeVarSplit",
##           function(spl, df, vals = NULL, lbls = NULL) {
##     dat = df[!is.na(df[[spl_payload(spl)]]),]
##     list(values = make_splvalue_vec(spl_payload(spl)),
##          datasplit = list(dat),
##          labels = obj_label(spl))
## })



 
## setMethod("apply_split", "ComparisonSplit",
##           function(spl, df, vals = NULL, lbls = NULL) {

##     lbls = make_comp_labels(spl, df)
##     extras = make_comp_extargs(spl, df)
##     values = make_splvalue_vec(seq_along(extras), extrs = extras)
##     list(values = values,
##          datasplit = rep(df, length(values)),
##          labels = lbls,
##          extras = extras)
## })
          
    
