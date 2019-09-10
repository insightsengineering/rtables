
make_splvalue_vec = function(vals, extrs = list(list())) {
    if(are(vals, "SplitValue")) {
        warning("attempted to make SplitValue vector out of existing SplitValue objects")
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

reord_levs_sfun = function(neworder, newlbls = neworder) {
    function(spl, df) {
        .apply_split_inner(spl, df, vals = neworder, lbls = newlbls)
    }
}


setGeneric("apply_split", 
           function(spl, df, vals = NULL, lbls = NULL) standardGeneric("apply_split"))

setMethod("apply_split", "VarLevelSplit",
          function(spl, df, vals = NULL, lbls = NULL) {
    if(!is.null(split_fun(spl))) 
        ret = split_fun(spl)(df, spl,
            vals = vals, lbls = lbls)
    else
        ret = .apply_split_inner(spl, df, vals = vals, lbls = lbls)
    if(!is.null(spl_child_order(spl))) {
        if(are(ret$values, "SplitValue"))
            retrawvals = lapply(ret$values, splv_rawvalues)
        else
            retrawvals = ret$values
        vord = spl_child_order(spl)
        ## this trick is a bit brain hurty but it works
        ## y[match(x,y)] will return only the values of y
        ## which appeared in x, in the order they appear in
        ## y
        neword = match(vord, retrawvals)
        neword = neword[!is.na(neword)]
        ret$values = ret$values[neword] ##as.character(reorder(ret$values, vord))
    }
    if(!are(ret$values, "SplitValue"))
        ret$values = make_splvalue_vec(ret$values)
    ret
    
})

setMethod("apply_split", "MultiVarSplit",
          function(spl, df, vals = NULL, lbls = NULL) {
    vars = spl_payload(spl)
    lst = lapply(vars, function(v) {
        df[!is.na(df[[v]]),]
    })
    names(lst) = vars
    list(values = make_splvalue_vec(vars),
         datasplit = lst,
         labels = spl_varlbls(spl))
})

setMethod("apply_split", "AllSplit",
          function(spl, df,
                   vals = NULL,
                   lbls = NULL) list(values = SplitValue(TRUE),
                                 datasplit = list(df),
                                 labels = obj_label(spl)))

setMethod("apply_split", "NULLSplit",
          function(spl, df,
                   vals = NULL,
                   lbls = NULL) list(values = SplitValue(FALSE),
                                 datasplits = list(df[0,]),
                                 labels = ""))
    

setMethod("apply_split", "AnalyzeVarSplit",
          function(spl, df, vals = NULL, lbls = NULL) {
    dat = df[!is.na(df[[spl_payload(spl)]]),]
    list(values = make_splvalue_vec(spl_payload(spl)),
         datasplit = list(dat),
         labels = obj_label(spl))
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


make_comp_extargs = function(spl, df) {
    stopifnot(is(spl, "ComparisonSplit"))
    ssets1 = subsets_from_factory(spl@subset1_gen, df)
    ssets2 = subsets_from_factory(spl@subset2_gen, df)
    mapply(function(s1, s2) list(subset1 = s1, subset2 = s2),
           SIMPLIFY=FALSE)
}
 
setMethod("apply_split", "ComparisonSplit",
          function(spl, df, vals = NULL, lbls = NULL) {

    lbls = make_comp_labels(spl, df)
    extras = make_comp_extargs(spl, df)
    values = make_splvalue_vec(seq_along(extras), extrs = extras)
    list(values = values,
         datasplit = rep(df, length(values)),
         labels = lbls)
})
          
    
