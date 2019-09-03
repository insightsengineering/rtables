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
        return(split_fun(spl)(df, spl,
            vals = vals, lbls = lbls))
    .apply_split_inner(spl, df, vals = vals, lbls = lbls)
})

setMethod("apply_split", "MultiVarSplit",
          function(spl, df, vals = NULL, lbls = NULL) {
    vars = spl_payload(spl)
    lst = lapply(vars, function(v) {
        df[!is.na(df[[v]]),]
    })
    names(lst) = vars
    list(values = vars,
         datasplit = lst,
         labels = spl_varlbls(spl))
})

setMethod("apply_split", "AllSplit",
          function(spl, df,
                   vals = NULL,
                   lbls = NULL) list(values = TRUE,
                                 datasplit = list(df),
                                 labels = obj_label(spl)))

setMethod("apply_split", "NULLSplit",
          function(spl, df,
                   vals = NULL,
                   lbls = NULL) list(values = FALSE,
                                 datasplits = list(df[0,]),
                                 labels = ""))
    

setMethod("apply_split", "AnalyzeVarSplit",
          function(spl, df, vals = NULL, lbls = NULL) {
    dat = df[!is.na(df[[spl_payload(spl)]]),]
    list(values = spl_payload(spl),
         datasplit = list(dat),
         labels = obj_label(spl))
})



    
