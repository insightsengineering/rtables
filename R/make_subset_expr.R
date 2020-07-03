
setGeneric("make_subset_expr", function(spl, val) standardGeneric("make_subset_expr"))
setMethod("make_subset_expr", "VarLevelSplit",
          function(spl, val) {
    v = rawvalues(val)
    as.expression(bquote((!is.na(.(a)) & .(a) == .(b)), list(a = as.name(spl_payload(spl)),
                              b = v)))


})

setMethod("make_subset_expr", "MultiVarSplit",
          function(spl, val) {
    v = rawvalues(val)
    as.expression(bquote(!is.na(.(a)), list(a = v)))
})

setMethod("make_subset_expr", "AnalyzeVarSplit",
          function(spl, val) {
    as.expression(bquote(!is.na(.(a)),
                         list(a = as.name(spl_payload(spl)))))
})

## XXX these are going to be ridiculously slow
## FIXME

setMethod("make_subset_expr", "VarStaticCutSplit",
          function(spl, val) {
    v = rawvalues(val)
    as.expression(bquote(which(cut(.(a), breaks=.(brk), labels = .(lbls),
                                   include.lowest = TRUE) == .(b)),
                  list(a = as.name(spl_payload(spl)),
                       b = v,
                       brk = spl_cuts(spl),
                       lbls = spl_cutlbls(spl))))
})

## NB this assumes spl_cutlbls(spl) is in order!!!!!!
setMethod("make_subset_expr", "CumulativeCutSplit",
          function(spl, val) {
    v = rawvalues(val)
    as.expression(bquote(which(as.integer(cut(.(a), breaks=.(brk),
                                              labels = .(lbls),
                                              include.lowest = TRUE)) <=
                               as.integer(factor(.(b), levels = .(lbls)))),
                  list(a = as.name(spl_payload(spl)),
                       b = v,
                       brk = spl_cuts(spl),
                       lbls = spl_cutlbls(spl))))
})



setMethod("make_subset_expr", "VarDynCutSplit",
          function(spl, val) {
    v = rawvalues(val)
    as.expression(bquote(which(.(fun)(.(a)) == .(b)),
                  list(a = as.name(spl_payload(spl)),
                       b = v,
                       fun = spl@cut_fun)))
})

setMethod("make_subset_expr", "AllSplit",
          function(spl, val) expression(TRUE))



## probably don't need this

setMethod("make_subset_expr", "expression",
          function(spl, val) spl)

setMethod("make_subset_expr", "character",
          function(spl, val) {
    newspl = VarLevelSplit(spl, spl)
    make_subset_expr(newspl, val)
})

.combine_subset_exprs = function(ex1, ex2) {
    if(is.null(ex1) && is.expression(ex2))
        return(ex2)
    
    stopifnot(is.expression(ex1), is.expression(ex2))
    as.expression(bquote((.(a)) & .(b), list(a = ex1[[1]], b = ex2[[1]])))
}
 

make_pos_subset = function(spls = pos_splits(pos),
                           svals = pos_splvals(pos),
                           pos) {
    expr = NULL
    for(i in seq_along(spls)) {
        newexpr = make_subset_expr(spls[[i]], svals[[i]])
        expr = .combine_subset_exprs(expr, newexpr)
    }
    expr
}


get_pos_extra = function(svals = pos_splvals(pos),
                         pos) {
    ret = list()
    for(i in seq_along(svals)) {
        extrs = splv_extra(svals[[i]])
        if(any(names(ret) %in% names(extrs)))
            stop("same extra argument specified at multiple levels of nesting. Not currently supported")
        ret = c(ret, extrs)
    }
    ret
}

get_col_extras = function(ctree) {
    leaves = collect_leaves(ctree)
    lapply(leaves,
           function(x) get_pos_extra(pos = tree_pos(x)))
}

setGeneric("make_col_subsets",function(lyt, df) standardGeneric("make_col_subsets"))
setMethod("make_col_subsets", "PreDataTableLayouts",
          function(lyt, df) {
    make_col_subsets(clayout(lyt), df)
})
setMethod("make_col_subsets", "PreDataColLayout",
          function(lyt, df) {
    unlist(lapply(lyt, make_col_subsets, df = df))
})

setMethod("make_col_subsets", "SplitVector",
          function(lyt, df) {
    build_splits_expr(lyt, df)
    
})

setMethod("make_col_subsets", "LayoutColTree",
          function(lyt, df) {
    leaves = collect_leaves(lyt)
    lapply(leaves, make_col_subsets)
})

setMethod("make_col_subsets", "LayoutColLeaf",
          function(lyt, df) {
    make_pos_subset(pos = tree_pos(lyt))
})



create_colinfo = function(lyt, df, rtpos = TreePos(),
                          counts = NULL) {
    ## this will work whether clayout is pre or post
    ## data
    clayout = clayout(lyt)
    
    ctree = coltree(clayout, df = df, rtpos = rtpos)

    cexprs = make_col_subsets(ctree, df)
    cextras = cextra_args(ctree)

    ## calculate the counts based on the df
    ## This presumes that it is called on the WHOLE dataset,
    ## NOT after any splitting has occured. Otherwise
    ## the counts will obviously be wrong.
    if(is.null(counts)) {
        counts = sapply(cexprs, function(ex) {
            if(identical(ex, expression(TRUE)))
                nrow(df)
            else if (identical(ex, expression(FALSE)))
                0
            else {
                vec = eval(ex, envir = df)
                if(is(vec, "numeric"))
                    length(vec) ## sum(is.na(.)) ????
                else if(is(vec, "logical"))
                    sum(vec, na.rm= TRUE)
            }
        })
    }
    fmt =  colcount_fmt(lyt)
    InstantiatedColumnInfo(treelyt = ctree,
                           csubs = cexprs,
                           extras = cextras,
                           cnts = counts,
                           dispcounts = disp_ccounts(lyt),
                           countfmt = fmt)
    
}

