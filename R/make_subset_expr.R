setGeneric("make_subset_expr", function(spl, val) standardGeneric("make_subset_expr"))
setMethod("make_subset_expr", "VarLevelSplit",
          function(spl, val) {

    as.expression(bquote(.(a) == .(b), list(a = as.name(spl_payload(spl)),
                              b = val)))


})

setMethod("make_subset_expr", "MultiVarSplit",
          function(spl, val) {
    as.expression(bquote(!is.na(.(a)), list(a = val)))
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
    as.expression(bquote(cut(.(a)) == .(b)),
                  list(a = as.name(spl_payload(spl)),
                       b = val))
})

setMethod("make_subset_expr", "VarDynCutSplit",
          function(spl, val) {
    as.expression(bquote(.(fun)(.(a)) == .(b)),
                  list(a = as.name(spl_payload(spl)),
                       b = val,
                       fun = spl@cut_fun))
})

setMethod("make_subset_expr", "AllSplit",
          function(spl, val) expression(TRUE))



## probably don't need this

setMethod("make_subset_expr", "expression",
          function(spl, val) spl)

setMethod("make_subset_expr", "character",
          function(spl, val) {
    newspl = Split(spl, type = "varlevels", spl)
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
