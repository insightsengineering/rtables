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

## probably don't need this

setMethod("make_subset_expr", "expression",
          function(spl, val) spl)

setMethod("make_subset_expr", "character",
          function(spl, val) {
    newspl = Split(spl, type = "varlevels", spl)
    make_subset_expr(newspl, val)
})

.combine_subset_exprs = function(ex1, ex2) {
    if(is.null(ex1) && is.expresssion(ex2))
        return(exb2)
    
    stopifnot(is.expression(ex1), is.expression(ex2))
    as.expression(bquote((.(a)) & .(b), list(a = ex1[[1]], b = ex2[[1]])))
}
 
