
docat = function(obj) {
    if(!is(obj, "ElementaryTable") && nrow(obj@content) > 0 ){
        crows = nrow(obj@content)
        ccols = if(crows == 0) 0 else ncol(obj@content)
        cat(rep("*", obj@level), sprintf(" %s [%d x %d]\n",
                                         obj@content@label,
                                         crows, ccols),
            sep = "")
        
    }
    if(is(obj, "VTableTree") && length(tree_children(obj))) {
        kids = tree_children(obj)
        isr = which(sapply(kids, is, "TableRow"))
        ## can they ever be inteerspersed, I don't think so
        if(length(isr)) {
            r = kids[[isr[1]]]
            lv = r@level
            if(is.na(lv)) lv = 0
            cat(rep("*", lv),
                sprintf(" %s [%d x %d] \n",
                        obj@label,
                        length(kids),
                        length(r@leaf_value)),
                sep="")
            kids = kids[-isr]
        }
        lapply(kids, docat)
    }
    invisible(NULL)
}
    
## setMethod("show", "TableTree",
##           function(object) {
##     cat("\nA TableTree object\n")
##     docat(object)
    
## })

## setMethod("show", "ElementaryTable",
##           function(object) {
##     cat("\nAn ElementaryTableTree object\n")
##     docat(object)
    
## })

setMethod("show", "VTableTree",
          function(object) print(to_s3compat(object)))

setGeneric("payloadmsg", function(spl) standardGeneric("payloadmsg"))

setMethod("payloadmsg", "VarLevelSplit",
          function(spl) {
    spl_payload(spl)
})

setMethod("payloadmsg", "MultiVarSplit",
          function(spl) "variable")

setMethod("payloadmsg", "VarLevWBaselineSplit",
          function(spl) paste0(spl_payload(spl), "[baseline ",
                               spl@baseline_value, # XXX XXX
                               "]"))
setMethod("payloadmsg", "ANY",
          function(spl) {
    warning("don't nkow how to make payload print message for Split of class", class(spl))
    "XXX"
})

spldesc = function(spl, value = "") {
    if(is(value, "SplitValue"))
        value = splv_rawvalues(value)
    payloadmsg = payloadmsg(spl)
    fmt = "%s (%s)"
    sprintf(fmt,
            value,
            payloadmsg)
            

}


docatlayout = function(obj) {
    ## if(!is(obj, "VLayoutNode"))
    ##     stop("how did a non layoutnode object get in docatlayout??")

    pos = tree_pos(obj)
    spllst = pos_splits(pos)
    spvallst = pos_splvals(pos)
    if(istree <- is(obj, "LayoutAxisTree")) {
        kids = tree_children(obj)
        lapply(kids, docatlayout)
        return(NULL)
        
    }
    
    msg = paste(collapse = " -> ",
                mapply(spldesc, 
                       spl = spllst,
                       value = spvallst))
    cat(msg,
        "\n")
    NULL
}

setMethod("show", "LayoutAxisTree",
          function(object) {
    docatlayout(object)
    invisible(object)
})


setGeneric("spltype_abbrev", function(obj) standardGeneric("spltype_abbrev"))

setMethod("spltype_abbrev", "VarLevelSplit",
          function(obj) "lvls")

setMethod("spltype_abbrev", "VarLevWBaselineSplit", 
          function(obj) paste("baseline", obj@baseline_value))


setMethod("spltype_abbrev", "MultiVarSplit",
          function(obj) "vars")

setMethod("spltype_abbrev", "VarStaticCutSplit",
          function(obj) "scut")

setMethod("spltype_abbrev", "VarDynCutSplit",
          function(obj) "dcut")
setMethod("spltype_abbrev", "AllSplit",
          function(obj) "all obs")
setMethod("spltype_abbrev", "NULLSplit",
          function(obj) "no obs")

setMethod("spltype_abbrev", "AnalyzeVarSplit",
          function(obj) "** analyzed var **")

           


docat_splitvec = function(object, indent = 0) {
    if(indent > 0)
        cat(rep(" ", times = indent), sep = "")
    if(length(object) == 1L && is(object[[1]], "VTableNodeInfo")) {
        tab = object[[1]]
        msg = sprintf("A Pre-Existing Table [%d x %d]",
                      nrow(tab), ncol(tab))
    } else {

        plds = ploads_to_str(lapply(object, spl_payload))
        tabbrev = sapply(object, spltype_abbrev)
        msg = paste(collapse = " -> ",
                    paste0(plds, " (", tabbrev, ")"))
    }
    cat(msg, "\n")
}

setMethod("show", "SplitVector",
          function(object) {

    cat("A SplitVector Pre-defining a Tree Structure\n\n")
    docat_splitvec(object)
    cat("\n")
    invisible(object)
})


docat_predataxis = function(object, indent = 0) {
    lapply(object, docat_splitvec)
}

setMethod("show", "PreDataColLayout",
          function(object) {
    cat("A Pre-data Column Layout Object\n\n")
    docat_predataxis(object)
})


setMethod("show", "PreDataRowLayout",
          function(object) {
    cat("A Pre-data Row Layout Object\n\n")
    docat_predataxis(object)
})


setMethod("show", "PreDataTableLayouts",
          function(object) {
    cat("A Pre-data Table Layout\n")
    cat("\nColumn-Split Structure:\n")
    docat_predataxis(object@col_layout)
    cat("\nRow-Split Structure:\n")
    docat_predataxis(object@row_layout)
    cat("\n")
})


setMethod("show", "TreePos",
          function(object) {
    chars = mapply(function(lbl, val)
        {
            paste0(lbl, " [", val, "]")
        }, lbl = pos_split_lbls(object),
        val = pos_splval_lbls(object))
        
    msg = paste(chars, collapse = " -> ")
    cat("An object of class ", class(object), "\n\n", msg)
})
