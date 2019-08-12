## These functions will build up a "layout" object.
##  S4

## e.g.
##
## add_colby_total() %>%>
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

empty_dominant_axis = function(layout) {
    stopifnot(is(layout, "RTablesLayout"))
    ((is(layout, "RowDominantLayout") && n_leaves(row_tree(layout)) == 0L) ||
     (is(layout, "ColDominantLayout") && n_leaves(col_tree(layout)) == 0L))

}

## pre-data layouts:
## structured as lists of lists of splits
## e.g. reference table 1 would have column  proto-layout
##  list(list(VarSplit("ARM"), VarSplit("SEX")))
## and row layout
##  list(list(AllSplit()),
##       list(VarSplit("RACE"),  VarSplit("Factor2")),
##       list(AllSplit()) ## remember var3  is the variable analyzed

setMethod("c", "SplitVector", function(x, ...) {
    arglst = list(...)
    stopifnot(all(sapply(arglst, is, "Split")))
    tmp = c(unclass(x), arglst)
    SplitVector(lst = tmp)
})

setGeneric("add_row_split", function(lyt = NULL, spl, pos) standardGeneric("add_row_split"))

setMethod("add_row_split", "NULL", function(lyt, spl, pos) {
    rl = PreDataRowLayout(SplitVector(spl))
    cl = PreDataColLayout()
    PreDataTableLayouts(rlayout = rl, clayout = cl)
})

setMethod("add_row_split", "PreDataRowLayout",
          function(lyt, spl, pos) {
    stopifnot(pos >0 && pos <= length(lyt) + 1)
    tmp  = if (pos <= length(lyt)) {
               add_row_split(lyt[[pos]], spl, pos)
           } else {
               SplitVector(spl)
           }
    lyt[[pos]] = tmp
    lyt
})
setMethod("add_row_split", "SplitVector",
          function(lyt, spl, pos) {
    tmp = c(unclass(lyt), spl)
    SplitVector(lst = tmp)
})
    

setMethod("add_row_split", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt = lyt@row_layout
    rlyt = add_row_split(rlyt, spl, pos)
    lyt@row_layout = rlyt
    lyt
})

setMethod("add_row_split", "ANY",
          function(lyt, spl, pos) stop("nope. can't add a row split to that (", class(lyt), "). contact the maintaner.")
          )

setGeneric("add_col_split", function(lyt = NULL, spl, pos) standardGeneric("add_col_split"))

setMethod("add_col_split", "NULL", function(lyt, spl, pos) {
    cl = PreDataColLayout(SplitVector(spl))
    rl = PreDataRowLayout()
    PreDataTableLayouts(rlayout = rl, clayout = cl)
})

setMethod("add_col_split", "PreDataColLayout",
          function(lyt, spl, pos) {
    stopifnot(pos >0 && pos <= length(lyt) + 1)
    tmp  = if (pos <= length(lyt)) {
               add_col_split(lyt[[pos]], spl, pos)
           } else {
               SplitVector(spl)
           }
    lyt[[pos]] = tmp
    lyt
})

setMethod("add_col_split", "SplitVector",
          function(lyt, spl, pos) {
    tmp = c(lyt, spl)
    SplitVector(lst = tmp)
})

setMethod("add_col_split", "PreDataTableLayouts",
          function(lyt, spl, pos){
    rlyt = lyt@col_layout
    rlyt = add_col_split(rlyt, spl, pos)
    lyt@col_layout = rlyt
    lyt
})

setMethod("add_col_split", "ANY",
          function(lyt, spl, pos) stop("nope. can't add a col split to that (", class(lyt), "). contact the maintaner.")
          )




add_new_rowtree = function(lyt, spl) {
    add_row_split(lyt, spl, length(lyt) + 1)
}


add_new_coltree = function(lyt, spl) {
    add_col_split(lyt, spl, length(lyt) + 1)
}


add_colby_varlevels = function(lyt,  var, lbl, valuelblvar = var,  newtoplev = FALSE) {
    spl = VarLevelSplit(var = var, splbl = lbl, valuelblvar = valuelblvar)
    pos = length(lyt) + as.numeric(newtoplev)
    add_col_split(lyt, spl, pos)
}


add_rowby_varlevels = function(lyt,  var, lbl,  valuelblvar, newtoplev = FALSE) {
    spl = VarLevelSplit(var = var, splbl = lbl)
    pos = length(lyt) + as.numeric(newtoplev)
    add_row_split(lyt, spl, pos)
}


add_colby_multivar = function(lyt, vars, lbl, varlbls,
                              newtoplev = FALSE) {
    spl = MultiVarSplit(vars = vars, splbl = lbl, varlbls)
    pos = length(lyt) + as.numeric(newtoplev)
    add_col_split(lyt, spl, pos)


}


add_rowby_multivar = function(lyt, vars, lbl, varlbls,
                              newtoplev = FALSE) {
    spl = MultiVarSplit(vars = vars, splbl = lbl, varlbls)
    pos = length(lyt) + as.numeric(newtoplev)
    add_row_split(lyt, spl, pos)
}



add_colby_staticcut = function(lyt, var, lbl, cuts,
                            cutlbls = NULL,
                            newtoplev = FALSE) {
    spl = VarStaticCutSplit(var, lbl, cuts, cutlbls)
    pos = length(lyt) + as.numeric(newtoplev)
    add_col_split(lyt, spl, pos)
}

add_rowby_staticcut = function(lyt, var, lbl, cuts,
                            cutlbls = NULL,
                            newtoplev = FALSE) {
    spl = VarStaticCutSplit(var, lbl, cuts, cutlbls)
    pos = length(lyt) + as.numeric(newtoplev)
    add_row_split(lyt, spl, pos)
}

add_colby_dyncut = function(lyt, var, lbl, cutfun,
                            newtoplev = FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun)
    pos = length(lyt) + as.numeric(newtoplev)
    add_col_split(lyt, spl, pos)
}


add_rowby_dyncut = function(lyt, var, lbl, cutfun,
                            newtoplev = FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun)
    pos = length(lyt) + as.numeric(newtoplev)
    add_row_split(lyt, spl, pos)
}












## playground, once done modify rtabulate_default etc

rtabulate_layout <- function(x, layout, FUN, ...,
                              format = NULL, row.name = "", indent  = 0,
                              col_wise_args = NULL) {
  
  force(FUN)
 # check_stop_col_by(col_by, col_wise_args)
  
  column_data <- if (n_leaves(col_tree(layout)) == 0L) {
    setNames(list(x), "noname(for now FIXME)")
  } else {
      ## if (length(x) != length(col_by)) stop("dimension missmatch x and col_by")

      ## not handling nesting right now at all
      leaves = layout_children(col_tree(layout))
      setNames(lapply(leaves,
                      function(leaf) x[leaf@subset]),
               sapply(leaves,
                      function(leaf) leaf@label)
               )
  }
    
  cells <- if (is.null(col_wise_args)) {
    
    lapply(column_data, FUN, ...)
    
  } else {
    
    dots <- list(...)
    args <- lapply(seq_len(nlevels(col_by)), function(i) c(dots, lapply(col_wise_args, `[[`, i)))
    
    Map(function(xi, argsi) {
      do.call(FUN, c(list(xi), argsi))
    }, column_data, args)
  }
  
  rr <- rrowl(row.name = row.name, cells, format = format, indent = indent)
  
  rtable(header = sapply(layout_children(col_tree(layout)), function(leaf) leaf@label), rr)
}

setGeneric("apply_split", 
           function(spl, df, curexpr = NULL) standardGeneric("apply_split"))

setMethod("apply_split", "VarLevelSplit",
          function(spl, df) {
    varvec = df[[spl_payload(spl)]]
    fct = factor(varvec, levels = unique(varvec))
    spl = split(df, fct)
})

setMethod("apply_split", "MultiVarSplit",
          function(spl, df) {
    vars = spl_payload(spl)
    lst = lapply(vars, function(v) {
        df[!is.na(df[[v]]),]
    })
    names(lst) = vars
    lst
})

setMethod("apply_split", "AllSplit",
          function(spl, df) list(df))

setMethod("apply_split", "NULLSplit",
          function(spl, df) list(df[0,]))
    

setMethod("apply_split", "AnalyzeVarSplit",
          function(spl, df) {
    dat = df[!is.na(df[[spl_payload(spl)]]),]
    list(dat)
})


setGeneric("build_table", function(spl, df, colexprs) standardGeneric("build_table"))

setMethod("build_table", "Split",
          function(spl, df, colexprs) {
    if(!is.null(content_fun(spl))) {
        rawvals = lapply(colexprs,
                         function(csub) {
            content_fun(spl)(df[csub,])
        })
        ncrows = max(sapply(rawvals, length))
        stopifnot(ncrows > 0)
        valrows = lapply(1:ncrows, function(i) {
            lapply(rawvals, function(colvals) colvals[[i]])
        })
        

    }


})






setGeneric("build_splits_expr", function(lyt, df, curexpr) standardGeneric("build_splits_expr"))




setGeneric("expr_stubs", function(spl, df) standardGeneric("expr_stubs"))

setMethod("expr_stubs", "VarLevelSplit",
          function(spl, df) {
    var = spl_payload(spl)
    values = unique(df[[var]])
    lapply(values, function(val) make_subset_expr(spl, val))
})

setMethod("expr_stubs", "MultiVarSplit",
          function(spl, df) {
    vars = spl_payload(spl)
    lapply(vars, function(val) make_subset_expr(spl, val)) 
})



setMethod("expr_stubs", "AllSplit",
          function(spl, df) expression(TRUE))

setMethod("expr_stubs", "NULLSplit",
          function(spl, df) expression(FALSE))

setMethod("expr_stubs", "SplitVector",
          function(spl, df) lapply(spl, expr_stubs, df = df))






setMethod("build_splits_expr", "SplitVector",
          function(lyt, df) {
    stubs = expr_stubs(lyt, df) 
    curexpr = NULL
    ret = vector("list", prod(sapply(stubs, length)))
    blocksize = 1
    for(pos in rev(seq_along(lyt))) {
        curstubs = stubs[[pos]]
        nstubs = length(curstubs)
        vecothangs = rep(rep(curstubs,
                             times = rep(blocksize, times = nstubs)),
                         times = length(ret) / (blocksize * nstubs))
        blocksize = blocksize * nstubs
        if(is.null(ret[[1]]))
            ret = vecothangs
        else
            ret = mapply(.combine_subset_exprs,
                     ex2 = ret,
                     ex1 = vecothangs, SIMPLIFY = FALSE)
    }
    ret
})

tmpfun = function(lyt, df) {

}

## use data to transform dynamic cuts to static cuts
setGeneric("fix_dyncuts", function(spl, df) standardGeneric("fix_dyncuts"))

setMethod("fix_dyncuts", "Split", function(spl, df) spl)
setMethod("fix_dyncuts", "VarDyCutSplit",
          function(spl, df) {

    var = spl_payload(spl)
    varvec = df[[var]]

    cfun = spl@cut_fun
    cuts = cfun(varvec)
    VarStaticCutSplit(var = var, splbl = spl_lbl(spl),
                      cuts = cuts, cutlbls = names(cuts))
})


.fd_helper = function(spl, df) {
    lst = lapply(spl, fix_dyncuts, df = df)
    as(lst, class(spl))
}
       
setMethod("fix_dyncuts", "PreDataAxisLayout",
          function(spl, df) {
    .fd_helper(spl, df)
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

setGeneric("make_col_subsets",function(lyt, df) standardGeneric("make_col_subsets"))
setMethod("make_col_subsets", "PreDataTableLayouts",
          function(lyt, df) {
    make_col_subsets(clayout(lyt), df)
})
setMethod("make_col_subsets", "PreDataColLayout",
          function(lyt, df) {
    unlist(lapply(lyt, make_col_subsets))
})

setMethod("make_col_subsets", "SplitVector",
          function(lyt, df) {
})

rtabulate_layout2 =  function(x, layout, FUN, ...,
                              format = NULL, row.name = "", indent  = 0,
                              col_wise_args = NULL) {
  
  force(FUN)
 # check_stop_col_by(col_by, col_wise_args)
  
  column_data <- if (n_leaves(col_tree(layout)) == 0L) {
    setNames(list(x), "noname(for now FIXME)")
  } else {
      ## if (length(x) != length(col_by)) stop("dimension missmatch x and col_by")

      ## not handling nesting right now at all
      leaves = layout_children(col_tree(layout))
      setNames(lapply(leaves,
                      function(leaf) x[leaf@subset]),
               sapply(leaves,
                      function(leaf) leaf@label)
               )
  }
    
  cells <- if (is.null(col_wise_args)) {
    
    lapply(column_data, FUN, ...)
    
  } else {
    
    dots <- list(...)
    args <- lapply(seq_len(nlevels(col_by)), function(i) c(dots, lapply(col_wise_args, `[[`, i)))
    
    Map(function(xi, argsi) {
      do.call(FUN, c(list(xi), argsi))
    }, column_data, args)
  }
  
  rr <- rrowl(row.name = row.name, cells, format = format, indent = indent)
  
  rtable(header = sapply(layout_children(col_tree(layout)), function(leaf) leaf@label), rr)
}

    
