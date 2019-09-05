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

## add_row_split and add_col_split are "recursive method stacks" which follow
## the general pattern of accept object -> call add_*_split on slot of object ->
## update object with value returned from slot method, return object.
##
## Thus each of the methods is idempotent, returning an updated object of the
## same class it was passed. The exception for idempotency is the NULL method
## which constructs a PreDataTableLayouts object with the specified split in the
## correct place.

## The cascading (by class) in this case is as follows for the row case:
## PreDataTableLayouts -> PreDataRowLayout -> SplitVector
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
    add_row_split(lyt, spl, next_rpos(lyt, TRUE))
}


add_new_coltree = function(lyt, spl) {
    add_col_split(lyt, spl, next_cpos(lyt, TRUE))
}


## Pipe-able functions to add the various types of splits to the current layout for both
## row and column.  These all act as wrappers to the add_col_split and add_row_split
## method stacks.
add_colby_varlevels = function(lyt,  var, lbl, valuelblvar = var, splfmt = NULL, newtoplev = FALSE) {
    spl = VarLevelSplit(var = var, splbl = lbl, valuelblvar = valuelblvar, splfmt = splfmt)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}


add_rowby_varlevels = function(lyt,  var, lbl,  vlblvar = var, splfun = NULL, fmt = NULL, newtoplev = FALSE) {
    spl = VarLevelSplit(var = var,
                        splbl = lbl,
                        valuelblvar = vlblvar,
                        splfun = splfun,
                        splfmt = fmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}


add_colby_multivar = function(lyt, vars, lbl, varlbls,
                              newtoplev = FALSE) {
    spl = MultiVarSplit(vars = vars, splbl = lbl, varlbls)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}


add_rowby_multivar = function(lyt, vars, lbl, varlbls,
                              splfmt = NULL,
                              newtoplev = FALSE) {
    spl = MultiVarSplit(vars = vars, splbl = lbl, varlbls,
                        splfmt = splfmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}

add_colby_staticcut = function(lyt, var, lbl, cuts,
                            cutlbls = NULL,
                            newtoplev = FALSE) {
    spl = VarStaticCutSplit(var, lbl, cuts, cutlbls)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}

add_rowby_staticcut = function(lyt, var, lbl, cuts,
                               cutlbls = NULL,
                               splfmt = NULL,
                            newtoplev = FALSE) {
    spl = VarStaticCutSplit(var, lbl, cuts, cutlbls,
                            splfmt = splfmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}

add_colby_dyncut = function(lyt, var, lbl, cutfun,
                            splfmt = NULL,
                            newtoplev = FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun,
                         splfmt = splfmt)
    pos = next_cpos(lyt, newtoplev)
    add_col_split(lyt, spl, pos)
}


add_rowby_dyncut = function(lyt, var, lbl, cutfun,
                            splfmt = NULL,
                            newtoplev = FALSE) {
    spl = VarDynCutSplit(var, lbl, cutfun,
                         splfmt = splfmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}


## add an anlysis split. this will be the
## end of the SplitVector at the position
## specified, because it defines an anlaysis
## that will generate rows, rather than
## a further partition of the data.
add_analyzed_var = function(lyt, var, lbl, afun,
                            fmt = NULL,
                            newtoplev = FALSE) {
    spl = AnalyzeVarSplit(var, lbl, afun = afun,
                          splfmt = fmt)
    pos = next_rpos(lyt, newtoplev)
    add_row_split(lyt, spl, pos)
}

## Add a total column at the next **top level** spot in
## the column layout. 
add_col_total = function(lyt, lbl) {
    spl = AllSplit(lbl)
    add_col_split(lyt,
                  spl,
                  next_cpos(lyt, TRUE))
}

setGeneric("add_summary",
           function(lyt, lbl, cfun, cfmt = NULL) standardGeneric("add_summary"))
setMethod("add_summary", "PreDataTableLayouts",
          function(lyt, lbl, cfun, cfmt = NULL) {
    tmp = add_summary(rlayout(lyt), lbl, cfun,
                      cfmt = cfmt)
    rlayout(lyt) = tmp
    lyt
})

setMethod("add_summary", "PreDataRowLayout",
          function(lyt, lbl, cfun, cfmt = NULL) {
    if(length(lyt) == 0 ||
       (length(lyt) == 1 && length(lyt[[1]]) == 0)) {
        rt = root_spl(lyt)
        rt = add_summary(rt, lbl, cfun, cfmt = cfmt)
        root_spl(lyt) = rt
    } else {
        ind = length(lyt)
        tmp = add_summary(lyt[[ind]], lbl, cfun,
                          cfmt = cfmt)
        lyt[[ind]] = tmp
    }
    lyt
})

setMethod("add_summary", "SplitVector",
          function(lyt, lbl, cfun, cfmt = NULL) {
    ind = length(lyt)
    if(ind == 0) stop("no split to add content rows at")
    spl = lyt[[ind]]
    ## if(is(spl, "AnalyzeVarSplit")) stop("can't add content rows to analyze variable split")
    tmp = add_summary(spl, lbl, cfun, cfmt = cfmt)
    lyt[[ind]] = tmp
    lyt
})

setMethod("add_summary", "Split",
          function(lyt, lbl, cfun, cfmt = NULL) {
    content_fun(lyt) = cfun
    obj_fmt(lyt) = cfmt
    lyt
})

add_summary_count = function(lyt, var = NULL, lblfmt = "%s", valfmt = "(N=xx)" ){
    fun = function(df, lblstr = "") {
        lbl = sprintf(lblfmt, lblstr)
        if(!is.null(var))
            ret = sum(!is.na(df[[var]]))
        else
            ret = nrow(df)
        attr(ret, "format") = valfmt
        names(ret) = lbl
        ret
    }
    add_summary(lyt, lbl = lbl, cfun = fun)
}

    
## Currently existing tables can ONLY be added 
## as new entries at the top level, never at any
## level of nesting.
add_existing_table = function(lyt, tab) {

    lyt = add_row_split(lyt,
                        tab,
                        next_rpos(lyt, TRUE))
    lyt
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



.make_tablerows = function(dfpart, func, colexprs, coltree, tabpos, datcol = NULL, lev = 1L, rvlab = NA_character_, rvtypes = NULL, format = NULL) {
    if(is.null(datcol) && !is.na(rvlab))
        stop("NULL datcol but non-na rowvar label")
    rawvals = lapply(colexprs,
                     function(csub) {
        inds = eval(csub, envir = dfpart)
        dat = dfpart[inds,]
        if(!is.null(datcol))
            dat = dat[[datcol]]
        func(dat)
    })
    rowvar = if(!is.null(datcol)) datcol else NA_character_
    if(is.null(rvtypes))
        rvtypes = rep(NA_character_, length(rawvals))
    lens = sapply(rawvals, length)
    stopifnot(length(unique(lens)) == 1)
    lbls = names(rawvals[[1]])
    ncrows = lens[1]
    stopifnot(ncrows > 0)
    trows = lapply(1:ncrows, function(i) {
        rowvals = lapply(rawvals, function(colvals) colvals[[i]])
        TableRow(val = rowvals,tpos = make_rowpos(tabpos, i),
                 clayout = coltree,
                 lev = lev,
                 lab = lbls[i],
                 var = rowvar,
                 var_lbl = rvlab,
                 v_type = rvtypes[i],
                 fmt = format
                 ##XXX TODO label!!!!
                 ## lab = spl@label)
                 )
        
    })
    trows
}

.make_ctab = function(df, lvl, spl, treepos, colexprs, coltree, parent_cfun = NULL, format = NULL) {

 
    ctpos = make_tablepos(treepos, iscontent = TRUE)
    if(!is.null(parent_cfun)) {
        splabs = pos_splval_lbls(ctpos)
        if(length(splabs) >= 1)
            clblstr = tail(splabs, 1)
        else
            clblstr = ""
        ## no need to include the format here because
        ## it will be set recursively by the
        ## ElementaryTable constructor below.
        contkids = .make_tablerows(df,
                                   lev = lvl,
                                   function(df2) parent_cfun(df2, clblstr),
                                   colexprs,
                                   coltree,
                                   ctpos)
    } else {
        contkids = list()
    }
    clbl = if(length(contkids) == 1) {
               obj_label(contkids[[1]])
           } else {
               obj_label(spl)
           }
    
    ctab = ElementaryTable(kids = contkids,
                           lev = lvl,
                           tpos = ctpos,
                           clayout = coltree,
                           iscontent = TRUE,
                           lab = clbl,
                           fmt = format)
    ctab
}



recursive_applysplit = function( df, lvl = 0L, splvec, treepos = NULL, colexprs, coltree, parent_cfun = NULL, cformat = NULL) {
    

    ## lvl is level of indentation, which starts at 0
    ## pos, is position in the splvec, which starts at 1
    ## because R is 1-indexed, so pos = lvl + 1L
    pos = lvl + 1L;
    stopifnot(pos <= length(splvec),
              is(splvec, "SplitVector"))
    spl = splvec[[pos]]

    ## pre-existing table was added to the layout
    if(is(spl, "VTableNodeInfo"))
        return(spl)
    ## the content function is the one from the PREVIOUS
    ## split, ie the one whose children we are now constructing
    ## this is a bit annoying but makes the semantics for
    ## declaring layouts much more sane.
    ctab = .make_ctab(df, lvl, spl, treepos, colexprs, coltree, parent_cfun, format = cformat)
    if(pos < length(splvec)) { ## there's more depth, recurse
        rawpart = apply_split(spl, df)
        dataspl = rawpart[["datasplit"]]
        splvals = rawpart[["values"]]
        partlbls = rawpart[["labels"]]
        kids = unlist(mapply(function(dfpart, val, lbl) {
            newpos = make_child_pos(treepos,
                                    spl,
                                    val,
                                    lbl)
            
            recursive_applysplit(dfpart,
                                 lvl = lvl+1L,
                                 splvec = splvec,
                                 treepos = newpos,
                                 colexprs = colexprs,
                                 coltree = coltree,
                                 parent_cfun = content_fun(spl),
                                 cformat = content_fmt(spl))
        }, dfpart = dataspl, val = splvals,
        lbl = partlbls,
        SIMPLIFY=FALSE))
    } else { ## we're at full depth, analyze
        stopifnot(is(spl, "AnalyzeVarSplit"))
        kids = .make_tablerows(df,
                               analysis_fun(spl),
                               colexprs,
                               coltree,
                               tabpos = make_tablepos(treepos, iscontent = FALSE),
                               datcol = spl_payload(spl),
                               lev = lvl + 1L,
                               format = obj_fmt(spl))
    }
    TableTree(cont = ctab, kids = kids,
              lev = lvl,
              tpos = make_tablepos(treepos = treepos,
                                   iscontent = FALSE),
              iscontent = FALSE,
              spl = spl,
              lab = obj_label(spl),
              clayout = coltree)
}


build_table = function(lyt, df, ...) {
    rtpos = TreePos()
    lyt = set_def_child_ord(lyt, df)
    
    ctree = splitvec_to_coltree(df, clayout(lyt)[[1]],
                                rtpos)
    cexprs = build_splits_expr(clayout(lyt)[[1]], rawdat)

    rlyt = rlayout(lyt)
    rtspl = root_spl(rlyt)
    ctab = .make_ctab(df, 0L, rtspl,
                      rtpos, cexprs, ctree,
                      parent_cfun = content_fun(rtspl))
    kids = lapply(seq_along(rlyt), function(i) {
        pos = TreePos(list(rtspl), list(i), as.character(i)) 
        recursive_applysplit(df = df, lvl = 0L,
                             splvec = rlyt[[i]],
                             treepos = pos,
                             colexprs = cexprs,
                             coltree = ctree,
                             ## XXX is this ALWAYS right?
                             parent_cfun = NULL)
        })
                             
    tab = TableTree(cont = ctab,
                    kids = kids,
                    lev = 0L,
                    tpos = make_tablepos(rtpos, FALSE),
                    iscontent = FALSE,
                    spl = rtspl,
                    clayout = ctree,
                    fmt = obj_fmt(rtspl))
    tab
}


## the table is built by recursively splitting the data
## and doing things to each piece. The order (or even values) of unique(df[[col]]) is not guaranteed to be the same in all the different partitions. This addresses that.
setGeneric("set_def_child_ord", function(lyt, df) standardGeneric("set_def_child_ord"))

setMethod("set_def_child_ord", "PreDataTableLayouts",
          function(lyt, df) {
    clayout(lyt) = set_def_child_ord(clayout(lyt), df)
    rlayout(lyt) = set_def_child_ord(rlayout(lyt), df)
    lyt
})

setMethod("set_def_child_ord", "PreDataAxisLayout",
          function(lyt, df) {
    lyt[] = lapply(lyt, set_def_child_ord, df = df)
    lyt
})

setMethod("set_def_child_ord", "SplitVector",
          function(lyt, df) {
    lyt[] = lapply(lyt, set_def_child_ord, df = df)
    lyt
})

## for most split types, don't do anything
## becuause their ordering already isn't data-based
setMethod("set_def_child_ord", "ANY",
          function(lyt, df) lyt)

setMethod("set_def_child_ord", "VarLevelSplit",
          function(lyt, df) {
    if(!is.null(spl_child_order(lyt)))
        return(lyt)
    
    vec = df[[spl_payload(lyt)]]
    vals = unique(vec)
    spl_child_order(lyt) = vals
    lyt
})

splitvec_to_coltree = function(df, splvec, pos = NULL,
                               lvl = 1L, lbl = "") {
    stopifnot(lvl <= length(splvec) + 1L,
              is(splvec, "SplitVector"))
    
    if(lvl == length(splvec) + 1L) {
        LayoutColLeaf(lev = lvl - 1L,
                      lab = lbl,
                      tpos = pos)
    } else {
        spl = splvec[[lvl]]
        rawpart = apply_split(spl, df)
        datparts = rawpart[["datasplit"]]
        vals = rawpart[["values"]]
        kids = mapply(function(dfpart, value) {
            ## XXX TODO label
            partlab = ""
            newpos = make_child_pos(pos, spl, value, partlab) 
            splitvec_to_coltree(dfpart, splvec, newpos,
                                lvl + 1L, partlab)
        }, dfpart = datparts, value = vals, SIMPLIFY=FALSE)
        LayoutColTree(lev = lvl, lab = lbl,
                      spl = spl,
                      kids = kids, tpos = pos,
                      summary_function = content_fun(spl))
    }
}





setGeneric("build_splits_expr", function(lyt, df, curexpr) standardGeneric("build_splits_expr"))




setGeneric("expr_stubs", function(spl, df) standardGeneric("expr_stubs"))

setMethod("expr_stubs", "VarLevelSplit",
          function(spl, df) {
    sdat = apply_split(spl, df)
    values = sdat$values
    ## var = spl_payload(spl)
    ## values = unique(df[[var]])
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
setMethod("fix_dyncuts", "VarDynCutSplit",
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

    
