
match_extrargs = function(f, .N_col, .N_total, extras) {
    possargs = c(list(.N_col = .N_col, .N_total = .N_total),
                 extras)
    formargs = formals(f)
    possargs[names(possargs) %in% names(formargs)]
}


gen_rowvalues = function(dfpart, datcol, cinfo, func, spl) {
    colexprs = col_exprs(cinfo)
    colcounts = col_counts(cinfo)
    colextras = cextra_args(cinfo, NULL)
    ## XXX this is an assumption that could???? be
    ## wrong in certain really weird corner cases?
    totcount = sum(colcounts)
    
    if(!is.null(datcol) && is.na(datcol)) {
        colleaves =  collect_leaves(cinfo@tree_layout)
        datcol = sapply(colleaves,
                        function(x) {
            pos = tree_pos(x)
            spls = pos_splits(pos)
            splvals = splv_rawvalues(pos_splvals(pos))
            n = length(spls)
            if(is(spls[[n]], "MultiVarSplit"))
                splvals[n]
            else
                NA_character_
        })
        if(all(is.na(datcol)))
            datcol = list(NULL)
        else if(any(is.na(datcol)))
            stop("mix of var and non-var columns with NA analysis rowvara")
    } else if(!is.null(datcol)) {
        datcol = rep(datcol, length(colexprs))
    } else {
        datcol = list(NULL)
    }


    rawvals = mapply(function(csub, col, count, cextr) {


        inds = eval(csub, envir = dfpart)
        dat = dfpart[inds,]
        if(!is.null(col))
            dat = dat[[col]]
        args = list(dat)

        args = c(args,
                 match_extrargs(func, count, totcount, cextr))
        
        
        ## if(takes_coln(func))
        ##     args = c(args, list(.N_col = count))

        ## if(takes_totn(func))
        ##     args = c(args, list(.N_total = totcount))
        
        do.call(func, args)
    }, csub = colexprs, col = datcol,
    count = colcounts,
    cextr = colextras,
    SIMPLIFY= FALSE)

    names(rawvals) = names(colexprs)
    rawvals
}
 
.make_tablerows = function(dfpart, func,
                           ##colexprs, coltree,
                           cinfo,
                           tabpos, datcol = NULL, lev = 1L, rvlab = NA_character_, rvtypes = NULL, format = NULL) {
    if(is.null(datcol) && !is.na(rvlab))
        stop("NULL datcol but non-na rowvar label")
    if(!is.null(datcol) && !is.na(datcol)) {
        if(! all(datcol %in% names(dfpart)))
            stop("specified analysis variable (", datcol, ") not present in data")
        
        rowvar = datcol
    } else {
        rowvar  = NA_character_
    }
    
    rawvals = gen_rowvalues(dfpart, datcol, cinfo, func)
    ## rowvar = if(!is.null(datcol) && !is.na(datcol)) datcol else NA_character_
      if(is.null(rvtypes))
        rvtypes = rep(NA_character_, length(rawvals))
    lens = sapply(rawvals, length)
    stopifnot(length(unique(lens)) == 1)
    lbls = names(rawvals[[1]])
    ncrows = lens[1]
    stopifnot(ncrows > 0)
    ##recycle formats
    fmtvec = rep(format, length.out = ncrows)
    trows = lapply(1:ncrows, function(i) {
        rowvals = lapply(rawvals, function(colvals) colvals[[i]])
        TableRow(val = rowvals,tpos = make_rowpos(tabpos, i),
                 ##clayout = coltree(cinfo),
                 cinfo = cinfo,                 lev = lev,
                 lab = lbls[i],
                 var = rowvar,
                 var_lbl = rvlab,
                 v_type = rvtypes[i],
                 fmt = fmtvec[i]
                 ##XXX TODO label!!!!
                 ## lab = spl@label)
                 )
        
    })
    trows
}

.make_ctab = function(df, lvl, spl, treepos,
                      ##colexprs, coltree,
                      cinfo,
                      parent_cfun = NULL, format = NULL) {

 
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

        ## XXX Ugh. Combinatorial explosion X.X
        ## This is what I get for abusing scope to do
        ## the content label thing. Bad design.
        if(takes_coln(parent_cfun)) {
            if(takes_totn(parent_cfun)) {
                caller = function(df2, .N_col, .N_total, ...)
                    parent_cfun(df2, clblstr, .N_col = .N_col, .N_total = .N_total, ...)
            } else {
                 caller = function(df2, .N_col, ...)
                     parent_cfun(df2, clblstr, .N_col = .N_col, ...)
            }
        } else {
           if(takes_totn(parent_cfun)) {
                caller = function(df2, .N_total, ...)
                    parent_cfun(df2, clblstr, .N_total = .N_total, ...)
            } else {
                 caller = function(df2,x, ...)
                     parent_cfun(df2, clblstr, ...)
            }
        }
            
        contkids = .make_tablerows(df,
                                   lev = lvl,
                                   caller, 
                                   cinfo,
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
                           ##  clayout = coltree(cinfo),
                           cinfo = cinfo,
                           iscontent = TRUE,
                           lab = clbl,
                           fmt = format)
    ctab
}



recursive_applysplit = function( df, lvl = 0L, splvec, treepos = NULL,
                                ##colexprs, coltree,
                                cinfo,
                                parent_cfun = NULL, cformat = NULL) {
    

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
    ctab = .make_ctab(df, lvl, spl, treepos,
                      ##colexprs, coltree,
                      cinfo = cinfo,
                      parent_cfun, format = cformat)
    if(pos < length(splvec)) { ## there's more depth, recurse
        rawpart = do_split(spl, df) ##apply_split(spl, df)
        dataspl = rawpart[["datasplit"]]
        ## these are SplitValue objects
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
                                 ## colexprs = colexprs,
                                 ## coltree = coltree,
                                 cinfo = cinfo,
                                 parent_cfun = content_fun(spl),
                                 cformat = obj_fmt(spl))
        }, dfpart = dataspl, val = splvals,
        lbl = partlbls,
        SIMPLIFY=FALSE))
    } else { ## we're at full depth, analyze
        stopifnot(is(spl, "AnalyzeVarSplit"))
        check_validsplit(spl, df)
        kids = .make_tablerows(df,
                               analysis_fun(spl),
                               ## colexprs,
                               ## coltree,
                               cinfo = cinfo,
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
              ##clayout = coltree(cinfo))
              cinfo = cinfo)
}


build_table = function(lyt, df, ...) {
    rtpos = TreePos()
    lyt = set_def_child_ord(lyt, df)

    cinfo = create_colinfo(lyt, df, rtpos)
    
    rlyt = rlayout(lyt)
    rtspl = root_spl(rlyt)
    ctab = .make_ctab(df, 0L, rtspl,
                      rtpos, cinfo, ##cexprs, ctree,
                      parent_cfun = content_fun(rtspl),
                      format = content_fmt(rtspl))
    kids = lapply(seq_along(rlyt), function(i) {
        pos = TreePos(list(rtspl), list(i), as.character(i)) 
        recursive_applysplit(df = df, lvl = 0L,
                             splvec = rlyt[[i]],
                             treepos = pos,
                             cinfo = cinfo,
                             ## XXX is this ALWAYS right?
                             parent_cfun = NULL,
                             cformat = obj_fmt(rlyt[[i]][[1]]))
        })
                             
    tab = TableTree(cont = ctab,
                    kids = kids,
                    lev = 0L,
                    tpos = make_tablepos(rtpos, FALSE),
                    iscontent = FALSE,
                    spl = rtspl,
                    ##clayout = coltree(cinfo),
                    cinfo = cinfo,
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
        ## XXX this should be a LayoutColTree I Think.
        LayoutColLeaf(lev = lvl - 1L,
                      lab = lbl,
                      tpos = pos)
    } else {
        spl = splvec[[lvl]]
        rawpart = do_split(spl,df)
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
    sdat = do_split(spl, df) ##apply_split(spl, df)
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



## XXX do I really need two generics, make_col_subsets
## AND build_splits_expr???



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

rtabulate_layout2 =  function(x, layout, FUN, ...,
                              format = NULL, row.name = "", indent  = 0,
                              col_wise_args = NULL) {
  
  force(FUN)
 # check_stop_col_by(col_by, col_wise_args)
  
  column_data <- if (n_leaves(coltree(layout)) == 0L) {
    setNames(list(x), "noname(for now FIXME)")
  } else {
      ## if (length(x) != length(col_by)) stop("dimension missmatch x and col_by")

      ## not handling nesting right now at all
      leaves = layout_children(coltree(layout))
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
  
  rtable(header = sapply(layout_children(coltree(layout)), function(leaf) leaf@label), rr)
}

    
