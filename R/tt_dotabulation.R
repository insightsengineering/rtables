
match_extrargs = function(f, .N_col, .N_total, extras) {
    possargs = c(list(.N_col = .N_col, .N_total = .N_total),
                 extras)
    formargs = formals(f)
    possargs[names(possargs) %in% names(formargs)]
}


.takes_df = function(f) !is.null(formals(f)) && names(formals(f))[1] == "df"

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
        if(nrow(dat) == 0L)
            return(NULL)
        
        if(!is.null(col) && !.takes_df(func))
            dat = dat[[col]]
        args = list(dat)

        args = c(args,
                 match_extrargs(func, count, totcount, cextr))
        
        ret = do.call(func, args)
        if(!is.list(ret) && length(ret) > 1)
            ret = list(ret)
        ret
    }, csub = colexprs, col = datcol,
    count = colcounts,
    cextr = colextras,
    SIMPLIFY= FALSE)

    
    names(rawvals) = names(colexprs)
    rawvals
}

.make_tablerows = function(dfpart, func,
                           cinfo,
                           datcol = NULL,
                           lev = 1L,
                           rvlab = NA_character_,
##                           rvtypes = NULL,
                           format = NULL,
                           defrowlabs = NULL,
                           rowconstr = DataRow) {
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

    ## if(is.null(rvtypes))
    ##     rvtypes = rep(NA_character_, length(rawvals))
    lens = sapply(rawvals, length)
    unqlens = unique(lens)
    stopifnot(length(unqlens) == 1 ||
              (0 %in% unqlens && length(unqlens) == 2))
    maxind = match(max(unqlens), lens)

    ## look if we got labels, if not apply the
    ## default row labels
    lbls = names(rawvals[[maxind]])
    if(is.null(lbls) && length(rawvals[[maxind]]) == length(defrowlabs))
        lbls = defrowlabs
        
    ncrows = max(unqlens)
    stopifnot(ncrows > 0)
    ##recycle formats
    if(!is.null(format))
        fmtvec = rep(format, length.out = ncrows)
    else
        fmtvec = NULL
    trows = lapply(1:ncrows, function(i) {
        rowvals = lapply(rawvals, function(colvals) colvals[[i]])
        rowconstr(val = rowvals,
                  cinfo = cinfo,
                  lev = lev,
                  lab = lbls[i],
                  name = lbls[i], ## XXX this is probably the wrong thing!
                  var = rowvar,
                  fmt = fmtvec[i]
                  )
        
    })
    trows
}

.both_caller = function(pcfun, lblstr) {
    function(df2, .N_col, .N_total, ...) {
        pcfun(df2, lblstr, .N_col = .N_col, .N_total = .N_total, ...)
    }
}

.ncol_caller = function(pcfun, lblstr) {
    function(df2, .N_col, ...) {
        pcfun(df2, lblstr, .N_col = .N_col, ...)
    }
}

.ntot_caller  = function(pcfun, lblstr) {
    function(df2, .N_total, ...) {
        pcfun(df2, lblstr, .N_total = .N_total, ...)
    }
}

.neither_caller = .ntot_caller  = function(pcfun, lblstr) {
    function(df2,  ...) {
        pcfun(df2, lblstr, ...)
    }
}


.make_caller = function(parent_cfun, clblstr) {
    ## XXX Ugh. Combinatorial explosion X.X
    ## This is what I get for abusing scope to do
    ## the content label thing. Bad design.

    ## Ugh number 2. If we assign each of htese to the same name
    ## R CMD check complains so we return them as anon funcs to sneak
    ## by. Yet mr
    if(takes_coln(parent_cfun)) {
        if(takes_totn(parent_cfun)) {
            .both_caller(parent_cfun, clblstr)
        } else {
            .ncol_caller(parent_cfun, clblstr)
        }
    } else {
        if(takes_totn(parent_cfun)) {
            .ntot_caller(parent_cfun, clblstr)
        } else {
            .neither_caller(parent_cfun, clblstr)
        }
    }
}

.make_ctab = function(df, lvl, ##treepos,
                      name,
                      ##colexprs, coltree,
                      cinfo,
                      parent_cfun = NULL, format = NULL) {

 
    if(!is.null(parent_cfun)) {
         
            
        contkids = .make_tablerows(df,
                                   lev = lvl,
                                   func = .make_caller(parent_cfun, name), 
                                   cinfo = cinfo,
                                   rowconstr = ContentRow)
    } else {
        contkids = list()
    }
    
    ctab = ElementaryTable(kids = contkids,
                           name = paste0(name, "@content"),
                           lev = lvl,
                           labrow = LabelRow(),
                           cinfo = cinfo,
                           iscontent = TRUE,
                           fmt = format)
    ctab
}


.make_analyzed_tab = function(df, spl, cinfo, name, dolab = TRUE, ctab, lvl) {
    stopifnot(is(spl, "AnalyzeVarSplit"))
    check_validsplit(spl, df)
    kids = .make_tablerows(df,
                           func = analysis_fun(spl),
                           defrowlabs = spl@default_rowlabel, ##XXX XXX
                           cinfo = cinfo,
                           datcol = spl_payload(spl),
                           lev = lvl + 1L,
                           format = obj_fmt(spl))
    if(dolab)
        lab = obj_label(spl)
    else
        lab = ""
    TableTree(kids = kids,
              name = spl_payload(spl),
              lab = lab,
              lev = lvl,
              cinfo = cinfo,
              fmt = obj_fmt(spl))
}

recursive_applysplit = function( df,
                                lvl = 0L,
                                splvec,
                                name,
                                label,
                                make_lrow,
                                ##colexprs, coltree,
                                cinfo,
                                parent_cfun = NULL,
                                cformat = NULL) {
    

    ## pre-existing table was added to the layout
    if(length(splvec) == 1L && is(splvec[[1]], "VTableNodeInfo"))
        return(spl)
        
    ## the content function is the one from the PREVIOUS
    ## split, ie the one whose children we are now constructing
    ## this is a bit annoying but makes the semantics for
    ## declaring layouts much more sane.
    ctab = .make_ctab(df,
                      lvl = lvl,
                      name = name, 
                      cinfo = cinfo,
                      parent_cfun = parent_cfun, format = cformat)
    ## XXX TODO is this right??!?!?
    if((is.na(make_lrow) && nrow(ctab) > 0) ||
       identical(make_lrow, FALSE))
        label = ""
    ##    if(length(splvec) > 0) { ## there's more depth, recurse
        ## lvl is level of indentation, which starts at 0
    ## pos, is position in the splvec, which starts at 1
    ## because R is 1-indexed, so pos = lvl + 1L
    ## pos = lvl + 1L;
    ## stopifnot(pos <= length(splvec),
    ##           is(splvec, "SplitVector"))
    if(length(splvec) == 0L) {
        kids = list()
    } else {
        
        spl = splvec[[1]]
        splvec = splvec[-1]
        nonroot = lvl != 0L
        lab = obj_label(spl)

        if(is(spl, "AnalyzeVarSplit")) { ## we're at full depth, single analyze var
            kids = list(.make_analyzed_tab(df = df,
                                           spl = spl,
                                           cinfo = cinfo,
                                           lvl = lvl + 1L,
                                           dolab = nonroot))
            names(kids) = sapply(kids, obj_name)
        } else if(is(spl, "AnalyzeMultiVars")) { ## full depth multiple analyze vars
            ##     lab = ""
            kids = lapply(spl_payload(spl),
                          function(sp) {
                .make_analyzed_tab(df = df,
                                   spl = sp,
                                   cinfo = cinfo,
                                   lvl = lvl + 1L
                               )}
                )
            ## this will be the variables
            nms = sapply(spl_payload(spl), spl_payload)
            nms[is.na(nms)] = ""
            names(kids) = nms
        } else { #not Analyze(Multi)Var(s)
            rawpart = do_split(spl, df) ##apply_split(spl, df)
            dataspl = rawpart[["datasplit"]]
            ## these are SplitValue objects
            splvals = rawpart[["values"]]
            partlbls = rawpart[["labels"]]
            if(is.factor(partlbls))
                partlbls = as.character(partlbls)
            nms = unlist(splv_rawvalues(splvals))
            if(is.factor(nms))
                nms = as.character(nms)
            
            innerlev = lvl + 1L
            if(nonroot)
                innerlev = innerlev + 1L
            inner = unlist(mapply(function(dfpart,  nm, lbl) {
                recursive_applysplit(dfpart,
                                     name = nm, 
                                     label = lbl,
                                     lvl = innerlev,
                                     splvec = splvec,
                                     cinfo = cinfo,
                                     make_lrow = label_kids(spl),
                                     parent_cfun = content_fun(spl),
                                     cformat = obj_fmt(spl))
            }, dfpart = dataspl,
            lbl = partlbls,
            nm = nms,
            SIMPLIFY=FALSE))
            if(nonroot) {
                kids = list(TableTree(kids = inner,
                                      name = obj_name(spl),
                                      lab = obj_label(spl),
                                      lev = innerlev - 1L,
                                      cinfo = cinfo,
                                      spl = spl,
                                      fmt = obj_fmt(spl)))
                ## I'm enforcing this in the TableTree
                ## constructor now
                ##names(kids) = obj_name(spl)
            } else { ## is root
                kids = inner
            } ## end nonroot
        } ## end split type
    } ## end length(splvec)

    if(nrow(ctab) > 0L || length(kids) > 0L) {
        ret = TableTree(cont = ctab, kids = kids,
                        name = name,
                        lev = lvl,
                    iscontent = FALSE, 
                    spl = spl,
                    labrow = LabelRow(lev = lvl,
                                      lab = label,
                                      cinfo = cinfo),
                    cinfo = cinfo)
    } else {
        ret = NULL
    }
    ret
    
}


#' Create a table from a layout and data
#' 
#' Layouts are used to describe a table pre-data. `build_rable` is used to create a table using a layout and a dataset.
#' 
#' @inheritParams argument_conventions
#' 
#' @examples
#' 
#' library(magrittr)
#' 
#' l <- NULL %>% add_colby_varlevels("ARM") %>% 
#'     add_analyzed_vars("AGE", afun = function(x) {
#'       setNames(as.list(fivenum(x)), c("minimum", "lower-hinge", "median", "upper-hinge", "maximum"))
#'     })
#' 
#' l
#' 
#' build_table(l, DM) 
#' @export
build_table = function(lyt, df, ...) {
    rtpos = TreePos()
    lyt = set_def_child_ord(lyt, df)

    cinfo = create_colinfo(lyt, df, rtpos)
    
    rlyt = rlayout(lyt)
    rtspl = root_spl(rlyt)
    ctab = .make_ctab(df, 0L,
                      name = "root",
                      cinfo = cinfo, ##cexprs, ctree,
                      parent_cfun = content_fun(rtspl),
                      format = content_fmt(rtspl))
    kids = lapply(seq_along(rlyt), function(i) {
        
        splvec = rlyt[[i]]
        firstspl = splvec[[1]]
        nm = obj_label(firstspl) ## XXX this should be name!
        lab = obj_label(firstspl)
        recursive_applysplit(df = df, lvl = 0L,
                             name = nm,
                             label = lab,
                             splvec = splvec,
                             cinfo = cinfo,
                             ## XXX are these ALWAYS right?
                             make_lrow = NA,
                             parent_cfun = NULL,
                             cformat = obj_fmt(firstspl))
    })
    names(kids) = sapply(kids, obj_name)
                             
    tab = TableTree(cont = ctab,
                    kids = kids,
                    lev = 0L,
                    name = "root",
                    lab="",
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

setMethod("set_def_child_ord", "VarLevWBaselineSplit",
          function(lyt, df) {
    bline = spl_baseline(lyt)
    if(!is.null(spl_child_order(lyt)) &&
       match(bline, spl_child_order(lyt), nomatch = -1) == 1L)
        return(lyt)
    
    vec = df[[spl_payload(lyt)]]
    vals = unique(vec)
    if(is.factor(vals))
        vals = levels(relevel(droplevels(vals), bline)) # this sorts the levels
  
    stopifnot(bline %in% vals)
    pos = match(bline, vals)
    ## same order except baseline always first
    vals = c(bline, vals[-pos])
    spl_child_order(lyt) = vals
    lyt
})


splitvec_to_coltree = function(df, splvec, pos = NULL,
                               lvl = 1L, lbl = "") {
    stopifnot(lvl <= length(splvec) + 1L,
              is(splvec, "SplitVector"))

    nm = unlist(tail(splv_rawvalues(pos), 1)) %||% ""
    if(lvl == length(splvec) + 1L) {
        ## XXX this should be a LayoutColTree I Think.
        LayoutColLeaf(lev = lvl - 1L,
                      lab = lbl,
                      tpos = pos,
                      name = nm
                      )
    } else {
        spl = splvec[[lvl]]
        rawpart = do_split(spl,df, trim = )
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
                      name = nm, 
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



## rtabulate_layout2 =  function(x, layout, FUN, ...,
##                               format = NULL, row.name = "", indent  = 0,
##                               col_wise_args = NULL) {
  
##   force(FUN)
##  # check_stop_col_by(col_by, col_wise_args)
  
##   column_data <- if (n_leaves(coltree(layout)) == 0L) {
##     setNames(list(x), "noname(for now FIXME)")
##   } else {
##       ## if (length(x) != length(col_by)) stop("dimension missmatch x and col_by")

##       ## not handling nesting right now at all
##       leaves = layout_children(coltree(layout))
##       setNames(lapply(leaves,
##                       function(leaf) x[leaf@subset]),
##                sapply(leaves,
##                       function(leaf) leaf@label)
##                )
##   }
    
##   cells <- if (is.null(col_wise_args)) {
    
##     lapply(column_data, FUN, ...)
    
##   } else {
    
##     dots <- list(...)
##     args <- lapply(seq_len(nlevels(col_by)), function(i) c(dots, lapply(col_wise_args, `[[`, i)))
    
##     Map(function(xi, argsi) {
##       do.call(FUN, c(list(xi), argsi))
##     }, column_data, args)
##   }
  
##   rr <- rrowl(row.name = row.name, cells, format = format, indent = indent)
  
##   rtable(header = sapply(layout_children(coltree(layout)), function(leaf) leaf@label), rr)
## }

    
