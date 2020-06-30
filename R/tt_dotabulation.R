
match_extrargs = function(f, .N_col, .N_total, extras) {
    possargs = c(list(.N_col = .N_col, .N_total = .N_total),
                 extras)
    formargs = formals(f)
    formnms = names(formargs)
    if(is.null(formargs))
        return(NULL)
    else if("..." %in% names(formargs))
        formnms = c(formnms, names(extras))
    
    possargs[names(possargs) %in% formnms]
}


.takes_df = function(f) !is.null(formals(f)) && names(formals(f))[1] == "df"


gen_onerv = function(csub, col, count, cextr, dfpart, func, totcount, splextra) {
        inds = eval(csub, envir = dfpart)

        dat = dfpart[inds,,drop = FALSE]
                
        ## if(nrow(dat) == 0L)
        ##     return(list(NULL))
        
        if(!is.null(col) && !.takes_df(func))
            dat = dat[[col]]
        args = list(dat)

        args = c(args,
                 match_extrargs(func, count, totcount, extras = c(cextr, splextra)))
        
        val = do.call(func, args)
        if(is.list(val))
            ret = lapply(val, rcell)
        else
            ret = rcell(val)
        ret
}



gen_rowvalues = function(dfpart, datcol, cinfo, func, splextra) {
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
            splvals = rawvalues(pos)
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

    rawvals = mapply(gen_onerv, csub = colexprs, col = datcol,
                     count = colcounts,
                     cextr = colextras,
                     MoreArgs = list(dfpart = dfpart,
                                     func = func,
                                     totcount = totcount,
                                     splextra= splextra),
    SIMPLIFY= FALSE)

    
    names(rawvals) = names(colexprs)
    rawvals
}



.strip_lst_rvals <- function(lst) {
    lapply(lst, rawvalues)
}

#' @noRd    
#' @return a list of table rows, even when only one is generated
.make_tablerows = function(dfpart,
                           func,
                           cinfo,
                           datcol = NULL,
                           lev = 1L,
                           rvlab = NA_character_,
                           format = NULL,
                           defrowlabs = NULL,
                           rowconstr = DataRow,
                           splextra = list()) {
    if(is.null(datcol) && !is.na(rvlab))
        stop("NULL datcol but non-na rowvar label")
    if(!is.null(datcol) && !is.na(datcol)) {
        if(! all(datcol %in% names(dfpart)))
            stop("specified analysis variable (", datcol, ") not present in data")
        
        rowvar = datcol
    } else {
        rowvar  = NA_character_
    }
    
    rawvals = gen_rowvalues(dfpart, datcol, cinfo, func,splextra =  splextra)

    ## if(is.null(rvtypes))
    ##     rvtypes = rep(NA_character_, length(rawvals))
    lens = sapply(rawvals, length)
    unqlens = unique(lens)
    stopifnot(length(unqlens) == 1 ||
              (0 %in% unqlens && length(unqlens) == 2))
    maxind = match(max(unqlens), lens)

    ## look if we got labels, if not apply the
    ## default row labels
    rv1col = rawvals[[maxind]]
    if(is(rv1col, "CellValue"))
        lbls = obj_label(rv1col)
    else if (!is.null(names(rv1col)))
        lbls = names(rv1col)
    else if(are(rv1col, "CellValue"))
        lbls = sapply(rv1col, obj_label)
    else
        lbls = NULL
    
    if(is.null(lbls)) {
        if(length(rawvals[[maxind]]) == length(defrowlabs))
            lbls = defrowlabs
        else
            lbls = rep("", length(rawvals[[maxind]]))
    }
    ncrows = max(unqlens)
    stopifnot(ncrows > 0)
    ##recycle formats
    if(ncrows ==  1)  {
        return(list(rowconstr(val = rawvals,
                         cinfo = cinfo,
                         lev = lev,
                         lbl = lbls,
                         name = lbls,
                         var = rowvar,
                         fmt = format)))
    }
    fmtvec = NULL
    if(!is.null(format)) {
        if(is.function(format) )
            format = list(format)
        fmtvec = rep(format, length.out = ncrows)
    }
    
    trows = lapply(1:ncrows, function(i) {
        rowvals = lapply(rawvals, function(colvals)
            {
                colvals[[i]]
            })
        rowconstr(val = rowvals,
                  cinfo = cinfo,
                  lev = lev,
                  lbl = lbls[i],
                  name = lbls[i], ## XXX this is probably the wrong thing!
                  var = rowvar,
                  fmt = fmtvec[[i]]
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
                      lbl,
                      cinfo,
                      parent_cfun = NULL,
                      format = NULL,
                      indent_mod = 0L) {

 
    if(!is.null(parent_cfun)) {
        contkids = .make_tablerows(df,
                                   lev = lvl,
                                   func = .make_caller(parent_cfun, lbl), 
                                   cinfo = cinfo,
                                   rowconstr = ContentRow)
    } else {
        contkids = list()
    }
    ctab = ElementaryTable(kids = contkids,
                           name = paste0(name, "@content"),
                           lev = lvl,
                           lblrow = LabelRow(),
                           cinfo = cinfo,
                           iscontent = TRUE,
                           fmt = format,
                           indent_mod = indent_mod)
    ctab
}


.make_analyzed_tab = function(df,
                              spl,
                              cinfo,
                              partlbl = "",
                              dolab = TRUE,
                              lvl) {
    stopifnot(is(spl, "AnalyzeVarSplit"))
    check_validsplit(spl, df)
    defrlbl = spl@default_rowlabel
    didlab  = FALSE
    if(nchar(defrlbl) == 0 && !missing(partlbl) && nchar(partlbl) > 0) {
        defrlbl = partlbl
        didlab = TRUE
    }
    kids = .make_tablerows(df,
                           func = analysis_fun(spl),
                           defrowlabs = defrlbl, # XXX
                           cinfo = cinfo,
                           datcol = spl_payload(spl),
                           lev = lvl + 1L,
                           format = obj_fmt(spl),
                           splextra = split_exargs(spl))
    vis = TRUE
    if(dolab && (!didlab || !identical(obj_label(spl), sapply(kids, obj_name))))
        lab = obj_label(spl)
    else
        lab = ""
    ret = TableTree(kids = kids,
              name = obj_name(spl),
              lbl = lab,
              lev = lvl,
              cinfo = cinfo,
              fmt = obj_fmt(spl),
              indent_mod = indent_mod(spl))
    ret
}

recursive_applysplit = function( df,
                                lvl = 0L,
                                splvec,
                                name,
                       #         label,
                                make_lrow = NA,
                                partlbl = "",
                                cinfo,
                                parent_cfun = NULL,
                                cformat = NULL,
                                cindent_mod = 0L) {
    

    ## pre-existing table was added to the layout
    if(length(splvec) == 1L && is(splvec[[1]], "VTableNodeInfo"))
        return(splvec[[1]])
        
    ## the content function is the one from the PREVIOUS
    ## split, ie the one whose children we are now constructing
    ## this is a bit annoying but makes the semantics for
    ## declaring layouts much more sane.
    ctab = .make_ctab(df,
                      lvl = lvl,
                      name = name,
                      lbl = partlbl,
                      cinfo = cinfo,
                      parent_cfun = parent_cfun,
                      format = cformat,
                      indent_mod = cindent_mod)
    
    
    if(length(splvec) == 0L) {
        kids = list()
    } else {
        
        spl = splvec[[1]]
        splvec = splvec[-1]
        nonroot = lvl != 0L
        lab = obj_label(spl)
        indentmod = 0L ## changed if necessary in else block

        if(is(spl, "AnalyzeVarSplit")) { ## we're at full depth, single analyze var
            ret = .make_analyzed_tab(df = df,
                                     spl = spl,
                                     cinfo = cinfo,
                                     lvl = lvl + 1L,
                                     dolab = isTRUE(make_lrow),
                                     partlbl = obj_label(spl))##partlbl)
  
            kids = list(ret)
            names(kids) = obj_name(ret)
        } else if(is(spl, "AnalyzeMultiVars")) { ## full depth multiple analyze vars
            ##     lab = ""

            avspls = spl_payload(spl)
            
            kids = lapply(avspls,
                          function(sp) {
                .make_analyzed_tab(df = df,
                                   spl = sp,
                                   cinfo = cinfo,
                                   lvl = lvl + 1L,
                                   partlbl = obj_label(sp),
                                   dolab = isTRUE(label_kids(spl)) || length(avspls) > 1)}
                )
            if(!identical(make_lrow, FALSE) &&
               nrow(ctab) == 0 &&
               length(kids) == 1) {
                ## we only analyzed one var so
                ## we don't need an extra wrapper table
                ## in the structure
                stopifnot(identical(obj_name(kids[[1]]),
                                    spl_payload(spl)))
                return(kids[[1]])
            }
            ## this will be the variables
            nms = sapply(spl_payload(spl), spl_payload)
            nms[is.na(nms)] = ""
            names(kids) = nms
        } else { #not Analyze(Multi)Var(s)
            rawpart = do_split(spl, df)
            dataspl = rawpart[["datasplit"]]
            ## these are SplitValue objects
            splvals = rawpart[["values"]]
            partlbls = rawpart[["labels"]]
            if(is.factor(partlbls))
                partlbls = as.character(partlbls)
            nms = unlist(rawvalues(splvals))
            if(is.factor(nms))
                nms = as.character(nms)
            
            innerlev = lvl + (nrow(ctab) > 0 || is.na(make_lrow) || make_lrow)
            ## if(nonroot)
            ##     innerlev = innerlev + 1L
            inner = unlist(mapply(function(dfpart,  nm, lbl) {
                recursive_applysplit(dfpart,
                                     name = nm, 
                     #                label = lbl,
                                     lvl = innerlev,
                                     splvec = splvec,
                                     cinfo = cinfo,
                                     make_lrow = label_kids(spl),
                                     parent_cfun = content_fun(spl),
                                     cformat = obj_fmt(spl),
                                     partlbl = lbl,
                                     cindent_mod = content_indent_mod(spl))
            }, dfpart = dataspl,
            lbl = partlbls,
            nm = nms,
            SIMPLIFY=FALSE))
            kids = inner
            indentmod = indent_mod(spl)
        } ## end split type
    } ## end length(splvec)

    if(is.na(make_lrow))
        make_lrow = if(nrow(ctab) > 0 || !nzchar(partlbl)) FALSE else TRUE
    ## never print an empty row label for root. 
    if(make_lrow && partlbl == "" && !nonroot)
        make_lrow = FALSE

    if(nrow(ctab) == 0L && length(kids) == 1L && !make_lrow) {
        ret = kids[[1]]
        indent_mod(ret) = indent_mod(spl)
     } else if(nrow(ctab) > 0L || length(kids) > 0L) {
        ## avoid visible label rows when the row.names
        ## directly repeat the info.
         if(length(kids) == 1L &&
            identical(partlbl, row.names(kids[[1]])))
             tlbl = ""
         else
             tlbl = partlbl
         kids = lapply(kids, function(x) {
             indent_mod(x) = indent_mod(spl)
             x
         })
         ret = TableTree(cont = ctab,
                         kids = kids,
                        name = name,
                        lbl = tlbl, #partlbl,
                        lev = lvl,
                    iscontent = FALSE, 
                    lblrow = LabelRow(lev = lvl,
                                      lbl = tlbl,
                                      cinfo = cinfo,
                                      vis = make_lrow),
                    cinfo = cinfo)
    } else {
        ret = NULL
    }
    ## message(sprintf("indent modifier: %d", indentmod))
    ## if(!is.null(ret))
    ##     indent_mod(ret) = indentmod
    ret
    
}



#' Create a table from a layout and data
#'
#' Layouts are used to describe a table pre-data. `build_rable` is used to
#' create a table using a layout and a dataset.
#' 
#' 
#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @param col_counts numeric (or `NULL`). If non-null, column counts which
#'   override those calculated automatically during tabulation.
#'
#' @note When overriding the column counts care must be taken that, e.g.,
#'   `length()` or `nrow()` are not called within tabulation functions, because
#'   those will NOT give the overridden counts. Writing/using tabulation
#'   functions which accept \code{.N_col} and \code{.N_total} or do not rely on
#'   column counts at all (even implicitly) is the only way to ensure overriden
#'   counts are fully respected.
#'   
#' @export
#' 
#' @author Gabriel Becker
#' 
#' @examples
#' 
#' l <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   analyze("Sepal.Length", afun = function(x) {
#'   list(
#'     "mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
#'     "range" = diff(range(x))
#'   )
#' })
#' 
#' l
#' 
#' build_table(l, iris)
#' 
#' # analyze multiple variables
#' l <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = function(x) {
#'   list(
#'     "mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
#'     "range" = diff(range(x))
#'   )
#' })
#' 
#' build_table(l, iris)
#' 
#' # an example more relevant for clinical trials
#' l <- basic_table() %>%
#'     split_cols_by("ARM") %>% 
#'     analyze("AGE", afun = function(x) {
#'       setNames(as.list(fivenum(x)), c("minimum", "lower-hinge", "median", "upper-hinge", "maximum"))
#'     })
#' 
#' build_table(l, DM)
#' 
#' build_table(l, subset(DM, AGE > 40))
#' 
#' # with column counts
#' l2 <- l %>%
#'   add_colcounts()
#' build_table(l2, DM)
#' 
#' # with manual column counts
#' build_table(l, DM, col_counts = 1:3)
#' 
build_table = function(lyt, df,
                       col_counts = NULL,
                       ...) {

    ## if no columns are defined (e.g. because lyt is NULL)
    ## add a single overall column as the "most basic"
    ## table column structure that makes sense
    clyt = clayout(lyt)
    if(length(clyt) ==1 && length(clyt[[1]]) == 0) {
        clyt[[1]] = add_overall_col(clyt[[1]], "")
        clayout(lyt) = clyt
    }
    
    lyt = fix_dyncuts(lyt, df)
    rtpos = TreePos()
    lyt = set_def_child_ord(lyt, df)

    cinfo = create_colinfo(lyt, df, rtpos,
                           counts = col_counts)
    
    rlyt = rlayout(lyt)
    rtspl = root_spl(rlyt)
    ctab = .make_ctab(df, 0L,
                      name = "root",
                      lbl = "",
                      cinfo = cinfo, ##cexprs, ctree,
                      parent_cfun = content_fun(rtspl),
                      format = content_fmt(rtspl),
                      indent_mod = 0L)
    kids = lapply(seq_along(rlyt), function(i) {
        splvec = rlyt[[i]]
        firstspl = splvec[[1]]
        nm = obj_label(firstspl) ## XXX this should be name!
        lab = obj_label(firstspl)
        recursive_applysplit(df = df, lvl = 0L,
                             name = nm,
  #                           label = lab,
                             splvec = splvec,
                             cinfo = cinfo,
                             ## XXX are these ALWAYS right?
                             make_lrow = label_kids(firstspl),
                             parent_cfun = NULL,
                             cformat = obj_fmt(firstspl))
    })
    names(kids) = sapply(kids, obj_name)
    if(nrow(ctab) == 0L &&
       length(kids) == 1L &&
       is(kids[[1]], "VTableTree")) {
        tab = kids[[1]]
    } else {
        tab = TableTree(cont = ctab,
                        kids = kids,
                        lev = 0L,
                        name = "root",
                        lbl="",
                        iscontent = FALSE,
                        cinfo = cinfo,
                        fmt = obj_fmt(rtspl))
    }
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
    vals <- if(is.factor(vec))
                levels(vec)
            else
                unique(vec)
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

    nm = unlist(tail(rawvalues(pos), 1)) %||% ""
    if(lvl == length(splvec) + 1L) {
        ## XXX this should be a LayoutColTree I Think.
        LayoutColLeaf(lev = lvl - 1L,
                      lbl = lbl,
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
        LayoutColTree(lev = lvl, lbl = lbl,
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

