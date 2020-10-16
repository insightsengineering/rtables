
match_extra_args = function(f, .N_col, .N_total, .var, .ref_group = NULL, .ref_full = NULL, .in_ref_col = NULL, .parent_splval = NULL, .N_row, .df_row, extras) {
    possargs = c(list(.N_col = .N_col, .N_total = .N_total, .N_row = .N_row, .df_row = .df_row),
                 extras)
    ## specialized arguments that must be named in formals, cannot go anonymously into ...
    if(!is.null(.var) && nzchar(.var))
        possargs = c(possargs, list(.var = .var))
    if(!is.null(.ref_group))
        possargs = c(possargs, list(.ref_group = .ref_group))
    if(!is.null(.ref_full))
        possargs = c(possargs, list(.ref_full = .ref_full))
    if(!is.null(.in_ref_col))
        possargs = c(possargs, list(.in_ref_col = .in_ref_col))
    if(!is.null(.parent_splval))
        possargs = c(possargs, list(.parent_splval= .parent_splval))


    formargs = formals(f)
    formnms = names(formargs)
    exnms = names(extras)
    if(is.null(formargs))
        return(NULL)
    else if("..." %in% names(formargs))
        formnms = c(formnms, exnms[nzchar(exnms)])

    possargs[names(possargs) %in% formnms]
}


.takes_df = function(f) {
    if(is.list(f))
        return(vapply(f, .takes_df, NA))
    !is.null(formals(f)) && names(formals(f))[1] == "df"
}

gen_onerv = function(csub, col, count, cextr, dfpart, func, totcount, splextra,
                     takesdf = .takes_df(func),
                     baselinedf,
                     inclNAs,
                     last_splval) {
        inds = eval(csub, envir = dfpart)

        dat = dfpart[inds,,drop = FALSE]

        if(!is.null(col) && !inclNAs)
            dat <- dat[!is.na(dat[[col]]),,drop = FALSE]

        fullrefcoldat = cextr$.ref_full
        if(!is.null(fullrefcoldat))
            cextr$.ref_full = NULL
        inrefcol = cextr$.in_ref_col
        if(!is.null(fullrefcoldat))
            cextr$.in_ref_col = NULL

        exargs = c(cextr, splextra)

        ## behavior for x/df and ref-data (full and group)
        ## match
        if(!is.null(col) && !takesdf) {
            dat = dat[[col]]
            fullrefcoldat = fullrefcoldat[[col]]
            baselinedf = baselinedf[[col]]
        }
        args = list(dat)


        args = c(args,
                 match_extra_args(func,
                                  .N_col = count,
                                  .N_total = totcount,
                                  .var = col,
                                  .ref_group = baselinedf,
                                  .ref_full = fullrefcoldat,
                                  .in_ref_col = inrefcol,
                                  .N_row = NROW(dfpart),
                                  .df_row = dfpart,
                                  .parent_splval = last_splval,
                                  extras = c(cextr,
                                             splextra)))

        val = do.call(func, args)
        if(!is(val, "CellValue") && is.list(val)) {
            ret = lapply(val, rcell)
            if(length(ret) == 1) {
                nm = names(ret)
                ret = ret[[1]]
                if(is.null(obj_label(ret)))
                    obj_label(ret) = nm
            }
        } else {
            ret = rcell(val)
        }
        ret
}



## Generate all values (one for each column) for one or more rows
## by calling func once per column (as defined by cinfo)
gen_rowvalues = function(dfpart,
                         datcol,
                         cinfo,
                         func,
                         splextra,
                         takesdf = NULL,
                         baselines, inclNAs,
                         last_splval = last_splval) {
    colexprs = col_exprs(cinfo)
    colcounts = col_counts(cinfo)
    colextras = col_extra_args(cinfo, NULL)
    splextra = c(splextra, list(last_splval))
    ## XXX this is an assumption that could???? be
    ## wrong in certain really weird corner cases?
    totcount = col_total(cinfo) ##sum(colcounts)

    colleaves =  collect_leaves(cinfo@tree_layout)


    gotflist <- is.list(func)

    ## one set of named args to be applied to all columns
    if(!is.null(names(splextra)))
        splextra = list(splextra)
    else
        length(splextra) = ncol(cinfo)


    if(!gotflist) {
        func <- list(func)
    } else  if(length(splextra) != length(func)) {
        splextra  = rep(splextra, length.out = length(func))
    }

    ## if(length(func)) == 1 && names(spl)
    ##     splextra = list(splextra)

    ## we are in analyze_colvars, so we have to match
    ## the exargs value by position for each column repeatedly
    ## across the higher level col splits.
    if(!is.null(datcol) && is.na(datcol)) {
        datcol <- character(length(colleaves))
        exargs <- vector("list", length(colleaves))
        for(i in 1:length(colleaves)) {
            x  = colleaves[[i]]

            pos = tree_pos(x)
            spls = pos_splits(pos)
            splvals = rawvalues(pos)
            n = length(spls)
            datcol[i] <- if(is(spls[[n]], "MultiVarSplit"))
                             splvals[n]
                         else
                             NA_character_
            argpos <- match(datcol[i], spl_payload(spls[[n]]))
            ## single bracket here because assigning NULL into a list removes the position entirely
            exargs[i] <- if(argpos <= length(splextra)) splextra[argpos]  else list(NULL)
        }
        ## })
        if(all(is.na(datcol)))
            datcol = list(NULL)
        else if(any(is.na(datcol)))
            stop("mix of var and non-var columns with NA analysis rowvara")
    } else {
        exargs <- splextra
        if(is.null(datcol))
            datcol = list(NULL)
        datcol = rep(datcol, length(colexprs))
        if(gotflist)
            length(exargs) <- length(func) ## func is a list
        exargs <- rep(exargs, length.out = length(colexprs))

    }


 #       exargs = rep## else if(!is.null(datcol)) { ## datcol is NA, we're in analyze_colvars
    ##     datcol = rep(datcol, length(colexprs))
    ##     exargs = rep(splextra, length(colexprs))
    ## } else { ## datcol is NULL, I think this means we're in
    ##     datcol = list(NULL)
    ##     exargs = rep(splextra, length(colexprs))

    ## length(exargs) <- length(func)
    ## exargs <- rep(exargs, length.out = length(colexprs))
    allfuncs <- rep(func, length.out = length(colexprs))



    if(is.null(takesdf))
        takesdf = .takes_df(allfuncs)
    rawvals = mapply(gen_onerv,
                     csub = colexprs,
                     col = datcol,
                     count = colcounts,
                     cextr = colextras,
                     baselinedf = baselines,
                     func = allfuncs,
                     takesdf = takesdf,
                     splextra = exargs,
                     MoreArgs = list(dfpart = dfpart,
                                     totcount = totcount,
#                                     splextra= splextra,
                                     inclNAs = inclNAs,
                                     last_splval = last_splval),
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
                           splextra = list(),
                           takesdf = NULL,
                           baselines = replicate( length(col_exprs(cinfo)),
                                                 list(dfpart[0,])),
                           inclNAs,
                           last_splval) {
    if(is.null(datcol) && !is.na(rvlab))
        stop("NULL datcol but non-na rowvar label")
    if(!is.null(datcol) && !is.na(datcol)) {
        if(! all(datcol %in% names(dfpart)))
            stop("specified analysis variable (", datcol, ") not present in data")

        rowvar = datcol
    } else {
        rowvar  = NA_character_
    }

    rawvals = gen_rowvalues(dfpart,
                            datcol = datcol,
                            cinfo = cinfo,
                            func = func,
                            splextra =  splextra,
                            takesdf = takesdf,
                            baselines = baselines,
                            inclNAs = inclNAs,
                            last_splval = last_splval)

    ## if(is.null(rvtypes))
    ##     rvtypes = rep(NA_character_, length(rawvals))
    lens = vapply(rawvals, length, NA_integer_)
    unqlens = unique(lens)
    stopifnot(length(unqlens) == 1 ||
              (0 %in% unqlens && length(unqlens) == 2))
    maxind = match(max(unqlens), lens)

    ## look if we got labels, if not apply the
    ## default row labels
    rv1col = rawvals[[maxind]]
    if(is(rv1col, "CellValue"))
        labels = obj_label(rv1col)
    else if(are(rv1col, "CellValue"))
        labels = unlist(value_labels(rv1col))
    else if (!is.null(names(rv1col)))
        labels = names(rv1col)
    else
        labels = NULL


    ncrows = max(unqlens)
    stopifnot(ncrows > 0)


    if(is.null(labels)) {
        if(length(rawvals[[maxind]]) == length(defrowlabs))
            labels = defrowlabs
        else
            labels = rep("", ncrows)
    }

    nms = names(rawvals)
    if(is.null(nms)) {
        if(all(nzchar(labels)))
            nms = labels
        else
            nms = paste0("row", seq_len(ncrows))
    }



    ##recycle formats
    if(ncrows ==  1)  {
        return(list(rowconstr(val = rawvals,
                         cinfo = cinfo,
                         lev = lev,
                         label = labels,
                         name = labels,
                         var = rowvar,
                         format = format,
                         indent_mod = indent_mod(rawvals[[1]]))))
    }
    formatvec = NULL
    if(!is.null(format)) {
        if(is.function(format) )
            format = list(format)
        formatvec = rep(format, length.out = ncrows)
    }

    trows = lapply(1:ncrows, function(i) {
        rowvals = lapply(rawvals, function(colvals)
            {
                colvals[[i]]
            })
        imod = unique(vapply(rowvals, indent_mod, 0L))
        if(length(imod) != 1)
            stop("Different cells in the same row appear to have been given different indent_mod values")
        rowconstr(val = rowvals,
                  cinfo = cinfo,
                  lev = lev,
                  label = labels[i],
                  name = labels[i], ## XXX this is probably the wrong thing!
                  var = rowvar,
                  format = formatvec[[i]],
                  indent_mod = imod
                  )

    })
    trows
}

## ## THIS IS HORRIBLE!!!!!!!!!!!!
## ## I don't think I've ever written hackier code in my entire life
## rename_caller_arg <- function(fun, oldname, newname) {
##     forms = formals(fun)
##     formpos = match(names(forms), oldname)
##     if(is.na(formpos))
##         stop("the hacky argument renamer for ")
##     names(forms)[formpos] = newname
## }

.both_caller = function(pcfun, labelstr) {
    function(df2, .N_col, .N_total, ...) {
        pcfun(df2, labelstr, .N_col = .N_col, .N_total = .N_total, ...)
    }
}

.ncol_caller = function(pcfun, labelstr) {
    function(df2, .N_col, ...) {
        pcfun(df2, labelstr, .N_col = .N_col, ...)
    }
}

.ntot_caller  = function(pcfun, labelstr) {
    function(df2, .N_total, ...) {
        pcfun(df2, labelstr, .N_total = .N_total, ...)
    }
}

.neither_caller = .ntot_caller  = function(pcfun, labelstr) {
    function(df2,  ...) {
        pcfun(df2, labelstr, ...)
    }
}


## .make_caller = function(parent_cfun, clabelstr) {
##     ## XXX Ugh. Combinatorial explosion X.X
##     ## This is what I get for abusing scope to do
##     ## the content label thing. Bad design.

##     ## Ugh number 2. If we assign each of htese to the same name
##     ## R CMD check complains so we return them as anon funcs to sneak
##     ## by. Yet mr
##     if(takes_coln(parent_cfun)) {
##         if(takes_totn(parent_cfun)) {
##             .both_caller(parent_cfun, clabelstr)
##         } else {
##             .ncol_caller(parent_cfun, clabelstr)
##         }
##     } else {
##         if(takes_totn(parent_cfun)) {
##             .ntot_caller(parent_cfun, clabelstr)
##         } else {
##             .neither_caller(parent_cfun, clabelstr)
##         }
##     }
## }

.make_caller <- function(parent_cfun, clabelstr="") {

    formalnms <- names(formals(parent_cfun))
    ## note the <- here
    if(!is.na(dotspos <- match("...", formalnms))) {
        hasdots <- TRUE
        toremove <- dotspos
    } else {
        hasdots <- FALSE
        toremove <- NULL
    }

    labelstrpos <- match("labelstr", names(formals(parent_cfun)))
    if(is.na(labelstrpos)) {
             stop("content function does not appear to accept the labelstr arguent")
    }
    toremove <- c(toremove, labelstrpos)
    formalnms <- formalnms[-1*toremove]

    caller <- eval(parse(text = paste("function() { parent_cfun(",
                                      paste(formalnms, "=", formalnms, collapse = ", "),
                                      ", labelstr = clabelstr, ...)}")))
    formals(caller) <- c(formals(parent_cfun)[-labelstrpos], alist("..."=))
    caller

}

.make_ctab = function(df, lvl, ##treepos,
                      name,
                      label,
                      cinfo,
                      parent_cfun = NULL,
                      format = NULL,
                      indent_mod = 0L,
                      cvar = NULL,
                      inclNAs,
                      extra_args) {

    if(length(cvar) == 0 || is.na(cvar) || identical(nchar(cvar), 0L))
        cvar = NULL
    if(!is.null(parent_cfun)) {
        cfunc <- .make_caller(parent_cfun, label)
        contkids = .make_tablerows(df,
                                   lev = lvl,
                                   func = cfunc,
                                   cinfo = cinfo,
                                   rowconstr = ContentRow,
                                   datcol = cvar,
                                   takesdf = rep(.takes_df(cfunc),
                                                 ncol(cinfo)),
                                   inclNAs = FALSE,
                                   splextra = extra_args,
                                   last_splval = "")
    } else {
        contkids = list()
    }
    ctab = ElementaryTable(kids = contkids,
                           name = paste0(name, "@content"),
                           lev = lvl,
                           labelrow = LabelRow(),
                           cinfo = cinfo,
                           iscontent = TRUE,
                           format = format,
                           indent_mod = indent_mod)
    ctab
}


.make_analyzed_tab = function(df,
                              spl,
                              cinfo,
                              partlabel = "",
                              dolab = TRUE,
                              lvl,
                              baselines,
                              last_splval) {
    stopifnot(is(spl, "VAnalyzeSplit"))
    check_validsplit(spl, df)
    defrlabel = spl@default_rowlabel
    didlab  = FALSE
    if(nchar(defrlabel) == 0 && !missing(partlabel) && nchar(partlabel) > 0) {
        defrlabel = partlabel
        didlab = TRUE
    }
    kids = .make_tablerows(df,
                           func = analysis_fun(spl),
                           defrowlabs = defrlabel, # XXX
                           cinfo = cinfo,
                           datcol = spl_payload(spl),
                           lev = lvl + 1L,
                           format = obj_format(spl),
                           splextra = split_exargs(spl),
                           baselines = baselines,
                           inclNAs = avar_inclNAs(spl),
                           last_splval = last_splval)
    lab = obj_label(spl)
    ret = TableTree(kids = kids,
              name = obj_name(spl),
              label = lab,
              lev = lvl,
              cinfo = cinfo,
              format = obj_format(spl),
              indent_mod = indent_mod(spl))
    labelrow_visible(ret) = dolab
    ret
}

#' @param \dots ALL arguments to recurse_applysplit, methods may only use some of them.
#' @return list of children to place at this level
#'
setGeneric(".make_split_kids", function(spl, have_controws, make_lrow,  ...) { ## df, lvl, splvec, name, make_lrow, partlabel,
                                  ##          cinfo, parent_cfun, cformat, cindent_mod,
                                  ## cextra_args, cvar, baselines, last_splval)
    standardGeneric(".make_split_kids")})

## single AnalyzeSplit
setMethod(".make_split_kids", "VAnalyzeSplit",
          function(spl,
                   have_controws, ## unused here
                   make_lrow, ## unused here
                   ...,
                   df,
                   lvl,
                   name,
                   cinfo,
                   baselines,
                   last_splval,
                   nsibs = 0
                   ) {
    spvis = labelrow_visible(spl)
    if(is.na(spvis))
        spvis = nsibs > 0

    ret = .make_analyzed_tab(df = df,
                             spl = spl,
                             cinfo = cinfo,
                             lvl = lvl + 1L,
                             dolab = spvis,
                             partlabel = obj_label(spl),
                             baselines = baselines,
                             last_splval = last_splval)##partlabel)

    kids = list(ret)
    names(kids) = obj_name(ret)
    kids
})
## 1 or more AnalyzeSplits
setMethod(".make_split_kids", "AnalyzeMultiVars",
          function(spl,
                   have_controws,
                   make_lrow, ## used here
                   ... ## all passed directly down to VAnalyzeSplit method
                   ) {

    avspls = spl_payload(spl)

    nspl = length(avspls)

    kids = unlist(lapply(avspls,
                  .make_split_kids,
                  nsibs = nspl - 1,
                  have_controws = have_controws,
                  make_lrow = make_lrow,
                  ...))

    ## XXX this seems like it should be identical not !identical
    ## TODO FIXME
    if(!identical(make_lrow, FALSE) &&
       !have_controws &&
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
    kids
})


setMethod(".make_split_kids", "Split",
          function(spl,
                   have_controws,
                   make_lrow,
                   ...,
                   splvec, ## passed to recursive_applysplit
                   df, ## used to apply split
                   lvl,  ## used to calculate innerlev
                   cinfo, ## used for sanity check
                   baselines, ## used to calc new baselines
                   last_splval) {

    ## do the core splitting of data into children for this split
    rawpart = do_split(spl, df)
    dataspl = rawpart[["datasplit"]]
    ## these are SplitValue objects
    splvals = rawpart[["values"]]
    partlabels = rawpart[["labels"]]
    if(is.factor(partlabels))
        partlabels = as.character(partlabels)
    nms = unlist(value_names(splvals))
    if(is.factor(nms))
        nms = as.character(nms)

    ##
    ## Get new baseline values
    ##
    ## XXX this is a lot of data churn, if it proves too slow
    ## we can
    ## a) check if any of the analyses (ie the afuns) need the baseline in this splitvec
    ##    and not do any of this if not, or
    ## b) refactor row splitting to behave like column splitting
    ##
    ## (b) seems the better design but is a major reworking of the guts of how rtables tabulation works
    ## (a) will only help if analyses that use baseline info are mixed with those who don't.
    newbl_raw = lapply(baselines,
                       function(dat) {
        if(is.null(dat))
            return(NULL)
        ## apply the same splitting on the
                bldataspl = do_split(spl, dat)[["datasplit"]]
        ## we only keep the ones correspnoding with actual data splits
        res =lapply(names(dataspl),
                    function(nm) {
            if(nm %in% names(bldataspl))
                bldataspl[[nm]]
            else
                dataspl[[1]][0,]
        })

        names(res) = names(dataspl)
        res
    })
    newbaselines = lapply(names(dataspl),
                          function(nm) {
        lapply(newbl_raw,
               function(rawdat) {
            if(nm %in% names(rawdat))
                rawdat[[nm]]
            else
                rawdat[[1]][0,]
        })
    })


    stopifnot(length(newbaselines) == length(dataspl),
              length(newbaselines) == 0 || identical(unique(sapply(newbaselines, length)), length(col_exprs(cinfo))))
    innerlev = lvl + (have_controws || is.na(make_lrow) || make_lrow)

    ## do full recursive_applysplit on each part of the split defined by spl
    inner = unlist(mapply(function(dfpart,  nm, label, baselines, splval) {
        recursive_applysplit(df = dfpart,
                             name = nm,
                             lvl = innerlev,
                             splvec = splvec,
                             cinfo = cinfo,
                             make_lrow = label_kids(spl),
                             parent_cfun = content_fun(spl),
                             cformat = obj_format(spl),
                             partlabel = label,
                             cindent_mod = content_indent_mod(spl),
                             cvar = content_var(spl),
                             baselines = baselines,
                             cextra_args = content_extra_args(spl),
                             last_splval = splval)
    }, dfpart = dataspl,
    label = partlabels,
    nm = nms,
    baselines = newbaselines,
    splval = splvals,
    SIMPLIFY=FALSE))
    ## This is where we need to build the structural tables
    ## even if they are invisible becasue their labels are not
    ## not shown.
    innertab = TableTree(kids = inner,
                         name = obj_name(spl),
                         labelrow = LabelRow(label =obj_label(spl),
                                             vis = isTRUE(vis_label(spl))),
                         cinfo = cinfo,
                         iscontent = FALSE,
                         indent_mod = indent_mod(spl))
    ##kids = inner
    kids = list(innertab)
    kids
})

recursive_applysplit = function( df,
                                lvl = 0L,
                                splvec,
                                name,
                       #         label,
                                make_lrow = NA,
                                partlabel = "",
                                cinfo,
                                parent_cfun = NULL,
                                cformat = NULL,
                                cindent_mod = 0L,
                                cextra_args = list(),
                                cvar = NULL,
                                baselines = lapply(col_extra_args(cinfo),
                                                   function(x) x$.ref_full),
                                last_splval = "") {
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
                      label = partlabel,
                      cinfo = cinfo,
                      parent_cfun = parent_cfun,
                      format = cformat,
                      indent_mod = cindent_mod,
                      cvar = cvar,
                      extra_args = cextra_args)


    nonroot = lvl != 0L
    if(length(splvec) == 0L) {
        kids = list()
    } else {
        spl = splvec[[1]]
        splvec = splvec[-1]

        ## we pass this everything recursive_applysplit received and
        ## it all gets passed around through ... as needed
        ## to the various methods of .make_split_kids
        kids = .make_split_kids(spl = spl, df = df, lvl = lvl, splvec = splvec,
                                name = name, make_lrow = make_lrow, partlabel = partlabel,
                                cinfo = cinfo, parent_cfun = parent_cfun, cformat = cformat,
                                cindent_mod = cindent_mod, cextra_args = cextra_args, cvar =cvar,
                                baselines = baselines, last_splval = last_splval,
                                have_controws = nrow(ctab) > 0)
    } ## end length(splvec)

    if(is.na(make_lrow))
        make_lrow = if(nrow(ctab) > 0 || !nzchar(partlabel)) FALSE else TRUE
    ## never print an empty row label for root.
    if(make_lrow && partlabel == "" && !nonroot)
        make_lrow = FALSE

    if(nrow(ctab) == 0L && length(kids) == 1L && !make_lrow) {
        ret = kids[[1]]
        indent_mod(ret) = indent_mod(spl)
     } else if(nrow(ctab) > 0L || length(kids) > 0L) {
        ## avoid visible label rows when the row.names
        ## directly repeat the info.
         if(length(kids) == 1L &&
            identical(partlabel, row.names(kids[[1]])))
             tlabel = ""
         else
             tlabel = partlabel
         kids = lapply(kids, function(x) {
             indent_mod(x) = indent_mod(spl)
             x
         })
         ret = TableTree(cont = ctab,
                         kids = kids,
                        name = name,
                        label = tlabel, #partlabel,
                        lev = lvl,
                    iscontent = FALSE,
                    labelrow = LabelRow(lev = lvl,
                                      label = tlabel,
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
#'   override those calculated automatically during tabulation. Must specify
#' "counts" for \emph{all} resulting columns if non-NULL. \code{NA} elements
#' will be replaced with the automatically calculated counts.
#' @param col_total integer(1). The total observations across all columns. Defaults to \code{nrow(df)}.
#' @param \dots currently ignored.
#'
#' @note When overriding the column counts or totals care must be taken that, e.g.,
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
                       col_total = nrow(df),
                       ...) {
    ## if no columns are defined (e.g. because lyt is NULL)
    ## add a single overall column as the "most basic"
    ## table column structure that makes sense
    clyt = clayout(lyt)
    if(length(clyt) ==1 && length(clyt[[1]]) == 0) {
        clyt[[1]] = add_overall_col(clyt[[1]], "")
        clayout(lyt) = clyt
    }

    ## do checks and defensive programming now that we have the data
    lyt = fix_dyncuts(lyt, df)
    lyt = set_def_child_ord(lyt, df)
    lyt = fix_analyze_vis(lyt)
    df <- fix_split_vars(lyt, df)


    rtpos = TreePos()
    cinfo = create_colinfo(lyt, df, rtpos,
                           counts = col_counts,
                           total = col_total)
    if(!is.null(col_counts))
        disp_ccounts(cinfo) = TRUE

    rlyt = rlayout(lyt)
    rtspl = root_spl(rlyt)
    ctab = .make_ctab(df, 0L,
                      name = "root",
                      label = "",
                      cinfo = cinfo, ##cexprs, ctree,
                      parent_cfun = content_fun(rtspl),
                      format = content_format(rtspl),
                      indent_mod = 0L,
                      cvar = content_var(rtspl),
                      extra_args = content_extra_args(rtspl))
    kids = lapply(seq_along(rlyt), function(i) {
        splvec = rlyt[[i]]
        if(length(splvec) == 0)
            return(NULL)
        firstspl = splvec[[1]]
        nm = obj_label(firstspl) ## XXX this should be name!
        lab = obj_label(firstspl)
        recursive_applysplit(df = df, lvl = 0L,
                             name = nm,
                              splvec = splvec,
                             cinfo = cinfo,
                             ## XXX are these ALWAYS right?
                             make_lrow = label_kids(firstspl),
                             parent_cfun = NULL,
                             cformat = obj_format(firstspl),
                             cvar = content_var(firstspl),
                             cextra_args = content_extra_args(firstspl),
                             last_splval = NULL)
    })
    kids = kids[!sapply(kids, is.null)]
    if(length(kids) > 0)
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
                        label="",
                        iscontent = FALSE,
                        cinfo = cinfo,
                        format = obj_format(rtspl))
    }
    tab
}

fix_one_split_var <- function(spl, df) {
    var <- spl_payload(spl)
    varvec <- df[[var]]
    if(!is(varvec, "character") && !is.factor(varvec)) {
        message(sprintf("Split var [%s] was not character or factor. Converting to factor",
                        var))
        varvec <- factor(varvec)
        df[[var]] <- varvec
    }

    ## handle label var
    lblvar <- spl_label_var(spl)
    have_lblvar <- !identical(var, lblvar)
    if(have_lblvar) {
        lblvec <- df[[lblvar]]
        tab <- table(varvec, lblvec)

        if(any(rowSums(tab > 0) > 1) ||
           any(colSums(tab > 0) > 1))
            stop(sprintf("There does not appear to be a 1-1 correspondence between values in split var [%s] and label var [%s]",
                         var, lblvar))

        if(!is(varvec, "character") && !is.factor(varvec)) {
            message(sprintf("Split label var [%s] was not character or factor. Converting to factor",
                            var))
            lblvec <- factor(lblvec)
            df[[lblvar]] <- lblvec
        }
    }

    ## maybe. not sure.
    ## if(is.factor(varvec) && is.factor(lblvec) &&
    ##    length(levels(varvec)) != length(levels(lblvec)))
    ##     stop("Split var [%s] and label var [%s] are both factors but have different numbers of levels")


    df
}

fix_split_vars <- function(lyt, df) {
    clyt <- clayout(lyt)
    rlyt <- rlayout(lyt)

    allspls <- unlist(list(clyt, rlyt))
    ## VarLevelSplit includes sublclass VarLevWBaselineSplit
    varspls <- allspls[sapply(allspls, is, "VarLevelSplit")]
    unqvarinds <- !duplicated(sapply(varspls, spl_payload))
    unqvarspls <- varspls[unqvarinds]
    for(spl in unqvarspls)
        df <- fix_one_split_var(spl, df)

    df
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
    lyt@.Data = lapply(lyt, set_def_child_ord, df = df)
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
    bline = spl_ref_group(lyt)
    if(!is.null(spl_child_order(lyt)) &&
       match(bline, spl_child_order(lyt), nomatch = -1) == 1L)
        return(lyt)

    vec = df[[spl_payload(lyt)]]
    vals = unique(vec)
    if(is.factor(vals))
        vals = levels(relevel(droplevels(vals), bline)) # this sorts the levels

    stopifnot(bline %in% vals)
    pos = match(bline, vals)
    ## same order except ref_group always first
    vals = c(bline, vals[-pos])
    spl_child_order(lyt) = vals
    lyt
})



splitvec_to_coltree = function(df, splvec, pos = NULL,
                               lvl = 1L, label = "") {
    stopifnot(lvl <= length(splvec) + 1L,
              is(splvec, "SplitVector"))

    ## nm = unlist(tail(rawvalues(pos), 1)) %||% ""
    nm = unlist(tail(value_names(pos), 1)) %||% ""
    lab = unlist(tail(value_labels(pos), 1)) %||% ""

    if(lvl == length(splvec) + 1L) {
        ## XXX this should be a LayoutColTree I Think.
        LayoutColLeaf(lev = lvl - 1L,
                      label = label,
                      tpos = pos,
                      name = nm
                      )
    } else {
        spl = splvec[[lvl]]
        rawpart = do_split(spl,df, trim =FALSE )
        datparts = rawpart[["datasplit"]]
        vals = rawpart[["values"]]
        kids = mapply(function(dfpart, value) {
            ## XXX TODO label
            partlab = ""
            newpos = make_child_pos(pos, spl, value, partlab)
            splitvec_to_coltree(dfpart, splvec, newpos,
                                lvl + 1L, partlab)
        }, dfpart = datparts, value = vals, SIMPLIFY=FALSE)
        LayoutColTree(lev = lvl, label = label,
                      spl = spl,
                      kids = kids, tpos = pos,
                      name = nm,
                      summary_function = content_fun(spl))
    }
}

## now that we know for sure the number of siblings
## collaplse NAs to TRUE/FALSE for whether
## labelrows should be visible for ElementaryTables
## generatead from analyzing a single variable
setGeneric("fix_analyze_vis", function(lyt) standardGeneric("fix_analyze_vis"))
setMethod("fix_analyze_vis", "PreDataTableLayouts",
          function(lyt) {
    rlayout(lyt) = fix_analyze_vis(rlayout(lyt))
    lyt
})
setMethod("fix_analyze_vis", "PreDataRowLayout",
          function(lyt) {
    splvecs = lapply(lyt, fix_analyze_vis)
    PreDataRowLayout(root = root_spl(lyt),
                     lst = splvecs)
})

setMethod("fix_analyze_vis", "SplitVector",
          function(lyt) {
    len = length(lyt)
    if(len == 0)
        return(lyt)
    lastspl = lyt[[len]]
    if(!(is(lastspl, "VAnalyzeSplit") ||
         is(lastspl, "AnalyzeMultivar")))
        return(lyt)

    if(is(lastspl, "VAnalyzeSplit") && is.na(labelrow_visible(lastspl)))
        labelrow_visible(lastspl) = FALSE
    else if (is(lastspl, "AnalyzeMultiVar")) { ## must be AnalyzeMultiVar by check above
        pld = spl_payload(lastspl)
        newpld = lapply(pld, function(sp, havesibs) {
            if(is.na(labelrow_visible(sp)))
                labelrow_visible(sp) = havesibs
        }, havesibs = len > 1)
        spl_payload(lastspl) = newpld
        ## pretty sure this isn't needed...
        if(is.na(label_kids(lastspl)))
            label_kids(lastspl) = len > 1
    }
    lyt[[len]] = lastspl
    lyt
})


setGeneric("build_splits_expr", function(lyt, df, curexpr) standardGeneric("build_splits_expr"))


setGeneric("expr_stubs", function(spl, df) standardGeneric("expr_stubs"))

setMethod("expr_stubs", "VarLevelSplit",
          function(spl, df) {
    sdat = do_split(spl, df)
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

