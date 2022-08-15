
match_extra_args = function(f, .N_col, .N_total, .var, .ref_group = NULL, .ref_full = NULL, .in_ref_col = NULL, .spl_context = NULL, .N_row, .df_row, extras) {
    possargs = c(list(.N_col = .N_col, .N_total = .N_total, .N_row = .N_row, .df_row = .df_row),
                 extras)
    formargs = formals(f)
    formnms = names(formargs)

    ## specialized arguments that must be named in formals, cannot go anonymously into ...
    if(!is.null(.var) && nzchar(.var))
        possargs = c(possargs, list(.var = .var))
    if(!is.null(.ref_group))
        possargs = c(possargs, list(.ref_group = .ref_group))
    if(!is.null(.ref_full))
        possargs = c(possargs, list(.ref_full = .ref_full))
    if(!is.null(.in_ref_col))
        possargs = c(possargs, list(.in_ref_col = .in_ref_col))
    if(!is.null(.spl_context) && !(".spl_context" %in% names(possargs)))
        possargs = c(possargs, list(.spl_context= .spl_context))
    else
        possargs$.spl_context <- NULL


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

#' @noRd
#' @return a RowsVerticalSection object representing the k x 1 section of the table
#' being generated, with k the number of rows the analysis function generates
gen_onerv = function(csub, col, count, cextr, dfpart, func, totcount, splextra,
                     takesdf = .takes_df(func),
                     baselinedf,
                     inclNAs,
                     col_parent_inds,
                     spl_context) {

    spl_context$cur_col_subset <- col_parent_inds
    spl_context$cur_col_n <- vapply(col_parent_inds, sum, 1L)
    ## workaround for https://github.com/Roche/rtables/issues/159
    if(NROW(dfpart) > 0) {
        inds = eval(csub, envir = dfpart)
        dat = dfpart[inds,,drop = FALSE]
    } else {
        dat <- dfpart
    }
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

    exargs <-    match_extra_args(func,
                              .N_col = count,
                              .N_total = totcount,
                              .var = col,
                              .ref_group = baselinedf,
                              .ref_full = fullrefcoldat,
                                  .in_ref_col = inrefcol,
                              .N_row = NROW(dfpart),
                              .df_row = dfpart,
                              .spl_context = spl_context,
                              extras = c(cextr,
                                         splextra))

    args = c(args, exargs)


    val = do.call(func, args)
    if(!is(val, "RowsVerticalSection")) {
        if(!is(val, "list"))
            val <- list(val)
        ret <- in_rows(.list = val, .labels = unlist(value_labels(val)), .names = names(val))

    } else {
            ret <- val
    }
    ret
}


strip_multivar_suffix <- function(x) {
    gsub( "\\._\\[\\[[0-9]\\]\\]_\\.$", "", x)
}


## Generate all values (one for each column) for one or more rows
## by calling func once per column (as defined by cinfo)
#' @noRd
#' @return A list of m RowsVerticalSection objects, one for each
#' (leaf) column in the table.
gen_rowvalues = function(dfpart,
                         datcol,
                         cinfo,
                         func,
                         splextra,
                         takesdf = NULL,
                         baselines,
                         inclNAs,
                         spl_context = spl_context) {
    colexprs = col_exprs(cinfo)
    colcounts = col_counts(cinfo)
    colextras = col_extra_args(cinfo, NULL)
    ## XXX I don't think this is used anywhere???
    ##splextra = c(splextra, list(.spl_context = spl_context))
    totcount = col_total(cinfo)

    colleaves =  collect_leaves(cinfo@tree_layout)


    gotflist <- is.list(func)

    ## one set of named args to be applied to all columns
    if(!is.null(names(splextra)))
        splextra = list(splextra)
    else
        length(splextra) = ncol(cinfo)


    if(!gotflist) {
        func <- list(func)
    } else  if(length(splextra) == 1) {
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
            ## values have the suffix but we are populating datacol
            ## so it has to match var numbers so strip the suffixes back off
            splvals = strip_multivar_suffix(rawvalues(pos))
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
        ## if(gotflist)
        ##     length(exargs) <- length(func) ## func is a list
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
                     col_parent_inds = spl_context[,names(colexprs), drop = FALSE],
                     splextra = exargs,
                     MoreArgs = list(dfpart = dfpart,
                                     totcount = totcount,
#                                     splextra= splextra,
                                     inclNAs = inclNAs,
                                     spl_context = spl_context),
                                   ##  spl_context = spl_context[,1:3]), ## parent value full_parent_df
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
                           spl_context = context_df_row(cinfo = cinfo)) {
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
                            spl_context = spl_context)

    ## if(is.null(rvtypes))
    ##     rvtypes = rep(NA_character_, length(rawvals))
    lens = vapply(rawvals, length, NA_integer_)
    unqlens = unique(lens)
    ## length 0 returns are ok to not match cause they are
    ## just empty space we can fill in as needed.
    if(length(unqlens[unqlens >0]) != 1L) {## length(unqlens) != 1 &&
       ## (0 %in% unqlens && length(unqlens) != 2)) {
        stop("Number of rows generated by analysis function do not match across all columns. ",
             if(!is.na(datcol) && is.character(dfpart[[datcol]]))
                 paste("\nPerhaps convert analysis variable", datcol,
                       "to a factor?"))
    }
    maxind = match(max(unqlens), lens)

    ## look if we got labels, if not apply the
    ## default row labels
    ## this is guaranteed to be a RowsVerticalSection object.
    rv1col = rawvals[[maxind]]
    if(!is(rv1col, "RowsVerticalSection"))
        stop("gen_rowvalues appears to have generated something that was not a RowsVerticalSection object. Please contact the maintainer.") # nocov
    labels <- value_labels(rv1col)

    ncrows = max(unqlens)
    if(ncrows == 0)
        return(list())
    stopifnot(ncrows > 0)

    if(is.null(labels)) {
        if(length(rawvals[[maxind]]) == length(defrowlabs))
            labels = defrowlabs
        else
            labels = rep("", ncrows)
    }

    rfootnotes <- rep(list(list(), length(rv1col)))
    nms <- value_names(rv1col)
    rfootnotes <- row_footnotes(rv1col)

    imods <- indent_mod(rv1col) ##rv1col@indent_mods
    unwrapped_vals <- lapply(rawvals, as, Class = "list", strict = TRUE)

    formatvec = NULL
    if(!is.null(format)) {
        if(is.function(format) )
            format = list(format)
        formatvec = rep(format, length.out = ncrows)
    }



    trows = lapply(1:ncrows, function(i) {
        rowvals = lapply(unwrapped_vals, function(colvals)
            {
                colvals[[i]]
            })
        imod = unique(vapply(rowvals, indent_mod, 0L))
        if(length(imod) != 1)
            stop("Different cells in the same row appear to have been given different indent_mod values")
        rowconstr(vals = rowvals,
                  cinfo = cinfo,
                  lev = lev,
                  label = labels[i],
                  name = nms[i], ##labels[i], ## XXX this is probably the wrong thing!
                  var = rowvar,
                  format = formatvec[[i]],
                  indent_mod = imods[[i]] %||% 0L,
                  footnotes = rfootnotes[[i]] ## one bracket so list
                  )

    })
    trows
}

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

    caller <- eval(parser_helper(text = paste("function() { parent_cfun(",
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
                      extra_args,
                      spl_context = context_df_row(cinfo = cinfo)) {

    if(length(cvar) == 0 || is.na(cvar) || identical(nchar(cvar), 0L))
        cvar = NULL
    if(!is.null(parent_cfun)) {
        ##cfunc <- .make_caller(parent_cfun, label)
        cfunc <- lapply(parent_cfun, .make_caller, clabelstr = label)
        contkids <- tryCatch(.make_tablerows(df,
                                   lev = lvl,
                                   func = cfunc,
                                   cinfo = cinfo,
                                   rowconstr = ContentRow,
                                   datcol = cvar,
                                   takesdf = rep(.takes_df(cfunc),
                                                 length.out = ncol(cinfo)),
                                   inclNAs = FALSE,
                                   splextra = extra_args,
                                   spl_context = spl_context),
                             error = function(e) e)
        if(is(contkids, "error")) {
            stop("Error in content (summary) function: ", contkids$message,
                 "\n\toccured at path: ",
                 spl_context_to_disp_path(spl_context),
                 call. = FALSE)
        }
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


.make_analyzed_tab <- function(df,
                              spl,
                              cinfo,
                              partlabel = "",
                              dolab = TRUE,
                              lvl,
                              baselines,
                              spl_context,
                              section_sep = NA_character_) {
    stopifnot(is(spl, "VAnalyzeSplit"))
    check_validsplit(spl, df)
    defrlabel <- spl@default_rowlabel
    didlab <- FALSE
    if(nchar(defrlabel) == 0 && !missing(partlabel) && nchar(partlabel) > 0) {
        defrlabel <- partlabel
        didlab <- TRUE
    }
    fixup_warn <- function(w) {
        warning(w,
                "\n\toccured at (row) path: ",
                spl_context_to_disp_path(spl_context),
                .call = FALSE)
        invokeRestart("muffleWarning")
    }
    kids <- tryCatch(.make_tablerows(df,
                                     func = analysis_fun(spl),
                                     defrowlabs = defrlabel, # XXX
                                     cinfo = cinfo,
                                     datcol = spl_payload(spl),
                                     lev = lvl + 1L,
                                     format = obj_format(spl),
                                     splextra = split_exargs(spl),
                                            baselines = baselines,
                                     inclNAs = avar_inclNAs(spl),
                                     spl_context = spl_context),
                     error = function(e) e)
    if(is(kids, "error")) {
        stop("Error applying analysis function (var - ",
             spl_payload(spl) %||% "colvars", "): ", kids$message,
             "\n\toccured at (row) path: ",
             spl_context_to_disp_path(spl_context),
             call. = FALSE)
    }
    lab <- obj_label(spl)
    ret <- TableTree(kids = kids,
              name = obj_name(spl),
              label = lab,
              lev = lvl,
              cinfo = cinfo,
              format = obj_format(spl),
              indent_mod = indent_mod(spl),
              trailing_sep = section_sep)
    labelrow_visible(ret) <- dolab
    ret
}

#' @noRd
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
                   spl_context,
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
                             spl_context = spl_context)
    indent_mod(ret) <- indent_mod(spl)

    kids = list(ret)
    names(kids) = obj_name(ret)
    kids
})

.set_kids_sect_sep <- function(lst, spl) {
    sect_sep <- spl_section_div(spl)
    if(!is.na(sect_sep)) {
        lst <- lapply(lst,
                      function(k) {
            if(is(k, "VTableTree"))
                trailing_sep(k) <- sect_sep
            k
        })
    }
    lst
}

## 1 or more AnalyzeSplits
setMethod(".make_split_kids", "AnalyzeMultiVars",
          function(spl,
                   have_controws,
                   make_lrow, ## used here
                   spl_context,
                   ... ## all passed directly down to VAnalyzeSplit method
                   ) {
    avspls = spl_payload(spl)

    nspl = length(avspls)

    kids = unlist(lapply(avspls,
                  .make_split_kids,
                  nsibs = nspl - 1,
                  have_controws = have_controws,
                  make_lrow = make_lrow,
                  spl_context = spl_context,
                  ...,
                  section_sep = spl_section_div(spl)))




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
    ##nms = sapply(spl_payload(spl), spl_payload)

    nms <- vapply(kids, obj_name, "")
    labs <- vapply(kids, obj_label, "")
    if(length(unique(nms))  != length(nms) &&
       length(unique(nms))  != length(nms)) {
        warning("Non-unique sibling analysis table names. Using Labels instead. ",
                "Use the table_names argument to analyze to avoid this when analyzing ",
                "the same variable multiple times.",
                "\n\toccured at (row) path: ",
                spl_context_to_disp_path(spl_context),
                call. = FALSE)
        kids <- mapply(function(k, nm) {
            obj_name(k) <- nm
            k
        }, k = kids, nm = labs, SIMPLIFY = FALSE)
        nms <- labs
    }
    kids <- .set_kids_sect_sep(kids, spl)

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
                   spl_context) {

    ## do the core splitting of data into children for this split
    rawpart = do_split(spl, df, spl_context = spl_context)
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
                bldataspl = do_split(spl, dat, spl_context = spl_context)[["datasplit"]]
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

        rsplval <- context_df_row(split = obj_name(spl), value = value_names(splval),
                                  full_parent_df = list(dfpart),
                                  cinfo = cinfo)

        ## if(length(rsplval) > 0)
        ##     rsplval <- setNames(rsplval, obj_name(spl))
        recursive_applysplit(df = dfpart,
                             name = nm,
                             lvl = innerlev,
                             splvec = splvec,
                             cinfo = cinfo,
                             make_lrow = label_kids(spl),
                             parent_cfun = content_fun(spl),
                             cformat = content_format(spl),
                             partlabel = label,
                             cindent_mod = content_indent_mod(spl),
                             cvar = content_var(spl),
                             baselines = baselines,
                             cextra_args = content_extra_args(spl),
                             ##splval should still be retaining its name
                             spl_context = rbind(spl_context, rsplval))
    }, dfpart = dataspl,
    label = partlabels,
    nm = nms,
    baselines = newbaselines,
    splval = splvals,
    SIMPLIFY=FALSE))

    inner <- .set_kids_sect_sep(inner, spl)
    ## This is where we need to build the structural tables
    ## even if they are invisible becasue their labels are not
    ## not shown.
    innertab = TableTree(kids = inner,
                         name = obj_name(spl),
                         labelrow = LabelRow(label =obj_label(spl),
                                             vis = isTRUE(vis_label(spl))),
                         cinfo = cinfo,
                         iscontent = FALSE,
                         indent_mod = indent_mod(spl),
                         page_title = ptitle_prefix(spl)
                         )
    ##kids = inner
    kids = list(innertab)
    kids
})

context_df_row <- function(split = character(), value = character(), full_parent_df = list(),
                           cinfo = NULL) {
    ret <- data.frame(split = split, value = value, full_parent_df = I(full_parent_df),
                                        #     parent_cold_inds = I(parent_col_inds),
                      stringsAsFactors = FALSE)
    if(nrow(ret) > 0)
        ret$all_cols_n <- nrow(full_parent_df[[1]])
    else
        ret$all_cols_n <- integer() ## should this be numeric???

    if(!is.null(cinfo)) {
        if(nrow(ret) > 0)
            colcols <- as.data.frame(lapply(col_exprs(cinfo), function(e) {
                vals <- eval(e, envir = full_parent_df[[1]])
                if(identical(vals, TRUE))
                    vals <- rep(vals, length.out = nrow(full_parent_df[[1]]))
                I(list(vals))
            }))
        else
            colcols <- as.data.frame(rep(list(logical()), ncol(cinfo)))
        names(colcols) <- names(col_exprs(cinfo))
        ret <- cbind(ret, colcols)
    }
    ret
}


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
                                spl_context = context_df_row(cinfo = cinfo),
                                no_outer_tbl = FALSE,
                                parent_sect_split = NA_character_
                                ) {
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
                      extra_args = cextra_args,
                      spl_context = spl_context)


    nonroot = lvl != 0L

    if(is.na(make_lrow))
        make_lrow = if(nrow(ctab) > 0 || !nzchar(partlabel)) FALSE else TRUE
    ## never print an empty row label for root.
    if(make_lrow && partlabel == "" && !nonroot)
        make_lrow = FALSE

    if(length(splvec) == 0L) {
        kids = list()
        imod = 0L
        spl = NULL
    } else {
        spl = splvec[[1]]
        splvec = splvec[-1]

        ## we pass this everything recursive_applysplit received and
        ## it all gets passed around through ... as needed
        ## to the various methods of .make_split_kids
        kids = .make_split_kids(spl = spl,
                                df = df,
                                lvl = lvl,
                                splvec = splvec,
                                name = name,
                                make_lrow = make_lrow,
                                partlabel = partlabel,
                                cinfo = cinfo,
                                parent_cfun = parent_cfun,
                                cformat = cformat,
                                cindent_mod = cindent_mod,
                                cextra_args = cextra_args, cvar =cvar,
                                baselines = baselines,
                                spl_context = spl_context,
                                have_controws = nrow(ctab) > 0)
        ## if(make_lrow && indent_mod(spl) != 0 ) {
        ##     kids = lapply(kids, `indent_mod<-`, value = 0L)
        ##     imod = indent_mod(spl)
        ## } else {
            imod = 0L
        ## }
    } ## end length(splvec)

    if(is.na(make_lrow))
        make_lrow = if(nrow(ctab) > 0 || !nzchar(partlabel)) FALSE else TRUE
    ## never print an empty row label for root.
    if(make_lrow && partlabel == "" && !nonroot)
        make_lrow = FALSE

    ## this is only true when called from build_table and the first split
    ## in (one of the) SplitVector is NOT an AnalyzeMultiVars split.
    ## in that case we would be "double creating" the structural
    ## subtable
    if(no_outer_tbl) {
        ret = kids[[1]]
        indent_mod(ret) = indent_mod(spl)
    } else if(nrow(ctab) > 0L || length(kids) > 0L) {
         ## previously we checked if the child had an identical label
         ## but I don't think tahts needed anymore.
         tlabel = partlabel

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
                         cinfo = cinfo,
                         indent_mod = imod)
     } else {
         ret = NULL
     }

    ## if(!is.null(spl) && !is.na(spl_section_sep(spl)))
    ##     ret <- apply_kids_section_sep(ret, spl_section_sep(spl))
    ## ## message(sprintf("indent modifier: %d", indentmod))
    ## if(!is.null(ret))
    ##     indent_mod(ret) = indentmod
    ret
}



#' Create a table from a layout and data
#'
#' Layouts are used to describe a table pre-data. `build_table` is used to
#' create a table using a layout and a dataset.
#'
#'
#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @param col_counts numeric (or `NULL`). Deprecated. If non-null, column counts which
#'   override those calculated automatically during tabulation. Must specify
#' "counts" for \emph{all} resulting columns if non-NULL. \code{NA} elements
#' will be replaced with the automatically calculated counts.
#' @param col_total integer(1). The total observations across all columns. Defaults to \code{nrow(df)}.
#' @param \dots currently ignored.
#'
#' @details
#'
#' When \code{alt_counts_df} is specified, column counts are calculated by applying the exact
#' column subsetting expressions determined when applying column splitting to the main data
#' (\code{df}) to \code{alt_counts_df} and counting the observations in each resulting subset.
#'
#' In particular, this means that in the case of splitting based on cuts of the data, any
#' dynamic cuts will have been calculated based on \code{df} and simply re-used for
#' the count calculation.
#'
#' @note When overriding the column counts or totals care must be taken that, e.g.,
#'   `length()` or `nrow()` are not called within tabulation functions, because
#'   those will NOT give the overridden counts. Writing/using tabulation
#'   functions which accept \code{.N_col} and \code{.N_total} or do not rely on
#'   column counts at all (even implicitly) is the only way to ensure overriden
#'   counts are fully respected.
#'
#' @export
#' @return A \code{TableTree} or \code{ElementaryTable} object representing the table created by performing
#' the tabulations declared in \code{lyt} to the data \code{df}.
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
#'
#' # with column counts calculated based on different data
#' miniDM <- DM[sample(1:NROW(DM), 100),]
#' build_table(l2, DM, alt_counts_df = miniDM)
#'
#' build_table(l, DM, col_counts = 1:3)
build_table = function(lyt, df,
                       alt_counts_df = NULL,
                       col_counts = NULL,
                       col_total = if(is.null(alt_counts_df)) nrow(df) else nrow(alt_counts_df),
                       topleft = NULL,
                       hsep = default_hsep(),
                       ...) {
    if(!is(lyt, "PreDataTableLayouts")) {
        stop("lyt must be a PreDataTableLayouts object. Got object of class ", class(lyt))
    }

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
    df <- fix_split_vars(lyt, df, char_ok = is.null(col_counts))


    rtpos = TreePos()
    cinfo = create_colinfo(lyt, df, rtpos,
                           counts = col_counts,
                           alt_counts_df = alt_counts_df,
                           total = col_total,
                           topleft)
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
        nm = obj_name(firstspl) ##obj_label(firstspl) ## XXX this should be name!
        lab = obj_label(firstspl)
        recursive_applysplit(df = df, lvl = 0L,
                             name = nm,
                              splvec = splvec,
                             cinfo = cinfo,
                             ## XXX are these ALWAYS right?
                             make_lrow = label_kids(firstspl),
                             parent_cfun = NULL,
                             cformat = content_format(firstspl),
                             cvar = content_var(firstspl),
                             cextra_args = content_extra_args(firstspl),
                             spl_context = context_df_row(cinfo = cinfo),
                             ## we DO want the 'outer table' if the first one is a multi-analyze
                             no_outer_tbl = !is(firstspl, "AnalyzeMultiVars"))
    })
    kids = kids[!sapply(kids, is.null)]
    if(length(kids) > 0)
        names(kids) = sapply(kids, obj_name)

    if(nrow(ctab) == 0L &&
       length(kids) == 1L &&
       is(kids[[1]], "VTableTree")) {
        tab = kids[[1]]
        main_title(tab) <- main_title(lyt)
        subtitles(tab) <- subtitles(lyt)
        main_footer(tab) <- main_footer(lyt)
        prov_footer(tab) <- prov_footer(lyt)
    } else {
        tab = TableTree(cont = ctab,
                        kids = kids,
                        lev = 0L,
                        name = "root",
                        label="",
                        iscontent = FALSE,
                        cinfo = cinfo,
                        format = obj_format(rtspl),
                        title = main_title(lyt),
                        subtitles = subtitles(lyt),
                        main_footer = main_footer(lyt),
                        prov_footer = prov_footer(lyt))
    }


    ## This seems to be unneeded, not clear what 'top_left' check it refers to
    ## but both top_left taller than column headers and very long topleft are now
    ## allowed, so this is just wasted computation.


    ## ## this is where the top_left check lives right now. refactor later maybe
    ## ## but now just call it so the error gets thrown when I want it to
    ## unused <- matrix_form(tab)
    tab <- update_ref_indexing(tab)
    horizontal_sep(tab) <- hsep
    tab
}

fix_one_split_var <- function(spl, df, char_ok = TRUE) {
    var <- spl_payload(spl)
    if(!(var %in% names(df)))
        stop("Split variable [", var, "] not found in data being tabulated.")
    varvec <- df[[var]]
    if(!is(varvec, "character") && !is.factor(varvec)) {
        message(sprintf("Split var [%s] was not character or factor. Converting to factor",
                        var))
        varvec <- factor(varvec)
        df[[var]] <- varvec
    } else if (is(varvec, "character") && !char_ok) {
        stop("Overriding column counts is not supported when splitting on character variables.\n",
             "  Please convert all column split variables to factors.")
    }

    if(is.factor(varvec))
        levs <- levels(varvec)
    else
        levs <- unique(varvec)
    if(!all(nzchar(levs))) {
        stop("Got empty string level in splitting variable ", var, "This is not supported.\n",
             "  If display as an empty level is desired use a value-labeling variable.")
    }

    ## handle label var
    lblvar <- spl_label_var(spl)
    have_lblvar <- !identical(var, lblvar)
    if(have_lblvar) {
        if(!(lblvar %in% names(df)))
            stop("Value label variable [", lblvar, "] not found in data being tabulated.")
        lblvec <- df[[lblvar]]
        tab <- table(varvec, lblvec)

        if(any(rowSums(tab > 0) > 1) ||
           any(colSums(tab > 0) > 1))
            stop(sprintf("There does not appear to be a 1-1 correspondence between values in split var [%s] and label var [%s]",
                         var, lblvar))

        if(!is(lblvec, "character") && !is.factor(lblvec)) {
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


fix_split_vars <- function(lyt, df, char_ok) {
    df <- fix_split_vars_inner(clayout(lyt), df, char_ok = char_ok)
    df <- fix_split_vars_inner(rlayout(lyt), df, char_ok = TRUE)
    df

    ## clyt <- clayout(lyt)
    ## rlyt <- rlayout(lyt)

    ## allspls <- unlist(list(clyt, rlyt))
    ## VarLevelSplit includes sublclass VarLevWBaselineSplit

}

fix_split_vars_inner <- function(lyt, df, char_ok) {
    stopifnot(is(lyt, "PreDataAxisLayout"))
    allspls <- unlist(lyt)
    varspls <- allspls[sapply(allspls, is, "VarLevelSplit")]
    unqvarinds <- !duplicated(sapply(varspls, spl_payload))
    unqvarspls <- varspls[unqvarinds]
    for(spl in unqvarspls)
        df <- fix_one_split_var(spl, df, char_ok = char_ok)

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

    if(!is.null(split_fun(lyt))) {
        ## expensive but sadly necessary, I think
        pinfo = do_split(lyt, df, spl_context = context_df_row())
        vals = sort(unlist(value_names(pinfo$values)))
    } else {
        vec = df[[spl_payload(lyt)]]
        vals <- if(is.factor(vec))
                levels(vec)
            else
                unique(vec)

        if(is.factor(vals))
            vals = levels(relevel(droplevels(vals), bline)) # this sorts the levels
    }

    stopifnot(bline %in% vals)
    pos = match(bline, vals)
    ## same order except ref_group always first
    vals = c(bline, vals[-pos])
    spl_child_order(lyt) = vals
    lyt
})


splitvec_to_coltree = function(df, splvec, pos = NULL,
                               lvl = 1L, label = "",
                               spl_context = context_df_row(cinfo = NULL)) {
    stopifnot(lvl <= length(splvec) + 1L,
              is(splvec, "SplitVector"))


    if(lvl == length(splvec) + 1L) {
        ## XXX this should be a LayoutColTree I Think.
        nm = unlist(tail(value_names(pos), 1)) %||% ""
        LayoutColLeaf(lev = lvl - 1L,
                      label = label,
                      tpos = pos,
                      name = nm
                      )
    } else {
        spl = splvec[[lvl]]
        nm = if(is.null(pos)) obj_name(spl) else unlist(tail(value_names(pos), 1))
        rawpart = do_split(spl,df, trim =FALSE ,
                           spl_context = spl_context)
        datparts = rawpart[["datasplit"]]
        vals = rawpart[["values"]]
        labs = rawpart[["labels"]]


        kids = mapply(function(dfpart, value, partlab) {
            newprev <- context_df_row(split = obj_name(spl), value = value_names(value), full_parent_df = list(dfpart), cinfo = NULL)
            newpos = make_child_pos(pos, spl, value, partlab)
            splitvec_to_coltree(dfpart, splvec, newpos,
                                lvl + 1L, partlab,
                                spl_context = rbind(spl_context, newprev))
        }, dfpart = datparts, value = vals,
        partlab = labs, SIMPLIFY=FALSE)
        names(kids) = value_names(vals)
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

    if(is(lastspl, "VAnalyzeSplit") && is.na(labelrow_visible(lastspl))) {
        ##  labelrow_visible(lastspl) = FALSE
        labelrow_visible(lastspl) = "hidden"
    } else if (is(lastspl, "AnalyzeMultiVar")) { ## must be AnalyzeMultiVar by check above
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


