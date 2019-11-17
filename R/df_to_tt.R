


nasentinel = "_x___x_NA_x___x_"
na_to_sentinel = function(v) {
    v[is.na(v)] = nasentinel
    v
}


df_to_tt = function(df) {
    stopifnot(nrow(df) > 0)
    df = fixup_rtable_df(df)
    tab = recursive_split(df, 1)
    #layout = recursive_layout(df, 1)
    tab
}


varsplit = function(df, level = 0L) {
    if(anyNA(df[[avar_col]])) stop("got NA var where I shouldn't have")
    grp = cumsum(c(FALSE, head(df[[avar_col]], -1) != tail(df[[avar_col]], - 1)))
    dfspl = split(df, grp)
    lapply(dfspl, function(dat) {
        lbl = unique(dat$rowvarlbl)
        stopifnot(length(lbl) == 1)
        dfrows_to_table(dat, lvl = level, label = lbl, cont = NULL)
    })
}


recursive_split = function(df, i = 1L) {
 
    
    rsplval = sprintf(rsvalue_templ, i)
    rspl = sprintf(rsvar_templ, i)
    rvlbl = sprintf(rsvalue_lbl_templ, i)
    rsplblcol = sprintf(rsvar_lbl_templ, i)
    levl = as.integer(i-1)
    
    ## remember, that e.g., r2value CANNOT be non-NA if r1value is NA
    if(! (rsplval %in% names(df)) || all(is.na(df[[rsplval]]))) { # no more levels to split on
        cinds = which(is.na(df[[avar_col]]))
        stopifnot(length(cinds) == 0 || length(cinds) == 1)


        if(rsplblcol %in% names(df))
            tlab = unique(df[[rsplblcol]])
        else
            tlab = unique(df[[avarlbl_col]])
    
        if(length(cinds) > 0) {
            cdf = df[cinds,]
            ctab = dfrows_to_table(cdf, levl, label = cdf$rowlbl,
                                   iscontent = TRUE)
            df = df[-cinds,]
            ## XXX I think this is wrong
            ##tlab  =  cdf$rowlbl
        } else {
            ctab = ElementaryTable(iscontent = TRUE)
            ## XXX I think this is wrong
          
            
        }

        ##  gross special casing :(
        if(rsplval %in% names(df)) { #all(is.na(df(rsplval)))
            l = levl
        } else  {
            l = levl + 1L
        }
        kids = varsplit(df, level = l) ##tree_children(rows_to_el_table(df, levl+1L))
    } else  {
        tlab = unique(df[[rsplblcol]])
        cinds = which(is.na(df[[rsplval]]) & is.na(df[[avar_col]]))
        if(length(cinds) > 1) stop()
        if(length(cinds)) {
            cdf = df[cinds,]
            ctab = dfrows_to_table(df[cinds,], lvl = levl,
                                   label = cdf$rowlbl,
                                   iscontent = TRUE)
            df = df[-cinds,]
        } else {
            ctab  = ElementaryTable(iscontent = TRUE)
        }
        vkey = na_to_sentinel(df[[rsplval]])
        ## declaring levels this way ensures they aren't sorted in a way we don't want.
        vkey = factor(vkey, levels = unique(vkey))
        spkey = na_to_sentinel(df[[rspl]])
        ## split is ok here because we already forced the sort-order we want
        ## for the splitting factor
        spl = split(df, vkey)
        kids = lapply(spl, recursive_split, i = i+1)
    }
    tpos = .tpos_from_dfrows(df, levl, iscontent = FALSE)
    thisspl = .nextsplit_from_drows(df, levl)
    vuniq = .get_ravar(df)
    if(all(is.na(vuniq)) || length(vuniq) > 1) {
        var = NA_character_
        varlbl = NA_character_
    } else {
        var = vuniq
        varlbl = .get_ravar_lbl(df)
    }
    ret = TableTree(cont = ctab, kids = kids,
                    lev = levl, lab  = tlab,
                    tpos = tpos,
                    spl = thisspl,
                    iscontent = FALSE,
                    var = var,
                    var_lbl = varlbl,
                    ## FIXME this is gonna be needlessly called
                    ## a bunch of times so if its slow, fix this
                    clayout = dfrow_to_clayout(df[1,])
                    )
    
    ret
}




## XXX realtalk time, is it bettr to just have
## column layout in the data.frame version of
## a tree structure rather than the actual tree structure?
## We don't need the hierarchy for pagination...
##
## i.e.
## > dfrow_to_clayout(df2[1,])
##    ARM SEX
## 1 ARM1   M
## 2 ARM1   F
## 3 ARM2   M
## 4 ARM2   F
##
## Only problem I can think of is if we want to be able
## to transpose tables, but we can make th tree structuree
## out of the above data.frame "easily"

.get_ctyp_clodf = function(clodf, i) {
    ret = unique(clodf[[paste0("csptype_", i)]])
    stopifnot(length(ret) == 1 && !is.na(ret))
    ret
}


.get_clbl_clodf = function(clodf, i) {
    ret = unique(clodf[[paste0("csplbl_", i)]])
    stopifnot(length(ret) == 1 && !is.na(ret))
    ret
}


recursive_build_clayout = function(clodf, parpos= NULL, curexpr = expression(TRUE), label = "") {

    ## data columns are guaranteed to be first here, followed
    ## directly by colsplit label columns, so we find the first
    ## colsplit label column and go back one.
    ##
    ## XXX TODO: write a better, proper accessor for this
    ## instead of this ugly hack
    
    ndcols = min(grep("csplbl_", names(clodf))) -1L
    clodatdf = clodf[,1:ndcols]
    nuniqs  = sapply(clodatdf, function(x) length(unique(x)))
    if(all(nuniqs == 1)) 
        firstvary = ndcols + 1L
    else {
        firstvary = min(which(nuniqs  >  1))
        thistyp = .get_ctyp_clodf(clodf, firstvary)
        thislbl = .get_clbl_clodf(clodf, firstvary)
        thiscolnm = names(clodf)[firstvary]
        ## TODO lbl
        thisspl = Split(thiscolnm, thistyp, lbl = thislbl)
        
    }
    lastinvar = firstvary - 1L


    ## if we're not all the way descended we ignore extras
    ## XXX This may be wrong in some corner cases
    ## FIXME
    if(nrow(clodf) == 1)
        extr = clodf[["__colextra__"]]
    else
        extr = list()
        
    
    if(firstvary > 1) {
        if(!is.null(parpos)) {
            colnm = names(clodf)[lastinvar]
            ## unique() calls below are len 1 guaranteed
            payload = unique(clodf[[lastinvar]])
            typ = .get_ctyp_clodf(clodf,  lastinvar)
            lbl = .get_clbl_clodf(clodf, lastinvar)
            ## TODO lbl value
            newspl = Split(colnm, typ, lbl = lbl)
            tpos = make_child_pos(parpos, newspl, payload)
        } else {
            tspls = lapply(1:lastinvar, function(i) {
                colnm = names(clodf)[i]
                payload = unique(clodf[[i]]) # this is len 1
                typ = .get_ctype_clodf(clodf, i)
                lbl = .get_clbl_clodf(clodf, i)
                Split(colnm, typ, lbl)
            })
            tspvals = lapply(1:lastinvar, function(i) {
                unique(clodf[[i]]) # this is len 1
            })
            svals = make_splvalue_vec(tspvals, c(rep(list(list()), length(tspvals) - 1L), extr))
            tpos = TreePos(spls = tspls, svals = svals, NULL)
        }
    } else {
        tpos = TreePos()
    }

    if(firstvary > ndcols)
        spldf = list()
    else {
        fct = factor(clodf[[firstvary]], levels = unique(clodf[[firstvary]]))
        spldf = split(clodf, fct)
    }
    kids = lapply(seq(along = spldf), function(i) {
        thisdf = spldf[[i]]
        if(nrow(thisdf) == 1L) {
            thisextra = thisdf[["__colextra__"]]
        } else {
            thisextra = list()
        }
        colnm = names(clodf)[firstvary]
        colval = SplitValue(unique(thisdf[[colnm]]), extr = thisextra)
        thisexpr = make_subset_expr(colnm, colval)
        recursive_build_clayout(thisdf,
                                parpos = tpos,
                                curexpr = .combine_subset_exprs(curexpr, thisexpr),
                                label = as.character(splv_rawvalues(colval)))
    })
    names(kids) = names(spldf)
    if(length(kids)) {
        LayoutColTree(#sub = curexpr,
            spl = thisspl,
            tpos = tpos,
            kids = kids,
            lab = label,
            lev = lastinvar)#,
        ## svar = names(clodf)[firstvary])
    } else {
        LayoutColLeaf(tpos = tpos, lab = label, lev = lastinvar)
    }
}


dfrow_to_clayout = function(dfrow) {
    stopifnot(nrow(dfrow) == 1L)
    ncspl = length(grep("csp_", names(dfrow)))
    datcols = grep("___", names(dfrow), value = TRUE)
    csplvals = do.call(rbind.data.frame,
                       c(strsplit(datcols, "___"),
                         stringsAsFactors = FALSE))
    names(csplvals) = unlist(dfrow[, paste0("csp_", 1:ncspl)])
    cstypedf = dfrow[,c(.csvarlbl_colnames(dfrow),
                        .csvartype_colnames(dfrow))]
    row.names(cstypedf) = NULL
    csplvals = cbind.data.frame(csplvals, cstypedf,
                                "__colextra__" = I(as.list(dfrow[,.csvalextra_colnames(dfrow)])))
    recursive_build_clayout(csplvals)
}


.stripNA = function(v) {
    v[!is.na(v)]
}

.stripNULL = function(l) {
    l[!sapply(l, is.null)]

}

.tpos_from_dfrows = function(rows, lvl, iscontent = NULL) {
    rsvars = .stripNA(.get_rsvar_vec(rows, lvl))
    rstypes = .stripNA(.get_rstype_vec(rows, lvl))
    rsvalues = .stripNA(.get_rsvalue_vec(rows, lvl))
    rsvallbls = .stripNA(.get_rsvalue_lbls(rows, lvl))
    rsextras = rows[[rowextra_col]]
    if(length(rsvallbls) == 0)
        rsvallbls = as.character(rsvalues)
    
    rslbls = .stripNA(.get_rs_lbls(rows, lvl))
    if(length(rslbls) == 0)
        rslbls = vapply(rsvars, function(x) paste(x, collapse = ":"),
                        character(1))
    spls = mapply(Split, var = rsvars,
                  type = rstypes,
                  lbl = rslbls)
    if(length(rsvalues) > 0) {
        splvals = make_splvalue_vec(rsvalues, rsextras)
    } else {
        splvals = list()
    }
    
  
    sub = make_pos_subset(spls = spls,
                          svals = splvals)
    if(is.null(iscontent))
        TreePos(spls = spls, svals = splvals, svlbls = rsvallbls, sub =  sub)
    else
        TableTreePos(spls = spls, svals  = splvals, svlbls = rsvallbls,
                     sub = sub, iscontent = iscontent)
}

## lvl is the level we're currently at, we're  effectively looking
## for what the split will be at curlvl +1
.nextsplit_from_drows = function(rows, curlvl) {
    targlvl = curlvl + 1L
    tp = .tpos_from_dfrows(rows[1,], targlvl)
    allspls = pos_splits(tp)
    if(length(allspls) >= targlvl)
        allspls[[targlvl]]
    else
        NULLSplit()
}

dfrows_to_table = function(df, lvl, label = "", cont = NULL, iscontent = FALSE) {
    if(nrow(df) == 0)
        return(ElementaryTable())
    rowinds = seq(along = df[[1]])
    tpos = .tpos_from_dfrows(df, lvl, iscontent)
    spl = .nextsplit_from_drows(df, lvl)
    
    datacols = .data_colnames(df)
    kids = lapply(rowinds, function(i) {
        rowpos = make_rowpos(tpos, i)
        TableRow(lev = lvl,
                 lab = as.character(df$rowlbl[i]),
                 val = as.list(df[i, datacols]),
                 cspan = df[[cspan_col]][[i]],
                 ## is the clayout always constant in df? if so could move this out
                 ## I think it is, but why prematurely optimize?
                 clayout = dfrow_to_clayout(df[i,]),
                 tpos = rowpos,
                 var = .get_ravar(df[i,]),
                 var_lbl = .get_ravar_lbl(df[i,]),
                 v_type = .get_val_type(df[i,])
                 )
    })
    rspan = df[,grep("rspan_col_", names(df))]
    TableTree(cont = cont, kids = kids, lev = lvl,
              lab = label, rspans = rspan,
              tpos = tpos, spl = spl,
              var = .get_ravar(df),
              var_lbl = .get_ravar_lbl(df))
}







layout_from_df = function(df) {
    rlyt = recursive_row_layout(df, 1)
    clyt = recursive_col_layout(df, 1)


}
    

