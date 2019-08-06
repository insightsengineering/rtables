##
## Patterns and constants for for the column naming
##
##

rsvar_pat = "^rsp_[0-9]+$"
rsvalue_pat = "^r[0-9]+value$"
rsvalue_lbl_pat = "^r[0-9]+vlbl$"
rsvar_lbl_pat = "^rsplbl_[0-9]+$"
rstype_pat = "^rsptype_[0-9]+$"

## if theres only one colsplit the col is still <splitval>___
## for detection purposes e.g.,
## single and nested VarLevel splits
## ARM1___, ARM2___
## ARM1___M, ARM1___F, ARM2___M, ARM2___F
## Single MultiVar split
## Value___, PCTDiff___ (this is a multivariable split)
## Multivar split nested inside VarLevel split
## ARM1___Value, ARM1___PCTDiff, ARM2___Value, ARM2___PCTDiff
datacol_pat = "^([[:alnum:]_]+___)+[[:alnum:]_]*$" 


csvar_pat = "^csp_[0-9]+$"
cstype_pat = "^csptype_[0-9]+$"
cs_lbl_pat = "^csplbl_[0-9]+$"


cspan_col = "c__colspans__c"
rspan_col_pat = "^rspan_col_[0-9]+$"

avar_col = "rowvar"
avarlbl_col = "rowvarlbl"
rowlbl_col = "rowlbl"


.rsvar_colnames = function(df) 
    grep(rsvar_pat, names(df), value = TRUE)

.rsvarlbl_colnames = function(df)
    grep(rsvar_lbl_pat, names(df), value = TRUE)

.rsvalue_colnames = function(df)
    grep(rsvalue_pat, names(df), value = TRUE)

.rsvaluelbl_colnames = function(df)
    grep(rsvalue_lbl_pat, names(df), value = TRUE)

.rsvartype_colnames = function(df)
    grep(rstype_pat, names(df), value = TRUE)

.data_colnames = function(df)
    grep(datacol_pat, names(df), value = TRUE)

.csvar_colnames = function(df)
    grep(csvar_pat, names(df), value = TRUE)

.csvartype_colnames = function(df)
    grep(cstype_pat, names(df), value = TRUE)

.csvarlbl_colnames = function(df)
    grep(cs_lbl_pat, names(df), value = TRUE)

.rspan_colnames = function(df)
    grep(rspan_col_pat, names(df), value = TRUE)



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



.get_rscols_helper = function(df, i, colpat, naret) {
    if(i < 1)
        return(naret)
    if(nrow(df) == 0)
        return(character())
    allcols = grep(colpat, names(df), value = TRUE)
    if(i > length(allcols)) { ## this happens in varsplit
        i = length(allcols)
    }
    mycols = allcols[1:i]
    mycols
}
    



    
.get_rsvar_vec = function(df, i) {
    mycols = .get_rscols_helper(df, i, rsvar_pat, NA_character_)
    if(is.na(mycols) || length(mycols) == 0)
        return(mycols)
    vapply(mycols,
           function(x) {
        ret = unique(df[[x]])
        stopifnot(length(ret) == 1)
        ret
        }, character(1))
}


.get_rsvalue_vec = function(df, i) {
    mycols = .get_rscols_helper(df, i, rsvalue_pat, NA)
    if(is.na(mycols) || length(mycols) == 0)
        return(as.list(mycols))
    
    lapply(mycols,
           function(x) {
        ret = unique(df[[x]])
        stopifnot(length(ret) == 1)
        ret
        })
}

.get_rstype_vec = function(df, i) {
    
    mycols = .get_rscols_helper(df, i, rstype_pat, NA_character_)
    if(is.na(mycols) || length(mycols) == 0)
        return(mycols)
    
    vapply(mycols,
           function(x) {
        ret = unique(df[[x]])
        stopifnot(length(ret) == 1)
        ret
    },
    character(1))
}

.get_rs_lbls = function(df, i) {
    mycols = .get_rscols_helper(df, i, rsvar_lbl_pat, NA_character_)
    if(is.na(mycols) || length(mycols) == 0)
        return(mycols)

    vapply(mycols,
           function(x) {
        ret = unique(df[[x]])
        stopifnot(length(ret) == 1)
        ret
    }, character(1))
    
}

.get_rsvalue_lbls = function(df, i) {
    mycols = .get_rscols_helper(df, i, rsvalue_lbl_pat, NA_character_)
    if(is.na(mycols) || length(mycols) == 0)
        return(mycols)
     
    vapply(mycols,
           function(x) {
        ret = unique(df[[x]])
        stopifnot(length(ret) == 1)
        ret
    }, character(1))
}

.get_ravar = function(df) { unique(df[[avar_col]])}

.get_ravar_lbl = function(df) { unique(df[[avarlbl_col]])}

## XXX this was rowvaltype, but now its rowvartype are these different?
## if so which is correct?
.get_val_type = function(df) { unique(df$rowvartype)}

recursive_split = function(df, i = 1L) {
 
    
    rsplval = paste0("r", i, "value")
    rspl = paste0("rsp_", i)
    rvlbl = paste0("r", i, "vlbl")
    levl = as.integer(i-1)
    rsplblcol = paste0("rsplbl_",  i)
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
            tpos = TreePos(spls = tspls, svals = tspvals, NULL)
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
        colnm = names(clodf)[firstvary]
        colval = unique(thisdf[[colnm]])
        thisexpr = make_subset_expr(colnm, colval)
        recursive_build_clayout(thisdf,
                                parpos = tpos,
                                curexpr = .combine_subset_exprs(curexpr, thisexpr),
                                label = as.character(colval))
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
    csplvals = cbind.data.frame(csplvals, cstypedf)
    recursive_build_clayout(csplvals)
}


.stripNA = function(v) {
    v[!is.na(v)]
}

.tpos_from_dfrows = function(rows, lvl, iscontent = NULL) {
    rsvars = .stripNA(.get_rsvar_vec(rows, lvl))
    rstypes = .stripNA(.get_rstype_vec(rows, lvl))
    rsvalues = .stripNA(.get_rsvalue_vec(rows, lvl))
    rsvallbls = .stripNA(.get_rsvalue_lbls(rows, lvl))
    if(length(rsvallbls) == 0)
        rsvallbls = as.character(rsvalues)
    
    rslbls = .stripNA(.get_rs_lbls(rows, lvl))
    if(length(rslbls) == 0)
        rslbls = vapply(rsvars, function(x) paste(x, collapse = ":"),
                        character(1))
    spls = mapply(Split, var = rsvars,
                  type = rstypes,
                  lbl = rslbls)
    sub = make_pos_subset(spls = spls,
                          svals = rsvalues)
    if(is.null(iscontent))
        TreePos(spls = spls, svals = rsvalues, svlbls = rsvallbls, sub =  sub)
    else
        TableTreePos(spls = spls, svals  = rsvalues, svlbls = rsvallbls,
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




## ### XXXX I think the level stuff in here is all wrong, needs careful checking ones
## ## this bit is plugged in
## recursive_row_layout = function(df, i) {
    
##     rsplval = paste0("r", i, "value")
##     if(! (rsplval %in% names(df))) { # no more levels to split on
##         cinds = which(is.na(df$var))
##         if(length(cinds)) {
##             ctab = dfrows_to_table(df[cinds,], lvl = as.integer(i))
##             df = df[-cinds,]
##             ret = TableTree(cont = ctab, kids = tree_children(dfrows_to_table(df, lvl = as.integer(i+1))), lev = as.integer(i))
##         } else {
##             ret = dfrows_to_table(df, lvl = as.integer(i+1))
##         }
##     } else  {
##         cinds = which(is.na(df[[rsplval]]) & is.na(df$var))
##         if(length(cinds)) {
##             ctab = dfrows_to_table(df[cinds,], lvl = as.integer(i+1))
##             df = df[-cinds,]
##         } else {
##             ctab  = ElementaryTable()
##         }
##         key = na_to_sentinel(df[[rsplval]])
##         ## declaring levels this way ensures they aren't sorted in a way we don't want.
##         key = factor(key, levels = unique(key))
##         ## XXX can't use split here because it sorts the levels of f
##         spl = split(df, key)
##         ret = TableTree(cont = ctab,
##                         kids = lapply(spl, recursive_split, i = i+1), lev = as.integer(i))
##     }

##     ret
## }

    

