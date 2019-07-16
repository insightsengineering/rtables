
##
## TableTree <-> df Reference mapping
##
##
## Mapping is as follows:
##
##                           ARM1        ARM2
##                         M     F     M     F
##
## Overall (N)             10     12   14    16
##
## Race
##   White (n)             7       8    9     10
##     Factor2
##
##       A (n)             4       6    4     9
##         Age
##           mean          6.8     7.8  8.8   9.8    
##           median        6       7    8     9
##
##       B (n)             3       2    5     1
##         Age
##           mean          6.5     7.5  8.5   9.5    
##           median        5       6    7     8
##
##   Black (n)             3       4    5     6
##       Factor2
##         A (n)           0       1    5     0
##           mean          NA      4.5  5.2   NA
##           median        NA      5    3     NA
##
##         B (n)           3       3    0     6
##           mean          NA      3.5  2.2   NA
##           median        NA      4    2     NA
##
## Var3 (factor)
##   level1            4      3     2     1
##   level2            6      9     12    15


## Goes to (fully) long form, s3classed as c("rtable_df", <"data.frame"|"tbl_df">)
##
## The first row at each level of nesting is fully "repped" out, while
## subsequent some subsequent rows are ommitted for brevity.
##
## When multiple rows are ommitted:
## df columns which are constant across all ommitted rows contain their value
## <...> indicates the column is varying across the ommited rows

## Note c*split columns are completely constant across the whole table, but
## its not safe to just put them in an attribute instead of repeating them
## 
## var    valtype  value   rvalue1   rvalue2  c1value   c2value  fun        rsplit1  rsplit2  c1split c2split      <overall row>
## NA     N        10      NA        NA         ARM1     M       <NULL>     NA       NA       ARM      SEX         1
## NA     N        12      NA        NA         ARM1     F       <NULL>     NA       NA       ARM      SEX         1
## NA     N        14      NA        NA         ARM2     M       <NULL>     NA       NA       ARM      SEX         1
## NA     N        16      NA        NA         ARM2     F       <NULL>     NA       NA       ARM      SEX         1
## NA     n        7       White     NA         ARM1     M       <NULL>     Race     NA       ARM      SEX         2
## NA     n        <...>   White     NA         <...>    <...>   <NULL>     Race     NA       ARM      SEX         2
## NA     n        4       White     A          ARM1     M       <NULL>     Race     Factor2  ARM      SEX         3
## NA     n        <...>   White     A          <...>    <...>   <NULL>     Race     Factor2  ARM      SEX         3
## AGE    mean     6.8     White     A          ARM1     M       <closure>  Race     Factor2  ARM      SEX         4
## AGE    mean     7.8     White     A          ARM1     F       <closure>  Race     Factor2  ARM      SEX         4
## AGE    mean     8.8     White     A          ARM2     M       <closure>  Race     Factor2  ARM      SEX         4
## AGE    mean     9.8     White     A          ARM2     F       <closure>  Race     Factor2  ARM      SEX         4
## AGE    median   6       White     A          ARM1     M       <closure>  Race     Factor2  ARM      SEX         5
## AGE    median   <...>   White     A          <...>    <...>   <closure>  Race     Factor2  ARM      SEX         5
## NA     n        4       White     B          ARM1     M       <NULL>     Race     Factor2  ARM      SEX         6
## NA     n        <...>   White     B          <...>    <...>   <NULL>     Race     Factor2  ARM      SEX         6
## AGE    mean     6.8     White     B          ARM1     M       <closure>  Race     Factor2  ARM      SEX    
## AGE    <...>    <...>   White     B          <...>    <...>   <closure>  Race     Factor2  ARM      SEX         6-7
## AGE    median   6       White     B          ARM1     M       <closure>  Race     Factor2  ARM      SEX    
## NA     n        3       Black     NA         ARM1     M       <NULL>     Race     NA       ARM      SEX         8
## <...>  <...>    <...>   Black     <...>      <...>    <...>   <...>      Race     <...>    ARM      SEX         8-14
## Var3   level1   3       NA        NA         ARM1     M       <closure>  NA       NA       ARM      SEX         15
## Var3   <...>    <...>   NA        NA         <...>    <...>   <closure>  NA       NA       ARM      SEX         15 -16          
##
##
## Wide-ish/intermediate form
##
## (note this is probably the one people will want to analyze but not
## immediately clear to me how to make it roundtrippable back to a
## table tree without making everyone unhappy
##
## Here we will have to parse the data column names and split them on some fixed
## indicator (here its '___' that CAN NEVER APPEAR IN COLUMN SPLIT NAMES EVER EVER EVER)
## but that makes the column names ugly. Originally I put '_' there but I don't trust that
## to never appear in factor level names
##
## as above the cspl* columns are completely constant and the rspl* columns are mostly constant
## these are required to have the info we need to get back to a tree, but contain info largely
## useless to the end user.

##
## var   valtype varlbl      r1value  r2value  ARM1___M  ARM1___F  ARM2___M  ARM2___F rspl_1  rspl_2    cspl_1  cspl_2      
## NA    N       N           NA       NA       10        12        14       16        NA      NA        ARM     SEX      
## NA    n       n           White    NA       7         8         9        10        Race    NA        ARM     SEX      
## NA    n       n           White    A        4         6         4        9         Race    Factor2   ARM     SEX      
## Age   mean    Mean        White    A        6.8       7.8       8.8      9.8       Race    Factor2   ARM     SEX      
## Age   median  Median      White    A        6         7         8        9         Race    Factor2   ARM     SEX      
## NA    n       n           White    B        3         2         5        1         Race    Factor2   ARM     SEX      
## Age   mean    Mean        White    B        6.5       7.5       8.5      9.5       Race    Factor2   ARM     SEX      
## Age   median  Median      White    B        5         6         7        8         Race    Factor2   ARM     SEX      
## NA    n       n           Black    NA       7         8         9        10        Race    NA        ARM     SEX      
## NA    n       n           Black    A        4         6         4        9         Race    Factor2   ARM     SEX      
## Age   mean    Mean        Black    A        6.8       7.8       8.8      9.8       Race    Factor2   ARM     SEX      
## Age   median  Median      Black    A        6         7         8        9         Race    Factor2   ARM     SEX      
## NA    n       n           Black    B        3         2         5        1         Race    Factor2   ARM     SEX      
## Age   mean    Mean        Black    B        6.5       7.5       8.5      9.5       Race    Factor2   ARM     SEX      
## Age   median  Median      Black    B        5         6         7        8         Race    Factor2   ARM     SEX
## Var3  level1  level1 (n)  NA       NA       4         3         2        1         NA      NA        ARM     SEX      
## Var3  level2  level2 (n)  NA       NA       6         9         12       15        NA      NA        ARM     SEX      


## patterns for the column naming
rsvar_pat = "^rsp_[0-9]+$"
rsvalue_pat = "^r[0-9]+value$"
rsvalue_lbl_pat = "^r[0-9]+lbl$"
rsvar_lbl_pat = "^rsplbl_[0-9]+$"
datacol_pat = "___"

.rsvar_colnames = function(df) 
    grep(rsvar_pat, names(df), value = TRUE)

.rsvalue_colnames = function(df)
    grep(rsvalue_pat, names(df), value = TRUE)

.data_colnames = function(df)
    grep(datacol_pat, names(df), value = TRUE)






nasentinel = "_x___x_NA_x___x_"
na_to_sentinel = function(v) {
    v[is.na(v)] = nasentinel
    v
}


df_to_tt = function(df) {
    stopifnot(nrow(df) > 0)
    tab = recursive_split(df, 1)
    #layout = recursive_layout(df, 1)
    tab
}


varsplit = function(df, level = 0L) {
    if(anyNA(df$var)) stop("got NA var where I shouldn't have")
    grp = cumsum(c(FALSE, head(df$var, -1) != tail(df$var, - 1)))
    dfspl = split(df, grp)
    lapply(dfspl, function(dat) {
        lbl = unique(dat$varlbl)
        stopifnot(length(lbl) == 1)
        dfrows_to_table(dat, lvl = level, label = lbl, cont = NULL)
    })
}

### colspan information: list column called c__colspans__c
### rowspan information, k columns named rspan_col_1 to rspan_r_k



.fixup_df_crspans = function(df) {
    ndatcols = length(grep("[[:alnum:]]___[[:alnum:]]", names(df)))
    if(is.null(df$c__colspans__c)) {
    
        df$c__colspans__c = I(replicate(nrow(df), seq(1, ndatcols), simplify= FALSE))
    }
    rspancols = paste0("rspan_col_", seq(1, ndatcols))
    for(curcol in rspancols) {
        if(is.null(df[[curcol]]))
            df[[curcol]] = seq(1:nrow(df))
    }
    df
}

.get_rsvar_vec = function(df, i) {
    if(i < 1)
        return(NA_character_)
    if(nrow(df) == 0)
        return(character())
    allcols = .rsvar_colnames(df)
    if(i > length(allcols)) {
        warning("i is too big. Off-by-one?")
        print(df)
        print(i)
        i = length(allcols)
    }
    mycols = allcols[1:i]
    vapply(mycols,
           function(x) {
        ret = unique(df[[x]])
        stopifnot(length(ret) == 1)
        ret
        }, character(1))
}


.get_rsvalue_vec = function(df, i) {
    if(i < 1)
        return(NA)
    if(nrow(df) == 0)
        return(character())
    allcols = .rsvalue_colnames(df)
    if(i > length(allcols)) {
        warning("i is too big. Off-by-one?")
        print(df)
        print(i)
        i = length(allcols) 
    }
    mycols = allcols[1:i]
    lapply(mycols,
           function(x) {
        ret = unique(df[[x]])
        stopifnot(length(ret) == 1)
        ret
        })
}



recursive_split = function(df, i = 1L) {
    df = .fixup_df_crspans(df)
    
    rsplval = paste0("r", i, "value")
    rspl = paste0("rsp_", i)
    rvlbl = paste0("r", i, "vlbl")
    levl = as.integer(i-1)
    ## remember, that e.g., r2value CANNOT be non-NA if r1value is NA
    if(! (rsplval %in% names(df)) || all(is.na(df[[rsplval]]))) { # no more levels to split on
        cinds = which(is.na(df$var))
        stopifnot(length(cinds) == 0 || length(cinds) == 1)

        rsplblcol = paste0("rsplbl_",  i)
        if(rsplblcol %in% names(df))
            tlab = unique(df[[rsplblcol]])
        else
            tlab = unique(df[["varlbl"]])
    
        if(length(cinds) > 0) {
            cdf = df[cinds,]
            ctab = dfrows_to_table(cdf, levl, label = cdf$rowlbl)
            df = df[-cinds,]
            ## XXX I think this is wrong
            ##tlab  =  cdf$rowlbl
        } else {
            ctab = ElementaryTable()
            ## XXX I think this is wrong
          
            
        }

        ##  gross special casing :(
        if(rsplval %in% names(df)) { #all(is.na(df(rsplval)))
            l = levl
        } else  {
            l = levl + 1L
        }
        kids = varsplit(df, level = l) ##tree_children(rows_to_el_table(df, levl+1L))

        ret = TableTree(cont = ctab, kids = kids, lev = levl, lab  = tlab,
                        rs_vars = .get_rsvar_vec(df, levl),
                        rs_values = .get_rsvalue_vec(df, levl))
        
    } else  {
        cinds = which(is.na(df[[rsplval]]) & is.na(df$var))
        if(length(cinds) > 1) stop()
        if(length(cinds)) {
            cdf = df[cinds,]
            ctab = dfrows_to_table(df[cinds,], lvl = levl,
                                    label = cdf$rowlbl)
            df = df[-cinds,]
        } else {
            ctab  = ElementaryTable()
        }
        vkey = na_to_sentinel(df[[rsplval]])
        ## declaring levels this way ensures they aren't sorted in a way we don't want.
        vkey = factor(vkey, levels = unique(vkey))
        spkey = na_to_sentinel(df[[rspl]])
        ## split is ok here because we already forced the sort-order we want
        ## for the splitting factor
        spl = split(df, vkey)
        ret = TableTree(cont = ctab,
                        kids = lapply(spl, recursive_split, i = i+1),
                        lev = levl,
                        rs_vars = .get_rsvar_vec(df, levl),
                        rs_values = .get_rsvalue_vec(df, levl))
    }

    ret
}

.makesubsetexpr = function(colname, colval) {
    ## not really sure if this is needed, honestly
    if(is(colval, "factor"))
        colval = as.character(colval)
    as.expression(bquote(.(a) == .(b), list(a = as.name(colname),
                              b = colval)))
}


.combine_subset_exprs = function(ex1, ex2) {
    stopifnot(is.expression(ex1), is.expression(ex2))
    extrue = expression(TRUE)
    ## ex1b = ex1
    ## ex2b = ex2
    ## if(!identical(ex1, extrue))
    ##     ex1b = ex1[[1]]
    ## if(!identical(ex2, extrue))
    ##     ex2b = ex2[[1]]
    as.expression(bquote((.(a)) & .(b), list(a = ex1[[1]], b = ex2[[1]])))
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

recursive_build_clayout = function(clodf, curexpr = expression(TRUE), label = "") {
    ## XXX all of this assumes no labeling columns are here!!!
    ## Will need to be fixed if (when) we slap labeling
    ## info in clodf, cause then all the ncol stuff will be
    ## wrong
    nuniqs  = sapply(clodf, function(x) length(unique(x)))
    if(max(nuniqs) <= 1)
        return(LayoutColLeaf(lev = ncol(clodf),
                             lab = label,
                             sub = curexpr))
    firstvary = min(which(nuniqs  >  1))
    ## XXX split is sorting the factors!!!!!!
    fct = factor(clodf[[firstvary]], levels = unique(clodf[[firstvary]]))
    spldf = split(clodf, fct)
    kids = lapply(seq(along = spldf), function(i) {
        thisdf = spldf[[i]]
        colnm = names(clodf)[firstvary]
        colval = unique(thisdf[[colnm]])
        thisexpr = .makesubsetexpr(colnm, colval)
        recursive_build_clayout(thisdf,
                                curexpr = .combine_subset_exprs(curexpr, thisexpr),
                                label = as.character(colval))
    })
    names(kids) = names(spldf)
    LayoutColTree(sub = curexpr, kids = kids,
                  lab = label,
                  lev = firstvary,
                  svar = names(clodf)[firstvary])
}


dfrow_to_clayout = function(dfrow) {
    
    ncspl = length(grep("csp_", names(dfrow)))
    datcols = grep("___", names(dfrow), value = TRUE)
    csplvals = do.call(rbind.data.frame, c(strsplit(datcols, "___"),
                                stringsAsFactors = FALSE)
                       )
    names(csplvals) = unlist(dfrow[, paste0("csp_", 1:ncspl)])
    recursive_build_clayout(csplvals)
       


}





dfrows_to_table = function(df, lvl, label = "", cont = NULL) {
    if(nrow(df) == 0)
        return(ElementaryTable())
    rowinds = seq(along = df[[1]])
    
    datacols = .data_colnames(df)
    kids = lapply(rowinds, function(i) {
        TableRow(lev = lvl,
                 lab = as.character(df$rowlbl[i]),
                 val = as.list(df[i, datacols]),
                 cspan = df$c__colspans__c[[i]],
                 ## is the clayout always constant in df? if so could move this out
                 ## I think it is, but why prematurely optimize?
                 clayout = dfrow_to_clayout(df[i,]),
                 rs_vars = .get_rsvar_vec(df[i,], lvl - 1L),
                 rs_values = .get_rsvalue_vec(df[i,], lvl - 1L)
                 )
    })
    rspan = df[,grep("rspan_col_", names(df))]
    if(is.null(cont))
        ElementaryTable(kids = kids, lev = lvl, lab = label, rspan = rspan,
                        rs_vars = .get_rsvar_vec(df, lvl),
                        rs_values = .get_rsvalue_vec(df, lvl))
    else
        TableTree(cont = cont, lev = lvl, lab = label, kids = kids, rspan = rspan,
                  rs_vars = .get_rsvar_vec(df, lvl),
                  rs_values = .get_rsvalue_vec(df, lvl))
}




## returns a list of list(row, splitinfo) for reach row it finds 
## We can't go straight to df because we don't know how many split columns there will be
## until we fully recurse the tree, so that is a later step that will process these.


setGeneric("recursive_row_collect",
           function(ttree,
                    rsplits = character(),
                    rsvals = character())
    standardGeneric("recursive_row_collect"))

setMethod("recursive_row_collect", "TableTree",
          function(ttree, rsplits = character(), rsvals = character()) {
    newrsplits = c(rsplits, ttree@rowsplit_var)
    newrsvals  = c(rsvals, ttree@rowsplit_value)
    c(list(recursive_row_collect(ttree@content)),
      lapply(tree_children(ttree), recursive_row_collect))
})

setMethod("recursive_row_collect", "ElementaryTable",
          function(ttree, rsplits = NULL, rsvals = NULL) {
    splits  = ttree@rowsplit_vars
    vals = ttree@rowsplit_values

    lapply(tree_children(ttree),
           recursive_row_collect,
           rsplits = splits,
           rsvals = vals)
})

setMethod("recursive_row_collect", "TableRow",
          function(ttree, rsplits, rsvals) {
    list(ttree, rowsplits = rsplits, rsvalues = rsvals)
})





ttrows_to_df = function(rows, prevsplitss = character(), prevsplvals = list()) {
  




}

## recursive_row_collect = function(ttree, prevsplits = character(), prevsplvals = list()) {
##     newspl = iiii
##     if(is(ttree, "TableTree") && nrow(ttree@content) > 0) {
##         crows = ttrows_to_df(ttree@dcontent, is(ttree, "ElementaryTable") || is(ttree, "TableRow"))
##         return(ttree)
##     return(c(list(ttree@content), lapply(tree_children(ttree), recursive_row_collect)))
## }




tt_to_df = function(ttree) {
    






}

layout_from_df = function(df) {
    rlyt = recursive_row_layout(df, 1)
    clyt = recursive_col_layout(df, 1)


}




### XXXX I think the level stuff in here is all wrong, needs careful checking ones
## this bit is plugged in
recursive_row_layout = function(df, i) {
    
    rsplval = paste0("r", i, "value")
    if(! (rsplval %in% names(df))) { # no more levels to split on
        cinds = which(is.na(df$var))
        if(length(cinds)) {
            ctab = dfrows_to_table(df[cinds,], lvl = as.integer(i))
            df = df[-cinds,]
            ret = TableTree(cont = ctab, kids = tree_children(dfrows_to_table(df, lvl = as.integer(i+1))), lev = as.integer(i))
        } else {
            ret = dfrows_to_table(df, lvl = as.integer(i+1))
        }
    } else  {
        cinds = which(is.na(df[[rsplval]]) & is.na(df$var))
        if(length(cinds)) {
            ctab = dfrows_to_table(df[cinds,], lvl = as.integer(i+1))
            df = df[-cinds,]
        } else {
            ctab  = ElementaryTable()
        }
        key = na_to_sentinel(df[[rsplval]])
        ## declaring levels this way ensures they aren't sorted in a way we don't want.
        key = factor(key, levels = unique(key))
        ## XXX can't use split here because it sorts the levels of f
        spl = split(df, key)
        ret = TableTree(cont = ctab,
                        kids = lapply(spl, recursive_split, i = i+1), lev = as.integer(i))
    }

    ret
}

    
