
.make_cspan_col = function(df) {
    ndatcols = length(.data_colnames(df))
    if(is.null(df[[cspan_col]])) {
        
        df[[cspan_col]] = I(replicate(nrow(df), rep(1L, ndatcols), simplify= FALSE))
    }
    df
}

.make_rspan_cols = function(df, n = length(.data_colnames(df))) {
    stopifnot(n>0)
    colnms = paste0("rspan_col_", 1:n)
    for(col in colnms) {
        if(is.null(df[[col]]))
            df[[col]] = seq_along(df[[1]])
    }
    df
}


.make_lbl_cols = function(df, colnms, lblcolfmt) {

    colseq = seq_along(colnms)
    lblcolnms = sprintf(lblcolfmt, colseq)
    for(i in colseq) {
        if(is.null(df[[ lblcolnms[ i ] ]])) {
            cspval = df[[ colnms[ i ] ]]
            df[[ lblcolnms[ i ] ]] = cspval
        }
    }
    df
}

.make_csplbl_cols = function(df) {
    .make_lbl_cols(df,
                   .csvar_colnames(df),
                   "csplbl_%d")
}

.make_rsplbl_cols = function(df) {
    .make_lbl_cols(df,
                   .rsvar_colnames(df),
                   "rsplbl_%d")
}

.make_rsval_lbls = function(df) {
    .make_lbl_cols(df,
                   .rsvalue_colnames(df),
                   "r%dvlbl")
}

.make_rstype_cols = function(df) {
    rspcols = .rsvar_colnames(df)
    ncols = length(rspcols)
    typcols = paste0("rsptype_", seq_along(rspcols))
    for(i in seq_along(rspcols)) {
        tc = typcols[i]
        if(is.null(df[[ tc ]])) {
            df[[tc]] = NA_character_
            df[[tc]][!is.na(df[[ rspcols[i] ]])] = "varlevels"
        }
            
    }
    df
}

.make_cstype_cols = function(df) {
    cspcols = .csvar_colnames(df)
    ncols = length(cspcols)
    typcols = paste0("csptype_", seq_along(cspcols))
    for(i in seq_along(cspcols)) {
        tc = typcols[i]
        if(is.null(df[[ tc ]])) {
            df[[tc]] = NA_character_
            df[[tc]][!is.na(df[[ cspcols[i] ]])] = "varlevels"
        }
            
    }
    df
}

.make_csvextra_cols = function(df) {
    datcols = .data_colnames(df)
    ncolumns = length(datcols)
    ncseq = seq(1, ncolumns)
    extcols = sprintf(cextras_templ, ncseq)
    for(col in extcols)
        df[[col]] = I(rep(list(list()), nrow(df)))
    df
}

.make_rowextra_col = function(df) {
    if(!(rowextra_col %in% names(df)))
        df[[rowextra_col]] = I(rep(list(list()), nrow(df)))
    df
}






.make_vartype_cols = function(df) {
    if(is.null(df$rowvartype))
        df$rowvartype = "varlevels"
    df
}

.order_rtablesdf_cols = function(df) {
    nmord = c(rnum_col,
              rvar_col,
              rvarlbl_col,
              rvartype_col, 
              rlbl_col,
              rowextra_col,
              .rsvar_colnames(df),
              .rsvarlbl_colnames(df),
              .rsvalue_colnames(df),
              .rsvaluelbl_colnames(df),
              .rsvartype_colnames(df),
              .data_colnames(df),
              .csvalextra_colnames(df),
              .csvar_colnames(df),
              .csvarlbl_colnames(df),
              .csvartype_colnames(df),
              cspan_col, #constant from df_to_tt.R
              .rspan_colnames(df)
              )
    stopifnot(length(setdiff(names(df), nmord)) == 0 &&
              length(setdiff(nmord, names(df))) == 0)
    df[,nmord]
}


## implemenets defaults for things if would be annoying to always
## put into the data.frame version if it is created first

### colspan information: list column called c__colspans__c
### rowspan information, k columns named rspan_col_1 to rspan_col_k


##'@title fixup_rtable_df
##'@description Pad an existing data.frame with default
##' values for labeling, spanning, and split-type columns.
##' Only those columns which are not already present are
##' created.
##' @param df A data.frame which matches the basic structure
##' of a tabletree in data.frame form, but which may lack
##' some of the expected secondary-information  columns
##' @details The following are added, if not present:
##' \itemize{
##' \item{columnspan column. Defaults to no column spanning.}
##' \item{rowspan columns.  Defaults to no row spanning.}
##' \item{column split labels. Defaults to the value of the existing column split "variables" (value).}
##' \item{column split types columns. Defaults to "varlevels" for all splits}
##' \item{rowsplit variable labels. Defaults to the rowsplit variables.}
##' \item{rowsplit value labels. Defaults to being the same as the}
##' \item{rowsplit split type columns. Defaults to "varlevels" for all splits.}
##' 
fixup_rtable_df = function(df) {

    ## add column and row span information
    
    df = .make_cspan_col(df)
    df = .make_rspan_cols(df = df)

    ## add any missing label information
    df = .make_csplbl_cols(df)
    df = .make_rsplbl_cols(df)
    df = .make_rsval_lbls(df)

    ## add split type information
    df = .make_cstype_cols(df)
    df = .make_rstype_cols(df)
    df = .make_vartype_cols(df)

    ## add column and row extra args information
    df = .make_csvextra_cols(df)
    df = .make_rowextra_col(df)

    ## reorder columns so that the added columns appear in
    ## the right order,  just incase.
    
    df = .order_rtablesdf_cols(df)
    df
}







## .make_cslbl_cols = function(df, n = length(.data_colnames(df))) {
##     stopifnot(n>0)
##     csplitcols = .csvar_colnames
##     colseq = seq_along(csplitcols)
##     colnms = paste0("csplbl_", colseq)
##     for(i in colseq) {
##         if(is.null(df[[ colnms[ i ] ]])) {
##             cspval = df[[ csplitcols[ i ] ]]
##             df[[ colnms[ i ] ]] = cspval
##         }
##     }
##     df
## }

## .make_rsvar_lblcols = function(df) {
##     rsvarnms = .rsvar_colnames(df)
##     colseq = seq_along(rsvarnms)
##     colnms = paste0("rsplbl_", colseq)
##     for(i in colseq) {
##         if(is.null(df[[ colnms[ i ] ]])) {
##             valvec = df[[ rsvarnms[ i ] ]]
##             df[[ colnms[ i ] ]] = valvec
##         }
##     }
##     df
## }
