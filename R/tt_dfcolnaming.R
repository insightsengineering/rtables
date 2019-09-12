###
### tt_dfcolnaming.R - column naming and access utilities for df form of a tabletree
###


###
### Patterns, constants and templates for column naming in df form
###
### *_pat are regexs
### *_templ are sprintf templates with a %d component
### *_col are literal constants

## Data column naming
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


## row split information

## constant
rnum_col = "rownum"
rvar_col = "rowvar"
rvarlbl_col = "rowvarlbl"
rvartype_col = "rowvartype"
rlbl_col = "rowlbl"
rowextra_col = "._rvalextra"

## patterns
rsvar_pat = "^rsp_[0-9]+$"
rsvalue_pat = "^r[0-9]+value$"
rsvalue_lbl_pat = "^r[0-9]+vlbl$"
rsvar_lbl_pat = "^rsplbl_[0-9]+$"
rstype_pat = "^rsptype_[0-9]+$"

## templates
rsvar_templ = "rsp_%d"
rsvalue_templ = "r%dvalue"
rsvalue_lbl_templ = "r%dvlbl"
rsvar_lbl_templ = "rsplbl_%d"
rstype_templ = "rsptype_%d"


## column split information
## patterns
csvar_pat = "^csp_[0-9]+$"
cstype_pat = "^csptype_[0-9]+$"
cs_lbl_pat = "^csplbl_[0-9]+$"
cextras_pat = "^colextra_[0-9]+$"


## templates
csvar_templ = "csp_%d"
cstype_templ = "csptype_%d"
cs_lbl_templ = "csplbl_%d"
cextras_templ = "colextra_%d"

## row/colspanning information
cspan_col = "c__colspans__c"
rspan_col_pat = "^rspan_col_[0-9]+$"
rspan_col_templ = "rspan_col_%d"



avar_col = "rowvar"
avarlbl_col = "rowvarlbl"
rowlbl_col = "rowlbl"



###
### Access functions for column names
###


.rsvar_colnames = function(df) 
    grep(rsvar_pat, names(df), value = TRUE)

.rsvarlbl_colnames = function(df)
    grep(rsvar_lbl_pat, names(df), value = TRUE)

.rsvalue_colnames = function(df)
    grep(rsvalue_pat, names(df), value = TRUE)

.rsvaluelbl_colnames = function(df)
    grep(rsvalue_lbl_pat, names(df), value = TRUE)

.rsvalueextra_colnames = function(df)
    grep(rsvalue_extra_pat, names(df), value = TRUE)

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

.csvalextra_colnames = function(df)
    grep(cextras_pat, names(df), value = TRUE)

.rspan_colnames = function(df)
    grep(rspan_col_pat, names(df), value = TRUE)


###
### Value access utilities for df form
###



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

## .get_rsvalue_extras = function(df, i) {
##     df[[
##     mycols = .get_rscols_helper(df, i, rsvar_lbl_pat, NA_character_)
##     if(is.na(mycols) || length(mycols) == 0)
##         return(mycols)

##     vapply(mycols,
##            function(x) {
##         ret = unique(df[[x]])
##         stopifnot(length(ret) == 1)
##         ret
##     }, character(1))
    
## }


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
.get_val_type = function(df) { unique(df[[rvartype_col]])}
