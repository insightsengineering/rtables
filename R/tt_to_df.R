
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




## second reference table
##
##                        ARM1                       ARM2
##                 Var          PctDiff       Var           PctDiff
##
##  Baseline
##       n         20                         40
##     median      5                          10                
##
##  Followup
##       n        12              12          30            30
##     median     8               60          5             -50


## Note the variables analyzed are in the **columns** here
##
## df (minus labeling cols)
##
## var   valtype    r1value    ARM1___Var  ARM1___PctDiff    ARM2___Var   ARM2____PctDiff   rsp_1   csp_1   csp1_type  csp_2    csp2_type
## NA    n          Baseline   20          NA                40           NA                visit   ARM     value      NA       var_analyzed
## NA    median     Baseline    5          NA                10           NA                visit   ARM     value      NA       var_analyzed     
## NA    n          Followup   12          12                30           30                visit   ARM     value      NA       var_analyzed
## NA    median     Followup    8          60                 5           -50               visit   ARM     value      NA       var_analyzed     


## returns a (flat) list of TableRow objects
## Note  that trow_to_dfrow will result in
## different numbers of columns for different
## levels of the tree, so we can't just
## do rbind.data.frame willy-nilly

## setGeneric("recursive_row_collect",
##            function(ttree)
##     standardGeneric("recursive_row_collect"))

## setMethod("recursive_row_collect", "TableTree",
##           function(ttree) {
##     ret = c(tree_children(content_table(ttree)),
##       lapply(tree_children(ttree),
##              recursive_row_collect))
##     unlist(ret, recursive = TRUE)
## })

## setMethod("recursive_row_collect", "ElementaryTable",
##           function(ttree) {
##     ret = lapply(tree_children(ttree),
##                  recursive_row_collect)
##     unlist(ret, recursive = TRUE)
## })

## setMethod("recursive_row_collect", "TableRow",
##           function(ttree) { 
##     ttree
## })


safepaste0 = function(bef, x, after = "") {
    if(length(x) == 0)
        return(character(0))

    paste0(bef, x, after)
}

##' @param vals Values for a single row of the resulting dataframe, as a list or vector
##' @param templ Template for the column names, e.g. a const from df_to_tt.R ending in _templ
##' @return vals, with names generated from templ
.put_colnames = function(vals, templ) {
    ## sprintf already returns length 0 character if the "data" part is len 0
    sq = seq_along(vals)
    names(vals) = sprintf(templ, sq)
    vals
}



## 
ploads_to_str = function(x, collapse = ":") sapply(x,
                                                   paste,
                                                   collapse = collapse)




trow_to_dfrow = function(trow) {
    nrowsplit = length(pos_splits(trow))
    rsvalues = .put_colnames(splv_rawvalues(pos_splvals(trow)),
                           rsvalue_templ)
    
    ## names(rsvalues) = sprintf(safepaste0("r", seq_along(rsvalues), "value")
    datvals = trow@leaf_value ##leaf_values(trow)
    names(datvals) = df_datcol_names(trow)
    rsvars = .put_colnames(ploads_to_str(pos_payloads(trow)),
                         rsvar_templ)
    ## names(rsvars) = safepaste0("rsp_", seq_along(rsvars))
    rsvarlbls = .put_colnames(pos_split_lbls(trow),
                            rsvar_lbl_templ)
    ## names(rsvarlbls) = safepaste0("rsplbl_", seq_along(rsvarlbls))
    rsvaluelbls = .put_colnames(pos_splval_lbls(trow),
                              rsvalue_lbl_templ)
    ## names(rsvaluelbls) = safepaste0("r",
    ##                             seq_along(rsvaluelbls),
    ##                             "vlbl")

    rstypes = .put_colnames(pos_spltypes(trow),
                            rstype_templ)

    ## column stuff, repetitive, and could probably be moved out, none of this
    ## stuff is varying at the row level
    ##
    ## not currently worth the refactor, revisit if doing this lots of
    ## times ends up costing enough to actually be an issue
    clspls = clayout_splits(trow)
    cspvar = .put_colnames(ploads_to_str(lapply(clspls, spl_payload)),
                           csvar_templ)
    
    cspvarlbl = .put_colnames(sapply(clspls, obj_label),
                              cs_lbl_templ)
    
    csptypes = .put_colnames(sapply(clspls, split_texttype),
                             cstype_templ)

    ##I() for a list col
    ## list() to insure it only makes 1 row

    cextras = .put_colnames(lapply(get_col_extras(clayout(trow)),
                                   function(x) I(list(x))),
                            cextras_templ)

    ## outer list() is to hold the name, its "one column"

    ## I() is to indicate its a list column
    ## inner list() is to indicate the first and only element
    ## row_cspans returns the vector that goes in
    ## that one element in the list column
    cspan = .put_colnames(list(I(list(row_cspans(trow)))), cspan_col) 
    ## names(cspvar) = safepaste0("csp_", seq_along(cspvar))
    ## names(cspvarlbl) = safepaste0("csplbl_", seq_along(cspvar))
    ## names(csptypes) = safepaste0("csptype_", seq_along(cspvar))
    

    ## annoying, but still better to have a single place to change
    ## colnames
    rinfolst = list(NA_integer_, ## row number, replaced elsewhere
                    row_variable(trow),
                    rowvar_label(trow),
                    trow@value_type,
                    trow@label,
                    I(list(get_pos_extra(pos = tree_pos(trow)))))
    names(rinfolst) = c(rnum_col,
                        rvar_col,
                        rvarlbl_col,
                        rvartype_col,
                        rlbl_col,
                        rowextra_col)

    
    ## don't worry terribly about order here, we fix it below
    ret = as.list(c(rinfolst,
                    rsvalues,
                    rsvaluelbls,
                    datvals,
                    rsvars,
                    rstypes,
                    rsvarlbls,
                    cspvar,
                    cspvarlbl,
                    csptypes,
                    cextras,
                    cspan))
    ret = as.data.frame(ret, stringsAsFactors = FALSE)

    ## enforce the correct order
    .order_rtablesdf_cols(ret)

}



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
    
    rws = collect_leaves(ttree, incl.cont = TRUE) ##recursive_row_collect(ttree)
    dfrws = lapply(rws, trow_to_dfrow)
    cnamevecs = lapply(dfrws, names)
    frowind = min(which.max(sapply(cnamevecs, length)))
    
    fnamevec = cnamevecs[[frowind]]
    ## XXX class vectors of length > 1 will break this
    ## but shouldn't happen, right?
    fcls = sapply(dfrws[[frowind]], class)

    dfrws = lapply(dfrws, function(rw) {
        for(newcol in setdiff(fnamevec, names(rw))) {
            rw[[newcol]] = switch(fcls[newcol],
                                  character = NA_character_,
                                  factor = NA_integer_,
                                  integer = NA_integer_,
                                  default = NA)
        }
        rw[,fnamevec]
    })

    ret = do.call(rbind.data.frame, dfrws)
    rownames(ret) = seq_along(ret[[1]])
    ## rndf = data.frame(seq_along(ret[[1]]))
    ## names(rndf) = rnum_col
    ## ret = cbind.data.frame(rndf, ret)
    ret[[rnum_col]] = seq_along(ret[[1]])
    ## this pads the fake rowspans and ensures order
    ret = fixup_rtable_df(ret)
    ret
}
