
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



## returns a (flat) list of TableRow objects
## Note  that trow_to_dfrow will result in
## different numbers of columns for different
## levels of the tree, so we can't just
## do rbind.data.frame willy-nilly

setGeneric("recursive_row_collect",
           function(ttree)
    standardGeneric("recursive_row_collect"))

setMethod("recursive_row_collect", "TableTree",
          function(ttree) {
    ret = c(tree_children(content_table(ttree)),
      lapply(tree_children(ttree),
             recursive_row_collect))
    unlist(ret, recursive = TRUE)
})

setMethod("recursive_row_collect", "ElementaryTable",
          function(ttree) {
    ret = lapply(tree_children(ttree),
                 recursive_row_collect)
    unlist(ret, recursive = TRUE)
})

setMethod("recursive_row_collect", "TableRow",
          function(ttree) { 
    ttree
})

trow_to_dfrow = function(trow) {
    nrowsplit = length(rs_values(trow))
    rsvalues = rs_values(trow)
    names(rsvalues) = paste0("r", seq_along(rsvalues), "value")
    datvals = trow@leaf_value ##leaf_values(trow)
    names(datvals) = df_datcol_names(trow)
    rsvars = rs_vars(trow)
    names(rsvars) = paste0("rsp_", seq_along(rsvars))
    rsvarlbls = rs_var_lbls(trow)
    names(rsvarlbls) = paste0("rsplbl_", seq_along(rsvarlbls))
    rsvaluelbls = rs_value_lbls(trow)
    names(rsvaluelbls) = paste0("r",
                                seq_along(rsvaluelbls),
                                "vlbl")
                                
    cspvar = character()
    cspvarlbl = character()
    cl = clayout(trow)
    tmp = cl
    while(!is(tmp, "LayoutColLeaf")) {
        cspvar = c(cspvar, tmp@splitvar)
        tmplbl =  tmp@splitlbl
        if(length(tmplbl) == 0 || is.na(tmplbl))
            tmplbl = tmp@splitvar
        
        cspvarlbl = c(cspvarlbl, tmplbl)
        tmp = tree_children(tmp)[[1]]
    }
    names(cspvar) = paste0("csp_", seq_along(cspvar))
    names(cspvarlbl) = paste0("csplbl_", seq_along(cspvar))
    
    
    ret = as.list(c(var = trow@var_analyzed, ##a_var(trow),
                    varlbl = trow@var_label,
                    valtype = trow@value_type,
                    rowlbl = trow@label,
                    rsvalues,
                    rsvaluelbls,
                    datvals,
                    rsvars,
                    rsvarlbls,
                    cspvar,
                    cspvarlbl))
    as.data.frame(ret, stringsAsFactors = FALSE)

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
    
    rws = recursive_row_collect(ttree)
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
    ret
}
