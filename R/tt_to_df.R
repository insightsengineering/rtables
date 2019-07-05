
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






nasentinel = "_x___x_NA_x___x_"
na_to_sentinel = function(v) {
    v[is.na(v)] = nasentinel
    v
}


df_to_tt = function(df) {
    stopifnot(nrow(df) > 0)
    tab = recursive_split(df, 1)
    layout = recursive_layout(df, 1)
    
}


recursive_split = function(df, i) {
    rsplval = paste0("r", i, "value")
    if(! (rsplval %in% names(df))) { # no more levels to split on
        cinds = which(is.na(df$var))
        if(length(cinds)) {
            ctab = rows_to_el_table(df[cinds,], level = NA_integer_)
            df = df[-cinds,]
            ret = TableTree(cont = ctab, kids = tree_children(rows_to_el_table(df, level = as.integer(i+1))), lev = as.integer(i))
        } else {
            ret = rows_to_el_table(df, level = as.integer(i))
        }
    } else  {
        cinds = which(is.na(df[[rsplval]]) & is.na(df$var))
        if(length(cinds)) {
            ctab = rows_to_el_table(df[cinds,], level = NA_integer_)
            df = df[-cinds,]
        } else {
            ctab  = ElementaryTable()
        }
        key = na_to_sentinel(df[[rsplval]])
        ## declaring levels this way ensures they aren't sorted in a way we don't want.
        key = factor(key, levels = unique(key))
        ## XXX can't use split here because it sorts the levels of f
        spl = split(df, key)
        ret = TableTree(cont = ctab, kids = lapply(spl, recursive_split, i = i+1), lev = as.integer(i))
    }

    ret
}

rows_to_el_table = function(df, level, label = "", cont = NULL) {
    rowinds = seq(along = df[[1]])
    datacols = grep("___", names(df), value = TRUE)
    kids = lapply(rowinds, function(i) {
        TableRow(lev = level, lab = as.character(df$rowlabel[i]), val = as.list(df[i, datacols]))
    })
    if(is.null(cont))
        ElementaryTable(kids = kids, lev = level, lab = label)
    else
        TableTree(cont = cont, lev = level, lab = label, kids = kids)
}




layout_from_df = function(df) {
    rlyt = recursive_row_layout(df, 1)
    clyt = recursive_col_layout(df, 1)


}

recursive_row_layout = function(df, i) {
    
    rsplval = paste0("r", i, "value")
    if(! (rsplval %in% names(df))) { # no more levels to split on
        cinds = which(is.na(df$var))
        if(length(cinds)) {
            ctab = rows_to_el_table(df[cinds,], level = NA_integer_)
            df = df[-cinds,]
            ret = TableTree(cont = ctab, kids = tree_children(rows_to_el_table(df, level = as.integer(i+1))), lev = as.integer(i))
        } else {
            ret = rows_to_el_table(df, level = as.integer(i))
        }
    } else  {
        cinds = which(is.na(df[[rsplval]]) & is.na(df$var))
        if(length(cinds)) {
            ctab = rows_to_el_table(df[cinds,], level = NA_integer_)
            df = df[-cinds,]
        } else {
            ctab  = ElementaryTable()
        }
        key = na_to_sentinel(df[[rsplval]])
        ## declaring levels this way ensures they aren't sorted in a way we don't want.
        key = factor(key, levels = unique(key))
        ## XXX can't use split here because it sorts the levels of f
        spl = split(df, key)
        ret = TableTree(cont = ctab, kids = lapply(spl, recursive_split, i = i+1), lev = as.integer(i))
    }

    ret
}

