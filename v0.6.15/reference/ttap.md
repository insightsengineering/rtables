# Access or set table elements at specified path

Access or set table elements at specified path

## Usage

``` r
tt_at_path(tt, path, ...)

tt_at_path(tt, path, ...) <- value
```

## Arguments

- tt:

  (`TableTree` or related class)  
  a `TableTree` object representing a populated table.

- path:

  (`character`)  
  a vector path for a position within the structure of a `TableTree`.
  Each element represents a subsequent choice amongst the children of
  the previous choice.

- ...:

  unused.

- value:

  (`ANY`)  
  the new value.

## Note

Setting `NULL` at a defined path removes the corresponding sub-table.

## Examples

``` r
# Accessing sub table.
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  split_rows_by("BMRKR2") %>%
  analyze("AGE")

tbl <- build_table(lyt, ex_adsl) %>% prune_table()
sub_tbl <- tt_at_path(tbl, path = c("SEX", "F", "BMRKR2"))

# Removing sub table.
tbl2 <- tbl
tt_at_path(tbl2, path = c("SEX", "F")) <- NULL
tbl2
#>                    A: Drug X   B: Placebo   C: Combination
#> ——————————————————————————————————————————————————————————
#> M                                                         
#>   LOW                                                     
#>     Mean             34.43       37.13          32.73     
#>   MEDIUM                                                  
#>     Mean             37.67       38.78          34.35     
#>   HIGH                                                    
#>     Mean             35.07       36.21          37.42     
#> U                                                         
#>   LOW                                                     
#>     Mean             31.00       27.00          37.00     
#>   MEDIUM                                                  
#>     Mean             33.00         NA           33.00     
#>   HIGH                                                    
#>     Mean              NA         35.00          38.00     
#> UNDIFFERENTIATED                                          
#>   LOW                                                     
#>     Mean             28.00         NA           45.00     

# Setting sub table.
lyt3 <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_rows_by("SEX") %>%
  analyze("BMRKR2")

tbl3 <- build_table(lyt3, ex_adsl) %>% prune_table()

tt_at_path(tbl3, path = c("SEX", "F", "BMRKR2")) <- sub_tbl
tbl3
#>                    A: Drug X   B: Placebo   C: Combination
#> ——————————————————————————————————————————————————————————
#> F                                                         
#>   LOW                                                     
#>     Mean             32.19       34.05          33.73     
#>   MEDIUM                                                  
#>     Mean             32.00       33.21          33.82     
#>   HIGH                                                    
#>     Mean             33.72       36.11          37.87     
#> M                                                         
#>   LOW                 21           23             11      
#>   MEDIUM              15           18             23      
#>   HIGH                15           14             26      
#> U                                                         
#>   LOW                  2           1              1       
#>   MEDIUM               1           0              2       
#>   HIGH                 0           1              1       
#> UNDIFFERENTIATED                                          
#>   LOW                  1           0              2       
```
