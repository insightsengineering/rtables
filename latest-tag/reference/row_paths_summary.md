# Print row/column paths summary

Print row/column paths summary

## Usage

``` r
row_paths_summary(x)

col_paths_summary(x)
```

## Arguments

- x:

  (`VTableTree`)  
  an `rtable` object.

## Value

A data frame summarizing the row- or column-structure of `x`.

## Examples

``` r
ex_adsl_MF <- ex_adsl %>% dplyr::filter(SEX %in% c("M", "F"))

lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX", split_fun = drop_split_levels) %>%
  analyze(c("AGE", "BMRKR2"))

tbl <- build_table(lyt, ex_adsl_MF)
tbl
#>              A: Drug X      B: Placebo      C: Combination  
#>              F       M       F       M        F         M   
#> ————————————————————————————————————————————————————————————
#> AGE                                                         
#>   Mean     32.76   35.57   34.12   37.44    35.20     35.38 
#> BMRKR2                                                      
#>   LOW       26      21      21      23       26        11   
#>   MEDIUM    21      15      38      18       17        23   
#>   HIGH      32      15      18      14       23        26   

df <- row_paths_summary(tbl)
#> rowname     node_class    path                         
#> ———————————————————————————————————————————————————————
#> AGE         LabelRow      ma_AGE_BMRKR2, AGE           
#>   Mean      DataRow       ma_AGE_BMRKR2, AGE, Mean     
#> BMRKR2      LabelRow      ma_AGE_BMRKR2, BMRKR2        
#>   LOW       DataRow       ma_AGE_BMRKR2, BMRKR2, LOW   
#>   MEDIUM    DataRow       ma_AGE_BMRKR2, BMRKR2, MEDIUM
#>   HIGH      DataRow       ma_AGE_BMRKR2, BMRKR2, HIGH  
df
#>    label indent node_class         path
#> 1    AGE      0   LabelRow ma_AGE_B....
#> 2   Mean      1    DataRow ma_AGE_B....
#> 3 BMRKR2      0   LabelRow ma_AGE_B....
#> 4    LOW      1    DataRow ma_AGE_B....
#> 5 MEDIUM      1    DataRow ma_AGE_B....
#> 6   HIGH      1    DataRow ma_AGE_B....

col_paths_summary(tbl)
#> label             path                       
#> —————————————————————————————————————————————
#> A: Drug X         ARM, A: Drug X             
#>   F               ARM, A: Drug X, SEX, F     
#>   M               ARM, A: Drug X, SEX, M     
#> B: Placebo        ARM, B: Placebo            
#>   F               ARM, B: Placebo, SEX, F    
#>   M               ARM, B: Placebo, SEX, M    
#> C: Combination    ARM, C: Combination        
#>   F               ARM, C: Combination, SEX, F
#>   M               ARM, C: Combination, SEX, M

# manually constructed table
tbl2 <- rtable(
  rheader(
    rrow(
      "row 1", rcell("a", colspan = 2),
      rcell("b", colspan = 2)
    ),
    rrow("h2", "a", "b", "c", "d")
  ),
  rrow("r1", 1, 2, 1, 2), rrow("r2", 3, 4, 2, 1)
)
col_paths_summary(tbl2)
#> label    path                
#> —————————————————————————————
#> a        manual, a           
#>   a      manual, a, manual, a
#>   b      manual, a, manual, b
#> b        manual, b           
#>   c      manual, b, manual, c
#>   d      manual, b, manual, d
```
