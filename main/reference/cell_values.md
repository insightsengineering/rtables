# Retrieve cell values by row and column path

Retrieve cell values by row and column path

## Usage

``` r
cell_values(tt, rowpath = NULL, colpath = NULL, omit_labrows = TRUE)

value_at(tt, rowpath = NULL, colpath = NULL)

# S4 method for class 'VTableTree'
value_at(tt, rowpath = NULL, colpath = NULL)
```

## Arguments

- tt:

  (`TableTree` or related class)  
  a `TableTree` object representing a populated table.

- rowpath:

  (`character`)  
  path in row-split space to the desired row(s). Can include
  `"@content"`.

- colpath:

  (`character`)  
  path in column-split space to the desired column(s). Can include
  `"*"`.

- omit_labrows:

  (`flag`)  
  whether label rows underneath `rowpath` should be omitted (`TRUE`, the
  default), or return empty lists of cell "values" (`FALSE`).

## Value

- `cell_values` returns a `list` (regardless of the type of value the
  cells hold). If `rowpath` defines a path to a single row,
  `cell_values` returns the list of cell values for that row, otherwise
  a list of such lists, one for each row captured underneath `rowpath`.
  This occurs after subsetting to `colpath` has occurred.

- `value_at` returns the "unwrapped" value of a single cell, or an
  error, if the combination of `rowpath` and `colpath` do not define the
  location of a single cell in `tt`.

## Note

`cell_values` will return a single cell's value wrapped in a list. Use
`value_at` to receive the "bare" cell value.

## Examples

``` r
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX") %>%
  split_rows_by("RACE") %>%
  summarize_row_groups() %>%
  split_rows_by("STRATA1") %>%
  analyze("AGE")

library(dplyr) ## for mutate
tbl <- build_table(lyt, DM %>%
  mutate(SEX = droplevels(SEX), RACE = droplevels(RACE)))

row_paths_summary(tbl)
#> rowname                      node_class    path                                                                
#> ———————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> ASIAN                        ContentRow    RACE, ASIAN, @content, ASIAN                                        
#>   A                          LabelRow      RACE, ASIAN, STRATA1, A                                             
#>     Mean                     DataRow       RACE, ASIAN, STRATA1, A, AGE, Mean                                  
#>   B                          LabelRow      RACE, ASIAN, STRATA1, B                                             
#>     Mean                     DataRow       RACE, ASIAN, STRATA1, B, AGE, Mean                                  
#>   C                          LabelRow      RACE, ASIAN, STRATA1, C                                             
#>     Mean                     DataRow       RACE, ASIAN, STRATA1, C, AGE, Mean                                  
#> BLACK OR AFRICAN AMERICAN    ContentRow    RACE, BLACK OR AFRICAN AMERICAN, @content, BLACK OR AFRICAN AMERICAN
#>   A                          LabelRow      RACE, BLACK OR AFRICAN AMERICAN, STRATA1, A                         
#>     Mean                     DataRow       RACE, BLACK OR AFRICAN AMERICAN, STRATA1, A, AGE, Mean              
#>   B                          LabelRow      RACE, BLACK OR AFRICAN AMERICAN, STRATA1, B                         
#>     Mean                     DataRow       RACE, BLACK OR AFRICAN AMERICAN, STRATA1, B, AGE, Mean              
#>   C                          LabelRow      RACE, BLACK OR AFRICAN AMERICAN, STRATA1, C                         
#>     Mean                     DataRow       RACE, BLACK OR AFRICAN AMERICAN, STRATA1, C, AGE, Mean              
#> WHITE                        ContentRow    RACE, WHITE, @content, WHITE                                        
#>   A                          LabelRow      RACE, WHITE, STRATA1, A                                             
#>     Mean                     DataRow       RACE, WHITE, STRATA1, A, AGE, Mean                                  
#>   B                          LabelRow      RACE, WHITE, STRATA1, B                                             
#>     Mean                     DataRow       RACE, WHITE, STRATA1, B, AGE, Mean                                  
#>   C                          LabelRow      RACE, WHITE, STRATA1, C                                             
#>     Mean                     DataRow       RACE, WHITE, STRATA1, C, AGE, Mean                                  
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

cell_values(
  tbl, c("RACE", "ASIAN", "STRATA1", "B"),
  c("ARM", "A: Drug X", "SEX", "F")
)
#> $`A: Drug X.F`
#> [1] 33.75
#> 

# it's also possible to access multiple values by being less specific
cell_values(
  tbl, c("RACE", "ASIAN", "STRATA1"),
  c("ARM", "A: Drug X", "SEX", "F")
)
#> $A.AGE.Mean
#> $A.AGE.Mean$`A: Drug X.F`
#> [1] 30.4
#> 
#> 
#> $B.AGE.Mean
#> $B.AGE.Mean$`A: Drug X.F`
#> [1] 33.75
#> 
#> 
#> $C.AGE.Mean
#> $C.AGE.Mean$`A: Drug X.F`
#> [1] 36.92308
#> 
#> 
cell_values(tbl, c("RACE", "ASIAN"), c("ARM", "A: Drug X", "SEX", "M"))
#> $ASIAN
#> $ASIAN$`A: Drug X.M`
#> [1] 35.0000000  0.6862745
#> 
#> 
#> $STRATA1.A.AGE.Mean
#> $STRATA1.A.AGE.Mean$`A: Drug X.M`
#> [1] 34.41667
#> 
#> 
#> $STRATA1.B.AGE.Mean
#> $STRATA1.B.AGE.Mean$`A: Drug X.M`
#> [1] 34.875
#> 
#> 
#> $STRATA1.C.AGE.Mean
#> $STRATA1.C.AGE.Mean$`A: Drug X.M`
#> [1] 35.6
#> 
#> 

## any arm, male columns from the ASIAN content (i.e. summary) row
cell_values(
  tbl, c("RACE", "ASIAN", "@content"),
  c("ARM", "B: Placebo", "SEX", "M")
)
#> $`B: Placebo.M`
#> [1] 31.00  0.62
#> 
cell_values(
  tbl, c("RACE", "ASIAN", "@content"),
  c("ARM", "*", "SEX", "M")
)
#> $`A: Drug X.M`
#> [1] 35.0000000  0.6862745
#> 
#> $`B: Placebo.M`
#> [1] 31.00  0.62
#> 
#> $`C: Combination.M`
#> [1] 44.0000000  0.6470588
#> 

## all columns
cell_values(tbl, c("RACE", "ASIAN", "STRATA1", "B"))
#> $`A: Drug X.F`
#> [1] 33.75
#> 
#> $`A: Drug X.M`
#> [1] 34.875
#> 
#> $`B: Placebo.F`
#> [1] 32.46154
#> 
#> $`B: Placebo.M`
#> [1] 30.9375
#> 
#> $`C: Combination.F`
#> [1] 33.3
#> 
#> $`C: Combination.M`
#> [1] 35.91667
#> 

## all columns for the Combination arm
cell_values(
  tbl, c("RACE", "ASIAN", "STRATA1", "B"),
  c("ARM", "C: Combination")
)
#> $`C: Combination.F`
#> [1] 33.3
#> 
#> $`C: Combination.M`
#> [1] 35.91667
#> 

cvlist <- cell_values(
  tbl, c("RACE", "ASIAN", "STRATA1", "B", "AGE", "Mean"),
  c("ARM", "B: Placebo", "SEX", "M")
)
cvnolist <- value_at(
  tbl, c("RACE", "ASIAN", "STRATA1", "B", "AGE", "Mean"),
  c("ARM", "B: Placebo", "SEX", "M")
)
stopifnot(identical(cvlist[[1]], cvnolist))
```
