# Summarize table

Summarize table

## Usage

``` r
table_structure(x, detail = c("subtable", "row"))
```

## Arguments

- x:

  (`VTableTree`)  
  a table object.

- detail:

  (`string`)  
  either `row` or `subtable`.

## Value

No return value. Called for the side-effect of printing a row- or
subtable-structure summary of `x`.

## Examples

``` r
library(dplyr)

iris2 <- iris %>%
  group_by(Species) %>%
  mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
  ungroup()

lyt <- basic_table() %>%
  split_cols_by("Species") %>%
  split_cols_by("group") %>%
  analyze(c("Sepal.Length", "Petal.Width"),
    afun = list_wrap_x(summary),
    format = "xx.xx"
  )

tbl <- build_table(lyt, iris2)
tbl
#>                  setosa       versicolor      virginica  
#>                 a      b       a       b       a      b  
#> —————————————————————————————————————————————————————————
#> Sepal.Length                                             
#>   Min.         4.40   4.30   5.00    4.90    4.90    5.60
#>   1st Qu.      4.80   4.80   5.60    5.60    6.20    6.30
#>   Median       5.00   5.00   5.90    5.90    6.50    6.50
#>   Mean         5.02   4.99   5.99    5.88    6.50    6.67
#>   3rd Qu.      5.30   5.10   6.40    6.10    6.70    7.20
#>   Max.         5.80   5.70   7.00    6.70    7.70    7.90
#> Petal.Width                                              
#>   Min.         0.10   0.10   1.00    1.00    1.40    1.50
#>   1st Qu.      0.20   0.20   1.20    1.20    1.90    1.80
#>   Median       0.20   0.20   1.30    1.30    2.10    2.00
#>   Mean         0.23   0.26   1.35    1.30    2.08    1.98
#>   3rd Qu.      0.20   0.30   1.50    1.40    2.30    2.20
#>   Max.         0.40   0.60   1.80    1.70    2.50    2.50

row_paths(tbl)
#> [[1]]
#> [1] "ma_Sepal.Length_Petal.Width" "Sepal.Length"               
#> 
#> [[2]]
#> [1] "ma_Sepal.Length_Petal.Width" "Sepal.Length"               
#> [3] "Min."                       
#> 
#> [[3]]
#> [1] "ma_Sepal.Length_Petal.Width" "Sepal.Length"               
#> [3] "1st Qu."                    
#> 
#> [[4]]
#> [1] "ma_Sepal.Length_Petal.Width" "Sepal.Length"               
#> [3] "Median"                     
#> 
#> [[5]]
#> [1] "ma_Sepal.Length_Petal.Width" "Sepal.Length"               
#> [3] "Mean"                       
#> 
#> [[6]]
#> [1] "ma_Sepal.Length_Petal.Width" "Sepal.Length"               
#> [3] "3rd Qu."                    
#> 
#> [[7]]
#> [1] "ma_Sepal.Length_Petal.Width" "Sepal.Length"               
#> [3] "Max."                       
#> 
#> [[8]]
#> [1] "ma_Sepal.Length_Petal.Width" "Petal.Width"                
#> 
#> [[9]]
#> [1] "ma_Sepal.Length_Petal.Width" "Petal.Width"                
#> [3] "Min."                       
#> 
#> [[10]]
#> [1] "ma_Sepal.Length_Petal.Width" "Petal.Width"                
#> [3] "1st Qu."                    
#> 
#> [[11]]
#> [1] "ma_Sepal.Length_Petal.Width" "Petal.Width"                
#> [3] "Median"                     
#> 
#> [[12]]
#> [1] "ma_Sepal.Length_Petal.Width" "Petal.Width"                
#> [3] "Mean"                       
#> 
#> [[13]]
#> [1] "ma_Sepal.Length_Petal.Width" "Petal.Width"                
#> [3] "3rd Qu."                    
#> 
#> [[14]]
#> [1] "ma_Sepal.Length_Petal.Width" "Petal.Width"                
#> [3] "Max."                       
#> 

table_structure(tbl)
#> [TableTree] ma_Sepal.Length_Petal.Width
#>  [ElementaryTable] Sepal.Length (6 x 6)
#>  [ElementaryTable] Petal.Width (6 x 6)

table_structure(tbl, detail = "row")
#> TableTree: [ma_Sepal.Length_Petal.Width] ()
#>   labelrow: [] () - <not visible>
#>   children: 
#>     ElementaryTable: [Sepal.Length] (Sepal.Length)
#>       labelrow: [Sepal.Length] (Sepal.Length)
#>       children: 
#>         DataRow: [Min.] (Min.)
#>         DataRow: [1st Qu.] (1st Qu.)
#>         DataRow: [Median] (Median)
#>         DataRow: [Mean] (Mean)
#>         DataRow: [3rd Qu.] (3rd Qu.)
#>         DataRow: [Max.] (Max.)
#>     ElementaryTable: [Petal.Width] (Petal.Width)
#>       labelrow: [Petal.Width] (Petal.Width)
#>       children: 
#>         DataRow: [Min.] (Min.)
#>         DataRow: [1st Qu.] (1st Qu.)
#>         DataRow: [Median] (Median)
#>         DataRow: [Mean] (Mean)
#>         DataRow: [3rd Qu.] (3rd Qu.)
#>         DataRow: [Max.] (Max.)
```
