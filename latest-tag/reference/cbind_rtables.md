# Column-bind two `TableTree` objects

Column-bind two `TableTree` objects

## Usage

``` r
cbind_rtables(x, ..., sync_count_vis = TRUE)
```

## Arguments

- x:

  (`TableTree` or `TableRow`)  
  a table or row object.

- ...:

  one or more further objects of the same class as `x`.

- sync_count_vis:

  (`logical(1)`)  
  should column count visibility be synced across the new and existing
  columns. Currently defaults to `TRUE` for backwards compatibility but
  this may change in future releases.

## Value

A formal table object.

## Examples

``` r
x <- rtable(c("A", "B"), rrow("row 1", 1, 2), rrow("row 2", 3, 4))
y <- rtable("C", rrow("row 1", 5), rrow("row 2", 6))
z <- rtable("D", rrow("row 1", 9), rrow("row 2", 10))

t1 <- cbind_rtables(x, y)
t1
#>         A   B   C
#> —————————————————
#> row 1   1   2   5
#> row 2   3   4   6

t2 <- cbind_rtables(x, y, z)
t2
#>         A   B   C   D 
#> ——————————————————————
#> row 1   1   2   5   9 
#> row 2   3   4   6   10

col_paths_summary(t1)
#> label    path                  
#> ———————————————————————————————
#> A        cbind_tbl_1, manual, A
#> B        cbind_tbl_1, manual, B
#> C        cbind_tbl_2, manual, C
col_paths_summary(t2)
#> label    path                  
#> ———————————————————————————————
#> A        cbind_tbl_1, manual, A
#> B        cbind_tbl_1, manual, B
#> C        cbind_tbl_2, manual, C
#> D        cbind_tbl_3, manual, D
```
