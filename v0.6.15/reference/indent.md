# Change indentation of all `rrows` in an `rtable`

Change indentation of all `rrows` in an `rtable`

## Usage

``` r
indent(x, by = 1)
```

## Arguments

- x:

  (`VTableTree`)  
  an `rtable` object.

- by:

  (`integer`)  
  number to increase indentation of rows by. Can be negative. If final
  indentation is less than 0, the indentation is set to 0.

## Value

`x` with its indent modifier incremented by `by`.

## Examples

``` r
is_setosa <- iris$Species == "setosa"
m_tbl <- rtable(
  header = rheader(
    rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan = 2)),
    rrow(NULL, "mean", "median", "mean", "median")
  ),
  rrow(
    row.name = "All Species",
    mean(iris$Sepal.Length), median(iris$Sepal.Length),
    mean(iris$Petal.Length), median(iris$Petal.Length),
    format = "xx.xx"
  ),
  rrow(
    row.name = "Setosa",
    mean(iris$Sepal.Length[is_setosa]), median(iris$Sepal.Length[is_setosa]),
    mean(iris$Petal.Length[is_setosa]), median(iris$Petal.Length[is_setosa]),
    format = "xx.xx"
  )
)
indent(m_tbl)
#>                  Sepal.Length      Petal.Length  
#>                 mean    median    mean    median 
#> —————————————————————————————————————————————————
#>   All Species   5.84     5.80     3.76     4.35  
#>   Setosa        5.01     5.00     1.46     1.50  
indent(m_tbl, 2)
#>                    Sepal.Length      Petal.Length  
#>                   mean    median    mean    median 
#> ———————————————————————————————————————————————————
#>     All Species   5.84     5.80     3.76     4.35  
#>     Setosa        5.01     5.00     1.46     1.50  
```
