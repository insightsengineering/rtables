# Table dimensions

Table dimensions

## Usage

``` r
# S4 method for class 'VTableTree'
nrow(x)

# S4 method for class 'VTableNodeInfo'
ncol(x)

# S4 method for class 'VTableNodeInfo'
dim(x)
```

## Arguments

- x:

  (`TableTree` or `ElementaryTable`)  
  a table object.

## Value

The number of rows (`nrow`), columns (`ncol`), or both (`dim`) of the
object.

## Examples

``` r
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  analyze(c("SEX", "AGE"))

tbl <- build_table(lyt, ex_adsl)

dim(tbl)
#> [1] 7 3
nrow(tbl)
#> [1] 7
ncol(tbl)
#> [1] 3

NROW(tbl)
#> [1] 7
NCOL(tbl)
#> [1] 3
```
