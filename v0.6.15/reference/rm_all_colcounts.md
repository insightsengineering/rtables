# Set all column counts at all levels of nesting to NA

Set all column counts at all levels of nesting to NA

## Usage

``` r
rm_all_colcounts(obj)

# S4 method for class 'VTableTree'
rm_all_colcounts(obj)

# S4 method for class 'InstantiatedColumnInfo'
rm_all_colcounts(obj)

# S4 method for class 'LayoutColTree'
rm_all_colcounts(obj)

# S4 method for class 'LayoutColLeaf'
rm_all_colcounts(obj)
```

## Arguments

- obj:

  (`ANY`)  
  the object for the accessor to access or modify.

## Value

`obj` with all column counts reset to missing

## Examples

``` r
lyt <- basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX") %>%
  analyze("AGE")
tbl <- build_table(lyt, ex_adsl)

# before
col_counts(tbl)
#>  [1] 79 51  3  1 77 55  2  0 66 60  4  2
tbl <- rm_all_colcounts(tbl)
col_counts(tbl)
#>  [1] NA NA NA NA NA NA NA NA NA NA NA NA
```
