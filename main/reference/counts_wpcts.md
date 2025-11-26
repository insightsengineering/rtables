# Analysis function to count levels of a factor with percentage of the column total

Analysis function to count levels of a factor with percentage of the
column total

## Usage

``` r
counts_wpcts(x, .N_col)
```

## Arguments

- x:

  (`factor`)  
  a vector of data, provided by rtables pagination machinery.

- .N_col:

  (`integer(1)`)  
  total count for the column, provided by rtables pagination machinery.

## Value

A `RowsVerticalSection` object with counts (and percents) for each level
of the factor.

## Examples

``` r
counts_wpcts(DM$SEX, 400)
#> RowsVerticalSection (in_rows) object print method:
#> ----------------------------
#>           row_name formatted_cell indent_mod        row_label
#> 1                F    187 (46.8%)          0                F
#> 2                M    169 (42.2%)          0                M
#> 3                U       0 (0.0%)          0                U
#> 4 UNDIFFERENTIATED       0 (0.0%)          0 UNDIFFERENTIATED
```
