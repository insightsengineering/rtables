# Check if an object is a valid `rtable`

Check if an object is a valid `rtable`

## Usage

``` r
is_rtable(x)
```

## Arguments

- x:

  (`ANY`)  
  an object.

## Value

`TRUE` if `x` is a formal `TableTree` object, `FALSE` otherwise.

## Examples

``` r
is_rtable(build_table(basic_table(), iris))
#> [1] TRUE
```
