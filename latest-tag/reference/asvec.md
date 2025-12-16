# Convert to a vector

Convert an `rtables` framework object into a vector, if possible. This
is unlikely to be useful in realistic scenarios.

## Usage

``` r
# S4 method for class 'VTableTree'
as.vector(x, mode = "any")
```

## Arguments

- x:

  (`ANY`)  
  the object to be converted to a vector.

- mode:

  (`string`)  
  passed on to [`as.vector()`](https://rdrr.io/r/base/vector.html).

## Value

A vector of the chosen mode (or an error is raised if more than one row
was present).

## Note

This only works for a table with a single row or a row object.
