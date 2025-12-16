# Collect leaves of a `TableTree`

Collect leaves of a `TableTree`

## Usage

``` r
collect_leaves(tt, incl.cont = TRUE, add.labrows = FALSE)
```

## Arguments

- tt:

  (`TableTree` or related class)  
  a `TableTree` object representing a populated table.

- incl.cont:

  (`flag`)  
  whether to include rows from content tables within the tree. Defaults
  to `TRUE`.

- add.labrows:

  (`flag`)  
  whether to include label rows. Defaults to `FALSE`.

## Value

A list of `TableRow` objects for all rows in the table.
