# Names of a `TableTree`

Names of a `TableTree`

## Usage

``` r
# S4 method for class 'VTableNodeInfo'
names(x)

# S4 method for class 'InstantiatedColumnInfo'
names(x)

# S4 method for class 'LayoutColTree'
names(x)

# S4 method for class 'VTableTree'
row.names(x)
```

## Arguments

- x:

  (`TableTree`)  
  the object.

## Value

The column names of `x`, as defined in the details above.

## Details

For `TableTree`s with more than one level of splitting in columns, the
names are defined to be the top-level split values repped out across the
columns that they span.
