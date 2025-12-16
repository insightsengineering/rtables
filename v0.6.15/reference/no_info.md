# Exported for use in `tern`

Does the `table`/`row`/`InstantiatedColumnInfo` object contain no column
structure information?

## Usage

``` r
no_colinfo(obj)

# S4 method for class 'VTableNodeInfo'
no_colinfo(obj)

# S4 method for class 'InstantiatedColumnInfo'
no_colinfo(obj)
```

## Arguments

- obj:

  (`ANY`)  
  the object for the accessor to access or modify.

## Value

`TRUE` if the object has no/empty instantiated column information,
`FALSE` otherwise.
