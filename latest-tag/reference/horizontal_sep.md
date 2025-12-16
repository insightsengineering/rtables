# Access or recursively set header-body separator for tables

Access or recursively set header-body separator for tables

## Usage

``` r
horizontal_sep(obj)

# S4 method for class 'VTableTree'
horizontal_sep(obj)

horizontal_sep(obj) <- value

# S4 method for class 'VTableTree'
horizontal_sep(obj) <- value

# S4 method for class 'TableRow'
horizontal_sep(obj) <- value
```

## Arguments

- obj:

  (`ANY`)  
  the object for the accessor to access or modify.

- value:

  (`string`)  
  string to use as new header/body separator.

## Value

- `horizontal_sep` returns the string acting as the header separator.

- `horizontal_sep<-` returns `obj`, with the new header separator
  applied recursively to it and all its subtables.
