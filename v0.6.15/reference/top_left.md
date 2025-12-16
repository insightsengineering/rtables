# Top left material

A `TableTree` object can have *top left material* which is a sequence of
strings which are printed in the area of the table between the column
header display and the label of the first row. These functions access
and modify that material.

## Usage

``` r
top_left(obj)

# S4 method for class 'VTableTree'
top_left(obj)

# S4 method for class 'InstantiatedColumnInfo'
top_left(obj)

# S4 method for class 'PreDataTableLayouts'
top_left(obj)

top_left(obj) <- value

# S4 method for class 'VTableTree'
top_left(obj) <- value

# S4 method for class 'InstantiatedColumnInfo'
top_left(obj) <- value

# S4 method for class 'PreDataTableLayouts'
top_left(obj) <- value
```

## Arguments

- obj:

  (`ANY`)  
  the object for the accessor to access or modify.

- value:

  (`ANY`)  
  the new value.

## Value

A character vector representing the top-left material of `obj` (or `obj`
after modification, in the case of the setter).
