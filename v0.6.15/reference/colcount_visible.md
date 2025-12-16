# Value and Visibility of specific column counts by path

Value and Visibility of specific column counts by path

## Usage

``` r
colcount_visible(obj, path)

# S4 method for class 'VTableTree'
colcount_visible(obj, path)

# S4 method for class 'InstantiatedColumnInfo'
colcount_visible(obj, path)

# S4 method for class 'LayoutColTree'
colcount_visible(obj, path)

colcount_visible(obj, path) <- value

# S4 method for class 'VTableTree'
colcount_visible(obj, path) <- value

# S4 method for class 'InstantiatedColumnInfo'
colcount_visible(obj, path) <- value

# S4 method for class 'LayoutColTree'
colcount_visible(obj, path) <- value
```

## Arguments

- obj:

  (`ANY`)  
  the object for the accessor to access or modify.

- path:

  (`character`)  
  a vector path for a position within the structure of a `TableTree`.
  Each element represents a subsequent choice amongst the children of
  the previous choice.

- value:

  (`ANY`)  
  the new value.

## Value

for `colcount_visible` a logical scalar indicating whether the specified
position in the column hierarchy is set to display its column count; for
`colcount_visible<-`, `obj` updated with the specified count displaying
behavior set.

## Note

Users generally should not call `colcount_visible` directly, as setting
sibling facets to have differing column count visibility will result in
an error when printing or paginating the table.
