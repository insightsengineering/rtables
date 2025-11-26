# Retrieve or set content table from a `TableTree`

Returns the content table of `obj` if it is a `TableTree` object, or
`NULL` otherwise.

## Usage

``` r
content_table(obj)

content_table(obj) <- value
```

## Arguments

- obj:

  (`TableTree`)  
  the table object.

- value:

  (`ElementaryTable`)  
  the new content table for `obj`.

## Value

the `ElementaryTable` containing the (top level) *content rows* of `obj`
(or `NULL` if `obj` is not a formal table object).
