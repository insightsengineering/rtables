# Head and tail methods

Head and tail methods

## Usage

``` r
head(x, ...)

# S4 method for class 'VTableTree'
head(
  x,
  n = 6,
  ...,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  reindex_refs = FALSE
)

tail(x, ...)

# S4 method for class 'VTableTree'
tail(
  x,
  n = 6,
  ...,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  reindex_refs = FALSE
)
```

## Arguments

- x:

  an object

- ...:

  arguments to be passed to or from other methods.

- n:

  an integer vector of length up to `dim(x)` (or 1, for non-dimensioned
  objects). A `logical` is silently coerced to integer. Values specify
  the indices to be selected in the corresponding dimension (or along
  the length) of the object. A positive value of `n[i]` includes the
  first/last `n[i]` indices in that dimension, while a negative value
  excludes the last/first `abs(n[i])`, including all remaining indices.
  `NA` or non-specified values (when `length(n) < length(dim(x))`)
  select all indices in that dimension. Must contain at least one
  non-missing value.

- keep_topleft:

  (`flag`)  
  if `TRUE` (the default), top_left material for the table will be
  carried over to the subset.

- keep_titles:

  (`flag`)  
  if `TRUE` (the default), all title material for the table will be
  carried over to the subset.

- keep_footers:

  (`flag`)  
  if `TRUE`, all footer material for the table will be carried over to
  the subset. It defaults to `keep_titles`.

- reindex_refs:

  (`flag`)  
  defaults to `FALSE`. If `TRUE`, referential footnotes will be
  reindexed for the subset.
