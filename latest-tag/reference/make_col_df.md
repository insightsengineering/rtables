# Column layout summary

Used for pagination. Generate a structural summary of the columns of an
`rtables` table and return it as a `data.frame`.

## Usage

``` r
make_col_df(
  tt,
  colwidths = NULL,
  visible_only = TRUE,
  na_str = "",
  ccount_format = colcount_format(tt) %||% "(N=xx)"
)
```

## Arguments

- tt:

  (`ANY`)  
  object representing the table-like object to be summarized.

- colwidths:

  (`numeric`)  
  internal detail, do not set manually.

- visible_only:

  (`flag`)  
  should only visible aspects of the table structure be reflected in
  this summary. Defaults to `TRUE`. May not be supported by all methods.

- na_str:

  (`character(1)`)  
  The string to display when a column count is NA. Users should not need
  to set this.

- ccount_format:

  (`FormatSpec`)  
  The format to be used by default for column counts if one is not
  specified for an individual column count.
