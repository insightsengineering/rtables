# Subset a table or row to particular columns

Subset a table or row to particular columns

## Usage

``` r
subset_cols(
  tt,
  j,
  newcinfo = NULL,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  ...
)

# S4 method for class 'TableTree,numeric'
subset_cols(
  tt,
  j,
  newcinfo = NULL,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  ...
)

# S4 method for class 'ElementaryTable,numeric'
subset_cols(
  tt,
  j,
  newcinfo = NULL,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  ...
)

# S4 method for class 'ANY,character'
subset_cols(
  tt,
  j,
  newcinfo = NULL,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  ...
)

# S4 method for class 'TableRow,numeric'
subset_cols(
  tt,
  j,
  newcinfo = NULL,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  ...
)

# S4 method for class 'LabelRow,numeric'
subset_cols(
  tt,
  j,
  newcinfo = NULL,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  ...
)

# S4 method for class 'InstantiatedColumnInfo,numeric'
subset_cols(
  tt,
  j,
  newcinfo = NULL,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  ...
)

# S4 method for class 'LayoutColTree,numeric'
subset_cols(
  tt,
  j,
  newcinfo = NULL,
  keep_topleft = TRUE,
  keep_titles = TRUE,
  keep_footers = keep_titles,
  ...
)
```

## Arguments

- tt:

  (`TableTree` or related class)  
  a `TableTree` object representing a populated table.

- j:

  (`integer`, `logical` or `character`)  
  The column(s) to subset `tt` down to. Character vectors are
  interpreted as a *column path*, not as names. Path can include `"*"`
  wildcards.

- newcinfo:

  (`NULL` or `InstantiatedColumnInfo`)  
  The new column info, if precomputed. Generally should not be manually
  set by users.

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

- ...:

  Ignored.

## Examples

``` r
lyt <- basic_table(
  title = "Title",
  subtitles = c("Sub", "titles"),
  prov_footer = "prov footer",
  main_footer = "main footer"
) %>%
  split_cols_by("ARM") %>%
  split_cols_by("SEX") %>%
  analyze(c("AGE"))

tbl <- build_table(lyt, DM)

subset_cols(tbl, c(1, 3))
#> Title
#> Sub
#> titles
#> 
#> ———————————————————
#>         A: Drug X  
#>          F       U 
#> ———————————————————
#> Mean   33.71    NA 
#> ———————————————————
#> 
#> main footer
#> 
#> prov footer
subset_cols(tbl, c("ARM", "*", "SEX", "F"))
#> Title
#> Sub
#> titles
#> 
#> ——————————————————————————————————————————————
#>        A: Drug X   B: Placebo   C: Combination
#>            F           F              F       
#> ——————————————————————————————————————————————
#> Mean     33.71       33.84          34.89     
#> ——————————————————————————————————————————————
#> 
#> main footer
#> 
#> prov footer
```
