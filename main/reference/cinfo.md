# Instantiated column info

Instantiated column info

## Usage

``` r
InstantiatedColumnInfo(
  treelyt = LayoutColTree(colcount = total_cnt),
  csubs = list(expression(TRUE)),
  extras = list(list()),
  cnts = NA_integer_,
  total_cnt = NA_integer_,
  dispcounts = FALSE,
  countformat = "(N=xx)",
  count_na_str = "",
  topleft = character()
)
```

## Arguments

- treelyt:

  (`LayoutColTree`)  
  a `LayoutColTree` object.

- csubs:

  (`list`)  
  a list of subsetting expressions.

- extras:

  (`list`)  
  extra arguments associated with the columns.

- cnts:

  (`integer`)  
  counts.

- total_cnt:

  (`integer(1)`)  
  total observations represented across all columns.

- dispcounts:

  (`flag`)  
  whether the counts should be displayed as header info when the
  associated table is printed.

- countformat:

  (`string`)  
  format for the counts if they are displayed.

- count_na_str:

  (`character`)  
  string to use in place of missing values when formatting counts.
  Defaults to `""`.

- topleft:

  (`character`)  
  override values for the "top left" material to be displayed during
  printing.

## Value

An `InstantiateadColumnInfo` object.
