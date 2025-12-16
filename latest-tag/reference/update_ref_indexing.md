# Update footnote indices on a built table

Re-indexes footnotes within a built table.

## Usage

``` r
update_ref_indexing(tt)
```

## Arguments

- tt:

  (`TableTree` or related class)  
  a `TableTree` object representing a populated table.

## Details

After adding or removing referential footnotes manually, or after
subsetting a table, the reference indexes (i.e. the number associated
with specific footnotes) may be incorrect. This function recalculates
these based on the full table.

## Note

In the future this should not generally need to be called manually.
