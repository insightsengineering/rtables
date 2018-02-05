


## rtable 0.0.2

Version `0.0.2` is a major re-design with lots of internal refactoring and the
following API changes:

* Redesign: `rtable` has now `header` argument instead of `col.names`. A
`header` can be created with `rheader` and is a collection of `rrow`s. If
`header` is set to `c("A", "B")` then `rtable` will create the `rheader` with a
single `rrow`  and by setting `row.name` to `NULL`.

* renamed `get_rcell_formats` to `list_rcell_format_labels`

* If `rcell` format is `NULL` then the cell content will be converted to a string with `paste(as.character(x), collapse = ', ')`

* accessor `[i,]` works now to subset a table.

## rtable 0.0.1

* Initial public release