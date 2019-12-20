## rtables 0.1.4

* Replaced dots to underscore in class checking functions.

## rtables 0.1.3

* `col_by` in `rtabulate` now accepts matrices
  - `col_by_to_matrix`, `col_by_to_factor`, `by_factor_to_matrix`, 
  - `by_add_total`, `by_all`, `by_combine`, `by_quartile`, `by_compare_subset`,  `by_hierarchical`, `by_drop_empty_cols`
  
* New utility functions to deal with variable lables
  - `label`, `var_labels<-`, `var_labels`, `var_labels_remove`, `var_relabel`, `with_label`

* Other new functions:
  - `cbing_rtables`
  - `empty_rtables`, `is_empty_rtable`, `is_non_empty_rtable`, `is_rtable`
  - `header_indent`, `header_indent<-`, `header_row.names`, `header_row.names<-`
  - `insert_rrow`

## rtables 0.1.2

* `rbind.rtable` now supports binding rtables with rows, e.g. `rbind(tbl1, rrow(), tbl2)` or `rbind(tbl1, rrow("row name"), tbl2)`.
* `rbindl_rtables` supports `NULL` objects in the list (except for the first element)
* add `indent` function
* `header_add_N` deals gracefully with `NULL` objects

## rtables 0.1.1

* `rtablulate` family of functions do not support the `row_*_data_args` arguments anymore. Instead, the `col_wise_args` argument is introduced.
* add functions `order_rrows`, `sort_rrows`, `order_rtables`, and `sort_rtables` are introduced.
* prevent `rtables` from being unlisted with `unlist.rtables`


## rtables 0.1.0.6

* `Viewer` now also accepts objects of class `shiny.tag` (defined in package `htmltools`)
* `as.html` accepts `class.table`, `class.tr`, `class.th`, and `class.td` as an argument

## rtables 0.1.0.5

* added `sprintf_format` for formatting rcells (thanks to Doug Kelkhoff for the suggestion)
* added `"(N=xx)"` and `">999.9"` format labels
* `rtabulate` has now an argument `col_N` and the function `col_N()`

## rtables 0.1.0

Version `0.1.0` is a major re-design with lots of internal refactoring and the
following API changes:

* Redesign: `rtable` has now `header` argument instead of `col.names`. A
`header` can be created with `rheader` and is a collection of `rrow`s. If
`header` is set to `c("A", "B")` then `rtable` will create the `rheader` with a
single `rrow`  and by setting `row.name` to `NULL`.

* `header` and `header<-` function added

* renamed `get_rcell_formats` to `list_rcell_format_labels`

* If `rcell` format is `NULL` then the cell content will be converted to a string with `paste(as.character(x), collapse = ', ')`

* accessor `[i,]` works now to subset a table.

* `rbind` method for rtables

* `row.names<-.rtable` method

* `rtabulate` added for creating tables

* `indented_row.names` function added


## rtables 0.0.1

* Initial public release