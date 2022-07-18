## rtables 0.5.1.6
 * `paginate_table` now accepts `cpp` and will perform vertical pagination when it is non-null
 * `vpaginate_table` is now deprecated
 * Increased versioned dependency on `formatters` to `>=0.3.2.4`

## rtables 0.5.1.5
 * Support for section dividers (`section_div` argument in `split_rows_by*` function)
 * Updated versioned dependency on `formatters` to `>=0.3.2.3`
 * Equivalent split functions with different enclosing environments (e.g., 2 identical calls to `add_combo_levels` #340) no longer block `rbind`ing
 * Fixed various documentation bugs where description section was being added to header.
 
## rtables 0.5.1.4
 * empty level check for splitting variables reinstated.

## rtables 0.5.1.3
 * Throw informative error messages when custom analysis, content or split functions fail (#329)

## rtables 0.5.1.2
 * empty level check for splitting variables temporarily removed. It is very likely to be reinstated in a future release.

## rtables 0.5.1.1
 * `col_counts` getter and setter now accept `path` argument.
 * empty levels of a splitting variable now result in an informative error message (character and factor cases).
 * fixed bug in handling of column extra arguments that was preventing cbinding tables from working correctly (#324)
 
## rtables 0.5.1
 * empty factor levels are now *not* dropped for column splits when ref_group is set (#323)
 * `linesep` argument to `toString` and related functions renamed to `hsep`
 * Increase versioned dependency on `formatters` to `>=0.3.0`
 * Default "line separator" between header and body now falls back to "-" non-UTF charset locales.
 * New `hsep` argument to `build_table` which sets the horizontal separator for the constructed table (and subtables thereof)
 * New `horizontal_sep` and `horizontal_sep<-` accessors for constructed tables, the latter of which is mandatorily recursive.
 * `split_rows_by(var, child_labels="hidden")` no longer removes the structural subtable corresponding to levels of `var` (#314)
 
## rtables 0.5.0
 * `formatable` dependency renamed to `formatters` for suitability of release to CRAN
 * Update versioned dependency of `formatters` (previously `formatable`) to `>=0.2.0` 
 
## rtables 0.4.1.0004
 * Fix bug when function format combined with NULL cfun caused error (#307)
 * Fix bug in `path_enriched_df` (which powers tsv export), related to (#308)
 
## rtables 0.4.1.0002
 * added `table_shell` to display shell of table with formats

## rtables 0.4.1.0001
 * added `linesep` argument to `toString` to specify a character to create the line separator. Previously we used the en dash as the line separator character, now we changed the default to the em dash reducing the gap between the dash line elements.

## rtables 0.4.0
 * Initializing layouts with `NULL` is now deprecated
 * `insert_rrow` is deprecated in favor of new `insert_row_at_path` and `label_at_path<-` functions
 * split and analysis/content functions can now depend on values of splits they are nested inside by accepting and using the new `.spl_context` optional argument
 * new `trim_levels_to_map` split function  for dictating exact combinations of values which should appear across splits
 * `value_formats` function now exported which returns/displays the effective formats of all cells in a table
 * `compare_rtables` is now *much* faster for tables with many cells
 * `compare_rtables` now accepts `structure` argument which adds comparison of structure (by way of row- and column-path positions)
 * new `tt_to_flextable` coercion function
 * new `export_as_pdf` exporter function
 * `value_at` and `cell_values` functions now have methods for `TableRow` objects making them usable in sorting/pruning functions
 
## rtables 0.3.8.9001
 * new `trim_levels_to_map` split function based on wwojtek's work in #203
 * support for column referential footnotes
 * support for adding footnotes to existing table via `fnotes_at_path<-` function
 * `trim_levels_in_group` now trims empty levels of outer (split) variable by default
 * `value_at` and `cell_values` now work for tablerow objects
 * Fixed `as_html` bug in multvar split columns case 
 * Fixed pagination off-by-one error


## rtables 0.3.8.9000

## rtables 0.3.8
 * Add experimental support for newlines in column names, row labels, and cell values (not supported in top-left annotations)
 * `as_html` refactored to support newlines while respecting table structure
 * self_extent column of df returned by `make_row_df` now reflects extent *in lines*, thus will return larger values if the row-label or any cell values contain newlines.
 * Fix bug where tables output using `as_html` (or viewed in `Viewer`) were missing the table class attribute (#194)
 * inserting a DataRow with incorrect number of columns is now an error (#199)
 * Referential footer machinery now works in colspan case.
 * Fix extraneous footnote attribute bug (#198)
 * Fix max -Inf warning when content rows appear at positions whose children have 0 visible rows (#200)
 * Resync NEWS.md file
 * Introduce titles and footnotes
 * Support automatic population of top-left
 * Introduce referential footnote support for cells and row labels
 * Added `vars_in_layout` to list (explicitly named only) variables used in a layout
 * Fix column label ordering bug when value label variable is a factor (#173)

## rtables 0.3.7

Synchronize release with GitHub commit sha.

## rtables 0.3.6

Documentation revisions as requested by CRAN. No change to package code.

## rtables 0.3.5

Documentation-text only changes to introduction vignette to pass CRAN's URL checks. All package, example, test, and vignette code fully identical to that in tagged github release 0.3.4

## rtables 0.3.4

Minor changes to the 0.3.3 version in order to submit rtables to CRAN.

## rtables 0.3.3

This version completely refactors the `rtables` package. We do provide a backwards compatibility layer with the
`rtable`, `rcell`, `rrow`, `rheader`, and `rtabulate` family of functions. However the table data structure and main
tabulation framework have changed. We provide extensive documentation in the manuals `help(package = "rtables")` and
vignettes `vignette(package = "rtables")` of the package.

The changes to `rtables` have been undertaken to better meet the requirements of creating and analyzing & reporting
tables in the context of clinical trials.


## rtables 0.3.2.17.9046

* `make_afun` now `force()`s all customization arguments immediately, which prevents problems when called within loop/lapply constructs.

## rtables 0.3.2.17.9045

* Tabulation machinery no longer removes NAs mandatorily in some cases, including mutlivar column splits
* `analyze_colvars`'s `inclNAs` argument now respected.

## rtables 0.3.2.17.9044

* Fix indent modifier propogation during tabulation
* Fix indent calculation in `make_pagdf`
* Add significant testing to ensure `make_pagdf` indent calculation remains correct

## rtables 0.3.2.17.9043

* Rework how reference columns are handled so analyses which use `.in_ref_col` and `.ref_group` work correctly when custom splitting is used (including the provided combination-levels mechanism)

## rtables 0.3.2.17.9042

* Fix naming/pathing for columns in multivar case (split itself now has default name "multivars")
* Fix labelling bug when same variable appears multiple times in MultiVarSplit with different associated levels


## rtables 0.3.2.17.9041

* Allow single variable to be used within `split_cols_by_multivar` 
* Various removal of defunct 

## rtables 0.3.2.17.9040

* Fix regression caused by 0.3.2.17.9039 where column split values were displayed by name rather than label.

## rtables 0.3.2.17.9039

* Fix bug in display of column information when column structure is not symetric, as with recursive cbinds.

## rtables 0.3.2.17.9036

* Fixed bug in row subsetting when table has only content rows.
* Basic compare_rtables function now works as in previous versions, no awareness of row or column structure.

## rtables 0.3.2.17.9036

* `summarize_row_groups` can now accept a list of functions for the `cfun` argument as `analyze_colvars` does.

## rtables 0.3.2.17.9035

* Fix bug unearthed by change in 0.3.2.17.9034 where cell formats not retained during column subsetting

## rtables 0.3.2.17.9034

* Fix internal `value_formats` accessor so it operates on CellValues rather than the raw contained values (thus always returning NULL)
* `rrow` constructor no longer interpretes cell formats a row format when they are the same across all cells. Fixes bug in  "correct way" code discussed in #112

## rtables 0.3.2.17.9033

* Interpret .formats in `in_rows` as *cell* formats rather than row formats.

## rtables 0.3.2.17.9031

* cbind_rtables can now take more than 2 tables.

## rtables 0.3.2.17.9029

* Fix issue underlying spurious length-missmatch warning in some cases when using `analyze_colvars`

## rtables 0.3.2.17.9028

* `analyze_colvars` now takes and adheres to `inclNAs` argument

## rtables 0.3.2.17.9027

* issues with no news: 

## rtables 0.1.7

* added format `xx.xx (xx.xx - xx.xx)` and `x.xxxx | (<0.0001)`

## rtables 0.1.6

* Minor changes.

## rtables 0.1.5

* Changed testing approach to fit internal pipelines.

## rtables 0.1.4

* Replaced dots to underscore in class checking functions.

## rtables 0.1.3

* `col_by` in `rtabulate` now accepts matrices:
  - `col_by_to_matrix`, `col_by_to_factor`, `by_factor_to_matrix`.
  - `by_add_total`, `by_all`, `by_combine`, `by_quartile`, `by_compare_subset`,  `by_hierarchical`, `by_drop_empty_cols`.
  
* New utility functions to deal with variable labels:
  - `label`, `var_labels<-`, `var_labels`, `var_labels_remove`, `var_relabel`, `with_label`.

* Other new functions:
  - `cbing_rtables`.
  - `empty_rtables`, `is_empty_rtable`, `is_non_empty_rtable`, `is_rtable`.
  - `header_indent`, `header_indent<-`, `header_row.names`, `header_row.names<-`.
  - `insert_rrow`.

## rtables 0.1.2

* `rbind.rtable` now supports binding rtables with rows, e.g. `rbind(tbl1, rrow(), tbl2)` or `rbind(tbl1, rrow("row name"), tbl2)`.
* `rbindl_rtables` supports `NULL` objects in the list (except for the first element).
* Add `indent` function.
* `header_add_N` deals gracefully with `NULL` objects.

## rtables 0.1.1

* `rtablulate` family of functions do not support the `row_*_data_args` arguments anymore. Instead, the `col_wise_args` argument is introduced.
* Functions `order_rrows`, `sort_rrows`, `order_rtables`, and `sort_rtables` are introduced.
* Prevent `rtables` from being unlisted with `unlist.rtables`.


## rtables 0.1.0.6

* `Viewer` now also accepts objects of class `shiny.tag` (defined in package `htmltools`).
* `as.html` accepts `class.table`, `class.tr`, `class.th`, and `class.td` as an argument.

## rtables 0.1.0.5

* Added `sprintf_format` for formatting rcells (thanks to Doug Kelkhoff for the suggestion).
* Added `"(N=xx)"` and `">999.9"` format labels.
* `rtabulate` has now an argument `col_N` and the function `col_N()`.

## rtables 0.1.0

* Version `0.1.0` is a major re-design with lots of internal refactoring and the following API changes:
  - Redesign: `rtable` has now `header` argument instead of `col.names`. A `header` can be created with `rheader` and is a collection of `rrow`s. If `header` is set to `c("A", "B")` then `rtable` will create the `rheader` with a single `rrow`  and by setting `row.name` to `NULL`.
  - `header` and `header<-` function added.
  - Renamed `get_rcell_formats` to `list_rcell_format_labels`.
  - If `rcell` format is `NULL` then the cell content will be converted to a string with `paste(as.character(x), collapse = ', ')`.
  - Accessor `[i,]` works now to subset a table.
  - `rbind` method for rtables.
  - `row.names<-.rtable` method.
  - `rtabulate` added for creating tables.
  - `indented_row.names` function added.

## rtables 0.0.1

* Initial public release
