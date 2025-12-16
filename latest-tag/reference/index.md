# Package index

## Argument Conventions

The following dummy functions are unexported and used to document
argument conventions in the framework.

- [`lyt_args()`](https://insightsengineering.github.io/rtables/reference/lyt_args.md)
  : Layouting function argument conventions
- [`constr_args()`](https://insightsengineering.github.io/rtables/reference/constr_args.md)
  : Constructor argument conventions
- [`compat_args()`](https://insightsengineering.github.io/rtables/reference/compat_args.md)
  : Compatibility argument conventions
- [`gen_args()`](https://insightsengineering.github.io/rtables/reference/gen_args.md)
  : General argument conventions
- [`sf_args()`](https://insightsengineering.github.io/rtables/reference/sf_args.md)
  : Split function argument conventions

## Layout and Tabulation

Functions for declaring layout and tabulation

- [`qtable_layout()`](https://insightsengineering.github.io/rtables/reference/qtable_layout.md)
  [`qtable()`](https://insightsengineering.github.io/rtables/reference/qtable_layout.md)
  : Generalized frequency table
- [`build_table()`](https://insightsengineering.github.io/rtables/reference/build_table.md)
  : Create a table from a layout and data
- [`basic_table()`](https://insightsengineering.github.io/rtables/reference/basic_table.md)
  : Layout with 1 column and zero rows
- [`analyze()`](https://insightsengineering.github.io/rtables/reference/analyze.md)
  : Generate rows analyzing variables across columns
- [`append_topleft()`](https://insightsengineering.github.io/rtables/reference/append_topleft.md)
  : Append a description to the 'top-left' materials for the layout
- [`split_cols_by()`](https://insightsengineering.github.io/rtables/reference/split_cols_by.md)
  : Declaring a column-split based on levels of a variable
- [`add_colcounts()`](https://insightsengineering.github.io/rtables/reference/add_colcounts.md)
  : Add the column population counts to the header
- [`split_rows_by()`](https://insightsengineering.github.io/rtables/reference/split_rows_by.md)
  : Add rows according to levels of a variable
- [`summarize_row_groups()`](https://insightsengineering.github.io/rtables/reference/summarize_row_groups.md)
  : Add a content row of summary counts
- [`split_cols_by_multivar()`](https://insightsengineering.github.io/rtables/reference/split_cols_by_multivar.md)
  : Associate multiple variables with columns
- [`split_rows_by_multivar()`](https://insightsengineering.github.io/rtables/reference/split_rows_by_multivar.md)
  : Associate multiple variables with rows
- [`analyze_colvars()`](https://insightsengineering.github.io/rtables/reference/analyze_colvars.md)
  : Generate rows analyzing different variables across columns
- [`split_cols_by_cuts()`](https://insightsengineering.github.io/rtables/reference/varcuts.md)
  [`split_rows_by_cuts()`](https://insightsengineering.github.io/rtables/reference/varcuts.md)
  [`split_cols_by_cutfun()`](https://insightsengineering.github.io/rtables/reference/varcuts.md)
  [`split_cols_by_quartiles()`](https://insightsengineering.github.io/rtables/reference/varcuts.md)
  [`split_rows_by_quartiles()`](https://insightsengineering.github.io/rtables/reference/varcuts.md)
  [`split_rows_by_cutfun()`](https://insightsengineering.github.io/rtables/reference/varcuts.md)
  : Split on static or dynamic cuts of the data
- [`add_overall_col()`](https://insightsengineering.github.io/rtables/reference/add_overall_col.md)
  : Add overall column
- [`add_existing_table()`](https://insightsengineering.github.io/rtables/reference/add_existing_table.md)
  : Add an already calculated table to the layout
- [`table_inset()`](https://insightsengineering.github.io/formatters/latest-tag/reference/table_inset.html)
  [`` `table_inset<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/table_inset.html)
  : Access or (recursively) set table inset (from formatters)

## Tabulation Utility Functions

Functions that are useful to be used with the `analyze*` functions.

- [`in_rows()`](https://insightsengineering.github.io/rtables/reference/in_rows.md)
  : Create multiple rows in analysis or summary functions

- [`additional_fun_params`](https://insightsengineering.github.io/rtables/reference/additional_fun_params.md)
  :

  Additional parameters within analysis and content functions
  (`afun`/`cfun`)

- [`simple_analysis()`](https://insightsengineering.github.io/rtables/reference/rtinner.md)
  : Default tabulation

- [`make_afun()`](https://insightsengineering.github.io/rtables/reference/make_afun.md)
  : Create a custom analysis function wrapping an existing function

- [`list_wrap_x()`](https://insightsengineering.github.io/rtables/reference/list_wrap.md)
  [`list_wrap_df()`](https://insightsengineering.github.io/rtables/reference/list_wrap.md)
  : Returns a function that coerces the return values of a function to a
  list

- [`spl_context`](https://insightsengineering.github.io/rtables/reference/spl_context.md)
  : .spl_context within analysis and split functions

- [`spl_context_to_disp_path()`](https://insightsengineering.github.io/rtables/reference/spl_context_to_disp_path.md)
  : Translate spl_context to a path to display in error messages

- [`counts_wpcts()`](https://insightsengineering.github.io/rtables/reference/counts_wpcts.md)
  : Analysis function to count levels of a factor with percentage of the
  column total

## Split Functions

- [`add_overall_level()`](https://insightsengineering.github.io/rtables/reference/add_overall_level.md)
  [`select_all_levels`](https://insightsengineering.github.io/rtables/reference/add_overall_level.md)
  [`add_combo_levels()`](https://insightsengineering.github.io/rtables/reference/add_overall_level.md)
  : Add overall or combination levels to split groups
- [`keep_split_levels()`](https://insightsengineering.github.io/rtables/reference/split_funcs.md)
  [`remove_split_levels()`](https://insightsengineering.github.io/rtables/reference/split_funcs.md)
  [`drop_split_levels()`](https://insightsengineering.github.io/rtables/reference/split_funcs.md)
  [`drop_and_remove_levels()`](https://insightsengineering.github.io/rtables/reference/split_funcs.md)
  [`reorder_split_levels()`](https://insightsengineering.github.io/rtables/reference/split_funcs.md)
  [`trim_levels_in_group()`](https://insightsengineering.github.io/rtables/reference/split_funcs.md)
  : Split functions
- [`trim_levels_to_map()`](https://insightsengineering.github.io/rtables/reference/trim_levels_to_map.md)
  : Trim levels to map
- [`custom_split_funs`](https://insightsengineering.github.io/rtables/reference/custom_split_funs.md)
  : Custom split functions
- [`do_base_split()`](https://insightsengineering.github.io/rtables/reference/do_base_split.md)
  : Apply basic split (for use in custom split functions)

## Custom Split Functions

- [`make_split_fun()`](https://insightsengineering.github.io/rtables/reference/make_split_fun.md)
  : Create a custom splitting function

- [`drop_facet_levels()`](https://insightsengineering.github.io/rtables/reference/drop_facet_levels.md)
  :

  Pre-processing function for use in `make_split_fun`

- [`trim_levels_in_facets()`](https://insightsengineering.github.io/rtables/reference/trim_levels_in_facets.md)
  : Trim levels of another variable from each facet (post-processing
  split step)

- [`add_combo_facet()`](https://insightsengineering.github.io/rtables/reference/add_combo_facet.md)
  [`add_overall_facet()`](https://insightsengineering.github.io/rtables/reference/add_combo_facet.md)
  : Add a combination facet in post-processing

- [`make_split_result()`](https://insightsengineering.github.io/rtables/reference/make_split_result.md)
  [`add_to_split_result()`](https://insightsengineering.github.io/rtables/reference/make_split_result.md)
  : Construct split result object

- [`spl_variable()`](https://insightsengineering.github.io/rtables/reference/spl_variable.md)
  : Variable associated with a split

## Cell Formatting related Functions

cell formatting.

- [`format_rcell()`](https://insightsengineering.github.io/rtables/reference/format_rcell.md)
  :

  Format `rcell` objects

- [`value_formats()`](https://insightsengineering.github.io/rtables/reference/value_formats.md)
  : Value formats

## Table Structure Information

- [`nrow(`*`<VTableTree>`*`)`](https://insightsengineering.github.io/rtables/reference/dimensions.md)
  [`ncol(`*`<VTableNodeInfo>`*`)`](https://insightsengineering.github.io/rtables/reference/dimensions.md)
  [`dim(`*`<VTableNodeInfo>`*`)`](https://insightsengineering.github.io/rtables/reference/dimensions.md)
  : Table dimensions
- [`row_paths()`](https://insightsengineering.github.io/rtables/reference/make_col_row_df.md)
  [`col_paths()`](https://insightsengineering.github.io/rtables/reference/make_col_row_df.md)
  : Get a list of table row/column paths
- [`row_paths_summary()`](https://insightsengineering.github.io/rtables/reference/row_paths_summary.md)
  [`col_paths_summary()`](https://insightsengineering.github.io/rtables/reference/row_paths_summary.md)
  : Print row/column paths summary
- [`table_structure()`](https://insightsengineering.github.io/rtables/reference/table_structure.md)
  : Summarize table
- [`table_shell()`](https://insightsengineering.github.io/rtables/reference/table_shell.md)
  [`table_shell_str()`](https://insightsengineering.github.io/rtables/reference/table_shell.md)
  : Table shells
- [`make_row_df()`](https://insightsengineering.github.io/formatters/latest-tag/reference/make_row_df.html)
  : Make row layout summary data frames for use during pagination (from
  formatters)
- [`make_col_df()`](https://insightsengineering.github.io/rtables/reference/make_col_df.md)
  : Column layout summary

## Layout Structure Information

- [`vars_in_layout()`](https://insightsengineering.github.io/rtables/reference/vil.md)
  : List variables required by a pre-data table layout
- [`coltree_structure()`](https://insightsengineering.github.io/rtables/reference/coltree_structure.md)
  : Display column tree structure

## Access and Modify

- [`cell_values()`](https://insightsengineering.github.io/rtables/reference/cell_values.md)
  [`value_at()`](https://insightsengineering.github.io/rtables/reference/cell_values.md)
  : Retrieve cell values by row and column path

- [`top_left()`](https://insightsengineering.github.io/rtables/reference/top_left.md)
  [`` `top_left<-`() ``](https://insightsengineering.github.io/rtables/reference/top_left.md)
  : Top left material

- [`rbindl_rtables()`](https://insightsengineering.github.io/rtables/reference/rbind.md)
  [`rbind(`*`<VTableNodeInfo>`*`)`](https://insightsengineering.github.io/rtables/reference/rbind.md)
  [`rbind2(`*`<VTableNodeInfo>`*`,`*`<ANY>`*`)`](https://insightsengineering.github.io/rtables/reference/rbind.md)
  :

  Row-bind `TableTree` and related objects

- [`cbind_rtables()`](https://insightsengineering.github.io/rtables/reference/cbind_rtables.md)
  :

  Column-bind two `TableTree` objects

- [`as.vector(`*`<VTableTree>`*`)`](https://insightsengineering.github.io/rtables/reference/asvec.md)
  : Convert to a vector

- [`` `[<-`( ``*`<VTableTree>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<list>`*`)`](https://insightsengineering.github.io/rtables/reference/brackets.md)
  [`` `[`( ``*`<VTableTree>`*`,`*`<logical>`*`,`*`<logical>`*`)`](https://insightsengineering.github.io/rtables/reference/brackets.md)
  :

  Retrieve and assign elements of a `TableTree`

- [`subset_cols()`](https://insightsengineering.github.io/rtables/reference/subset_cols.md)
  : Subset a table or row to particular columns

- [`clear_indent_mods()`](https://insightsengineering.github.io/rtables/reference/clear_imods.md)
  : Clear all indent modifiers from a table

- [`head()`](https://insightsengineering.github.io/rtables/reference/head_tail.md)
  [`tail()`](https://insightsengineering.github.io/rtables/reference/head_tail.md)
  : Head and tail methods

- [`section_div()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`` `section_div<-`() ``](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`header_section_div()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`` `header_section_div<-`() ``](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`top_level_section_div()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`` `top_level_section_div<-`() ``](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`section_div_info()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`section_div_at_path()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`` `section_div_at_path<-`() ``](https://insightsengineering.github.io/rtables/reference/section_div.md)
  : Section dividers accessor and setter

- [`colcount_visible()`](https://insightsengineering.github.io/rtables/reference/colcount_visible.md)
  [`` `colcount_visible<-`() ``](https://insightsengineering.github.io/rtables/reference/colcount_visible.md)
  : Value and Visibility of specific column counts by path

- [`facet_colcount()`](https://insightsengineering.github.io/rtables/reference/facet_colcount.md)
  [`` `facet_colcount<-`() ``](https://insightsengineering.github.io/rtables/reference/facet_colcount.md)
  : Get or set column count for a facet in column space

- [`` `facet_colcounts_visible<-`() ``](https://insightsengineering.github.io/rtables/reference/facet_colcounts_visible-set.md)
  : Set visibility of column counts for a group of sibling facets

- [`rm_all_colcounts()`](https://insightsengineering.github.io/rtables/reference/rm_all_colcounts.md)
  : Set all column counts at all levels of nesting to NA

## Validating and Fixing Table Structure

- [`validate_table_struct()`](https://insightsengineering.github.io/rtables/reference/validate_table_struct.md)
  [`assert_valid_table()`](https://insightsengineering.github.io/rtables/reference/validate_table_struct.md)
  **\[experimental\]** : Validate and assert valid table structure
- [`sanitize_table_struct()`](https://insightsengineering.github.io/rtables/reference/sanitize_table_struct.md)
  **\[experimental\]** : Sanitize degenerate table structures
- [`find_degen_struct()`](https://insightsengineering.github.io/rtables/reference/find_degen_struct.md)
  **\[experimental\]** : Find degenerate (sub)structures within a table

## Sorting and Pruning

- [`prune_table()`](https://insightsengineering.github.io/rtables/reference/prune_table.md)
  :

  Recursively prune a `TableTree`

- [`all_zero_or_na()`](https://insightsengineering.github.io/rtables/reference/trim_prune_funs.md)
  [`all_zero()`](https://insightsengineering.github.io/rtables/reference/trim_prune_funs.md)
  [`content_all_zeros_nas()`](https://insightsengineering.github.io/rtables/reference/trim_prune_funs.md)
  [`prune_empty_level()`](https://insightsengineering.github.io/rtables/reference/trim_prune_funs.md)
  [`prune_zeros_only()`](https://insightsengineering.github.io/rtables/reference/trim_prune_funs.md)
  [`low_obs_pruner()`](https://insightsengineering.github.io/rtables/reference/trim_prune_funs.md)
  : Trimming and pruning criteria

- [`trim_rows()`](https://insightsengineering.github.io/rtables/reference/trim_rows.md)
  : Trim rows from a populated table without regard for table structure

- [`sort_at_path()`](https://insightsengineering.github.io/rtables/reference/sort_at_path.md)
  : Sorting a table at a specific path

- [`cont_n_allcols()`](https://insightsengineering.github.io/rtables/reference/score_funs.md)
  [`cont_n_onecol()`](https://insightsengineering.github.io/rtables/reference/score_funs.md)
  :

  Score functions for sorting `TableTrees`

## Compatibility Layer

These functions provide some backwards compatibility to the previous
(pre 2021) `rtables` framework.

- [`rtable()`](https://insightsengineering.github.io/rtables/reference/rtable.md)
  [`rtablel()`](https://insightsengineering.github.io/rtables/reference/rtable.md)
  : Create a table

- [`rrow()`](https://insightsengineering.github.io/rtables/reference/rrow.md)
  :

  Create an `rtable` row

- [`rcell()`](https://insightsengineering.github.io/rtables/reference/rcell.md)
  [`non_ref_rcell()`](https://insightsengineering.github.io/rtables/reference/rcell.md)
  : Cell value constructors

- [`rheader()`](https://insightsengineering.github.io/rtables/reference/rheader.md)
  : Create a header

- [`rrowl()`](https://insightsengineering.github.io/rtables/reference/rrowl.md)
  :

  Create an `rtable` row from a vector or list of values

- [`indent()`](https://insightsengineering.github.io/rtables/reference/indent.md)
  :

  Change indentation of all `rrows` in an `rtable`

- [`df_to_tt()`](https://insightsengineering.github.io/rtables/reference/df_to_tt.md)
  :

  Create an `ElementaryTable` from a `data.frame`

## Output Functions

These functions create ascii or html representations of the table

- [`as_html()`](https://insightsengineering.github.io/rtables/reference/as_html.md)
  :

  Convert an `rtable` object to a `shiny.tag` HTML object

- [`export_as_tsv()`](https://insightsengineering.github.io/rtables/reference/tsv_io.md)
  [`import_from_tsv()`](https://insightsengineering.github.io/rtables/reference/tsv_io.md)
  : Create enriched flat value table with paths

- [`toString(`*`<VTableTree>`*`)`](https://insightsengineering.github.io/rtables/reference/tostring.md)
  :

  Convert an `rtable` object to a string

- [`get_formatted_cells()`](https://insightsengineering.github.io/rtables/reference/gfc.md)
  [`get_cell_aligns()`](https://insightsengineering.github.io/rtables/reference/gfc.md)
  : Get formatted cells

- [`as_result_df()`](https://insightsengineering.github.io/rtables/reference/data.frame_export.md)
  [`path_enriched_df()`](https://insightsengineering.github.io/rtables/reference/data.frame_export.md)
  : Generate a result data frame

## Utility Functions

utility functions

- [`Viewer()`](https://insightsengineering.github.io/rtables/reference/Viewer.md)
  :

  Display an `rtable` object in the Viewer pane in RStudio or in a
  browser

- [`compare_rtables()`](https://insightsengineering.github.io/rtables/reference/compare_rtables.md)
  : Compare two rtables

- [`indent_string()`](https://insightsengineering.github.io/rtables/reference/indent_string.md)
  : Indent strings

- [`is_rtable()`](https://insightsengineering.github.io/rtables/reference/is_rtable.md)
  :

  Check if an object is a valid `rtable`

## Pagination

Pagination related functionality

- [`pag_tt_indices()`](https://insightsengineering.github.io/rtables/reference/paginate.md)
  [`paginate_table()`](https://insightsengineering.github.io/rtables/reference/paginate.md)
  :

  Pagination of a `TableTree`

- [`make_row_df()`](https://insightsengineering.github.io/formatters/latest-tag/reference/make_row_df.html)
  : Make row layout summary data frames for use during pagination (from
  formatters)

- [`make_col_df()`](https://insightsengineering.github.io/rtables/reference/make_col_df.md)
  : Column layout summary

## TableTree Framework Accessor Functions

- [`content_table()`](https://insightsengineering.github.io/rtables/reference/content_table.md)
  [`` `content_table<-`() ``](https://insightsengineering.github.io/rtables/reference/content_table.md)
  :

  Retrieve or set content table from a `TableTree`

- [`tree_children()`](https://insightsengineering.github.io/rtables/reference/tree_children.md)
  [`` `tree_children<-`() ``](https://insightsengineering.github.io/rtables/reference/tree_children.md)
  : Retrieve or set the direct children of a tree-style object

- [`collect_leaves()`](https://insightsengineering.github.io/rtables/reference/collect_leaves.md)
  :

  Collect leaves of a `TableTree`

- [`obj_avar()`](https://insightsengineering.github.io/rtables/reference/row_accessors.md)
  [`row_cells()`](https://insightsengineering.github.io/rtables/reference/row_accessors.md)
  [`` `row_cells<-`() ``](https://insightsengineering.github.io/rtables/reference/row_accessors.md)
  [`row_values()`](https://insightsengineering.github.io/rtables/reference/row_accessors.md)
  [`` `row_values<-`() ``](https://insightsengineering.github.io/rtables/reference/row_accessors.md)
  : Row attribute accessors

- [`no_colinfo()`](https://insightsengineering.github.io/rtables/reference/no_info.md)
  :

  Exported for use in `tern`

- [`clayout()`](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`` `clayout<-`() ``](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`col_info()`](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`` `col_info<-`() ``](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`coltree()`](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`col_exprs()`](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`col_counts()`](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`` `col_counts<-`() ``](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`col_total()`](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  [`` `col_total<-`() ``](https://insightsengineering.github.io/rtables/reference/col_accessors.md)
  : Column information/structure accessors

- [`horizontal_sep()`](https://insightsengineering.github.io/rtables/reference/horizontal_sep.md)
  [`` `horizontal_sep<-`() ``](https://insightsengineering.github.io/rtables/reference/horizontal_sep.md)
  : Access or recursively set header-body separator for tables

- [`table_inset()`](https://insightsengineering.github.io/formatters/latest-tag/reference/table_inset.html)
  [`` `table_inset<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/table_inset.html)
  : Access or (recursively) set table inset (from formatters)

- [`main_title()`](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`` `main_title<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`subtitles()`](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`` `subtitles<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`page_titles()`](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`` `page_titles<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`main_footer()`](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`` `main_footer<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`prov_footer()`](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`` `prov_footer<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`all_footers()`](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  [`all_titles()`](https://insightsengineering.github.io/formatters/latest-tag/reference/title_footer.html)
  : General title and footer accessors (from formatters)

- [`top_left()`](https://insightsengineering.github.io/rtables/reference/top_left.md)
  [`` `top_left<-`() ``](https://insightsengineering.github.io/rtables/reference/top_left.md)
  : Top left material

- [`obj_name()`](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`` `obj_name<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`obj_label()`](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`` `obj_label<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`obj_format()`](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`` `obj_format<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`obj_na_str()`](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`` `obj_na_str<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`obj_align()`](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  [`` `obj_align<-`() ``](https://insightsengineering.github.io/formatters/latest-tag/reference/lab_name.html)
  : Label, name, and format accessor generics (from formatters)

## TableTree Framework Constructors and S4 Classes

S4 classes and constructors

- [`manual_cols()`](https://insightsengineering.github.io/rtables/reference/manual_cols.md)
  : Manual column declaration

- [`CellValue()`](https://insightsengineering.github.io/rtables/reference/CellValue.md)
  : Constructor for Cell Value

- [`EmptyColInfo`](https://insightsengineering.github.io/rtables/reference/EmptyColInfo.md)
  [`EmptyElTable`](https://insightsengineering.github.io/rtables/reference/EmptyColInfo.md)
  [`EmptyRootSplit`](https://insightsengineering.github.io/rtables/reference/EmptyColInfo.md)
  [`EmptyAllSplit`](https://insightsengineering.github.io/rtables/reference/EmptyColInfo.md)
  : Empty table, column, split objects

- [`ManualSplit()`](https://insightsengineering.github.io/rtables/reference/ManualSplit.md)
  : Manually defined split

- [`MultiVarSplit()`](https://insightsengineering.github.io/rtables/reference/MultiVarSplit.md)
  : Split between two or more different variables

- [`VarLevelSplit()`](https://insightsengineering.github.io/rtables/reference/VarLevelSplit.md)
  [`VarLevWBaselineSplit()`](https://insightsengineering.github.io/rtables/reference/VarLevelSplit.md)
  : Split on levels within a variable

- [`AnalyzeVarSplit()`](https://insightsengineering.github.io/rtables/reference/avarspl.md)
  [`AnalyzeColVarSplit()`](https://insightsengineering.github.io/rtables/reference/avarspl.md)
  [`AnalyzeMultiVars()`](https://insightsengineering.github.io/rtables/reference/avarspl.md)
  : Define a subset tabulation/analysis

- [`make_static_cut_split()`](https://insightsengineering.github.io/rtables/reference/cutsplits.md)
  [`VarDynCutSplit()`](https://insightsengineering.github.io/rtables/reference/cutsplits.md)
  : Splits for cutting by values of a numeric variable

- [`InstantiatedColumnInfo()`](https://insightsengineering.github.io/rtables/reference/cinfo.md)
  : Instantiated column info

- [`LabelRow()`](https://insightsengineering.github.io/rtables/reference/rowclasses.md)
  [`.tablerow()`](https://insightsengineering.github.io/rtables/reference/rowclasses.md)
  [`DataRow()`](https://insightsengineering.github.io/rtables/reference/rowclasses.md)
  [`ContentRow()`](https://insightsengineering.github.io/rtables/reference/rowclasses.md)
  : Row classes and constructors

- [`ElementaryTable()`](https://insightsengineering.github.io/rtables/reference/tabclasses.md)
  [`TableTree()`](https://insightsengineering.github.io/rtables/reference/tabclasses.md)
  :

  `TableTree` classes

## Pathing Related Functions

Pathing and insertion related functions

- [`label_at_path()`](https://insightsengineering.github.io/rtables/reference/label_at_path.md)
  [`` `label_at_path<-`() ``](https://insightsengineering.github.io/rtables/reference/label_at_path.md)
  : Label at path
- [`tt_at_path()`](https://insightsengineering.github.io/rtables/reference/ttap.md)
  [`` `tt_at_path<-`() ``](https://insightsengineering.github.io/rtables/reference/ttap.md)
  : Access or set table elements at specified path
- [`insert_row_at_path()`](https://insightsengineering.github.io/rtables/reference/insert_row_at_path.md)
  : Insert row at path
- [`section_div()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`` `section_div<-`() ``](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`header_section_div()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`` `header_section_div<-`() ``](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`top_level_section_div()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`` `top_level_section_div<-`() ``](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`section_div_info()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`section_div_at_path()`](https://insightsengineering.github.io/rtables/reference/section_div.md)
  [`` `section_div_at_path<-`() ``](https://insightsengineering.github.io/rtables/reference/section_div.md)
  : Section dividers accessor and setter
- [`tt_row_path_exists()`](https://insightsengineering.github.io/rtables/reference/tt_row_path_exists.md)
  [`tt_normalize_row_path()`](https://insightsengineering.github.io/rtables/reference/tt_row_path_exists.md)
  : Pathing

## Referential Footnotes

Functions related to referential footnotes

- [`row_footnotes()`](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`` `row_footnotes<-`() ``](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`cell_footnotes()`](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`` `cell_footnotes<-`() ``](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`col_fnotes_here()`](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`` `col_fnotes_here<-`() ``](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`col_footnotes()`](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`` `col_footnotes<-`() ``](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`ref_index()`](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`` `ref_index<-`() ``](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`ref_symbol()`](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`` `ref_symbol<-`() ``](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`ref_msg()`](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  [`` `fnotes_at_path<-`() ``](https://insightsengineering.github.io/rtables/reference/ref_fnotes.md)
  : Referential footnote accessors
- [`update_ref_indexing()`](https://insightsengineering.github.io/rtables/reference/update_ref_indexing.md)
  : Update footnote indices on a built table

## Result Data Frame Functions

Functions for generating result data frames from rtables TableTree
objects

- [`as_result_df()`](https://insightsengineering.github.io/rtables/reference/data.frame_export.md)
  [`path_enriched_df()`](https://insightsengineering.github.io/rtables/reference/data.frame_export.md)
  : Generate a result data frame
