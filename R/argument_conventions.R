# nocov start

#' General argument conventions
#'
#' @inheritParams formatters::format_value
#' @param df (`data.frame` or `tibble`)\cr dataset.
#' @param alt_counts_df (`data.frame` or `tibble`)\cr alternative full dataset the rtables framework will use
#'   *only* when calculating column counts.
#' @param spl (`Split`)\cr a `Split` object defining a partitioning or analysis/tabulation of the data.
#' @param pos (`numeric`)\cr which top-level set of nested splits should the new layout feature be added to. Defaults
#'   to the current split.
#' @param tt (`TableTree` or related class)\cr a `TableTree` object representing a populated table.
#' @param tr (`TableRow` or related class)\cr a `TableRow` object representing a single row within a populated table.
#' @param verbose (`logical(1)`)\cr whether additional information should be displayed to the user. Defaults to `FALSE`.
#' @param colwidths (`numeric`)\cr a vector of column widths for use in vertical pagination.
#' @param obj (`any`)\cr the object for the accessor to access or modify.
#' @param x (`any`)\cr an object.
#' @param ... additional parameters passed to methods or tabulation functions.
#' @param value (`any`)\cr the new value.
#' @param object (`any`)\cr the object to modify in place.
#' @param verbose (`logical(1)`)\cr whether extra debugging messages should be shown. Defaults to `FALSE`.
#' @param path (`character`)\cr a vector path for a position within the structure of a `TableTree`. Each element
#'   represents a subsequent choice amongst the children of the previous choice.
#' @param label (`character(1)`)\cr a label (not to be confused with the name) for the object/structure.
#' @param label_pos (`character(1)`)\cr location where the variable label should be displayed. Accepts `"hidden"`
#'   (default for non-analyze row splits), `"visible"`, `"topleft"`, and `"default"` (for analyze splits only). For
#'   `analyze` calls, `"default"` indicates that the variable should be visible if and only if multiple variables are
#'   analyzed at the same level of nesting.
#' @param cvar (`character(1)`)\cr the variable, if any, that the content function should accept. Defaults to `NA`.
#' @param topleft (`character`)\cr override values for the "top left" material to be displayed during printing.
#' @param page_prefix (`character(1)`)\cr prefix to be appended with the split value when forcing pagination between
#'   the children of a split/table.
#' @param hsep (`character(1)`)\cr set of character(s) to be repeated as the separator between the header and body of
#'   the table when rendered as text. Defaults to a connected horizontal line (unicode 2014) in locals that use a UTF
#'   charset, and to `-` elsewhere (with a once per session warning). See [formatters::set_default_hsep()] for further
#'   information.
#' @param indent_size (`numeric(1)`)\cr number of spaces to use per indent level. Defaults to 2.
#' @param section_div (`character(1)`)\cr string which should be repeated as a section divider after each group defined
#'   by this split instruction, or `NA_character_` (the default) for no section divider.
#' @param inset (`numeric(1)`)\cr number of spaces to inset the table header, table body, referential footnotes, and
#'   main_footer, as compared to alignment of title, subtitle, and provenance footer. Defaults to 0 (no inset).
#' @param table_inset (`numeric(1)`)\cr number of spaces to inset the table header, table body, referential footnotes,
#'   and main footer, as compared to alignment of title, subtitles, and provenance footer. Defaults to 0 (no inset).
#'
#' @return No return value.
#'
#' @family conventions
#' @name gen_args
#' @keywords internal
gen_args <- function(df, alt_counts_df, spl, pos, tt, tr, verbose, colwidths, obj, x,
                     value, object, path, label, label_pos, # visible_label,
                     cvar, topleft, page_prefix, hsep, indent_size, section_div, na_str, inset,
                     table_inset,
                     ...) {
  NULL
}

#' Layouting function argument conventions
#'
#' @inheritParams gen_args
#' @param lyt (`PreDataTableLayouts`)\cr layout object pre-data used for tabulation.
#' @param var (`string`)\cr variable name.
#' @param vars (`character`)\cr vector of variable names.
#' @param var_labels (`character`)\cr vector of labels for one or more variables.
#' @param labels_var (`string`)\cr name of variable containing labels to be displayed for the values of `var`.
#' @param varlabels (`character`)\cr vector of labels for `vars`.
#' @param varnames (`character`)\cr vector of names for `vars` which will appear in pathing. When `vars` are all
#'   unique this will be the variable names. If not, these will be variable names with suffixes as necessary to enforce
#'   uniqueness.
#' @param split_format (`FormatSpec`)\cr default format associated with the split being created.
#' @param split_na_str (`character`)\cr NA string vector for use with `split_format`.
#' @param split_label (`string`)\cr label to be associated with the table generated by the split. Not to be confused
#'   with labels assigned to each child (which are based on the data and type of split during tabulation).
#' @param nested (`logical`)\cr whether this layout instruction should be applied within the existing layout structure
#'   *if possible* (`TRUE`, the default) or as a new top-level element (`FALSE`). Ignored if it would nest a split
#'   underneath analyses, which is not allowed.
#' @param format (`FormatSpec`)\cr format associated with this split. Formats can be declared via strings (`"xx.x"`)
#'   or function. In cases such as `analyze` calls, they can be character vectors or lists of functions.
#' @param align (`character(1)` or `NULL`)\cr alignment the value should be rendered with. Defaults to `"center"` if
#'   `NULL` is used. See [formatters::list_valid_aligns()] for all currently supported alignments.
#' @param cfun (`list`, `function`, or `NULL`)\cr tabulation function(s) for creating content rows. Must accept `x`
#'   or `df` as first parameter. Must accept `labelstr` as the second argument. Can optionally accept all optional
#'   arguments accepted by analysis functions. See [analyze()].
#' @param cformat (`format spec`)\cr format for content rows.
#' @param cna_str (`character`)\cr NA string for use with `cformat` for content table.
#' @param split_fun (`function` or `NULL`)\cr custom splitting function. See [custom_split_funs].
#' @param split_name (`string`)\cr name associated with the split (for pathing, etc.).
#' @param afun (`function`)\cr analysis function. Must accept `x` or `df` as its first parameter. Can optionally take
#'   other parameters which will be populated by the tabulation framework. See Details in [analyze()].
#' @param inclNAs (`logical`)\cr whether NA observations in the `var` variable(s) should be included when performing
#'   the analysis. Defaults to `FALSE`.
#' @param valorder (`character`)\cr the order that the split children should appear in resulting table.
#' @param ref_group (`character`)\cr value of `var` to be taken as the `ref_group`/control to be compared against.
#' @param compfun (`function` or `string`)\cr the comparison function which accepts the analysis function outputs for
#'   two different partitions and returns a single value. Defaults to subtraction. If a string, taken as the name of a
#'   function.
#' @param label_fstr (`string`)\cr a `sprintf` style format string. For non-comparison splits, it can contain up to
#'   one `"\%s"` which takes the current split value and generates the row/column label. For comparison-based splits
#'   it can contain up to two `"\%s"`.
#' @param child_labels (`string`)\cr the display behavior for the labels (i.e. label rows) of the children of this
#'   split. Accepts `"default"`, `"visible"`, and `"hidden"`. Defaults to `"default"` which flags the label row as
#'   visible only if the child has 0 content rows.
#' @param extra_args (`list`)\cr extra arguments to be passed to the tabulation function. Element position in the list
#'   corresponds to the children of this split. Named elements in the child-specific lists are ignored if they do
#'   not match a formal argument of the tabulation function.
#' @param name (`character(1)`)\cr name of the split/table/row being created. Defaults to the value of the
#'   corresponding label, but is not required to be.
#' @param cuts (`numeric`)\cr cuts to use.
#' @param cutlabels (`character` or `NULL`)\cr labels for the cuts.
#' @param cutlabelfun (`function`)\cr function which returns either labels for the cuts or `NULL` when passed the
#'   return value of `cutfun`.
#' @param cumulative (`logical(1)`)\cr whether the cuts should be treated as cumulative. Defaults to `FALSE`.
#' @param cutfun (`function`)\cr function which accepts the *full vector* of `var` values and returns cut points to be
#'   used (via `cut`) when splitting data during tabulation.
#' @param indent_mod (`numeric`)\cr modifier for the default indent position for the structure created by this
#'   function (subtable, content table, or row) *and all of that structure's children*. Defaults to 0, which
#'   corresponds to the unmodified default behavior.
#' @param show_labels (`character(1)`)\cr whether the variable labels corresponding to the variable(s) in `vars`
#'   should be visible in the resulting table.
#' @param table_names (`character`)\cr names for the tables representing each atomic analysis. Defaults to `var`.
#' @param page_by (`logical(1)`)\cr whether pagination should be forced between different children resulting from this
#'   split. An error will occur if the selected split does not contain at least one value that is not `NA`.
#' @param format_na_str (`character(1)`)\cr string which should be displayed when formatted if this cell's value(s)
#'   are all `NA`.
#'
#' @inherit gen_args return
#'
#' @family conventions
#' @name lyt_args
#' @keywords internal
lyt_args <- function(lyt, var, vars, label, labels_var, varlabels, varnames, split_format,
                     split_na_str, nested, format, cfun, cformat, cna_str, split_fun,
                     split_name, split_label, afun, inclNAs, valorder,
                     ref_group, compfun, label_fstr, child_labels, extra_args, name,
                     cuts, cutlabels, cutfun, cutlabelfun, cumulative,
                     indent_mod, show_labels, label_pos, # visible_label,
                     var_labels, cvar,
                     table_names, topleft, align, page_by, page_prefix,
                     format_na_str, section_div, na_str) {
  NULL
}

#' Constructor argument conventions
#'
#' @inheritParams gen_args
#' @inheritParams lyt_args
#' @param kids (`list`)\cr list of direct children.
#' @param cont (`ElementaryTable`)\cr content table.
#' @param lev (`integer(1)`)\cr nesting level (roughly, indentation level in practical terms).
#' @param iscontent (`logical(1)`)\cr whether the `TableTree`/`ElementaryTable` is being constructed as the content
#'   table for another `TableTree`.
#' @param cinfo (`InstantiatedColumnInfo` or `NULL`)\cr column structure for the object being created.
#' @param labelrow (`LabelRow`)\cr the `LabelRow` object to assign to the table. Constructed from `label` by default
#'   if not specified.
#' @param vals (`list`)\cr cell values for the row.
#' @param cspan (`integer`)\cr column span. `1` indicates no spanning.
#' @param cindent_mod (`numeric(1)`)\cr the indent modifier for the content tables generated by this split.
#' @param cextra_args (`list`)\cr extra arguments to be passed to the content function when tabulating row group
#'   summaries.
#' @param child_names (`character`)\cr names to be given to the subsplits contained by a compound split (typically
#'   an `AnalyzeMultiVars` split object).
#' @param title (`character(1)`)\cr single string to use as main title ([main_title()]). Ignored for subtables.
#' @param subtitles (`character`)\cr a vector of strings to use as subtitles ([subtitles()]), where every element is
#'   printed on a separate line. Ignored for subtables.
#' @param main_footer (`character`)\cr a vector of strings to use as main global (non-referential) footer materials
#'   ([main_footer()]), where every element is printed on a separate line.
#' @param prov_footer (`character`)\cr a vector of strings to use as provenance-related global footer materials
#'   ([prov_footer()]), where every element is printed on a separate line.
#' @param footnotes (`list` or `NULL`)\cr referential footnotes to be applied at current level. In post-processing,
#'   this can be achieved with [`fnotes_at_path<-`].
#' @param trailing_section_div (`character(1)`)\cr string which will be used as a section divider after the printing
#'   of the last row contained in this (sub)table, unless that row is also the last table row to be printed overall,
#'   or `NA_character_` for none (the default). When generated via layouting, this would correspond to the
#'   `section_div` of the split under which this table represents a single facet.
#' @param header_section_div (`character(1)`)\cr string which will be used to divide the header from the table. See
#'   [header_section_div()] for the associated getter and setter. Please consider changing last element of
#'   [section_div()] when concatenating tables that require a divider between them.
#' @param page_title (`character`)\cr page-specific title(s).
#'
#' @inherit gen_args return
#'
#' @family conventions
#' @name constr_args
#' @keywords internal
constr_args <- function(kids, cont, lev, iscontent, cinfo, labelrow, vals,
                        cspan, label_pos, cindent_mod, cvar, label, cextra_args,
                        child_names, title, subtitles, main_footer, prov_footer,
                        footnotes, page_title, page_prefix, section_div,
                        trailing_section_div, split_na_str,
                        cna_str, inset, table_inset, header_section_div) {
  NULL
}

#' Compatibility argument conventions
#'
#' @inheritParams gen_args
#' @param .lst (`list`)\cr an already-collected list of arguments to be used instead of the elements of `...`.
#'   Arguments passed via `...` will be ignored if this is specified.
#' @param row.name (`character(1)` or `NULL`)\cr row name. If `NULL`, an empty string is used as `row.name` of the
#'   [rrow()].
#' @param format (`character(1)` or `function`)\cr the format label (string) or formatter function to apply to the
#'   cell values passed via `...`. See [formatters::list_valid_format_labels()] for currently supported format labels.
#' @param indent `r lifecycle::badge("deprecated")`
#' @param inset (`integer(1)`)\cr the table inset for the row or table being constructed. See
#'   [formatters::table_inset()] for details.
#'
#' @inherit gen_args return
#'
#' @family conventions
#' @name compat_args
compat_args <- function(.lst, row.name, format, indent, label, inset) NULL

#' Split function argument conventions
#'
#' @inheritParams gen_args
#' @param trim (`logical(1)`)\cr whether splits corresponding with 0 observations should be kept when tabulating.
#' @param first (`logical(1)`)\cr whether the created split level should be placed first in the levels (`TRUE`) or
#'   last (`FALSE`, the default).
#'
#' @inherit gen_args return
#'
#' @family conventions
#' @name sf_args
sf_args <- function(trim, label, first) NULL

# nocov end
