## package imports
#' @importFrom utils head tail
#' @importFrom stats setNames na.omit prop.test binom.test relevel
#' @importFrom purrr transpose
#' @importFrom htmltools tags tagList
#' @importFrom magrittr %>%
#' @import methods
NULL


#' General Argument Conventions
#' @name gen_args
#' @family conventions
#' @param df dataset (data.frame or tibble)
#' @param spl A Split object defining a partitioning or analysis/tabulation of the data.
#' @param pos numeric.  Which top-level set of nested splits should the new layout feature be added to. Defaults to the current 
#' @param tt TableTree (or related class). A TableTree object representing a populated table.
#' @param verbose logical. Should additional information be displayed to the user. Defaults to FALSE.
#' @param colwidths numeric vector. Column widths for use with vertical pagination. Currently ignored.
#' @param obj ANY. The object for the accessor to access or modify
#' @param x An object
#' @param \dots Passed on to metthods or tabulation functions.
#' @param value The new value
#' @param object The object to modify in-place
#' @param verbose logical(1). Should extra debugging messages be shown. Defaults to \code{FALSE}.
#' @rdname gen_args
gen_args <- function(df, spl, pos, tt, verbose, colwidths, obj, x,
                     value, object, ...) NULL



#' Layouting Function Arg Conventions
#' @name lyt_args
#' @rdname lyt_args
#' @param lyt layout object pre-data used for tabulation
#' @param var string, variable name
#' @param vars character vector. Multiple variable names.
#' @param lbl string, label, often variable label
#' @param labels_var string, name of variable containing labels to be displayed for the values of \code{var}
#' @param varlbls character vector. Labels for \code{vars}
#' @param split_format format spec. Format associated with this split.
#' @param nested boolean, Add this as a new top-level split  (defining a new subtable directly under root). Defaults to \code{FALSE}
#' @param format format string for cells
#' @param cfun function/NULL. tabulation function for creating content rows. Must accept \code{df} as first parameter. Optionally accepts
#' @param cformat format spec. Format for content rows
#' @param splfun function/NULL. custom splitting function
#' @param splname string. Name associiated with this split (for pathing, etc)
#' @param splbl string. Labe associated with this split
#' @param afun function. Analysis function, must take \code{x} or \code{df} as its first parameter. Can optionally take other parameters which will be populated by the tabulation framework. See Details in \code{\link{analyze}}.
#' @param defrowlab character vector. Default row labels for resulting analysis rows.
#' @param inclNAs boolean. Should observations with NA in the \code{var} variable(s) be included when performing this analysis. Defaults to \code{FALSE}
#' @param valorder character vector. Order that the split children should appear in resulting table.
#' @param ref_group character. Value of \code{var} to be taken as the ref_group/control to be compared against.
#' @param compfun function/string. The comparison function which accepts the analysis function outputs for two different partitions and returns a single value. Defaults to subraction. If a string, taken as the name of a function.
#' @param lbl_fstr string. An sprintf style format string containing. For non-comparison splits, it can contain  up to one \code{"\%s"} which takes the current split value and generates the row/column label. Comparison-based splits it can contain up to two \code{"\%s"}.
#' @param lblkids logical. Should the children of this split have label rows. Defaults to \code{NA} which creates a label row only if the child has 0 content rows.
#' @param extra_args list. Extra arguments to be passed to the tabulation function. Element position in thte list corresponds to the children of this split. Named elements in the child-specific lists are ignored if they do not match a formal argument of the ttabulation function.
#' @param name character(1). Name of the split/table/row being creatted. Defaults to same as the corresponding llabel, but is not required to be.
#' @param cuts numeric. Cuts to use
#' @param cutlbls character (or NULL). Labels for the cuts
#' @param cutlblfun function. Function which returns either labels for the cuts or NULL when passed the return value of \code{cutfun}
#' @param cumulative logical. Should the cuts be treated as cumulative. Defaults to \code{FALSE}
#' @param cutfun function. Function which accepts the \emph{full vector} of \code{var} values and returns cut points to be used (via \code{cut}) when splitting data during tabulation
#' @param incl_all logical(1). Should an "all" comparison column be created. Defaults to \code{FALSE}.
#' @param indent_mod numeric. Modifier for the default indent position for the structure created by this function(subtable, content table, or row) \emph{and all of that structure's children}. Defaults to 0, which corresponds to the unmodified default behavior. 
#' @family conventions
lyt_args <- function( lyt, var, vars, lbl, labels_var, varlbls, split_format,
                     nested, format, cfun, cformat, splfun, splname,
                     splbl, afun, defrowlab, inclNAs, valorder,
                     ref_group, compfun, lbl_fstr, lblkids, extra_args, name,
                     cuts, cutlbls, cutfun, cutlblfun, cumulative,
                     incl_all,
                     indent_mod) NULL


#' Constructor Arg Conventions
#' @name constr_args
#' @family conventions
#' @param kids list. List of direct children.
#' @param cont ElementaryTable. Content table.
#' @param lev integer. Nesting level (roughly, indentation level in practical terms).
#' @param iscontent logical. Is the TableTree/ElementaryTable being constructed the content table for another TableTree.
#' @param cinfo InstantiatedColumnInfo (or NULL). Column structure for the object being created.
#' @param lblrow LabelRow. The LabelRow object to assign to this Table. Consructed from \code{lbl} by default if not specified.
#' @param vals list. cell values for the row
#' @param cspan integer. Column span. \code{1} indicates no spanning.
#' @rdname constr_args
constr_args <- function(kids, cont, lev, iscontent, cinfo, lblrow, vals, cspan) NULL

#' Compatability Arg Conventions
#' @name compat_args
#' @family conventions
#' @param .lst list. An already-collected list of arguments tot be used instead of the elements of \code{\dots}. Arguments passed via \code{\dots} will be ignored if this is specified.
#' @param FUN function. Tabulation fucntion. Will be passed subsets of \code{x} defined by the combination of \code{col_by} and \code{row_by} and returns corresponding cell value
#' @param col_by (\code{\link{factor}} or \code{\link{data.frame}}
#'   if a \code{\link{factor}} of length \code{nrow(x)} that defines
#'   which levels in \code{col_by} define a column.
#'   can use \code{\link{by_factor_to_matrix}} to create a matrix from a factor to use non-disjoint columns
#'   can use \code{\link{by_all}} to have a column with all rows, alternatively look at \code{\link{by_add_total}}
#' @param row_by rows in \code{x} to take per row in the resulting table
#' @param row.name if \code{NULL} then the \code{FUN} argument is deparsed and
#'   used as \code{row.name} of the \code{\link{rrow}}
#' @param format if \code{FUN} does not return a formatted \code{\link{rcell}}
#'   then the \code{format} is applied
#' @param indent deprecated.
#' @param col_wise_args a named list containing collections (e.g. vectors or
#'   lists) with data elements for each column of the resulting table. The data
#'   elements are then passed to the named argument \code{FUN} corresponding to
#'   the element name of the outer list. Hence, the length and order of each
#'   collection must match the levels in \code{col_by}. See examples.
#' @param label string. Label for the resulting column (or potentially row)
#' @rdname compat_args
compat_args <- function(.lst, FUN, col_by, row_by, row.name, format, indent, col_wise_args, label) NULL





## data

#' DM data
#'
#' @format rds (data.frame)
"DM"



#' Simulated CDISC Data for examples
#'
#' @format rds (data.frame)
#' @rdname cdisc_data
"ex_adsl"

#' @rdname cdisc_data
"ex_adae"

#' @rdname cdisc_data
"ex_adaette"

#' @rdname cdisc_data
"ex_adtte"

#' @rdname cdisc_data
"ex_adcm"

#' @rdname cdisc_data
"ex_adlb"

#' @rdname cdisc_data
"ex_admh"

#' @rdname cdisc_data
"ex_adqs"

#' @rdname cdisc_data
"ex_adrs"

#' @rdname cdisc_data
"ex_advs"
