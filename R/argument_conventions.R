## package imports
#' @importFrom utils head tail
#' @importFrom stats setNames na.omit prop.test binom.test relevel
#' @importFrom purrr transpose
#' @importFrom htmltools tags tagList
#' @importFrom magrittr %>%
#' @import methods
NULL


#' Dummy Function with aruments that are used throughout the framework
#' @name argument_conventions
#' 
#' @param lyt layout object pre-data used for tabulation
#' @param var string, variable name
#' @param vars character vector. Multiple variable names.
#' @param lbl string, label, often variable label
#' @param df dataset
#' @param vlblvar string, name of variable containing labels to be displayed for the values of \code{var}
#' @param varlbls character vector. Labels for \code{vars}
#' @param splfmt format spec. Format associated with this split.
#' @param newtoplev boolean, Add this as a new top-level split  (defining a new subtable directly under root). Defaults to \code{FALSE}
#' @param spl A Split object defining a partitioning or analysis/tabulation of the data.
#' @param pos numeric.  Which top-level set of nested splits should the new layout feature be added to. Defaults to the current 
#' @param fmt format string for cells
#' @param cfun function/NULL. tabulation function for creating content rows
#' @param cfmt format spec. Format for content rows
#' @param splfun function/NULL. custom splitting function
#' @param splname string. Name associiated with this split (for pathing, etc)
#' @param splbl string. Labe associated with this split
#' @param afun function. Analysis function.
#' @param defrowlab character vector. Default row labels for resulting analysis rows.
#' @param inclNAs boolean. Should observations with NA in the \code{var} variable(s) be included when performing this analysis. Defaults to \code{FALSE}
#' @param valorder character vector. Order that the split children should appear in resulting table.
#' @param baseline character. Value of \code{var} to be taken as the baseline/control to be compared against.
#' @param compfun function/string. The comparison function which accepts the analysis function outputs for two different partitions and returns a single value. Defaults to subraction. If a string, taken as the name of a function.
#' @param lbl_fstr string. An sprintf style format string containing. For non-comparison splits, it can contain  up to one \code{"\%s"} which takes the current split value and generates the row/column label. Comparison-based splits it can contain up to two \code{"\%s"}.
#' @param tt TableTree (or related class). A TableTree object representing a populated table.
#' @param verbose logical. Should additional information be displayed to the user. Defaults to FALSE.
#' @param colwidths numeric vector. Column widths for use with vertical pagination. Currently ignored.
#' @param obj ANY. The object for the accessor to access or modify
#' @param x An object
#' @param obj the object.
#' @param row.name The default row name to use
#' @param format format string for cells.
#' @param indent deprecated.
#' @param \dots Passed on to metthods or tabulation functions.
#' @param value The new value
#' @param object The object to modify in-place
#' @param lblkids logical. Should the children of this split have label rows. Defaults to \code{NA} which creates a label row only if the child has 0 content rows.
#' @param extrargs list. Extra arguments to be passed to the tabulation function. Element position in thte list corresponds to the children of this split. Named elements in the child-specific lists are ignored if they do not match a formal argument of the ttabulation function.
#' @param kids list. List of direct children.
#' @param cont ElementaryTable. Content table.
#' @param lev integer. Nesting level (roughly, indentation level in practical terms).
#' @param iscontent logical. Is the TableTree/ElementaryTable being constructed the content table for another TableTree.
#' @param cinfo InstantiatedColumnInfo (or NULL). Column structure for the object being created.
#' @param name character(1). Name of the split/table/row being creatted. Defaults to same as the corresponding llabel, but is not required to be.
#' @param verbose logical(1). Should extra debugging messages be shown. Defaults to \code{FALSE}.
#' @param incl_all logical(1). Should an "all" comparison column be created. Defaults to \code{FALSE}.
#' @param .lst list. An already-collected list of arguments tot be used instead of the elements of \code{\dots}. Arguments passed via \code{\dots} will be ignored if this is specified.
#' @param lblrow LabelRow. The LabelRow object to assign to this Table. Consructed from \code{lbl} by default if not specified.
#' @param col_by (\code{\link{factor}} or \code{\link{data.frame}}
#'   if a \code{\link{factor}} of length \code{nrow(x)} that defines
#'   which levels in \code{col_by} define a column.
#'   can use \code{\link{by_factor_to_matrix}} to create a matrix from a factor to use non-disjoint columns
#'   can use \code{\link{by_all}} to have a column with all rows, alternatively look at \code{\link{by_add_total}})

#' @rdname argument_conventions
NULL

argument_conventions <- function(lyt, var, lbl, valuelblvar, splfmt, newtoplev, spl, pos) {
  NULL
}


## data

#' DM data
#'
#' @format rds (data.frame)
"DM"
