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
#' @param valuelblvar string, name of variable containing split value display lables
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
#' @param rowlblf string. An sprintf style format string containing up to one \code{"\%s"} which takes the current split value and generates the row label for the summary.
#' @param tt TableTree. A TableTree object representing a populated table.
#' @param verbose logical. Should additional information be displayed to the user. Defaults to FALSE.
#' @param colwidths numeric vector. Column widths for use with vertical pagination. Currently ignored.
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
