% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colby_constructors.R
\name{additional_fun_params}
\alias{additional_fun_params}
\title{Additional parameters within analysis and content functions
(\code{afun/cfun})}
\description{
It is possible to add specific parameters to \code{afun} and \code{cfun}, in \link{analyze}
and \link{summarize_row_groups} respectively. These parameters grant access to
relevant information like the row split structure (see \link{spl_context}) and the
predefined baseline (\code{.ref_group}).
}
\details{
We list and describe here all the parameters that can be added to a custom
analysis function:

\describe{
\item{.N_col}{column-wise N (column count) for the full column being
tabulated within}
\item{.N_total}{overall N (all observation count, defined as sum of column
counts) for the tabulation}
\item{.N_row}{row-wise N (row group count) for the group of observations
being analyzed (i.e. with no column-based subsetting)}
\item{.df_row}{data.frame for observations in the row group being analyzed
(i.e. with no column-based subsetting)}
\item{.var}{variable that is analyzed}
\item{.ref_group}{data.frame or vector of subset corresponding to the
\code{ref_group} column including subsetting defined by row-splitting.
Optional and only required/meaningful if a \code{ref_group} column has been
defined}
\item{.ref_full}{data.frame or vector of subset corresponding to the
\code{ref_group} column without subsetting defined by row-splitting. Optional
and only required/meaningful if a \code{ref_group} column has been defined}
\item{.in_ref_col}{boolean indicates if calculation is done for cells
within the reference column}
\item{.spl_context}{data.frame, each row gives information about a
previous/'ancestor' split state. See \code{\link{spl_context}}}
\item{.alt_df_row}{data.frame, i.e. the \code{alt_count_df} after
row splitting. It can be used with \code{.all_col_exprs} and \code{.spl_context}
information to retrieve current faceting, but for \code{alt_count_df}.
It can be an empty table if all the entries are filtered out.}
\item{.alt_df}{data.frame, \code{.alt_df_row} but filtered by columns expression.
This data present the same faceting of main data \code{df}. This also filters
\code{NAs} out if related parameters are set to (e.g. \code{inclNAs} in \link{analyze}).
Similarly to \code{.alt_df_row}, it can be an empty table if all the entries
are filtered out.}
\item{.all_col_exprs}{list of expressions. Each of them represents a
different column splitting.}
\item{.all_col_counts}{vector of integers. Each of them represents the global
count for each column. It differs if \code{alt_counts_df} is used
(see \link{build_table}).}
}
}
\note{
If any of these formals is specified incorrectly or not
present in the tabulation machinery, it will be as if missing. For
example\code{.ref_group} will be missing if no baseline is previously defined
during data splitting (via \code{ref_group} parameters in, e.g., \link{split_rows_by}).
Similarly, if no \code{alt_counts_df} is provided into \link{build_table},
\code{.alt_df_row} and \code{.alt_df} will not be present.
}
