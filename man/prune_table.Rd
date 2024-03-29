% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tt_compare_tables.R
\name{prune_table}
\alias{prune_table}
\title{Recursively prune a \code{TableTree}}
\usage{
prune_table(
  tt,
  prune_func = prune_empty_level,
  stop_depth = NA_real_,
  depth = 0
)
}
\arguments{
\item{tt}{\code{TableTree} (or related class). A \code{TableTree} object representing a
populated table.}

\item{prune_func}{function. A Function to be called on each subtree which
returns TRUE if the entire subtree should be removed.}

\item{stop_depth}{numeric(1). The depth after which subtrees should not be
checked for pruning. Defaults to \code{NA} which indicates pruning should
happen at all levels}

\item{depth}{numeric(1). Used internally, not intended to be set by the end
user.}
}
\value{
A \code{TableTree} pruned via recursive application of \code{prune_func}.
}
\description{
Recursively prune a \code{TableTree}
}
\examples{
adsl <- ex_adsl
levels(adsl$SEX) <- c(levels(ex_adsl$SEX), "OTHER")

tbl_to_prune <- basic_table() \%>\%
  split_cols_by("ARM") \%>\%
  split_rows_by("SEX") \%>\%
  summarize_row_groups() \%>\%
  split_rows_by("STRATA1") \%>\%
  summarize_row_groups() \%>\%
  analyze("AGE") \%>\%
  build_table(adsl)

tbl_to_prune \%>\% prune_table()

}
\seealso{
\code{\link[=prune_empty_level]{prune_empty_level()}} for details on this and several other basic
pruning functions included in the \code{rtables} package.
}
