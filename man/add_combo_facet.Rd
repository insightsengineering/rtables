% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_split_fun.R
\name{add_combo_facet}
\alias{add_combo_facet}
\alias{add_overall_facet}
\title{Add a combination facet in postprocessing}
\usage{
add_combo_facet(name, label = name, levels, extra = list())

add_overall_facet(name, label, extra = list())
}
\arguments{
\item{name}{character(1). Name for the resulting facet (for use in pathing, etc).}

\item{label}{character(1). Label for the resulting facet.}

\item{levels}{character. Vector of levels to  combine within the resulting facet.}

\item{extra}{list. Extra arguments to be passed to analysis functions applied
within the resulting facet.}
}
\value{
a function which can be used within the \code{post} argument in
\code{make_split_fun}.
}
\description{
Add a combination facet during postprocessing stage in a custom split fun.
}
\details{
For \code{add_combo_facet}, the data associated with the resulting
facet will be the data associated with the facets for each level in
\code{levels}, \code{rbound} together. In particular, this means that if those levels
are overlapping, data that appears in both will be duplicated.
}
\examples{
mysplfun <- make_split_fun(post = list(
  add_combo_facet("A_B",
    label = "Arms A+B",
    levels = c("A: Drug X", "B: Placebo")
  ),
  add_overall_facet("ALL", label = "All Arms")
))

lyt <- basic_table(show_colcounts = TRUE) \%>\%
  split_cols_by("ARM", split_fun = mysplfun) \%>\%
  analyze("AGE")

tbl <- build_table(lyt, DM)

}
\seealso{
\code{\link{make_split_fun}}

Other make_custom_split: 
\code{\link{drop_facet_levels}()},
\code{\link{make_split_fun}()},
\code{\link{make_split_result}()},
\code{\link{trim_levels_in_facets}()}
}
\concept{make_custom_split}
