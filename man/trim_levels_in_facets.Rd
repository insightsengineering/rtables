% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_split_fun.R
\name{trim_levels_in_facets}
\alias{trim_levels_in_facets}
\title{Trim Levels of Another Variable From Each Facet (Postprocessing split step)}
\usage{
trim_levels_in_facets(innervar)
}
\arguments{
\item{innervar}{character. The variable(s) to trim (remove
unobserved levels) independently within each facet.}
}
\value{
a function suitable for use in the \code{pre}
(list) argument of \code{make_split_fun}
}
\description{
Trim Levels of Another Variable From Each Facet (Postprocessing split step)
}
\seealso{
make_split_fun

Other make_custom_split: 
\code{\link{add_combo_facet}()},
\code{\link{drop_facet_levels}()},
\code{\link{make_split_fun}()},
\code{\link{make_split_result}()}
}
\concept{make_custom_split}
