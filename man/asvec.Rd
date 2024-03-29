% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_accessors.R
\name{asvec}
\alias{asvec}
\alias{as.vector,VTableTree-method}
\title{convert to a vector}
\usage{
\S4method{as.vector}{VTableTree}(x, mode = "any")
}
\arguments{
\item{x}{ANY. The object to be converted to a vector}

\item{mode}{character(1). Passed on to \code{\link[base]{as.vector}}}
}
\value{
a vector of the chosen mode (or an error is raised if more than one row was present).
}
\description{
Convert an \code{rtables} framework object into a vector, if possible.
This is unlikely to be useful in realistic scenarios.
}
\note{
This only works for a table with a single row or a row object.
}
