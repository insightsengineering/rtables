% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_rcell.R
\name{format_rcell}
\alias{format_rcell}
\title{Format \code{rcell}}
\usage{
format_rcell(
  x,
  format,
  output = c("ascii", "html"),
  na_str = obj_na_str(x) \%||\% "NA",
  pr_row_format = NULL,
  pr_row_na_str = NULL,
  shell = FALSE
)
}
\arguments{
\item{x}{an object of class \code{\link{CellValue}}, or a raw value.}

\item{format}{character(1) or function. The format label (string) or
formatter function to apply to \code{x}.}

\item{output}{character(1). Output type.}

\item{na_str}{character(1). String that should be displayed when the value of
\code{x} is missing. Defaults to \code{"NA"}.}

\item{pr_row_format}{list of default format coming from the general row.}

\item{pr_row_na_str}{list of default \code{"NA"} string coming from the general row.}

\item{shell}{logical(1). Should the formats themselves be returned instead of the
values with formats applied. Defaults to \code{FALSE}.}
}
\value{
formatted text representing the cell
}
\description{
This is a wrapper around
\code{\link[formatters:format_value]{formatters::format_value}} for use with
\code{CellValue} objects
}
\examples{
cll <- CellValue(pi, format = "xx.xxx")
format_rcell(cll)

# Cell values precedes the row values
cll <- CellValue(pi, format = "xx.xxx")
format_rcell(cll, pr_row_format = "xx.x")

# Similarly for NA values
cll <- CellValue(NA, format = "xx.xxx", format_na_str = "This is THE NA")
format_rcell(cll, pr_row_na_str = "This is NA")

}
