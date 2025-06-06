% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tt_toString.R
\name{get_formatted_cells}
\alias{get_formatted_cells}
\alias{get_formatted_cells,TableTree-method}
\alias{get_formatted_cells,ElementaryTable-method}
\alias{get_formatted_cells,TableRow-method}
\alias{get_formatted_cells,LabelRow-method}
\alias{get_cell_aligns}
\alias{get_cell_aligns,TableTree-method}
\alias{get_cell_aligns,ElementaryTable-method}
\alias{get_cell_aligns,TableRow-method}
\alias{get_cell_aligns,LabelRow-method}
\title{Get formatted cells}
\usage{
get_formatted_cells(obj, shell = FALSE, round_type = c("iec", "sas"))

\S4method{get_formatted_cells}{TableTree}(obj, shell = FALSE, round_type = c("iec", "sas"))

\S4method{get_formatted_cells}{ElementaryTable}(obj, shell = FALSE, round_type = c("iec", "sas"))

\S4method{get_formatted_cells}{TableRow}(obj, shell = FALSE, round_type = c("iec", "sas"))

\S4method{get_formatted_cells}{LabelRow}(obj, shell = FALSE, round_type = c("iec", "sas"))

get_cell_aligns(obj)

\S4method{get_cell_aligns}{TableTree}(obj)

\S4method{get_cell_aligns}{ElementaryTable}(obj)

\S4method{get_cell_aligns}{TableRow}(obj)

\S4method{get_cell_aligns}{LabelRow}(obj)
}
\arguments{
\item{obj}{(\code{ANY})\cr the object for the accessor to access or modify.}

\item{shell}{(\code{flag})\cr whether the formats themselves should be returned instead of the values with formats
applied. Defaults to \code{FALSE}.}

\item{round_type}{(\code{"iec"} or \code{"sas"})\cr the type of rounding to perform. iec,
the default, peforms rounding compliant with IEC 60559 (see details), while
sas performs nearest-value rounding consistent with rounding within SAS.}
}
\value{
The formatted print-strings for all (body) cells in \code{obj}.
}
\description{
Get formatted cells
}
\examples{
\dontshow{if (require(dplyr)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(dplyr)

iris2 <- iris \%>\%
  group_by(Species) \%>\%
  mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) \%>\%
  ungroup()

tbl <- basic_table() \%>\%
  split_cols_by("Species") \%>\%
  split_cols_by("group") \%>\%
  analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary), format = "xx.xx") \%>\%
  build_table(iris2)

get_formatted_cells(tbl)
\dontshow{\}) # examplesIf}
}
