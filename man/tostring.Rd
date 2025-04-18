% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tt_toString.R
\name{toString,VTableTree-method}
\alias{toString,VTableTree-method}
\alias{tostring}
\title{Convert an \code{rtable} object to a string}
\usage{
\S4method{toString}{VTableTree}(
  x,
  widths = NULL,
  col_gap = 3,
  hsep = horizontal_sep(x),
  indent_size = 2,
  tf_wrap = FALSE,
  max_width = NULL,
  fontspec = font_spec(),
  ttype_ok = FALSE,
  round_type = c("iec", "sas")
)
}
\arguments{
\item{x}{(\code{ANY})\cr object to be prepared for rendering.}

\item{widths}{(\code{numeric} or  \code{NULL})\cr Proposed widths for the columns of \code{x}. The expected
length of this numeric vector can be retrieved with \code{ncol(x) + 1} as the column of row names
must also be considered.}

\item{col_gap}{(\code{numeric(1)})\cr space (in characters) between columns.}

\item{hsep}{(\code{string})\cr character to repeat to create header/body separator line. If
\code{NULL}, the object value will be used. If \code{" "}, an empty separator will be printed. See
\code{\link[formatters:default_hsep]{default_hsep()}} for more information.}

\item{indent_size}{(\code{numeric(1)})\cr number of spaces to use per indent level. Defaults to 2.}

\item{tf_wrap}{(\code{flag})\cr whether the text for title, subtitles, and footnotes should be wrapped.}

\item{max_width}{(\code{integer(1)}, \code{string} or \code{NULL})\cr width that title and footer (including
footnotes) materials should be word-wrapped to. If \code{NULL}, it is set to the current print width of the
session (\code{getOption("width")}). If set to \code{"auto"}, the width of the table (plus any table inset) is
used. Parameter is ignored if \code{tf_wrap = FALSE}.}

\item{fontspec}{(\code{font_spec})\cr a font_spec object specifying the font information to use for
calculating string widths and heights, as returned by \code{\link[formatters:font_spec]{font_spec()}}.}

\item{ttype_ok}{(\code{logical(1)})\cr should truetype (non-monospace) fonts be
allowed via \code{fontspec}. Defaults to \code{FALSE}. This parameter is primarily
for internal testing and generally should not be set by end users.}

\item{round_type}{(\code{"iec"} or \code{"sas"})\cr the type of rounding to perform. iec,
the default, peforms rounding compliant with IEC 60559 (see details), while
sas performs nearest-value rounding consistent with rounding within SAS.}
}
\value{
A string representation of \code{x} as it appears when printed.
}
\description{
Transform a complex object into a string representation ready to be printed or written
to a plain-text file.

All objects that are printed to console pass via \code{toString}. This function allows
fundamental formatting specifications to be applied to final output, like column widths
and relative wrapping (\code{width}), title and footer wrapping (\code{tf_wrap = TRUE} and
\code{max_width}), and horizontal separator character (e.g. \code{hsep = "+"}).
}
\details{
Manual insertion of newlines is not supported when \code{tf_wrap = TRUE} and will result in a warning and
undefined wrapping behavior. Passing vectors of already split strings remains supported, however in this
case each string is word-wrapped separately with the behavior described above.
}
\examples{
\dontshow{if (require(dplyr)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(dplyr)

iris2 <- iris \%>\%
  group_by(Species) \%>\%
  mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) \%>\%
  ungroup()

lyt <- basic_table() \%>\%
  split_cols_by("Species") \%>\%
  split_cols_by("group") \%>\%
  analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary), format = "xx.xx")

tbl <- build_table(lyt, iris2)

cat(toString(tbl, col_gap = 3))
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[formatters:wrap_string]{wrap_string()}}
}
