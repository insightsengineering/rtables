% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_accessors.R, R/tt_paginate.R
\name{obj_name,VNodeInfo-method}
\alias{obj_name,VNodeInfo-method}
\alias{formatters_methods}
\alias{obj_name,Split-method}
\alias{obj_name<-,VNodeInfo-method}
\alias{obj_name<-,Split-method}
\alias{obj_label,Split-method}
\alias{obj_label,TableRow-method}
\alias{obj_label,VTableTree-method}
\alias{obj_label,ValueWrapper-method}
\alias{obj_label<-,Split-method}
\alias{obj_label<-,TableRow-method}
\alias{obj_label<-,ValueWrapper-method}
\alias{obj_label<-,VTableTree-method}
\alias{obj_format,VTableNodeInfo-method}
\alias{obj_format,CellValue-method}
\alias{obj_format,Split-method}
\alias{obj_format<-,VTableNodeInfo-method}
\alias{obj_format<-,Split-method}
\alias{obj_format<-,CellValue-method}
\alias{obj_na_str,Split-method}
\alias{main_title,VTitleFooter-method}
\alias{main_title<-,VTitleFooter-method}
\alias{main_title,TableRow-method}
\alias{subtitles,VTitleFooter-method}
\alias{subtitles<-,VTitleFooter-method}
\alias{subtitles,TableRow-method}
\alias{main_footer,VTitleFooter-method}
\alias{main_footer<-,VTitleFooter-method}
\alias{main_footer,TableRow-method}
\alias{prov_footer,VTitleFooter-method}
\alias{prov_footer<-,VTitleFooter-method}
\alias{prov_footer,TableRow-method}
\alias{table_inset,VTableNodeInfo-method}
\alias{table_inset,PreDataTableLayouts-method}
\alias{table_inset<-,VTableNodeInfo-method}
\alias{table_inset<-,PreDataTableLayouts-method}
\alias{table_inset<-,InstantiatedColumnInfo-method}
\alias{nlines,TableRow-method}
\alias{nlines,LabelRow-method}
\alias{nlines,RefFootnote-method}
\alias{nlines,InstantiatedColumnInfo-method}
\alias{make_row_df,VTableTree-method}
\alias{make_row_df,TableRow-method}
\alias{make_row_df,LabelRow-method}
\title{Methods for generics in the \code{formatters} package}
\usage{
\S4method{obj_name}{VNodeInfo}(obj)

\S4method{obj_name}{Split}(obj)

\S4method{obj_name}{VNodeInfo}(obj) <- value

\S4method{obj_name}{Split}(obj) <- value

\S4method{obj_label}{Split}(obj)

\S4method{obj_label}{TableRow}(obj)

\S4method{obj_label}{VTableTree}(obj)

\S4method{obj_label}{ValueWrapper}(obj)

\S4method{obj_label}{Split}(obj) <- value

\S4method{obj_label}{TableRow}(obj) <- value

\S4method{obj_label}{ValueWrapper}(obj) <- value

\S4method{obj_label}{VTableTree}(obj) <- value

\S4method{obj_format}{VTableNodeInfo}(obj)

\S4method{obj_format}{CellValue}(obj)

\S4method{obj_format}{Split}(obj)

\S4method{obj_format}{VTableNodeInfo}(obj) <- value

\S4method{obj_format}{Split}(obj) <- value

\S4method{obj_format}{CellValue}(obj) <- value

\S4method{obj_na_str}{Split}(obj)

\S4method{main_title}{VTitleFooter}(obj)

\S4method{main_title}{VTitleFooter}(obj) <- value

\S4method{main_title}{TableRow}(obj)

\S4method{subtitles}{VTitleFooter}(obj)

\S4method{subtitles}{VTitleFooter}(obj) <- value

\S4method{subtitles}{TableRow}(obj)

\S4method{main_footer}{VTitleFooter}(obj)

\S4method{main_footer}{VTitleFooter}(obj) <- value

\S4method{main_footer}{TableRow}(obj)

\S4method{prov_footer}{VTitleFooter}(obj)

\S4method{prov_footer}{VTitleFooter}(obj) <- value

\S4method{prov_footer}{TableRow}(obj)

\S4method{table_inset}{VTableNodeInfo}(obj)

\S4method{table_inset}{PreDataTableLayouts}(obj)

\S4method{table_inset}{VTableNodeInfo}(obj) <- value

\S4method{table_inset}{PreDataTableLayouts}(obj) <- value

\S4method{table_inset}{InstantiatedColumnInfo}(obj) <- value

\S4method{nlines}{TableRow}(x, colwidths = NULL, max_width = NULL, fontspec, col_gap = 3)

\S4method{nlines}{LabelRow}(
  x,
  colwidths = NULL,
  max_width = NULL,
  fontspec = fontspec,
  col_gap = NULL
)

\S4method{nlines}{RefFootnote}(x, colwidths = NULL, max_width = NULL, fontspec, col_gap = NULL)

\S4method{nlines}{InstantiatedColumnInfo}(x, colwidths = NULL, max_width = NULL, fontspec, col_gap = 3)

\S4method{make_row_df}{VTableTree}(
  tt,
  colwidths = NULL,
  visible_only = TRUE,
  rownum = 0,
  indent = 0L,
  path = character(),
  incontent = FALSE,
  repr_ext = 0L,
  repr_inds = integer(),
  sibpos = NA_integer_,
  nsibs = NA_integer_,
  max_width = NULL,
  fontspec = NULL,
  col_gap = 3
)

\S4method{make_row_df}{TableRow}(
  tt,
  colwidths = NULL,
  visible_only = TRUE,
  rownum = 0,
  indent = 0L,
  path = "root",
  incontent = FALSE,
  repr_ext = 0L,
  repr_inds = integer(),
  sibpos = NA_integer_,
  nsibs = NA_integer_,
  max_width = NULL,
  fontspec,
  col_gap = 3
)

\S4method{make_row_df}{LabelRow}(
  tt,
  colwidths = NULL,
  visible_only = TRUE,
  rownum = 0,
  indent = 0L,
  path = "root",
  incontent = FALSE,
  repr_ext = 0L,
  repr_inds = integer(),
  sibpos = NA_integer_,
  nsibs = NA_integer_,
  max_width = NULL,
  fontspec,
  col_gap = 3
)
}
\arguments{
\item{obj}{(\code{ANY})\cr the object for the accessor to access or modify.}

\item{value}{(\code{ANY})\cr the new value.}

\item{x}{(\code{ANY})\cr an object.}

\item{colwidths}{(\code{numeric})\cr a vector of column widths for use in vertical pagination.}

\item{max_width}{(\code{numeric(1)})\cr width that strings should be wrapped to when
determining how many lines they require.}

\item{fontspec}{(\code{font_spec})\cr a font_spec object specifying the font information to use for
calculating string widths and heights, as returned by \code{\link[formatters:font_spec]{font_spec()}}.}

\item{col_gap}{(\code{numeric(1)})\cr width of gap between columns in number of spaces.
Only used by methods which must calculate span widths after wrapping.}

\item{tt}{(\code{TableTree} or related class)\cr a \code{TableTree} object representing a populated table.}

\item{visible_only}{(\code{flag})\cr should only visible aspects of the table structure be reflected
in this summary. Defaults to \code{TRUE}. May not be supported by all methods.}

\item{rownum}{(\code{numeric(1)})\cr internal detail, do not set manually.}

\item{indent}{(\code{integer(1)})\cr internal detail, do not set manually.}

\item{path}{(\code{character})\cr a vector path for a position within the structure of a \code{TableTree}. Each element
represents a subsequent choice amongst the children of the previous choice.}

\item{incontent}{(\code{flag})\cr internal detail, do not set manually.}

\item{repr_ext}{(\code{integer(1)})\cr internal detail, do not set manually.}

\item{repr_inds}{(\code{integer})\cr internal detail, do not set manually.}

\item{sibpos}{(\code{integer(1)})\cr internal detail, do not set manually.}

\item{nsibs}{(\code{integer(1)})\cr internal detail, do not set manually.}
}
\value{
\itemize{
\item Accessor functions return the current value of the component being accessed of \code{obj}
\item Setter functions return a modified copy of \code{obj} with the new value.
}
}
\description{
See the \code{formatters} documentation for descriptions of these generics.
}
\details{
When \code{visible_only} is \code{TRUE} (the default), methods should return a \code{data.frame} with exactly one
row per visible row in the table-like object. This is useful when reasoning about how a table will
print, but does not reflect the full pathing space of the structure (though the paths which are given
will all work as is).

If supported, when \code{visible_only} is \code{FALSE}, every structural element of the table (in row-space)
will be reflected in the returned \code{data.frame}, meaning the full pathing-space will be represented
but some rows in the layout summary will not represent printed rows in the table as it is displayed.

Most arguments beyond \code{tt} and \code{visible_only} are present so that \code{make_row_df} methods can call
\code{make_row_df} recursively and retain information, and should not be set during a top-level call.
}
\note{
The technically present root tree node is excluded from the summary returned by
both \code{make_row_df} and \code{make_col_df} (see relevant functions in\code{rtables}), as it is the
row/column structure of \code{tt} and thus not useful for pathing or pagination.
}
\examples{
# Expected error with matrix_form. For real case examples consult {rtables} documentation
mf <- basic_matrix_form(iris)
# make_row_df(mf) # Use table obj instead
}
