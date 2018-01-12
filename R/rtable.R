#' Create a Reporting Table
#' 
#' Reporting tables allow multiple values per cell, cell formatting and merging
#' cells. Currently an \code{rtable} can be converted to html and ascii.
#' 
#' 
#' @param ... each element is an \code{\link{rrow}} object
#' @param headers vector with column names
#' @param format a valid format string or a format function for
#'   \code{\link{rcell}}s. To get a list of all valid format strings use 
#'   \code{\link{get_rcell_formats}}.
#' 
#' @return \code{rtable} object
#' 
#' @details 
#' Rtable objects can be currently exported to text with
#' \code{\link{toString}} and to html with \code{\link{as_html}}. In
#' future we would plan to add the \code{as_latex} and \code{as_gridGrob}
#' outputting function.
#' 
#' Note that the formats propagate to the \code{\link{rrow}} and 
#' \code{\link{rcell}} if these do not specify their own format.
#' 
#' @importFrom shiny tags tagList
#' 
#' @export
#' 
#' @author Adrian Waddell \email{adrian.waddell@roche.com}
#' 
#' @seealso \code{\link{rrow}}, \code{\link{rcell}}
#' 
#' @examples
#' 
#' tbl <- rtable(
#'   rrow("A", c(104, .2), c(100, .4)),
#'   rrow("B", c(23, .4), c(43, .5)),
#'   rrow(),
#'   rrow("this is a very long section header"),
#'   rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
#'   rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2)),
#'   headers = c("Treatement\nN=100", "Comparison\nN=300"),
#'   format = "xx (xx.xx%)"
#' )
#' 
#' tbl
#' 
#' row.names(tbl)
#' names(tbl)
#' 
#' 
#' tbl[1,2]
#' 
#' tbl[3,2]
#' tbl[5,1]
#' tbl[5,2]
#' 
#' 
#' dim(tbl)
#' nrow(tbl)
#' ncol(tbl)
#' 
#' tbl[[1]][[1]]
#' 
#' as_html(tbl)
#' 
#' \dontrun{
#' Viewer(tbl)
#' }
#' 
#' # colspans
#' 
#' tbl2 <- rtable(
#'   rrow("r1", 1, 2, 3, 4, 5),
#'   rrow("r2", rcell("sp2", colspan = 2), "sp1", rcell("sp2-2", colspan = 2)),
#'   headers = c("A", "B", "C", "D", "E"),
#'   format = "xx"
#' )
#' 
#' tbl2
#' 
#' \dontrun{
#' Viewer(tbl2)
#' }
#' 
#' tbl2[1,3]
#' tbl2[2,1]
#' tbl2[2,2]
#' tbl2[2,3]
#' tbl2[2,4]
#' tbl2[2,5]
#' 
#' 
#' # custom format
#' my_format <- function(x, output) {
#'    paste(x, collapse = "/")
#' }
#' tbl3 <- rtable(
#'   rrow("row1", c(1,2,3,4), letters[1:10]),
#'   headers = c("A", "B"),
#'   format = my_format
#' )
#' tbl3
#'
rtable <- function(...) {
  UseMethod("rtable")
}

#' @export
rtable.rrow <- function(..., headers = '', format = '') {
  
  ncol <- length(headers)
  if (ncol <= 1) stop("table needs at least one 1 columns")
  
  ## check if n-cols correct
  rows <- list(...)
  
  if (!all(vapply(rows, is, logical(1), "rrow"))) stop("not all arguments in ... are of class rrow")
  nrow <- length(rows)
  
  check_consistent_ncols(rows, ncol)
  
  rows_formatted <- lapply(rows, function(row) {
    
    row_f <- lapply(row, function(cell) {
      rc <- if (is(cell, "rcell")) {
        if (is.null(attr(cell, "format"))) {
          structure(cell, format = format)
        } else {
          cell
        }
      } else {
        rcell(cell, format = format)
      }
      if (is.null(attr(rc, "format"))) stop("NULL format for cell: ", rc)
      rc
    }) 
    
    attributes(row_f) <- attributes(row)
    row_f
  })
  
  structure(
    rows_formatted,
    col.names = headers,
    ncol = ncol,
    nrow = nrow,
    class = "rtable"
  )
}



#' rtables constructor from dataframe source
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#'
#' @param data a dataframe containing one row per table row
#' @param headers the headers to use for the resulting table
#' @param format a single format string (parseable as a rtables format string
#'   (e.g. 'xx.xx'), a \code{\link[rtables]{spf}()} format string), a list of
#'   format strings, or a column name to reference for each row from the source
#'   dataframe.
#' @param indent a numeric value or a column name to reference for each row from
#'   the source dataframe.
#'
#' @return \code{rtable} object
#'
#' @examples
#' ## basic use with dataframe
#' t <- rtable(mtcars,
#'   headers = c('Weight (lbs (tons))', 'Transmission', 'Miles Per Gallon'),
#'   format = c(
#'     "%(wt * 1000).0f (%(wt * 0.5)0.2f)",
#'     "%(ifelse(am == 0, 'automatic', 'manual'))s",
#'     "%(mpg).1f"))
#'
#' \dontrun{
#' Viewer(t)
#' }
#'
#'
#' ## using a format column in the dataframe
#' my_mtcars <- mtcars
#' my_mtcars$format <- list( c(  # create column of format list for each row
#'     "%(wt * 1000).0f (%(wt * 0.5)0.2f)",
#'     "%(ifelse(am == 0, 'automatic', 'manual'))s",
#'     "%(mpg).1f"
#' ) )
#'
#' t <- rtable(my_mtcars,
#'   headers = c(
#'     'Weight (lbs (tons))',
#'     'Transmission',
#'     'Miles Per Gallon'),
#'   format = format)
#'
#' \dontrun{
#' Viewer(t)
#' }
#'
#'
#' ## composing from multiple analysis datasets
#' library(dplyr)
#' library(tibble) # for `column_to_rownames()`
#' options(stringsAsFactors = FALSE)
#'
#' # build analysis datasets
#' my_data <- bind_rows(
#'     data.frame(rownames = 'By Transmission'),
#'     mtcars %>% group_by(am) %>%
#'       summarize(n = n(), mpg.mean = mean(mpg), mpg.sd = sd(mpg)) %>%
#'       mutate(
#'         format = list(c('%(n)d', '%(mpg.mean).1f', '%(mpg.sd).1f')),
#'         indent = 1,
#'         rownames = as.character(am)) %>%
#'       select(-am),
#'     data.frame(rownames = 'By Forward Gear Count'),
#'     mtcars %>% group_by(gear) %>%
#'       summarize(n = n(), mpg.mean = mean(mpg), mpg.sd = sd(mpg)) %>%
#'       mutate(
#'         format = list(c('%(n)d', '%(mpg.mean).2f', '%(mpg.sd).2f')),
#'         indent = 1,
#'         rownames = as.character(gear)) %>%
#'       select(-gear)
#'    ) %>%
#'   column_to_rownames('rownames')
#'
#' t <- rtable(my_data, headers = c('n', 'mpg mean', 'mpg sd'), format, indent)
#'
#' \dontrun{
#' Viewer(t)
#' }
#'
#' @export
rtable.data.frame <- function(data, headers = '', format = '', indent = 0) {
  
  # pull unevaluated args (using names as header/format column name)
  args <- as.list(match.call())[-1]
  
  # handle datatypes for format
  if (is.name(args$format)) {
    format <- data[[deparse(args$format)]]
    format <- Map(function(f) {
      if (is.null(f) || is.na(f)) '' else f
    }, format)
  } else {
    format <- Map(function(f) {
      if (!length(names(f)) && is.character(f)) as.list(f) else f 
    }, format)
    format <- rep(list(format), nrow(data))
  }
  
  # handle datatypes for indent
  if (is.name(args$indent)) {
    indent <- Map(function(i) { 
      if (is.null(i) || is.na(i)) 0 else i
    }, data[[deparse(args$indent)]])
  } else {
    indent <- rep(list(indent), nrow(data))
  }
  
  # construct table from rowwise formatting
  rows_formatted <- Map(function(row_idx, row_format, row_indent) {
    row_f <- Map(function(cell_format) {
      rdata <- data[row_idx,]
      rdata <- rdata[unique(do.call('spf_varnames', as.list(cell_format)))]
      do.call('rcell', c(list(rdata), cell_format))
    }, row_format)
    rrow_args <- c(row.names(data[row_idx,]), row_f, list(indent = row_indent))
    do.call('rrow', rrow_args)
  }, row_idx = 1:nrow(data), row_format = format, row_indent = indent)
  
  # build class structure
  structure(
    rows_formatted,
    col.names = headers,
    ncol = length(headers),
    nrow = nrow(data),
    class = "rtable"
  )
}



#' Reporting Table Row
#' 
#' Defines a row for an \code{\link{rtable}}
#'
#' @param row.name string with row name
#' @inheritParams rtable 
#' @param ... data objects which are wrapped into \code{\link{rcell}} if they
#'   aren't \code{rcell}s already
#' @param indent non-negative integer where 0 means that the row should not be
#'   indented
#' 
#' @details 
#' Note the \code{rrow()} will return an empty row.
#' 
#' @export
#' 
#' @examples 
#' 
#' rrow("ABC", c(1,2), c(3,2))
#' 
rrow <- function(row.name, ..., format = NULL, indent = 0) {
  
  cells <- list(...)
  
  cells_f <- lapply(cells, function(cell) {
    if (is(cell, "rcell")) {
      if (is.null(attr(cell, "format"))) {
        structure(cell, format = format)
      } else {
        cell
      }
    } else {
      rcell(cell, format = format)
    }
  })
  
  
  structure(
    cells_f,
    row.name = if (missing(row.name)) NULL else row.name,
    indent = indent,
    class = "rrow"
  )
}

# Number of non-empty cells in an \code{\link{rrow}} object
# 
# row <- rrow("ABC", rcell(3.23, format = "xx.x", colspan = 2))
# 
ncells <- function(row) {
  if (length(row) == 0) {
    0 
  } else {
    ni <- vapply(row, function(cell) {
      if (is(cell, "rcell")) {
        attr(cell, "colspan")
      } else {
        1
      }
    }, numeric(1))
    sum(ni)
  } 
}

#' Reporting Table Cell
#' 
#' \code{\link{rcell}}s compose an \code{\link{rtable}}. An \code{rcell}
#' contains the encapsulated data object, a format and column span attributes.
#' 
#' @param x data object for the \code{rcell} object. Note that if the data
#'   object has attributes then it needs to be encasultated in a list as 
#'   \code{rcell} adds/modifies the attributes.
#' @inheritParams rtable
#' @param colspan positive integer, number of columns that the cell should span.
#' 
#' @return an object of class \code{rcell}
#' 
#' @export
rcell <- function(x, format = NULL, colspan=1) {
  
  if (!any(is.null(format), is.character(format) && length(format) == 1,
           is.function(format))) {
    stop("format needs to be a format label, a function, or NULL")
  } 
  
  if (missing(x) || is.null(x)) x <- list()
  
  structure(
    x,
    format = format,
    colspan = colspan,
    class = "rcell"
  )
}

check_consistent_ncols <- function(rows, ncols) {
  lapply(rows, function(row) {
    # zero length is possible (label only)
    if (length(row) != 0 && ncells(row) != ncols) {
      stop(paste("row", attr(row, "row.name"), "has", sum(ncells(row)), "cells instead of expected", ncols))
    }
  })
  invisible(TRUE)
}

#' Dimension of rtable object
#' 
#' Retrieve or set the dimension of an \code{rtable} object
#' 
#' @param x an \code{\link{rtable}} object
#' 
#' @return vector of length two with number of rows and number of columns.
#' 
#' @export
#' 
dim.rtable <- function(x) {
  as.vector(unlist(attributes(x)[c("nrow", "ncol")]))
}

#' Row names of an \code{\link{rtable}} object
#' 
#' Retrieve the row names of an \code{\link{rtable}} object
#'   
#' @inheritParams dim.rtable
#' 
#' @return a vector with the row names
#' 
#' @export
row.names.rtable <- function(x) {
  vapply(x, function(row) {
    rn <- attr(row, "row.name")
    if (is.null(rn)) "" else rn
  }, character(1))
}

#' Get column names of an \code{\link{rtable}} object
#' 
#' Retrieve the column names of an \code{\link{rtable}} object
#' 
#' @inheritParams dim.rtable
#' 
#' @return a vector with the column names 
#' 
#' @export
names.rtable <- function(x) {
  attr(x, "col.names")
}


#' Access rcells in an \code{\link{rtable}}
#' 
#' Accessor function
#' 
#' @param x object of class \code{\link{rtable}}
#' @param i row index
#' @param j column index
#' @param ... currently not used
#'
#' @details Note that if a cell spans multiple columns, e.g. the 3 columns
#'   \code{j} to \code{j + 3} then the accessing then \code{x[i, j]}, \code{x[i,
#'   j+1]}, \code{x[i, j+2]}, \code{x[i, j+3]} return the same
#'   \code{\link{rcell}} object.
#'
#' @export
`[.rtable` <- function(x, i, j, ...) {
  
  if (missing(i) || missing(j)) stop("both i and j need to be defined to access elements in rtable")
  
  row <- x[[i]]
  if (length(row) == 0) {
    NULL # no cell information
  } else {
    nc <- ncol(x)
    nci <- vapply(row, function(cell) attr(cell, "colspan") , numeric(1))
    j2 <- rep(1:length(nci), nci)
    
    row[[j2[j]]]
  }
}

