

#' Create a Reporting Table
#' 
#' Reporting tables allow multiple values per cell, cell formatting and merging
#' cells. Currently an \code{rtable} can be converted to html and ascii.
#' 
#' 
#' @param col.names vector with column names
#' @param ... each element is an \code{\link{rrow}} object
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
#'   header = c("Treatement\nN=100", "Comparison\nN=300"),
#'   format = "xx (xx.xx%)",
#'   rrow("A", c(104, .2), c(100, .4)),
#'   rrow("B", c(23, .4), c(43, .5)),
#'   rrow(),
#'   rrow("this is a very long section header"),
#'   rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
#'   rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
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
#'   c("A", "B", "C", "D", "E"),
#'   format = "xx",
#'   rrow("r1", 1, 2, 3, 4, 5),
#'   rrow("r2", rcell("sp2", colspan = 2), "sp1", rcell("sp2-2", colspan = 2))
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
#' # Multi-header table
#' 
#' iris
#' 
#' tbl <- rtable(
#'   header = rheader(
#'     rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan=2)),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "All Species",
#'     mean(iris$Sepal.Length), median(iris$Sepal.Length),
#'     mean(iris$Petal.Length), median(iris$Petal.Length),
#'     format = "xx.xx"
#'   )
#' )
#' 
#' tbl
#' 
#' # custom format
#' my_format <- function(x, output) {
#'    paste(x, collapse = "/")
#' }
#' tbl3 <- rtable(
#'   c("A", "B"),
#'   format = my_format,
#'   rrow("row1", c(1,2,3,4), letters[1:10])
#' )
#' tbl3
#'  
rtable <- function(header, ..., format = "xx") {
  
  if (!is(header, 'rheader')) {
    header <- rheader(header)
  }
  
  ncol_header <- vapply(header, ncells, numeric(1))
  
  
  ## check if n-cols correct
  rows <- list(...)
  if (!all(vapply(rows, is, logical(1), "rrow"))) stop("not all arguments in ... are of class rrow")  
  
  nrow <- length(rows)
  
  ncol_body <- vapply(rows, ncells, numeric(1))
  ncol <- max(ncol_header, ncol_body)
  
  if (!all(ncol_header %in% c(0, ncol))) stop(paste("not all header rows have", ncol, "columns"))
  if (!all(ncol_body %in% c(0, ncol))) stop(paste("not all body rows have", ncol, "columns"))
  
  if (ncol < 1) stop("table needs at least one 1 columns")
  
  rows_formated <- lapply(rows, function(row) {
    
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
    rows_formated,
    header = header,
    ncol = ncol,
    nrow = nrow,
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
#' rrow("ABC", c(1,2), c(3,2), format = "xx (xx.%)")
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


#' Create an rrow with cell-data stored within lists
#' 
#' The apply function family returns lists whose elements can be used as cell
#' data with the \code{lrow} function.
#' 
#' @param ...
#' 
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' x <- tapply(iris$Sepal.Length, iris$Species, mean, simplify = FALSE)
#' 
#' rrow(row.name = "row 1", x)
#' rrow("ABC", 2, 3)
#' 
#' rrowl("row 1", c(1, 2), c(3,4))
#' rrow("row 2", c(1, 2), c(3,4))
#' 
rrowl <- function(row.name = NULL, ...) {
  
  args <- list(...)
  args_list <- c(list(row.name = row.name), unlist(lapply(args, as.list), recursive = FALSE))
  do.call(rrow, args_list)
  
}

# Number of non-empty cells in an \code{\link{rrow}} object
# 
# row <- rrow("ABC", rcell(3.23, format = "xx.x", colspan = 2))
# 
ncells <- function(row) {
  
  if (!is(row, "rrow")) stop("element is not of class rrow")
  
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


#' Create a rheader object
#' 
#' @param ... elements that are either to be mapped to rrows
#' 
#' @export
#' 
#' @examples 
#' h1 <- rheader(c("A", "B", "C"))
#' h2 <- rheader(
#'   rrow(NULL, rcell("group 1", colspan = 2), rcell("group 2", colspan = 2)),
#'   rrow(NULL, "A", "B", "A", "B")
#' )
rheader <- function(..., format = "xx") {
  
  args <- list(...)
  
  rows <- if (length(args) == 1 && !is(args[[1]], "rrow")) {
    list(rrowl(row.name = NULL, args[[1]], format = format))
  } else if (all(vapply(args, is, logical(1), "rrow"))) {
    args
  } else {
    stop("either one one vector or only rrow objects can be passed to ...")
  }
  
  
  structure(
    setNames(rows, NULL),
    class = "rheader"
  )
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

