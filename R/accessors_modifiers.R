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
  c(attr(x, "nrow"), attr(x, "ncol"))
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
dim.rheader <- function(x) {
  c(attr(x, "nrow"), attr(x, "ncol"))
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

#' Row names of an \code{\link{rtable}} object with spaces
#' 
#' Retrieve the row names of an \code{\link{rtable}} object
#'   
#' @inheritParams dim.rtable
#' @param spaces numeric number of spaces per indent level
#' 
#' @return a vector with the row names
#' 
#' @export
indented_row.names <- function(x, spaces = 2) {
  
  if (!is(x, "rtable") && !is(x, "rheader")) stop("x is required to be a rtable or a rheader")
  
  if (spaces < 0) stop("spaces needs to be >= 0")
  
  vapply(x, function(row) {
    rn <- attr(row, "row.name")
    indent <- strrep(" ", attr(row, "indent")*spaces)
    if (is.null(rn)) "" else paste0(indent, rn)
  }, character(1))
}

#' Return Indentation of Header Rows
#' 
#' @inheritParams header
#' 
#' @export
#' 
#' @examples 
#' tbl <- rtable(
#'   header = rheader(
#'     rrow("", "A", "B"),
#'     rrow("", "X", "Y")
#'   ),
#'   rrow("row 1", 1, 2)
#' )
#' 
#' tbl
#' header_indent(tbl)
header_indent <- function(x) {
  stopifnot(is(x, "rtable"))
  
  h <- header(x)
  vapply(h, function(xi) attr(xi, "indent"), numeric(1))
}

#' Return Indentation of Header Rows
#' 
#' @inheritParams header
#' @param value non-negative inegers of indent of header rows
#' 
#' @export
#' 
#' @examples 
#' tbl <- rtable(
#'   header = rheader(
#'     rrow("A", "A", "B"),
#'     rrow("B", "X", "Y")
#'   ),
#'   rrow("row 1", 1, 2)
#' )
#' 
#' tbl
#' header_indent(tbl) <- c(0, 1)
#' tbl
`header_indent<-` <- function(x, value) {
  stopifnot(
    is(x, "rtable"), 
    is.numeric(value),
    all(value >= 0),
    length(value) == length(header(x))
  )
  
  h <- header(x)
  
  for (i in seq_along(value)) {
    attr(h[[i]], "indent") <- value[i]
  }
  header(x) <- h
  x
}




#' change row names of rtable
#' 
#' @param x an \code{\link{rtable}} object
#' @param value character vector with row names
#' 
#' @export
#' 
#' @examples 
#' 
#' tbl <- rtable(header = c("A", "B"), rrow("row 1", 1, 2))
#' tbl
#' row.names(tbl) <- "Changed Row Name"
#' tbl
`row.names<-.rtable` <- function(x, value) {

  nr <- nrow(x)
  
  if (length(value) != nr) stop("dimension mismatch")
  
  for (i in seq_along(x)) {
    attr(x[[i]], "row.name") <- value[i]
  }
  
  x
}

#' Row names of an \code{\link{rheader}} object
#' 
#' Retrieve the row names of an \code{\link{rheader}} object
#'   
#' @inheritParams row.names.rtable
#' 
#' @return a vector with the row names
#' 
#' @export
row.names.rheader <- function(x) {
  row.names.rtable(x)
}



#' Get content of first header row of an \code{\link{rtable}} object
#' 
#' Retrieve the content of the first header row of an \code{\link{rtable}} object
#' 
#' @inheritParams dim.rtable
#' 
#' @return a vector with the column names 
#' 
#' @export
names.rtable <- function(x) {
  row_i <- attr(x, "header")[[1]]
  
  unlist(lapply(row_i, function(cell) {
    colspan <- attr(cell, "colspan")
    rep(cell, colspan)
  }))
  
}


#' Get Header ot Rtable
#' 
#' 
#' @param x an rtable object
#' 
#' @return an \code{\link{rheader}} object
#' 
#' @export
#' 
#' @examples 
#' x <- rtable(header = letters[1:3], rrow("row 1", 1,2,3)) 
#' header(x)
old_header <- function(x) {
  
  if (!is(x, "rtable")) stop("x is required to be an object of class rtable")
  
  attr(x, "header")
}

#' Change Header of Rtable
#' 
#' @inheritParams header
#' @param value an \code{\link{rheader}} object
#' 
#' @export
#' 
#' @examples 
#' x <- rtable(header = letters[1:3], rrow("row 1", 1,2,3))
#' header(x) <- rheader(rrow("a", "a", "b", "d"))
#' x
`old_header<-` <- function(x, value) {
  if (!is(x, "rtable")) stop("x is not an rtable")
  
  if (!is(value, 'rheader')) value <- rheader(value)
  
  if (ncol(x) != ncol(value)) stop("number of columns do not match")
  
  attr(x, "header") <- value
  x
}


#' Access Header Row Names
#' 
#' @inheritParams header
#' 
#' @export
#' 
#' @examples 
#' tbl <- rtable(
#'   header = rheader(
#'     rrow("", "A", "B"),
#'     rrow("", "X", "Y")
#'   ),
#'   rrow("row 1", 1, 2)
#' )
#' 
#' tbl
#' header_row.names(tbl)
header_row.names <- function(x) {
  stopifnot(is(x, "rtable"))
  
  row.names(header(x))
}

#' Modify Header Row Names
#' 
#' @inheritParams header
#' @param value vector with new row names
#' 
#' @export
#' 
#' @examples 
#' tbl <- rtable(
#'   header = rheader(
#'     rrow("", "A", "B"),
#'     rrow("", "X", "Y")
#'   ),
#'   rrow("row 1", 1, 2)
#' )
#' 
#' tbl
#' header_row.names(tbl) <- c("S", "T")
#' tbl
`header_row.names<-` <- function(x, value) {
  stopifnot(
    is(x, "rtable"), 
    is.character(value) # note cannot delete them with NULL
  )
  
  h <- header(x)
  
  if (length(h) != length(value)) {
    stop("length of assigned value does not match number of rows in header")
  }
  
  for (i in seq_along(value)) {
    attr(h[[i]], "row.name") <- value[i]
  }
  
  header(x) <- h
  x
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
  
  if (missing(i) && missing(j)) {
    x
  } else if (missing(j) && !missing(i)) {  
    # subset the table (rows)
    
    rtablel(header = attr(x, "header"), unclass(x)[i])
    
  } else if (!missing(i) && !missing(j) && is.numeric(i) && is.numeric(j) && length(i) == 1 && length(j) == 1) {
    # access a single rcell
    
    if (!(i > 0 && i <= nrow(x) && j > 0 && ncol(x))) stop("index out of bound")
    
    row <- unclass(x)[[i]]
    if (length(row) == 0) {
      NULL # no cell information
    } else {
      nc <- ncol(x)
      nci <- vapply(row, function(cell) attr(cell, "colspan") , numeric(1))
      j2 <- rep(1:length(nci), nci)
      row[[j2[j]]]
    }
    
  } else {
    stop("accessor function `[` for rtable does currently not support the the requested indexing, see ?`[.rtable`")
  }
  
}


#' access cell in rheader
#' 
#' 
#' @param x an \code{\link{rtable}} object
#' @param i row index
#' @param j col index
#' @param ... arguments passed forward to \code{\link{[.rtable}}
#' 
#' @export
`[.rheader` <- function(x, i, j, ...) {
  `[.rtable`(x, i, j, ...)
}

set_rrow_attrs <- function(rrow, row.name, indent) {
  if (!is(rrow, "rrow")) stop("object of class rrow expected") 
  
  if (!missing(row.name)) {
    if (!is.character(row.name) || length(row.name) != 1) stop("row.name is expected to be a character string (vector of length 1)")
    attr(rrow, "row.name") <- row.name
  }
  
  if (!missing(indent)) {
    if (!is.numeric(indent) || indent < 0 || !(1.0 %% 1 == 0)) stop("indent is expected to be a positive integer")
    attr(rrow, "indent") <- indent
  }
  
  rrow
}

