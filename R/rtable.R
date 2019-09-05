

#' Create a Reporting Table
#' 
#' Reporting tables allow multiple values per cell, cell formatting and merging
#' cells. Currently an \code{rtable} can be converted to html and ascii.
#' 
#' 
#' @param header either a vector with column names or an object returned by
#'   \code{\link{rheader}} if special formating and multi-line headers are
#'   needed
#' @param ... each element is an \code{\link{rrow}} object
#' @param format a valid format string or a format function for
#'   \code{\link{rcell}}s. To get a list of all valid format strings use
#'   \code{\link{list_rcell_format_labels}}. If format is \code{NULL} then the elements
#'   of a cell get pasted separated by a comma.
#' 
#' @return an \code{rtable} object
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
#' @importFrom htmltools tags tagList
#' 
#' @export
#' 
#' @author Adrian Waddell \email{adrian.waddell@roche.com}
#' 
#' @seealso \code{\link{rrow}}, \code{\link{rcell}}
#' 
#' @examples
#' 
#' # Table with multirow header
#' mtbl <- rtable(
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
#' mtbl
#' 
#' names(mtbl) # always first row of header
#' 
#' # Single row header
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
#' # Subsetting
#' tbl[1,2]
#' tbl[3,2]
#' tbl[5,1]
#' tbl[5,2]
#' tbl[1:3]
#' 
#' 
#' # Data Structure methods
#' dim(tbl)
#' nrow(tbl)
#' ncol(tbl)
#' names(tbl)
#' 
#' 
#' # Output: html
#' as_html(tbl)
#' 
#' \dontrun{
#' Viewer(tbl)
#' }
#' 
#' # Colspans
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
#' 
#' # Custom format with functions (might be deprecated soon)
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
rtable <- function(header, ..., format = NULL) {
  
  is_rcell_format(format, stop_otherwise = TRUE)
  
  if (!is(header, 'rheader')) header <- rheader(header)
  
  body <- list(...)
  if (!are(body, "rrow")) stop("not all arguments in ... are of class rrow")  
  
  ncol_header <- vapply(header, ncell, numeric(1))
  ncol_body <- vapply(body, ncell, numeric(1))

  nrow <- length(body)  
  ncol <- max(c(ncol_header, ncol_body, 0))
  
  if (ncol < 1) stop("table needs at least one 1 columns")
  # because empty rows have currently 0 cells
  if (!all(ncol_header %in% c(0, ncol))) stop(paste("not all header rows have", ncol, "columns"))
  if (!all(ncol_body %in% c(0, ncol))) stop(paste("not all body rows have", ncol, "columns"))
  
  body_formatted <- lapply(body, propagate_format_to_rcells, format = format)
    
  structure(
    body_formatted,
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
  
  is_rcell_format(format, stop_otherwise = TRUE)
  if (!is.numeric(indent) || indent < 0) stop("indent must be >= 0")
  
  cells <- list(...)
  
  rcells_formatted <- lapply(cells, function(cell) {
    if (is(cell, "rcell")) {
      if (is.null(attr(cell, "format"))) {
        attr(cell, "format") <- format
      }
      cell
    } else {
      rcell(cell, format = format)
    }
  })
  
  
  structure(
    rcells_formatted,
    row.name = if (missing(row.name)) NULL else row.name,
    indent = indent,
    class = "rrow"
  )
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
  
  is_rcell_format(format, stop_otherwise = TRUE)
  
  if (missing(x) || is.null(x)) x <- list() # empty cells
  
  structure(
    x,
    format = format,
    colspan = colspan,
    class = "rcell"
  )
}


#' Create a rheader object
#' 
#' @param ... elements that are either to be mapped to rrows or a list of rr
#' @param format default format
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
  
  rrows <- if (length(args) == 1 && !is(args[[1]], "rrow")) {
    list(rrowl(row.name = NULL, args[[1]], format = format))
  } else if (are(args, "rrow")) {
    lapply(args, propagate_format_to_rcells, format = format)
  } else {
    stop("either one one vector or rrow objects can be passed to ...")
  }
  
  ncol <- vapply(rrows, ncell, numeric(1))
  if (!all(duplicated(ncol)[-1])) stop("number of columns to not match")
  
  structure(
    setNames(rrows, NULL),
    nrow = length(rrows),
    ncol = ncol[1],
    class = "rheader"
  )
}

rheaderl <- function(lst, format = "xx") {
    stopifnot(are(lst, "rrow"))
    do.call(rheader, c(list(format = format), lst))
}


#' Create an rheader with header-cell data stored within lists
#' 
#' The apply function family returns lists whose elements can be used as cell
#' data with the \code{lrow} function.
#' 
#' @inheritParams rheader
#' @param ... lists that get concatenated and then flattened by one level of
#'   depth. If one elemenet is not a list then it gets placed into a list.
#' 
#' 
#' @export
#' 
#' 


rheaderl <-  function(..., format = NULL) {
  dots <- list(...)
  args_list <- c(list(format = format), unlist(lapply(dots, as.list), recursive = FALSE))
  do.call(rheader, args_list)
}



#' Create an rrow with cell-data stored within lists
#' 
#' The apply function family returns lists whose elements can be used as cell
#' data with the \code{lrow} function.
#' 
#' @inheritParams rrow
#' @param ... lists that get concatenated and then flattened by one level of
#'   depth. If one elemenet is not a list then it gets placed into a list.
#' 
#' 
#' @export
#' 
#' 
#' @examples 
#' 
#' rrowl("a", c(1,2,3), format = "xx")
#' rrowl("a", c(1,2,3), c(4,5,6), format = "xx")
#' 
#' 
#' rrowl("N", table(iris$Species))
#' rrowl("N", table(iris$Species), format = "xx")
#' 
#' x <- tapply(iris$Sepal.Length, iris$Species, mean, simplify = FALSE)
#' 
#' rrow(row.name = "row 1", x)
#' rrow("ABC", 2, 3)
#' 
#' rrowl(row.name = "row 1", c(1, 2), c(3,4))
#' rrow(row.name = "row 2", c(1, 2), c(3,4))
#' 
rrowl <- function(row.name, ..., format = NULL, indent = 0) {
  dots <- list(...)
  args_list <- c(
    list(row.name = row.name, format = format, indent = indent), 
    unlist(lapply(dots, as.list), recursive = FALSE)
  )

  do.call(rrow, args_list)
}

#' Create an rtable from rrows stored in a list
#' 
#' This function is useful to create \code{\link{rtable}} objects with lists of
#' rrows that are returned by the apply function family.
#' 
#' @inheritParams rtable
#' @param ... lists with \code{\link{rrow}} objects
#' 
#' @return \code{\link{rtable}} object
#' 
#' @export
rtablel <- function(header, ..., format = NULL) {
  dots <- list(...)
  args_list <- c(list(header = header, format = format), unlist(lapply(dots, as.list), recursive = FALSE))
  do.call(rtable, args_list)
}

# propageate default formats to the rcells
propagate_format_to_rcells <- function(rrow, format) {
  if (!is(rrow, "rrow")) stop("object of type rrow expected")
  rrow_formatted <- lapply(rrow, function(rcell) {
    if (is.null(attr(rcell, "format"))) {
      attr(rcell, "format") <- format
    } 
    rcell
  })
  attributes(rrow_formatted) <- attributes(rrow) 
  rrow_formatted
}


# Number of non-empty cells in an \code{\link{rrow}} object
# 
# row <- rrow("ABC", rcell(3.23, format = "xx.x", colspan = 2))
# 
ncell <- function(rrow) {
  
  if (!is(rrow, "rrow")) stop("element is not of class rrow")
  
  if (length(rrow) == 0) {
    0 
  } else {
    ni <- vapply(rrow, function(cell) {
      if (is(cell, "rcell")) {
        attr(cell, "colspan")
      } else {
        1
      }
    }, numeric(1))
    sum(ni)
  } 
}





