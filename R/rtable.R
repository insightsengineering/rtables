## nocov

## TODO: REMOVE RELIANCE ON THESE FUNCTIONS IN .tbl_header_mat!!!


## ' Reporting Table Row
## ' 
## ' Defines a row for an \code{\link{old_rtable}}
## '
## ' @param row.name string with row name
## ' @inheritParams old_rtable 
## ' @param ... data objects which are wrapped into \code{\link{rcell}} if they
## '   aren't \code{rcell}s already
## ' @param indent non-negative integer where 0 means that the row should not be
## '   indented
## ' 
## ' @details 
## ' Note the \code{old_rrow()} will return an empty row.
## ' 
## ' 
## ' @examples 
## ' 
## ' old_rrow("ABC", c(1,2), c(3,2), format = "xx (xx.%)")
## ' old_rrow()
## ' 
old_rrow <- function(row.name = NULL, ..., format = NULL, indent = 0) {
  
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
      old_rcell(cell, format = format)
    }
  })
  
  
  structure(
    rcells_formatted,
    row.name = row.name,
    indent = indent,
    class = "rrow"
  )
}


## ' Reporting Table Cell
## ' 
## ' \code{\link{rcell}}s compose an \code{\link{old_rtable}}. An \code{rcell}
## ' contains the encapsulated data object, a format and column span attributes.
## ' 
## ' @param x data object for the \code{rcell} object. Note that if the data
## '   object has attributes then it needs to be encasultated in a list as 
## '   \code{rcell} adds/modifies the attributes.
## ' @inheritParams old_rtable
## ' @param colspan positive integer, number of columns that the cell should span.
## ' 
## ' @return an object of class \code{rcell}
## ' 
old_rcell <- function(x, format = NULL, colspan = 1) {
  
  is_rcell_format(format, stop_otherwise = TRUE)
  
  if (missing(x) || is.null(x)) x <- list() # empty cells
  
  structure(
    x,
    format = format,
    colspan = colspan,
    class = "rcell"
  )
}



## ' Create an rrow with cell-data stored within lists
## ' 
## ' The apply function family returns lists whose elements can be used as cell
## ' data with the \code{lrow} function.
## ' 
## ' @inheritParams rrow
## ' @param ... lists that get concatenated and then flattened by one level of
## '   depth. If one elemenet is not a list then it gets placed into a list.
## ' 
## ' 
## ' 
## '
## '
## no longer exported
## ' @examples 
## ' 
## ' old_rrowl("a", c(1,2,3), format = "xx")
## ' old_rrowl("a", c(1,2,3), c(4,5,6), format = "xx")
## ' 
## ' 
## ' old_rrowl("N", table(iris$Species))
## ' old_rrowl("N", table(iris$Species), format = "xx")
## ' 
## ' x <- tapply(iris$Sepal.Length, iris$Species, mean, simplify = FALSE)
## ' 
## ' old_rrow(row.name = "row 1", x)
## ' old_rrow("ABC", 2, 3)
## ' 
## ' old_rrowl(row.name = "row 1", c(1, 2), c(3,4))
## ' old_rrow(row.name = "row 2", c(1, 2), c(3,4))
## ' 
old_rrowl <- function(row.name, ..., format = NULL, indent = 0) {
  dots <- list(...)
  args_list <- c(
    list(row.name = row.name, format = format, indent = indent), 
    unlist(lapply(dots, as.list), recursive = FALSE)
  )

  do.call(old_rrow, args_list)
}

## nocov end
