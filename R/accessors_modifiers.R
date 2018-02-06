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
  row_i <- attr(x, "header")[[1]]
  
  unlist(lapply(row_i, function(cell) {
    colspan <- attr(cell, "colspan")
    rep(cell, colspan)
  }))
  
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



#' @export
set_rrow_attrs <- function(rrow, row.name, indent) {
  if (!is(rrow, "rrow")) stop("object of class rrow expected") 
  
  if (!missing(row.name)) {
    if (!is.character(row.name) || length(rowname) != 1) stop("row.name is expected to be a character string (vector of length 1)")
    attr(rrow, "row.name") <- row.name
  }
  
  if (!missing(indent)) {
    if (!is.numeric(indent) || indent < 0 || !(1.0 %% 1 == 0)) stop("indent is expected to be a positive integer")
    attr(rrow, "indent") <- indent
  }
  
  rrow
}

#' stack rtable objects 
#' @export
#' 
#' @examples 
#' 
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
#' mtbl2 <- with(subset(iris, Species == 'setosa'), rtable(
#'   header = rheader(
#'     rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan=2)),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "Setosa",
#'     mean(Sepal.Length), median(Sepal.Length),
#'     mean(Petal.Length), median(Petal.Length),
#'     format = "xx.xx"
#'   )
#' ))
#' 
#' tbl <- rbind(mtbl, mtbl2)
#' 
#' tbl
#' 
rbind.rtable <- function(...) {
  
  dots <- list(...)
  
  if (!are(dots, "rtable")) stop("not all elements are of type rtable")
  
  header <- attr(dots[[1]], "header")
  
  same_headers <- vapply(dots[-1], function(x) {
    identical(attr(x, "header"), header)
  }, logical(1))
  
  if (!all(same_headers)) stop("not all rtables have the same header")
  
  body <- unlist(dots, recursive = FALSE)
  
  rtablel(header = header, body)
}
