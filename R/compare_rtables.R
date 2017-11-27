#' Corpare two rtables
#' 
#' Prints a matrix where \code{.} means cell matches, \code{X} means cell does 
#' cells do not match, \code{+} cell (row) is missing, and \code{-} cell (row)
#' should not be there.
#' 
#' @param object rtable to test
#' @param expected rtable expected
#' @param tol numerical tolorance
#' @param comp.attr boolean compare attributes
#' 
#' 
#' @export
#' 
#' @examples 
#' 
#' t1 <- rtable(col.name = c("A", "B"), format = "xx", rrow("row 1", 1, 2))
#' t2 <- rtable(col.name = c("A", "B", "C"), format = "xx", rrow("row 1", 1, 2, 3))
#'
#' compare_rtables(object = t1, expected = t2) 
#' 
#' \dontrun{
#' Viewer(t1, t2) 
#' }
#' 
#' expected <- rtable(
#'    col.names = c("ARM A\nN=100", "ARM B\nN=200"),
#'    format = "xx",
#'    rrow("row 1", 10, 15),
#'    rrow(),
#'    rrow("section title"),
#'    rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
#' )
#' 
#' \dontrun{
#' Viewer(expected)
#' }
#' 
#' object <- rtable(
#'    col.names = c("ARM A\nN=100", "ARM B\nN=200"),
#'    format = "xx",
#'    rrow("row 1", 10, 15),
#'    rrow("section title"),
#'    rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
#' )
#' 
#' compare_rtables(object, expected)
#' 
#' compare_rtables(object, expected, comp.attr = FALSE)
#' 
#' object <- rtable(
#'    col.names = c("ARM A\nN=100", "ARM B\nN=200"),
#'    format = "xx",
#'    rrow("row 1", 10, 15),
#'    rrow(),
#'    rrow("section title")
#' )
#' 
#' compare_rtables(object, expected)
#' 
#' object <- rtable(
#'    col.names = c("ARM A\nN=100", "ARM B\nN=200"),
#'    format = "xx",
#'    rrow("row 1", 14, 15.03),
#'    rrow(),
#'    rrow("section title"),
#'    rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
#' )
#' 
#' compare_rtables(object, expected)
#' 
#' object <- rtable(
#'    col.names = c("ARM A\nN=100", "ARM B\nN=200"),
#'    format = "xx",
#'    rrow("row 1", 10, 15),
#'    rrow(),
#'    rrow("section title"),
#'    rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.x, xx.x)"))
#' )
#' 
#' compare_rtables(object, expected)
#' 
compare_rtables <- function(object, expected, tol=0.1, comp.attr = TRUE) {
  
  # if (identical(object, expected)) return(invisible(TRUE))
  
  if (!is(object, "rtable")) stop("argument object is expected to be of class rtable")
  if (!is(expected, "rtable")) stop("argument expected is expected to be of class rtable")
  
  dim_out <- apply(rbind(dim(object), dim(expected)), 2, max)
  
  X <- matrix(rep(".", dim_out[1] * dim_out[2]), ncol = dim_out[2])
  row.names(X) <- as.character(1:dim_out[1])
  colnames(X) <-  as.character(1:dim_out[2])
  
  if (!identical(names(object), names(expected))) {
    attr(X, "info") <- "column names are not the same"
  }
  
  if (!comp.attr) {
    attr(X, "info") <- c(attr(X, "info"), "cell attributes have not been compared")
  }
  
  nro <- nrow(object)
  nre <- nrow(expected)
  nco <- ncol(object)
  nce <- ncol(expected)
  
  for (i in 1:dim(X)[1]) {
    for (j in 1:dim(X)[2]) {
      
      is_equivalent <- TRUE
      if (i <= nro && i <= nre && j <= nco && j <= nce) {
        x <- object[i,j]
        y <- expected[i, j]
        
        attr_x <- attributes(x)
        attr_y <- attributes(y)
        
        attr_x_sorted <- if (is.null(attr_x)) NULL else attr_x[order(names(attr_x))]
        attr_y_sorted <- if (is.null(attr_y)) NULL else attr_y[order(names(attr_y))]
        
        if (comp.attr && !identical(attr_x_sorted, attr_y_sorted)) {
          is_equivalent <- FALSE
        } else if (is.numeric(x) && is.numeric(y)) {
          if (any(abs(na.omit(x - y)) > tol)) {
            is_equivalent <- FALSE
          }
        } else {
          if (!identical(x, y)) {
            is_equivalent <- FALSE
          }
        }
        
        if (!is_equivalent) {
          X[i,j] <- "X"
        }
      } else if (i > nro || j > nco) {
        ## missing in object
        X[i, j] <- "+"
      } else {
        ## too many elements
        X[i, j] <- "-"
      }
    } 
  }
  class(X) <- c("rtable_diff", class(X))
  X
}

print.rtable_diff <- function(x, ...) {
  print.default(unclass(x), quote = FALSE, ...)
}
