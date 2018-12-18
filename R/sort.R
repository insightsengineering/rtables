
#' Sort Rows in rtable
#'
#' Sort rows in an rtables based on values derived from values within a row.
#'
#' @param x rtable object
#' @param indices of column and cell to access a value within a row that is
#'   uused for sorting the rows. If the column index is 0 then a sum of the cell
#'   values over all columns is taken. If indices is a function then the
#'   function argument is an rrow object and the user has to specify how to
#'   extract the information based on which the rows are sorted.
#' @param ... arguments passed on to \code{\link{order}}
#'
#'
#' @return object of class rtable
#'
#' @export
#'
#' @examples
#' tbl <- rtable(
#'   header = c("A", "B"),
#'   rrow("r1", c(3,1), c(9,8,19)),
#'   rrow("r2", c(5,-1), c(8,9,21)),
#'   rrow("r3", c(1,3), c(3,2,22))
#' )
#'
#' sort_rrows(tbl, c(1, 1))
#' sort_rrows(tbl, c(1, 1))
#'
#' sort_rrows(tbl, c(0, 1))
#'
#' sort_rrows(tbl, c(2, 3), decreasing = FALSE)
#' sort_rrows(tbl, c(2, 3), decreasing = TRUE)
#'
#' sort_rrows(tbl, c(2, 4))
#'
#' sort_rrows(tbl, function(row) row[[2]][3] - row[[1]][1])
#' 
sort_rrows <- function(x, indices = c(1, 1), ...) {
  
  stopifnot(is(x, "rtable"))
  
  vals <- if (is.atomic(indices)) {
    stopifnot(is.numeric(indices))
    stopifnot(length(indices) == 2)
    
    icol <- indices[1]
    icell <- indices[2]
    
    stopifnot(icell > 0)
    stopifnot(icol >= 0)
    
    if (icol == 0) {
      vapply(x, function(row) {sum(vapply(row, `[`, numeric(1), icell))} , numeric(1))
    } else {
      vapply(x, function(row) row[[icol]][icell], numeric(1))
    }
  } else if (is.function(indices)) {
    vapply(x, indices, numeric(1))
  } else {
    stop("index is either an atomic vector of a function")
  }
  
  x[order(vals, ...), ]
}




#' Sort rtables in Nested Lists
#'
#' Sort rtables based on values within the tables.
#'
#' @inheritParams sort_rrows
#' @param x a nested list where the leaves are rtables objects
#' @param indices of row, column, and cell to access a value within a row that
#'   is uused for sorting the rows. If the column index is 0 then a sum of the
#'   cell values over all columns within the row is taken. If indices is a
#'   function then the function argument is an rrow object and the user has to
#'   specify how to extract the information based on which the rows are sorted.
#'
#' @export
#'
#' @examples
#'
#' tbls <- list(
#'    rtable(
#'      header = c("A", "B"),
#'      rrow("r1", c(4,1), c(9,1,19)),
#'      rrow("r2", c(5,-1), c(8,3,21)),
#'      rrow("r3", c(1,3), c(3,4,22))
#'    ),
#'    rtable(
#'      header = c("A", "B"),
#'      rrow("r1", c(6,1), c(9,2,19)),
#'      rrow("r2", c(5,-1), c(8,4,21)),
#'      rrow("r3", c(1,3), c(3,5,22))
#'    ),
#'    rtable(
#'      header = c("A", "B"),
#'      rrow("r1", c(9,1), c(9,0,19)),
#'      rrow("r2", c(5,-1), c(8,1,21)),
#'      rrow("r3", c(1,3), c(3,1,22))
#'    )
#' )
#'
#' sort_rtables_tree(tbls, c(1,1,1))
#' sort_rtables_tree(tbls, c(1,0,1))
#'
#' 
sort_rtables_tree <- function(x, indices = c(1, 0, 1), ...) {
  
  stopifnot(is.numeric(indices) && length(indices) == 3)
  
  if (is(x, "rtable")) {
    x
  } else if (is.list(x)) {
    
    if (all(vapply(x, is, logical(1), "rtable"))) {
      
      vals <- if (is.function(indices)) {
        
        vapply(x, indices, numeric(1))
        
      } else if (is.atomic(indices)) {
        
        irow <- indices[1]
        icol <- indices[2]
        icell <- indices[3]
        
        stopifnot(irow > 0)
        stopifnot(icell > 0)
        stopifnot(icol >= 0)
        
        vapply(x, function(tbl) {
          row <- tbl[[irow]]
          if (icol == 0) {
            sum(vapply(row, `[`, numeric(1), icell))
          } else {
            row[[icol]][icell]
          }
        }, numeric(1))
      } else {
        stop("index must either be atomic or a function")
      }
      
      x[order(vals, ...)]
    } else {
      
      lapply(x, sort_rtables_tree, index = indices, ...)
      
    }
  } else {
    stop("x needs to be either a list or an rtable")
  }
}

