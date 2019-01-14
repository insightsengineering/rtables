

#' Sort rrows in rtable
#'
#' Return order of rows in rtable based on derived values within each row.
#'
#' @param x rtable object
#' @param indices of column and cell within a row to access value 
#'   used for sorting. If the column index is 0 then the sum of the cell
#'   values across all columns is derived and used for sorting. 
#'   If \code{indices} is a function then the function argument is 
#'   rrow object and the user must specify how to extract the information 
#'   required to sort the rows. See examples.
#' @param ... arguments passed on to \code{\link{order}}
#'
#' @export
#' 
#' @return order of rows
#' 
#' @seealso \code{\link{sort_rrows}}, \code{\link{order_rtables}},
#'   \code{\link{sort_rtables}} 
#'   
#' @examples
#' tbl <- rtable(
#'   header = c("A", "B"),
#'   rrow("r1", c(3,1), c(9,8,19)),
#'   rrow("r2", c(4,-1), c(8,9,21)),
#'   rrow("r3", c(1,3), c(3,2,22))
#' ) 
#' 
#' order_rrows(tbl, c(1,1))
#' order_rrows(tbl, c(2, 3), decreasing = TRUE)
#' order_rrows(tbl, c(0,2))
#' 
#' order_rrows(tbl, function(row) row[[2]][3] - row[[1]][1])

order_rrows <- function(x, indices = c(1, 1), ...) {
  
  stopifnot(is(x, "rtable"))
  
  vals <- if (is.atomic(indices)) {
    stopifnot(is.numeric(indices))
    stopifnot(length(indices) == 2)
    
    icol <- indices[1]
    icell <- indices[2]
    
    stopifnot(icell > 0)
    stopifnot(icol >= 0)
    
    if (icol == 0) {
      vapply(x, function(row) {sum(vapply(row, `[[`, numeric(1), icell))} , numeric(1))
    } else {
      vapply(x, function(row) row[[icol]][[icell]], numeric(1))
    }
  } else if (is.function(indices)) {
    vapply(x, indices, numeric(1))
  } else {
    stop("index is either an atomic vector of a function")
  }
  order(vals, ...)
}

#' Sort Rows in rtable
#'
#' Sort rows in rtable based on derived values within each row.
#'
#' @inheritParams order_rrows
#'
#' @return object of class rtable
#'
#' @export
#'
#' @seealso \code{\link{order_rrows}}, \code{\link{order_rtables}},
#'   \code{\link{sort_rtables}} 
#'
#' @examples
#' tbl <- rtable(
#'   header = c("A", "B"),
#'   rrow("r1", c(3,1), c(9,8,19)),
#'   rrow("r2", c(4,-1), c(8,9,21)),
#'   rrow("r3", c(1,3), c(3,2,22))
#' )
#'
#' sort_rrows(tbl, c(1, 1))
#' sort_rrows(tbl, c(0, 1))
#'
#' sort_rrows(tbl, c(2, 3), decreasing = FALSE)
#' sort_rrows(tbl, c(2, 3), decreasing = TRUE)
#'
#' sort_rrows(tbl, function(row) row[[2]][3] - row[[1]][1])
#' 
sort_rrows <- function(x, indices = c(1, 1), ...) {
  x[order_rrows(x, indices , ...), ]
}


#' Order of rtables in a list
#'
#' Return the order of rtables in a list based on values within the tables.
#'
#' @inheritParams sort_rrows
#' @param x a list of rtables
#' @param indices of row, column, and cell to access a value within each rtable that
#'   is used for sorting the list \code{x}. If the column index is 0 then a sum of the
#'   cell values across all columns within the specified row is taken. If \code{indices}
#'   is a function then the function argument is rtable object and the user must
#'   specify how to extract the information required to sort \code{x}. See examples.
#'
#' @return order of rtables
#' 
#' @export
#'
#' @seealso \code{\link{sort_rtables}}, \code{\link{sort_rrows}}, 
#'   \code{\link{order_rrows}}
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
#'      rrow("r1", c(1,1), c(100,0,19)),
#'      rrow("r2", c(5,-1), c(8,1,21)),
#'      rrow("r3", c(1,3), c(3,1,22))
#'    )
#' )
#'
#' order_rtables(tbls, c(1,1,1))
#' order_rtables(tbls, c(1,1,1), decreasing = TRUE)
#' 
#' order_rtables(tbls, c(1,0,1))
#' 
#' order_rtables(tbls, function(tbl) tbl[[1]][[2]][3] - tbl[[1]][[1]][1]  )

order_rtables <- function(x, indices = c(1,0,1), ...) {
  
  stopifnot(is.list(x) && are(x, "rtable"))
  
  vals <- if (is.function(indices)) {
    vapply(x, indices, numeric(1))
  } else if (is.atomic(indices) && length(indices) == 3) {
    irow <- indices[1]
    icol <- indices[2]
    icell <- indices[3]
    
    if (icol == 0) {
      vapply(x, function(tbl) {sum(vapply(tbl[[irow]], `[[`, numeric(1), icell))} , numeric(1))
    } else {
      vapply(x, function(tbl) tbl[[irow]][[icol]][[icell]], numeric(1))
    }
    
  } else {
    stop("indices needs to be a vector of length 3 or a function")
  }
  order(vals, ...)
}

#' Sort rtables within a list
#' 
#' Return a sorted list of rtables in a list based on values within the tables.
#' 
#' @inheritParams order_rtables
#' 
#' @return list of rtables
#' 
#' @export
#' 
#' @seealso \code{\link{order_rtables}}, \code{\link{sort_rrows}}, 
#'   \code{\link{order_rrows}}
#' 
#' @examples
#'   
#' tbls <- list(
#'    "Table A"  = rtable(
#'      header = c("A", "B"),
#'      rrow("r1", c(4,1), c(9,1,19)),
#'      rrow("r2", c(5,-1), c(8,3,21)),
#'      rrow("r3", c(1,3), c(3,4,22))
#'    ),
#'    "Table B" = rtable(
#'      header = c("A", "B"),
#'      rrow("r1", c(6,1), c(9,2,19)),
#'      rrow("r2", c(5,-1), c(8,4,21)),
#'      rrow("r3", c(1,3), c(3,5,22))
#'    ),
#'    "Table C" = rtable(
#'      header = c("A", "B"),
#'      rrow("r1", c(1,1), c(100,0,19)),
#'      rrow("r2", c(5,-1), c(8,1,21)),
#'      rrow("r3", c(1,3), c(3,1,22))
#'    )
#' )
#'
#' sort_rtables(tbls, c(1,1,1))
#' sort_rtables(tbls, c(1,1,1), decreasing = TRUE)
#' 
#' sort_rtables(tbls, c(1,0,1))
#' 
#' sort_rtables(tbls, function(tbl) tbl[[1]][[2]][3] - tbl[[1]][[1]][1]  )
#' 
sort_rtables <- function(x, indices = c(1,0,1), ...) {
  x[order_rtables(x, indices , ...)]
}
