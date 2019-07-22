#' Convert a factor to a matrix by, each column corresponding to a factor level
#' 
#' Each column corresponds to a factor level and each row is TRUE for all indexes 
#' where factor equals this factor level
#' @param x factor to convert to matrix_by
#' 
#' @return matrix of size length(x) times nlevels(x)
#' 
#' @export
#' 
#' @examples 
#' x <- factor(c("a", "b", "a", "a", "b"))
#' factor_to_matrix_by(x)
factor_to_matrix_by <- function(x) {
  stopifnot(is.factor(x))
  res <- matrix(FALSE, nrow = length(x), ncol = nlevels(x))
  colnames(res) <- levels(x)
  for (level in levels(x)) {
    res[x == level, level] <- TRUE
  }
  data.frame(res)
}


#' Adds column to matrix that corresponds to taking all entries (column of all TRUE)
#' 
#' @param mat matrix to add column to, e.g. output of \code{\link{factor_to_matrix_by}}
#' @param label label of new column
#' 
#' @return new matrix with one column added
#' 
#' @export
#' 
#' @examples 
#' x <- factor(c("a", "b", "a", "a", "b"))
#' mat <- factor_to_matrix_by(x)
#' add_total_by(mat, label = "tot")
add_total_by <- function(mat, label = "total") {
  stopifnot(is.data.frame(mat))
  stopifnot(all(vapply(mat, function(col) is.logical(col), logical(1))))
  old_names <- names(mat)
  res <- cbind(mat, rep(TRUE, nrow(mat)))
  names(res) <- c(old_names, label)
  res
}
