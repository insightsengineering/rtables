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
#' by_factor_to_matrix(x)
by_factor_to_matrix <- function(x) {
  stopifnot(is.factor(x)) #todo: replace by is.factor.single or .vector
  res <- matrix(FALSE, nrow = length(x), ncol = nlevels(x))
  colnames(res) <- levels(x)
  for (level in levels(x)) {
    res[x == level, level] <- TRUE
  }
  data.frame(res)
}

#' converts to matrix if needed (if it is a factor)
#' handles case when it is of class by_all as well
#' x can be NULL if col_by is not of class by_all
#' returns transformed col_by as a matrix
#' @param col_by factor or data.frame to convert
#' @param x object that col_by is applied to, necessary if col_by is of class by_all to select all elements from x
col_by_to_matrix <- function(col_by, x = NULL) {
  new_col_by <- if (is.factor(col_by)) {
    by_factor_to_matrix(col_by)
  } else if (is.data.frame(col_by)) {
    col_by
  } else if (is(col_by, "by_all")) {
    stopifnot(!is.null(x))
    res <- data.frame(rep(TRUE, length(x)))
    colnames(res) <- col_by
    res
  } else {
    stop(paste("unknown class to convert", class(col_by)))
  }
  if (!is.null(x)) {
    stopifnot(nrow(col_by) == length(x))
  }
  stopifnot(
    all(vapply(new_col_by, function(col) is.logical.vector_modif(col, min_size = 0), logical(1)))
  )
  new_col_by
}

#' Adds column to matrix that corresponds to taking all entries (column of all TRUE)
#' 
#' @param mat factor or matrix to add column to, e.g. output of \code{\link{by_factor_to_matrix}}
#' @param label label of new column
#' 
#' @return new matrix with one column added
#' 
#' @export
#' 
#' @examples 
#' x <- factor(c("a", "b", "a", "a", "b"))
#' mat <- by_factor_to_matrix(x)
#' by_add_total(mat, label = "tot")
#' by_add_total(x, label = "tot")
by_add_total <- function(x, label = "total") {
  stopifnot(is.data.frame(x) || is.factor(x))
  mat <- col_by_to_matrix(x)
  old_names <- names(mat)
  res <- cbind(mat, rep(TRUE, nrow(mat)))
  names(res) <- c(old_names, label)
  res
}

#' Do not split data into columns in \code{rtabulate}
#' 
#' \code{\link{rtabulate}} has the argument \code{col_by}
#' which can either take a factor, a data frame. To select all rows, use
#' \code{by_all} which is equivalent to selecting all rows
#' 
#' @param name character column name to display in the table header
#' 
#' @export
#' 
#' @examples 
#' 
#' rtabulate(iris$Species, col_by = by_all("Total"))

by_all <- function(name) {
  structure(name, class = "by_all")
}