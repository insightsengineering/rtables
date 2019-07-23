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
  res <- data.frame(res)
  colnames(res) <- levels(x) # doing this after data.frame call makes sure that special chars are preserved
  res
}

#' Converts col_by to matrix if needed (if it is a factor)
#' Also handles case when it is of class by_all
#' x can be NULL if col_by is not of class by_all
#' returns transformed col_by as a matrix
#' 
#' @param col_by factor or data.frame to convert
#' @param x object that col_by is applied to, must be non-NULL if col_by is of class by_all to select all elements from x
#' 
#' @return matrix col_by
#' 
#' @export
#' 
#' @examples 
#' col_by <- c("a", "b", "a", "a", "b")
#' col_by <- col_by_to_matrix(col_by)
#' col_by_to_matrix(by_all("tot"), 1:5)
col_by_to_matrix <- function(col_by, x = NULL) {
  new_col_by <- if (is.factor(col_by)) {
    by_factor_to_matrix(col_by)
  } else if (is.data.frame(col_by)) {
    col_by
  } else if (is(col_by, "by_all")) {
    stopifnot(!is.null(x))
    res <- data.frame(rep(TRUE, `if`(is.data.frame(x), nrow(x), length(x))))
    colnames(res) <- col_by
    res
  } else {
    stop(paste("unknown class to convert", class(col_by)))
  }
  if (!is.null(x)) {
    # safety check
    stopifnot(nrow(new_col_by) == nrow(x))
  }
  stopifnot(
    all(vapply(new_col_by, function(col) is.logical.vector_modif(col, min_size = 0), logical(1)))
  )
  new_col_by
}

#' Adds column to matrix that corresponds to taking all entries (column of all TRUE)
#' 
#' @param col_by factor or matrix to add column to, e.g. output of \code{\link{by_factor_to_matrix}}
#' @param label label of new column
#' @param n number of rows in data vector, e.g. \code{length(x)}, useful when col_by can also be NULL
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
#' todo: maybe remove N, but this does not give enough flexibility because col_by may be NULL
by_add_total <- function(col_by, label = "total", n = NULL) {
  if (is.null(col_by)) {
    mat <- data.frame(rep(TRUE, n))
    colnames(mat) <- label
    mat
  } else {
    stopifnot(is.data.frame(col_by) || is.factor(col_by))
    mat <- col_by_to_matrix(col_by)
    n <- nrow(mat)
    old_names <- colnames(mat)
    res <- cbind(mat, rep(TRUE, n))
    colnames(res) <- c(old_names, label)
    res
  }
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