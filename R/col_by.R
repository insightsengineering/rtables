#' @include utils.R
NULL

#' Convert a factor to a matrix by, each column corresponding to a factor level
#' 
#' Each column corresponds to a factor level and each row is TRUE for all indexes 
#' where factor equals this factor level
#' @param x factor to convert to matrix_by
#' 
#' @return matrix of size length(x) times nlevels(x)
#' 
#' @examples 
#' x <- factor(c("a", "b", "a", "a", "b"))
#' rtables:::by_factor_to_matrix(x)
by_factor_to_matrix <- function(x) {
  stopifnot(is.factor(x))
  stopifnot(!any(is.na(x)))
  res <- matrix(FALSE, nrow = length(x), ncol = nlevels(x))
  colnames(res) <- levels(x)
  for (level in levels(x)) {
    res[x == level, level] <- TRUE
  }
  res <- data.frame(res)
  colnames(res) <- levels(x) # doing this after data.frame call makes sure that special chars are preserved
  with_label(res, label(x))
}

# removeAttributes <- function(x) {
#   attributes(x) <- NULL
#   x
# }

#' Convert a col_by to a factor, works for factors and matrices
#' 
#' This is the opposite of the function \code{\link{col_by_to_matrix}}. Only works when columns of matrix are disjoint.
#' 
#' 
#' @param x matrix or factor
#' 
#' @return factor
#' 
#' @export
#' 
#' @examples 
#' col_by_to_factor(data.frame(
#' x1 = c(TRUE, TRUE, FALSE, FALSE),
#' x2 = c(FALSE, FALSE, TRUE, FALSE),
#' x3 = c(FALSE, FALSE, FALSE, TRUE)
#' ))
col_by_to_factor <- function(x) {
  if (is.factor(x)) {
    return(x)
  }
  stopifnot(is.data.frame(x))
  if (!all(rowSums(x) - 1 == 0)) {
    stop(paste("Columns are not disjoint in ", x))
  }
  f <- rep(-1, nrow(x))
  for (i in seq_along(x)) {
    f[x[, i]] <- colnames(x)[[i]]
  }
  with_label(factor(f, levels = colnames(x)), label(x))
}

#' Drop empty columns from matrix-version of by
#' 
#' @param by factor or matrix
#' 
#' @return matrix with dropped columns
#' 
#' @export
#' 
#' @examples 
#' by <- factor(c("a", "b"), levels = c("a", "b", "c"))
#' col_by_to_matrix(by)
#' by_drop_empty_cols(by)
by_drop_empty_cols <- function(by) {
  by <- col_by_to_matrix(by)
  with_label(by[, vapply(by, any, logical(1)), drop = FALSE], label(by))
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
#' col_by <- factor(c("a", "b", "a", "a", "b"))
#' col_by <- col_by_to_matrix(col_by)
#' col_by_to_matrix(by_all("tot"), 1:5)
col_by_to_matrix <- function(col_by, x = NULL) {
  #todo: rename col_by_to_matrix to by_to_matrix, similarly for col_by_to_factor
  new_col_by <- if (is.factor(col_by)) {
    by_factor_to_matrix(col_by)
  } else if (is.data.frame(col_by)) {
    col_by
  } else if (is(col_by, "by_all")) {
    stopifnot(!is.null(x))
    res <- data.frame(rep(TRUE, `if`(is.data.frame(x), nrow(x), length(x))))
    colnames(res) <- col_by
    with_label(res, label(col_by))
    #stopifnot(!is.null(attr(col_by, "n"))) # you must set n because col_by is empty
    #data.frame(rep(TRUE, attr(col_by, "n")))
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
#' @param col_by factor or matrix to add column to, e.g. output of \code{\link{col_by_to_matrix}}
#' @param label label of new column
#' @param n number of rows in data vector, e.g. \code{length(x)}, useful when col_by can also be NULL
#' 
#' @return new matrix with one column added
#' 
#' @export
#' 
#' @examples 
#' x <- factor(c("a", "b", "a", "a", "b"))
#' mat <- col_by_to_matrix(x)
#' by_add_total(mat, label = "tot")
#' by_add_total(x, label = "tot")
#' 
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   library(dplyr)
#'   mat %>% by_add_total(label = "tot")
#' }
#' 
#' by_add_total(NULL, "total", n = 3)
by_add_total <- function(col_by, label = "total", n = NULL) {
  # todo: maybe remove n, but this does not give enough flexibility because col_by may be NULL
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
    res <- with_label(res, label(col_by))
    res
  }
}

#' Select all rows
#' 
#' This is the analog to \code{\link{by_add_total}} to start a dplyr pipeline.
#' 
#' @param name name of column in col_by (either in factor or in matrix)
#' 
#' @return col_by matrix
#' 
#' @export
#' 
#' @examples 
#' by_all("total")
#' col_by_to_matrix(by_all("total"), x = 1:3)
by_all <- function(name) {
  structure(name, class = "by_all")
}

#' Hierarchical col_by
#' 
#' @param by_lst recursive list of bys (either factors or matrices)
#'   when a by element in the list is a matrix, it allows for non-disjoint columns
#'
#' todo: returned col_by column names are not correct
#' 
#' @return list(headers, col_by matrix) a list of labelled headers (one per row), col_by matrix corresponding to all combinations
#' 
#' @examples
#' by_lst <- list(with_label(factor(c("M", "M", "F", "F", "F")), "Sex"))
#' rtables:::by_hierarchical(by_lst)
#' by_lst <- list(
#'   with_label(factor(c("M", "M", "F", "F", "F")), "Sex"), 
#'   with_label(factor(c("O", "Y", "Y", "Y", "Y")), "Age")
#' )
#' rtables:::by_hierarchical(by_lst)
by_hierarchical <- function(by_lst) {
  # old: to simplify, we only accept factors for now
  by <- col_by_to_matrix(by_lst[[1]])
  if (length(by_lst) == 1) {
    list(
      headers = list(rrowl(row.name = label(by), colnames(by))),
      col_by = by
    )
  } else {
    subres <- by_hierarchical(by_lst[-1])
    list(
      headers = do.call(
        rheader,
        c(
          list(rrowl(
            row.name = label(by), 
            lapply(colnames(by), function(col_name) rcell(col_name, colspan = ncell(subres$headers[[1]])))
          )),
          Reduce(combine_rrows, lapply(by, function(discard) subres$headers))
        )
      ), 
      col_by = do.call(cbind, lapply(by, function(rows) {
        do.call(cbind, lapply(subres$col_by, function(subrows) (rows & subrows))) 
      }))
    )
  }
}
