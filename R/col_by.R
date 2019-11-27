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

#' Convert a col_by to a factor, works for factors and matrices
#' 
#' This is the opposite of the function \code{\link{col_by_to_matrix}}. 
#' Only works when columns of matrix are disjoint.
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
  levelnames <- colnames(x) #by_header_to_string(by_header(x))
  for (i in seq_along(x)) {
    f[x[, i]] <- levelnames[[i]]
  }
  with_label(
    factor(f, levels = levelnames), 
    label(x)
  )
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
  cols_keep <- vapply(by, any, logical(1))
  if (!all(cols_keep)) {
    with_label(by[, cols_keep, drop = FALSE], label(by)) 
  } else {
    # to avoid losing the header attribute
    by
  }
}

#' Converts col_by to matrix if needed (if it is a factor)
#' Also handles case when it is of class by_all
#' x can be NULL if col_by is not of class by_all
#' returns transformed col_by as a matrix
#' 
#' @param col_by factor or data.frame to convert
#' @param x object that col_by is applied to, must be non-NULL if col_by is of class by_all 
#'   to select all elements from x
#' 
#' @return matrix col_by
#' 
#' @export
#' 
#' @examples 
#' col_by <- factor(c("a", "b", "a", "a", "b"))
#' col_by
#' col_by_to_matrix(col_by)
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
    stopifnot(nrow(new_col_by) == `if`(is.data.frame(x), nrow(x), length(x)))
  }
  stopifnot(
    all_true(new_col_by, is_logical_vector_modif, min_length = 0)
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
#' 
#' attr(by_add_total(
#' by_hierarchical(factor(c("F", "F", "M", "M")), factor(c("<40", ">40", "<40", ">40"))),
#' label = "All"
#' ), "header")
by_add_total <- function(col_by, label = "total", n = NULL) {
  # todo: maybe remove n, but this does not give enough flexibility because col_by may be NULL
  if (is.null(col_by)) {
    mat <- data.frame(rep(TRUE, n))
    colnames(mat) <- label
    mat
  } else {
    col_by <- col_by_to_matrix(col_by) # col_by not NULL
    n <- nrow(col_by)
    res <- cbind(col_by, rep(TRUE, n))
    colnames(res) <- c(colnames(col_by), label)
    
    # add hierarchical header correctly
    existing_header <- by_header(col_by)
    # bug when label is "" currently, so we take " "
    total_header <- replicate(nrow(existing_header), rrow(NULL, " "), simplify = FALSE)
    total_header[[1]] <- label # total_header has length >= 1
    res <- with_by_header(res, do.call(rheader, combine_rrows(existing_header, total_header)))
    
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


#' Combine levels
#' 
#' @inheritParams by_add_total
#' @param ... a series of named character vectors
#' 
#' @export
#' 
#' @examples 
#' # note partial arument matching, in this case col_by has to be written
#' head(by_combine(col_by = iris$Species, a = c("setosa", "versicolor"),
#'            b =  c("setosa", "virginica"), c = "setosa"))
by_combine <- function(col_by, ...) {
  lst <- list(...)
  if (is.null(names(lst)) || any(names(lst) == "")) {
    stop("named character vectors are required")
  }
  
  mat <- col_by_to_matrix(col_by)
  if (!all(unlist(lst) %in% names(mat))) {
    stop("not all levels exist in col_by")
  }
   
  as.data.frame(setNames(lapply(lst, function(levels) {
    apply(mat[, levels, drop = FALSE], 1, any)
  }), names(lst)))
  
}


#' Cut by quartiles
#' 
#' @param x a numerical vector
#' @param cumulative logical, if cumulative col_by should be returned
#' 
#' @importFrom stats quantile
#' @export
#' 
#' @examples 
#' head(by_quartile(iris$Sepal.Length))
#' head(by_quartile(iris$Sepal.Length, TRUE))
#' 
#' rtabulate(x = iris$Sepal.Length, col_by = by_quartile(iris$Sepal.Width), 
#'           FUN = mean, format = "xx.xx")
#' 
by_quartile <- function(x, cumulative = FALSE) {
  stopifnot(is.numeric(x), is_logical_single(cumulative), !any(is.na(x)))
  
  fct <- cut(x, breaks = quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1)), include.lowest=TRUE,
             labels = c("[min, Q1]", "(Q1, Med]", "(Med, Q3]", "(Q3,  max]"))

  mat <- col_by_to_matrix(fct)
  
  if (!cumulative) {
    mat
  } else {
    data.frame(
      "[min, Q1]" = mat[[1]],
      "[min, Med]" = apply(mat[, 1:2], 1, any),
      "[min, Q3]" = apply(mat[, 1:3], 1, any),
      "[min, max]" = rep(TRUE, nrow(mat)),
      check.names = FALSE
    )
  }
}


#' Compare Subset to Current col_by Levels
#' 
#' @inheritParams by_combine
#' @param subset logical
#' @param label_all character, label appended to level of \code{col_by}
#' @param label_subset character, label appended to the subset of the corresponding level of \code{col_by}
#' @param sep separator of new labels
#' 
#' @export
#' 
#' @examples 
#' 
#' cb <- by_compare_subset(col_by = factor(c("A", "A", "A", "B", "B")),
#'                   subset = c(TRUE, FALSE, TRUE, FALSE, TRUE))
#' cb
#' attr(cb, "header")
#' 
#' 
#' rtabulate(1:5, cb, mean, format = "xx.xx")
#' 
by_compare_subset <- function(col_by, subset, label_all = "all", label_subset = "subset", sep = " - ") {
  stopifnot(is.logical(subset), is.character(label_all), is.character(label_subset), is.character(sep))
  
  by_all_subset <- data.frame(all = rep(TRUE, length(subset)), subset = subset)
  colnames(by_all_subset) <- c(label_all, label_subset)
  by_hierarchical(col_by, by_all_subset, sep = sep)
}

#' Hierarchical col_by
#' 
#' @param ... factors or col_by matrices. When a by element in the list 
#'   is a matrix, it allows for non-disjoint columns
#' @param sep separator between hierarchical levels
#'
#' todo: returned col_by column names are not correct
#' 
#' @return structure(col_by matrix, header = ) a list of labelled headers (one per row), 
#'   col_by matrix corresponding to all combinations
#' 
#' @export
#' 
#' @examples
#' SEX <- with_label(factor(c("M", "M", "F", "F", "F")), "Sex")
#' AGEC <- with_label(factor(c("<40", ">40", "<40", ">40", "<40")), "Age Category")
#' VAL <- c(5, 4, 4, 6, 2)
#' 
#' rtabulate(x = VAL, col_by = by_hierarchical(SEX, AGEC))
#' 
#' library(dplyr)
#' rtabulate(
#'   x = VAL,
#'   col_by = by_hierarchical(SEX, AGEC) %>%
#'       by_add_total()
#' )
#' 
#' 
#' by_hierarchical(SEX)
#' col_by <- by_hierarchical(SEX, AGEC)
#' col_by
#' colnames(col_by)
#' attr(col_by, "header")
#' rtables:::by_header_to_string(attr(col_by, "header"))
by_hierarchical <- function(..., sep = ",") {
  by_lst <- list(...)
  stopifnot(length(by_lst) > 0)
  by <- col_by_to_matrix(by_lst[[1]])
  if (length(by_lst) == 1) {
    structure(
      by,
      header = list(rrowl(row.name = label(by), colnames(by)))
    )
  } else {
    subres <- do.call(function(...) by_hierarchical(..., sep = sep), by_lst[-1])
    header <- do.call(
      rheader,
      c(
        list(rrowl(
          row.name = label(by), 
          lapply(
            colnames(by), 
            function(col_name) rcell(col_name, colspan = ncell(attr(subres, "header")[[1]]))
          )
        )),
        Reduce(combine_rrows, lapply(by, function(discard) attr(subres, "header")))
      )
    )
    by_combined <- as.data.frame(do.call(cbind, lapply(by, function(rows) subres & rows)))
    colnames(by_combined) <- by_header_to_string(header, sep = sep)
    structure(
      by_combined,
      header = header
    )
  }
}

# todo: maybe put as S3 function, but then create a class from by functions
# This function is not currently used in tern
#' Get names to use for header
#' 
#' 
#' @param x object
#' 
#' @return header attribute if existent, otherwise colnames
#' 
#' @export
#' 
#' @importFrom rlang "%||%"
by_header <- function(x) {
  stopifnot(is(x, "data.frame"))
  attr(x, "header") %||% rheader(colnames(x))
}

#' Add header to x
#' 
#' @param x by object
#' @param header header to add
#' 
#' @return new by object
#' 
#' @export
with_by_header <- function(x, header) {
  stopifnot(is(header, "rheader"))
  attr(x, "header") <- header
  x
}

#' Convert the header to string
#' 
#' Useful to agnostically handle normal headers and hierarchical ones.
#' 
#' @param x rheader or regular list
#' @param sep separator between hierarchical levels
#' 
#' @return 1d list of header names
#' rtables:::by_header_to_string(
#'   rheader(
#'     rrow(NULL, rcell("F", colspan = 2), rcell("M", colspan = 2)),
#'     rrow(NULL, "<40", ">40", "<40", ">40")
#'   )
#' )
by_header_to_string <- function(x, sep = ",") {
  if (is(x, "rheader")) {
    do.call(
      function(...) paste(..., sep = sep),
      lapply(x, function(elem) rep(elem, each = ncol(x) / length(elem)))
    )
  } else {
    x
  }
}
