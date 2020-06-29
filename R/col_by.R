#' @include utils.R
NULL

#' Convert a factor to a matrix by, each column corresponding to a factor level
#' 
#' Each column corresponds to a factor level and each row is TRUE for all indexes 
#' where factor equals this factor level
#' @param x factor to convert to matrix_by
#' 
#' @return matrix of size length(x) times nlevels(x)
#' @rdname deprecated
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
#' @rdname deprecated
col_by_to_factor <- function(x) {
  if (is.factor(x)) {
    return(x)
  }
  stopifnot(is.data.frame(x))
  if (!all(rowSums(x) - 1 == 0)) {
    stop(paste("Columns are not disjoint in ", x))
  }
  f <- rep(-1, nrow(x))
  levelnames <- by_header_to_string(by_header(x))
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
## ' @examples 
## ' by <- factor(c("a", "b"), levels = c("a", "b", "c"))
## ' col_by_to_matrix(by)
## ' by_drop_empty_cols(by)
by_drop_empty_cols <- function(by) {
  by <- col_by_to_matrix(by)
  with_label(by[, vapply(by, any, logical(1)), drop = FALSE], label(by))
}

#' DEPRECATED Converts col_by to matrix if needed (if it is a factor)
#' Also handles case when it is of class by_all
#' x can be NULL if col_by is not of class by_all
#' returns transformed col_by as a matrix
#' 
#' @param col_by deprecated.
#' @rdname deprecated
#' @export
#' 
col_by_to_matrix <- function(col_by, x = NULL) {
    .Deprecated("This function is deprecated and should never be called directly by user code. The concept of representing column splitting via a matrix is no longer supported.")
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



by_add_total_old <- function(col_by, label = "total", n = NULL) {
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
by_all_old <- function(name) {
  structure(name, class = "by_all")
}


#' Combine levels
#'
#' @inheritParams compat_args
#' @param ... a series of named character vectors
#' 
#' @export
## TODO
## #' @examples 
## #' # note partial arument matching, in this case col_by has to be written
## #' head(by_combine(col_by = iris$Species, a = c("setosa", "versicolor"),
## #'            b =  c("setosa", "virginica"), c = "setosa"))
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
#' 
#' rtabulate(x = iris$Sepal.Length, col_by = by_quartile(iris$Sepal.Width), 
#'           FUN = mean, format = "xx.xx")
#' 
by_quartile <- function(x, cumulative = FALSE) {
  stopifnot(is.numeric(x), is.logical.single(cumulative), !any(is.na(x)))


  ret <- data.frame(x)
  class(ret) <- c(if(cumulative) "cmlquartcut_df" else "quartcut_df", class(ret))
  return(ret)
}

    

    
  
##   labs <- if(cumulative) c("[min, Q1]",
##                            "[min, Q2]",
##                            "[min, Q3]",
##                            "[min, max]") 
##   fct <- cut(x, breaks = quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1)), include.lowest=TRUE,
##              labels = labs)
##   return(
    

##   mat <- col_by_to_matrix(fct)
  
##   if (!cumulative) {
##     data.frame(.xx_quartcols_xx. = fct)
##   } else {
##     data.frame(
##       "[min, Q1]" = mat[[1]],
##       "[min, Med]" = apply(mat[, 1:2], 1, any),
##       "[min, Q3]" = apply(mat[, 1:3], 1, any),
##       "[min, max]" = rep(TRUE, nrow(mat)),
##       check.names = FALSE
##     )
##   }
## }


#' Compare Subset to Current col_by Levels
#' 
#' @param subset logical
#' @param label_all character, label appended to level of \code{col_by}
#' @param label_subset character, label appended to the subset of the corresponding level of \code{col_by}
#' @param sep separator of new labels
#' 
#' 
#' TODO: make hierarchical headers
#' 
#' @export
#' @rdname deprecated
## TODO
## ' @examples 
## ' 
## ' by_compare_subset(col_by = factor(c("A", "A", "A", "B", "B")),
## '                   subset = c(TRUE, FALSE, TRUE, FALSE, TRUE))
## ' 
by_compare_subset <- function(col_by, subset, label_all = "all", label_subset = "subset", sep = " - ") {
  
  mat <- col_by_to_matrix(col_by)
  
  stopifnot(is.logical(subset), nrow(mat) == length(subset), 
            is.character(label_all), is.character(label_subset), is.character(sep))
  
  lst <- lapply(mat, function(x) {
    list(
      x,
      x & subset
    )
  })
  
  df <- as.data.frame(do.call(`c`, lst))
  names(df) <- paste(rep(names(mat), each = 2), rep(c(label_all, label_subset), ncol(mat)), sep = sep)
  
  df
}

#' DEFUNCT Hierarchical col_by
#' 
#' @param ... DEFUNCT
#' 
#' @return DEFUNCT
#'
#' @seealso \code{\link{manual_cols}}
#' 
#' @examples
#' SEX <- with_label(factor(c("M", "M", "F", "F", "F")), "Sex")
#' AGEC <- with_label(factor(c("<40", ">40", "<40", ">40", "<40")), "Age Category")
#' VAL <- c(5, 4, 4, 6, 2, 1)
#' 
#' rtables:::by_hierarchical(SEX)
#' col_by <- rtables:::by_hierarchical(SEX, AGEC)
#' col_by
#' colnames(col_by)
#' attr(col_by, "header")
#' rtables:::by_header_to_string(attr(col_by, "header"))
#' @export
by_hierarchical <- function(...) {
    .Deprecated("manual_cols", package = "rtables", msg = "Use manual_cols to create nested, hierarchical column layouts")
    
    by_lst <- data.frame(...) ##list(...)
    return(by_lst)
    by_lst <- lapply(by_lst, function(vc) {
        if(is(vc, "factor"))
            levels(vc)
        else
            unique(vc)
    })
    lyt = NULL
    for(stuff in by_lst)
        lyt <- split_cols(lyt, ManualSplit(stuff, lbl = ""), pos = next_cpos(lyt))
    return(lyt)
    
  ## stopifnot(length(by_lst) > 0)
  ## by <- col_by_to_matrix(by_lst[[1]])
  ## if (length(by_lst) == 1) {
  ##   structure(
  ##     by,
  ##     header = list(old_rrowl(row.name = label(by), colnames(by)))
  ##   )
  ## } else {
  ##   subres <- do.call(by_hierarchical, by_lst[-1])
  ##   header <- do.call(
  ##     old_rheader,
  ##     c(
  ##       list(old_rrowl(
  ##         row.name = label(by), 
  ##         lapply(
  ##           colnames(by), 
  ##           function(col_name) old_rcell(col_name, colspan = ncell(attr(subres, "header")[[1]]))
  ##         )
  ##       )),
  ##       Reduce(combine_rrows, lapply(by, function(discard) attr(subres, "header")))
  ##     )
  ##   )
  ##   by_combined <- as.data.frame(do.call(cbind, lapply(by, function(rows) subres & rows)))
  ##   colnames(by_combined) <- by_header_to_string(header)
  ##   structure(
  ##     by_combined,
  ##     header = header
  ##   )
  ## }
}

## credit: rlang, Henry and Wickham.
## this one tiny utility function is NOT worth a dependency.
## modified it so any length 0 x grabs y
`%||%` <- function (x, y) {
    if (length(x) == 0L) 
        y
    else x
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
by_header <- function(x) {
  attr(x, "header") %||% colnames(x)
}

#' Convert the header to string
#' 
#' Useful to agnostically handle normal headers and hierarchical ones
#' 
#' @param x rheader or regular list
by_header_to_string <- function(x) {
  if (is(x, "rheader")) {
    do.call(
      function(...) paste(..., sep = ","),
      lapply(x, function(elem) rep(elem, each = ncol(x) / length(elem)))
    )
  } else {
    x
  }
}
