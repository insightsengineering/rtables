

#' 
#' Row names of an \code{\link{rtable}} object with spaces
#' 
#' Retrieve the row names of an \code{\link{rtable}} object
#'   
#' @inheritParams dim.rtable
#' @param spaces numeric number of spaces per indent level
#' 
#' @return a vector with the row names
#' @noRd
#' 
indented_row.names <- function(x, spaces = 2) {
  
  if (!is(x, "rtable") && !is(x, "rheader")) stop("x is required to be a rtable or a rheader")
  
  if (spaces < 0) stop("spaces needs to be >= 0")
  
  vapply(x, function(row) {
    rn <- attr(row, "row.name")
    indent <- strrep(" ", attr(row, "indent")*spaces)
    if (is.null(rn)) "" else paste0(indent, rn)
  }, character(1))
}

#' 
#' Return Indentation of Header Rows
#' 
#' @param x object representing the table header.
#' @noRd
#' 
#' @rdname deprecated
header_indent <- function(x) {
    .Deprecated("The concept of the table header being made up of rows is no longer supported.")
    stopifnot(is(x, "VTableTree"))
    hdr = .tbl_header_mat(x)
    rep(0, nrow(hdr$body))
}

header_indent_old <- function(x) {
  stopifnot(is(x, "rtable"))
  
  h <- header(x)
  vapply(h, function(xi) attr(xi, "indent"), numeric(1))
}




#' 
#' change row names of rtable
#' 
#' @param x an \code{\link{rtable}} object
#' @param value character vector with row names
#' @noRd
#' 
#' 
#' @examples 
#' 
#' tbl <- rtable(header = c("A", "B"), rrow("row 1", 1, 2))
#' tbl
#' 
#' # TODO: fix
#' # row.names(tbl) <- "Changed Row Name"
#' # tbl
`row.names<-.rtable` <- function(x, value) {

  nr <- nrow(x)
  
  if (length(value) != nr) stop("dimension mismatch")
  
  for (i in seq_along(x)) {
    attr(x[[i]], "row.name") <- value[i]
  }
  
  x
}


#' 
#' Get content of first header row of an \code{\link{rtable}} object
#' 
#' Retrieve the content of the first header row of an \code{\link{rtable}} object
#' 
#' @inheritParams dim.rtable
#' 
#' @return a vector with the column names 
#' @noRd
#' @method names rtable
#' 
names.rtable <- function(x) {
  row_i <- attr(x, "header")[[1]]
  
  unlist(lapply(row_i, function(cell) {
    colspan <- attr(cell, "colspan")
    rep(cell, colspan)
  }))
  
}

#' 
#' Convert a table object to an \code{\link{rtable}}
#' 
#' @inheritParams as.rtable
#' 
#' 
#' @noRd
#' @examples 
#' 
#' tbl <- as.rtable(table(iris$Species))
#' tbl
#' 
#' \dontrun{
#' Viewer(tbl)
#' }
#' 
#' 
#' tbl <- as.rtable(table(sample(letters[1:4], 200, TRUE), sample(LETTERS[1:4], 200, TRUE)))
#' tbl
#' \dontrun{
#' Viewer(tbl, row.names.bold = TRUE)
#' }
#' 
as.rtable.table <- function(x, format = "xx") {
  
  if (length(dim(x)) == 1) {
    rtable(header = names(x), format = format, do.call(rrow, c(list(row.name = "1"), as.list(as.vector(x)))))
  } else {
    X <- as.data.frame.matrix(x)
    do.call(rtable,
            c(list(
              header = names(X), format = format
            ),
            Map(function(row, row.name) {
              do.call(rrow, as.list(c(row.name=row.name, setNames(row, NULL))))
            }, as.data.frame(t(X)), rownames(X))
            )
    )
  }
}

#' 
#' DEFUNCT Hierarchical col_by
#' 
#' @param ... DEFUNCT
#' 
#' @return DEFUNCT
#'
#' @seealso \code{\link{manual_cols}}
#' @noRd
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
#' 
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
}



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
#' 
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



#' Unlist a Nested Lists with rtables as leafes
#'
#' Often it is useful to flatten a nested lists with rtables as leafes to a list
#' of rtables. The algorithm used is a depth first tree traversal.
#'
#' @param x a nested list of with rtables as leaf object
#'
#' @return a list of rtables
#'
#' @noRd
#'
#' @examples
#'
#' l_tbls <- list(
#'   list(
#'     rtabulate(iris$Sepal.Length, iris$Species, mean),
#'     rtabulate(iris$Sepal.Length, iris$Species, sd)
#'   ),
#'   list(
#'     rtabulate(iris$Sepal.Width, iris$Species, mean)
#'   )
#' )
#'
#' rtables:::unlist_rtables(l_tbls)
#'
unlist_rtables <- function(x) {
  
  n <- 0
  incr_n_if_rtable <- function(x) {
    if (is(x, "rtable")) {
      if (is_non_empty_rtable(x)) {
        n <<- n + 1
      }
    } else {
      lapply(x, incr_n_if_rtable)
    }
  }
  incr_n_if_rtable(x)
  
  i <- 1
  tbls <- vector(mode = "list", length = n)
  
  add_tbls <- function(x) {
    if (is(x, "rtable")) {
      if (is_non_empty_rtable(x)) {
        tbls[[i]] <<- x
        i <<- i + 1
      }
    } else {
      lapply(x, add_tbls)
    }
  }
  
  if (n > 0) {
    add_tbls(x)
  }
  
  tbls
  
}




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
#' @noRd
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

#' Drop empty columns from matrix-version of by
#' 
#' @param by factor or matrix
#' 
#' @return matrix with dropped columns
#' @rdname deprecated
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
    all(vapply(new_col_by, function(col) is.logical.vector_modif(col, min_size = 0), logical(1)))
  )
  new_col_by
}




#' Combine levels
#'
#' @inheritParams compat_args
#' @param ... a series of named character vectors
#' 
#' @rdname deprecated
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


