
#' Tabulation Methods
#'
#' \code{rtablulate} provides a number of methods to derive
#' \code{\link{rtable}}s. Conceptually the \code{rtabulate} has it's origin in
#' \code{\link{tapply}}.
#' 
#' The data is split into cell-data and a function can be specified that returns
#' a data structure (or \code{\link{rcell}}).
#'   
#' @param x either a vector or \code{data.frame}
#' @param ... arguments passed to methods
#' 
#' @return an \code{\link{rtable}} object
#' 
#' @author Adrian Waddell
#'       
#' @export
#' 
rtabulate <- function(x, ...) {
  UseMethod("rtabulate")
}

# rtabulate default for vectors
# 
# This method is used for vectors of type \code{logical} and \code{numeric}
# 
# see parameter descrition for rtabulate.numeric
#
rtabulate_default <- function(x, col_by = by_all("col_1"), FUN, ...,
                              format = NULL, row.name = "", indent  = 0,
                              col_wise_args = NULL) {
  
  force(FUN)
  col_by <- col_by_to_matrix(col_by, x)
  check_colwise_args(col_by, col_wise_args)
  
  # each column in col_by contains the rows to select from x
  column_data <- lapply(col_by, function(rows) x[rows])

  cells <- if (is.null(col_wise_args)) {
    lapply(column_data, FUN, ...)
  } else {
    dots <- list(...)
    # for each column of col_by, append the arguments in col_wise_args
    args <- lapply(transpose(col_wise_args), function(args) c(dots, args))
    
    Map(function(xi, argsi) {
      do.call(FUN, c(list(xi), argsi))
    }, column_data, args)
  }
  
  rr <- rrowl(row.name = row.name, cells, format = format, indent = indent)
  
  rtable(header = colnames(col_by), rr)
}



#' Tabulate a numeric vector
#'
#' By default each cell reports the mean based on the associated vector. 
#'
#'
#' @inheritParams rrow
#' @param x a vector
#' @param col_by (\code{\link{factor}} or \code{\link{data.frame}}
#'   if a \code{\link{factor}} of length \code{nrow(x)} that defines
#'   which levels in \code{col_by} define a column.
#'   can use \code{\link{by_factor_to_matrix}} to create a matrix from a factor to use non-disjoint columns
#'   can use \code{\link{by_all}} to have a column with all rows, alternatively look at \code{\link{by_add_total}})
#' @param FUN a function that processes the cell data
#' @param ... arguments passed to \code{FUN}
#' @param format if \code{FUN} does not return a formatted \code{\link{rcell}}
#'   then the \code{format} is applied
#' @param row.name if \code{NULL} then the \code{FUN} argument is deparsed and
#'   used as \code{row.name} of the \code{\link{rrow}}
#' @param col_wise_args a named list containing collections (e.g. vectors or
#'   lists) with data elements for each column of the resulting table. The data
#'   elements are then passed to the named argument \code{FUN} corresponding to
#'   the element name of the outer list. Hence, the length and order of each
#'   collection must match the levels in \code{col_by}. See examples.
#'
#' @inherit rtabulate return
#'
#' @export
#'
#' @examples
#'
#' rtabulate(iris$Sepal.Length)
#'
#' rtabulate(iris$Sepal.Length, col_by = by_all("Sepal.Length"))
#'
#' with(iris,  rtabulate(x = Sepal.Length, col_by = Species, row.name = "mean"))
#'
#' SL <- iris$Sepal.Length
#' Sp <- iris$Species
#' rbind(
#'   rtabulate(SL, Sp, length, row.name = "n"),
#'   rtabulate(SL, Sp, function(x)c(mean(x), sd(x)), format = "xx.xx (xx.xx)", row.name = "Mean (SD)"),
#'   rtabulate(SL, Sp, median, row.name = "Median"),
#'   rtabulate(SL, Sp, range, format = "xx.xx - xx.xx", row.name = "Min - Max")
#' )
#' 
#' x <- 1:100
#' cb <- factor(rep(LETTERS[1:3], c(20, 30, 50)))
#' 
#' rtabulate(
#'   x = x, col_by = cb, FUN = function(x, N) list(mean(x), sd(x), N),
#'   format = sprintf_format("%.2f (%.2f) and %i"), row.name = "Mean (SD) and N",
#'   col_wise_args = list(N = table(cb))
#' )
#' 
rtabulate.numeric <- function(x, col_by = by_all("col_1"), FUN = mean, ...,
                              format = NULL, row.name = NULL,
                              indent  = 0, col_wise_args = NULL) {
  
  if (is.null(row.name)) row.name <- paste(deparse(substitute(FUN)), collapse = ";")
  
  rtabulate_default(
    x = x, col_by = col_by, FUN = FUN, ...,
    format = format, row.name = row.name, indent = indent,
    col_wise_args = col_wise_args
  )
}

#' Tabulate a logical vector
#' 
#' By default each cell reports the number of \code{TRUE} observations from the associated vector. 
#' 
#' @inheritParams rtabulate.numeric
#' 
#' @inherit rtabulate return
#' 
#' @export
#' 
#' @examples 
#' rtabulate(iris$Species == "setosa")
#' 
#' rtabulate(iris$Species == "setosa", by_all("Species"),
#'    FUN = function(x, N) list(sum(x), sum(x)/N),
#'    row.name = "n (n/N)",
#'    col_wise_args = list(N = 150)
#' )
#' 
#' # default FUN is number of observations equal to TRUE
#' with(iris, rtabulate(Sepal.Length < 5, Species, row.name = "Sepal.Length < 5"))
#'  
#' # Custom FUN: number of TRUE records in a cell and precentages based on number of records
#' # in each column
#' with(iris, rtabulate(Sepal.Length < 5, Species,
#'   FUN = function(xi, N) sum(xi) * c(1, 1/N), 
#'   format = "xx.xx (xx.xx%)",
#'   row.name = "Sepal.Length < 5",
#'   col_wise_args = list(N = table(Species))
#' ))
#' 
rtabulate.logical <- function(x, col_by = by_all("col_1"),
                              FUN = sum,
                              ...,
                              format = NULL,
                              row.name = NULL,
                              indent = 0,
                              col_wise_args = NULL
                              ) {
  
  if (is.null(row.name)) row.name <- paste(deparse(substitute(FUN)), collapse = ";")
  
  rtabulate_default(
    x = x, col_by = col_by, FUN = FUN, ...,
    format = format, row.name = row.name, indent = indent,
    col_wise_args = col_wise_args
  )
}

#' Tabulate Factors
#' 
#' By default each cell reports the number of observations in
#' each level of \code{x}. 
#' 
#' @inheritParams rtabulate.numeric
#' @param useNA either one of ("no", "ifany", "always"). If \code{"no"} then \code{NA} values
#'   in \code{x} get dropped. When \code{"ifany"} is used a row for \code{NA} values is 
#'   included in the summary if any \code{NA}s exist in \code{x}. For option \code{"always"} 
#'   \code{NA} values are always included in the summary even if none exist in \code{x}. 
#' 
#' @inherit rtabulate return
#' 
#' @export
#'
#' @examples 
#' 
#' rtabulate(x = iris$Species)
#' 
#' rtabulate(x = iris$Species, by_all("sum"))
#' 
#' sl5 <- factor(iris$Sepal.Length > 5, levels = c(TRUE, FALSE),
#'    labels = c("S.L > 5", "S.L <= 5"))
#' 
#' rtabulate(iris$Species, col_by = sl5)
#' rtabulate(sl5, iris$Species)
#' 
#' rtabulate(iris$Species, col_by = sl5,
#'    FUN = function(cell_data, N) {
#'      if (length(cell_data) > 10) {
#'         length(cell_data) * c(1, 1/N)
#'      } else {
#'         rcell("-", format = "xx")
#'      }
#'    },
#'    format = "xx (xx.xx%)",
#'    col_wise_args = list(N = table(sl5))
#' )
#' 
#' rtabulate(x = factor(c("X", "Y"), c("X", "Y")),
#'           col_by = factor(c("a", "a"), c("a", "b")), FUN = length)
#' 
#' rtabulate(factor(c("Y", "Y"), c("X", "Y")),
#'           factor(c("b", "b"), c("a", "b")), length)
#' 
#' rtabulate(
#'   x = factor(c("Y", "Y"), c("X", "Y")),
#'   col_by = factor(c("b", "b"), c("a", "b")),
#'   FUN = function(x, N) list(length(x), N),
#'   col_wise_args = list(N = c(1,2))
#' )
#' 
#' @importFrom purrr transpose
rtabulate.factor <- function(x,
                             col_by = by_all("col_1"), 
                             FUN = length,
                             ...,
                             useNA = c("no", "ifany", "always"),
                             format = NULL,
                             indent  = 0,
                             col_wise_args = NULL) {
  
  useNA <- match.arg(useNA)
  
  if (any("<NA>" %in% levels(x))) stop("factor with level '<NA>' is not valid in rtabulate.factor")
  
  if (useNA %in% c("ifany", "always")) {
    if (useNA == "always" || any(is.na(x))) {
      levels(x) <- c(levels(x), "<NA>")
      x[is.na(x)] <- "<NA>"
    }
  } 
  
  ## note that splitting with empty-string creates a un-named list element
  if (any(levels(x) == "")) {
    if ("-" %in% levels(x)) {
      stop("x currently cannot have '' and levels called -")
    }
    levels(x) <- gsub("^$", "-", levels(x)) # replace "" -> "-"
    warning("'' levels were turned into level -")
  }
  
  columns <- levels(x)
  rtabulate(as.data.frame(x), 
            row_by = x, 
            col_by = col_by, 
            FUN = FUN, 
            format = format, 
            indent = indent, 
            col_wise_args = col_wise_args,
            ...
  )
}


#' Split data.frame and apply functions
#' 
#' @inheritParams rtabulate.factor
#' @param x data.frame 
#' @param row_by rows to take per row
#' @param col_by rows to take per column
#' 
#' For cell in (row, column), it takes the intersection of the corresponding row_by & col_by
#' 
#' @inherit rtabulate return
#' 
#' @export
#' 
#' 
#' @examples 
#' df <- expand.grid(aaa = factor(c("A", "B")), bbb = factor(c("X", "Y", "Z")))
#' df <- rbind(df, df)
#' df$val <- 1:nrow(df)
#' 
#' rtabulate(
#'   x = df,
#'   row_by = df$aaa,
#'   col_by = df$bbb,
#'   FUN = function(x) {  
#'      sum(x$val)
#'   }
#' )
#' 
#' rtabulate(
#'   x = iris,
#'   row_by = by_all("sum"),
#'   col_by = iris$Species, 
#'   FUN = function(x) sum(x$Sepal.Length)
#' )
#' 
#' rtabulate(
#'   x = iris,
#'   row_by = iris$Species,
#'   col_by = by_all("sum"), 
#'   FUN = function(x) sum(x$Sepal.Length)
#' )
#' 
#' fsl5 <- factor(iris$Sepal.Length > 5, levels = c(TRUE, FALSE),
#'     labels = c("S.L > 5", "S.L <= 5"))
#' 
#' tbl <- rtabulate(
#'   x = iris,
#'   row_by = fsl5,
#'   col_by = iris$Species, 
#'   FUN = function(x_cell) {
#'     if (nrow(x_cell) < 10) {
#'       rcell("-")
#'     } else {
#'       fit <- lm(Sepal.Length ~ Petal.Width, data = x_cell)
#'            
#'       rcell(list(fit), format = function(x, output) {
#'         paste("df:", x[[1]]$df.residual)
#'       })
#'     }
#'   }
#' )
#' tbl
#' 
#' rtabulate(
#'   x = iris,
#'   row_by = fsl5,
#'   col_by = iris$Species, 
#'   FUN = function(x_cell, N) {
#'      N
#'   },
#'   col_wise_args = list(N = c(10, 100, 200))
#' )
#' 
#'  
#' 
rtabulate.data.frame <- function(x,
                                 row_by,
                                 col_by,
                                 FUN,
                                 ...,
                                 format = NULL,
                                 indent = 0,
                                 col_wise_args = NULL) {
  
  force(FUN)
  row_by <- col_by_to_matrix(row_by, x)
  col_by <- col_by_to_matrix(col_by, x)
  check_colwise_args(col_by, col_wise_args)
  
  # list(row1 = list(c1, c2, ...), row2 = list(c1, c2, ...), ...)
  cell_data <- lapply(row_by, function(rows) {
    lapply(col_by, function(subrows) {
      x[rows & subrows, ]
    })
  })

  cells_by_row <- if (is.null(col_wise_args)) {
    lapply(cell_data, function(row_i) lapply(row_i, FUN, ...))
  } else {
    dots <- list(...)
    args <- lapply(transpose(col_wise_args), function(args) c(dots, args))
    
    lapply(cell_data, function(row_i) {
      Map(function(xi, argsi) {
        do.call(FUN, c(list(xi), argsi))
      }, row_i, args)
    })
    
  }
  
  rrows <- Map(function(cells_row, rowname) {
    rrowl(row.name = rowname, cells_row, format = format, indent = indent)
  }, cells_by_row, names(cells_by_row))
  
  rtablel(header = colnames(col_by), rrows)
}

check_colwise_args <- function(col_by, col_wise_args) {
  stopifnot(is.data.frame(col_by))
  if (!is.null(col_wise_args)) {
    if (!is.list(col_wise_args)) {
      stop("col_wise_args needs to be either a list or NULL")
    }
    if (!all(vapply(col_wise_args, length, numeric(1)) == ncol(col_by))) {
      stop("not all elements in col_wise_args have length ", ncol(col_by))
    }
  }
  invisible(NULL)
}

# #' Create a table with formulation
# #' 
# #' Formula elements on the LHS connected by a + mean stacking
# rtabulate.formula <- function(x, data) {
#   x <- Sepal.Length ~ Species
#   data <- iris
#   
#   # get model frame
#   mf <- match.call(expand.dots = FALSE)
#   names(mf) <- gsub("^x$", "formula", names(mf))
#   m <- match(c("formula", "data"), names(mf), 0L)
#   mf <- mf[c(1L, m)]
#   mf[[1L]] <- quote(stats::model.frame)
#   mf <- eval(mf, parent.frame())
#   
#   
#   
#   attributes(mf)
#   
#   mf <- match.call(lm, call = call("lm", x = Sepal.Length ~ Species, data = iris), expand.dots = FALSE)
#   
#   formula <- Sepal.Length ~ Species
#   
#   terms(formula)
#   
#   str(formula)
# }
