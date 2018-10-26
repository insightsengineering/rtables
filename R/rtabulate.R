
#' Tabulation Methods
#'
#' \code{rtablulate} provides a number of methods to derive
#' \code{\link{rtable}}s. Conceptually the \code{rtabulate} has it's origin in
#' \code{\link{tapply}}.
#' 
#' The data is split into cell-data and a function can be specified that return
#' a data structre (or \code{\link{rcell}}).
#'   
#' @param x either a vector or \code{data.frame}
#' @param ... arguments passed to methods
#' 
#' @return an \code{\link{rtable}} project
#' 
#' @author Adrian Waddell
#'       
#' @export
#' 
rtabulate <- function(x, ...) {
  UseMethod("rtabulate")
}


#' Do not split data into columns or row in \code{rtabulate}
#' 
#' \code{\link{rtabulate}} has the arguments \code{col_by} and \code{row_by}
#' which can either take a vector or if no splitting is needed the return value
#' of \code{no_by}.
#' 
#' @param name row name or column name
#' 
#' @export
#' 
no_by <- function(name) {
  structure(name, class = "no_by")
}


#' Check if object inherits from the \code{no_by} Class
#' 
#' Functions to test inheritance on \code{no_by}
#' 
#' @param x an object
#' 
#' @return \code{TRUE} or \code{FALSE}
#' 
#' @export
is.no_by <- function(x) {
  is(x, "no_by")
}

#' @export
levels.no_by <- function(x) as.vector(x)

# rtabulate default for vectors
# 
# this method is used for vectors of type \code{logical} and \code{numeric}
# 
# see parameter descrition for rtabulate.numeric
#
rtabulate_default <- function(x, col_by = no_by("col_1"), FUN, ...,
                              format = NULL, row.name = "", indent  = 0,
                              col_wise_args = NULL) {
  
  force(FUN)
  check_stop_col_by(col_by, col_wise_args)
  
  column_data <- if (is.no_by(col_by)) {
    setNames(list(x), col_by)
  } else {
    if (length(x) != length(col_by)) stop("dimension missmatch x and col_by")
    split(x, col_by, drop = FALSE)
  }
  
  cells <- if (is.null(col_wise_args)) {
    
    lapply(column_data, FUN, ...)
    
  } else {
    
    dots <- list(...)
    args <- lapply(seq_len(nlevels(col_by)), function(i) c(dots, lapply(col_wise_args, `[[`, i)))
    
    Map(function(xi, argsi) {
      do.call(FUN, c(list(xi), argsi))
    }, column_data, args)
  }
  
  rr <- rrowl(row.name = row.name, cells, format = format, indent = indent)
  
  rtable(header = levels(col_by), rr)
}



#' tabulate a numeric vector
#'
#' by default the \code{\link[stats]{fivenum}} function is applied to the
#' vectors associated to the cells
#'
#'
#' @inheritParams rrow
#' @param x a vecor
#' @param col_by a \code{\link{factor}} of length \code{nrow(x)} that defines
#'   which levels in \code{col_by} define a column. If data should not be split
#'   into columns use the \code{\link{no_by}} function.
#' @param FUN a function that processes the cell data, if \code{row_data_arg} is
#'   set to \code{TRUE} the a second argument with the row data is passed to
#'   \code{FUN}
#' @param ... arguments passed to \code{FUN}
#' @param row_data_arg call \code{FUN} with the row data as the second argument
#' @param format if \code{FUN} does not return a formatted \code{\link{rcell}}
#'   then the \code{format} is applied
#' @param row.name if \code{NULL} then the \code{FUN} argument is deparsed and
#'   used as \code{row.name} of the \code{\link{rrow}}
#' @param col_N The column total for each column. If specified then
#'   \code{\link{col_N}()} can be used on the cell data in order to retrieve the
#'   column total. If \code{NULL} then no header row for the column is
#'   displayed.
#'
#' @inherit rtabulate return
#'
#' @export
#'
#' @examples
#'
#' rtabulate(iris$Sepal.Length)
#'
#' rtabulate(iris$Sepal.Length, col_by = no_by("Sepal.Length"))
#'
#' with(iris,  rtabulate(x = Sepal.Length, col_by = Species, row.name = "fivenum"))
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
rtabulate.numeric <- function(x, col_by = no_by("col_1"), FUN = mean, ...,
                              format = NULL, row.name = NULL,
                              indent  = 0, col_wise_args = NULL) {
  
  if (is.null(row.name)) row.name <- paste(deparse(substitute(FUN)), collapse = ";")
  
  rtabulate_default(
    x = x, col_by = col_by, FUN = FUN, ...,
    format = format, row.name = row.name, indent = indent,
    col_wise_args = col_wise_args
  )
}

#' tabulate a logical vector
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
#' rtabulate(iris$Species == "setosa", no_by("Species"),
#'    FUN = function(x, N) list(sum(x), sum(x)/N),
#'    row.name = "n (n/N)",
#'    col_wise_args = list(N = 150))
#' 
#' # default: percentages equal \code{TRUE}
#' with(iris, rtabulate(Sepal.Length < 5, Species, row.name = "Sepal.Length < 5"))
#'  
#' # precentages with proportion of cell number of \code{TRUE}s to overvall
#' # number of \code{TRUE}s
#' with(iris, rtabulate(Sepal.Length < 5, Species,
#'   FUN = function(xi, N) sum(xi) * c(1, 1/N), 
#'   format = "xx.xx (xx.xx)",
#'   row.name = "Sepal.Length < 5",
#'   col_wise_args = list(N = table(cb))
#' ))
#' 
rtabulate.logical <- function(x, col_by = no_by("col_1"),
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
#' @inheritParams rtabulate.numeric
#' @param row_col_data_args boolean, if \code{TRUE} then \code{FUN} is called
#'   with the first three arguments being the cell, row, and column data,
#'   respectively
#' @param useNA boolean, if \code{TRUE} then \code{NA} values in \code{x} get
#'   turned into a factor level \code{"NA"}, if \code{FALSE} then the \code{NA}
#'   values in \code{x} get dropped.
#' 
#' @inherit rtabulate return
#' 
#' @export
#'
#' @examples 
#' 
#' rtabulate(x = iris$Species)
#' 
#' rtabulate(x = iris$Species, no_by("sum"))
#' 
#' sl5 <- factor(iris$Sepal.Length > 5, levels = c(TRUE, FALSE),
#'    labels = c("S.L > 5", "S.L <= 5"))
#' 
#' rtabulate(iris$Species, col_by=sl5)
#' 
#' rtabulate(iris$Species, col_by=sl5,
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
#' rtabulate(sl5, iris$Species)
#' 
#' 
#' 
#' rtabulate(x = factor(c("X", "Y"), c("X", "Y")), col_by = factor(c("a", "a"), c("a", "b")), FUN = length)
#' 
#' rtabulate(factor(c("Y", "Y"), c("X", "Y")), factor(c("b", "b"), c("a", "b")), length)
#' 
#' 
#' rtabulate(
#'   x = factor(c("Y", "Y"), c("X", "Y")),
#'   col_by = factor(c("b", "b"), c("a", "b")),
#'   FUN = function(x, N) list(length(x), N),
#'   col_wise_args = list(N = c(1,2))
#' )
#' 
#' 
rtabulate.factor <- function(x,
                             col_by = no_by("col_1"), 
                             FUN = length,
                             ...,
                             useNA = c("no", "ifany", "always"),
                             format = NULL,
                             indent  = 0,
                             col_wise_args = NULL) {

  force(FUN)
  check_stop_col_by(col_by, col_wise_args)

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
    if ("-" %in% levels(x)) stop("x currently cannot have '' and levels called -")
    levels(x) <- gsub("^$", "-", levels(x))
    warning("'' levels were turned into level -")
  }
  
  
  # cell_data = list(row1 = list(col1, col2, ...), row2 = list(col1, col2, ...), ...)
  cell_data_by_row <- if (is.no_by(col_by)) {
    lapply(split(x, x, drop = FALSE), function(row_i) setNames(list(row_i), col_by))
  } else {
    if (length(x) != length(col_by)) stop("dimension missmatch x and col_by")
    df <- data.frame(
      x = x,
      col_by = col_by
    )
    lapply(split(df, df$x, drop = FALSE), function(row_i) {
      split(row_i$x, row_i$col_by, drop = FALSE)
    })
  }
  
  
  cells_by_row <- if (is.null(col_wise_args)) {
    
    lapply(cell_data_by_row, function(row_i) lapply(row_i, FUN, ...)) 
    
  } else {
    
    dots <- list(...)
    args <- lapply(seq_len(nlevels(col_by)), function(i) c(dots, lapply(col_wise_args, `[[`, i)))
    
    lapply(cell_data_by_row, function(row_i) {
      Map(function(xi, argsi) {
        do.call(FUN, c(list(xi), argsi))
      }, row_i, args)
    })
  }
  
  rrows <- Map(function(row, rowname) rrowl(rowname, row, format = format, indent = indent), 
               cells_by_row, names(cells_by_row)) 
  
  rtablel(header = levels(col_by), rrows)
}




#' Split data.frame and apply functions
#' 
#' @inheritParams rtabulate.factor
#' @param row_by_var name of factor variable in \code{x}
#' @param col_by_var name of factor variable in \code{x}
#' 
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
#'   row_by = no_by("sum"),
#'   col_by = iris$Species, 
#'   FUN = function(x) sum(x$Sepal.Length)
#' )
#' 
#' rtabulate(
#'   x = iris,
#'   row_by = iris$Species,
#'   col_by = no_by("sum"), 
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
#' 
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
  check_stop_col_by(col_by, col_wise_args)
  check_stop_col_by(row_by)
  

  # list(row1 = list(c1, c2, ...), row2 = list(c1, c2, ...), ...)
  cell_data <- if (!is.no_by(row_by) && !is.no_by(col_by)) {
    xs <- split(x, row_by, drop = FALSE)
    cs <- split(col_by, row_by, drop = FALSE)
    setNames(Map(function(xi, col_by_i) split(xi, col_by_i), xs, cs), levels(row_by))
  } else if (is.no_by(row_by) && !is.no_by(col_by)) {
    setNames(list(split(x, col_by, drop = FALSE)), row_by)
  } else if (!is.no_by(row_by) && is.no_by(col_by)) {
    lapply(split(x, row_by, drop = FALSE), function(xi) list(xi))
  } else if (is.no_by(row_by) && is.no_by(col_by)) {
    setNames(list(list(x)), row_by)
  } else {
    stop("unexpected col_by & row_by combination")
  }

  
  cells_by_row <- if (is.null(col_wise_args)) {
    lapply(cell_data, function(row_i) lapply(row_i, FUN, ...))
  } else {
    
    dots <- list(...)
    args <- lapply(seq_len(nlevels(col_by)), function(i) c(dots, lapply(col_wise_args, `[[`, i)))
    
    lapply(cell_data, function(row_i) {
      Map(function(xi, argsi) {
        do.call(FUN, c(list(xi), argsi))
      }, row_i, args)
    })
    
  }
  
  rrows <- Map(function(cells_row, rowname) {
    rrowl(row.name = rowname, cells_row, format = format, indent = indent)
  }, cells_by_row, names(cells_by_row))
  
  rtablel(header = levels(col_by), rrows)
}



check_stop_col_by <- function(col_by, col_wise_args = NULL) {
  if (!is.factor(col_by) && !is.no_by(col_by)) stop("col_by is required to be a factor or no_by object")
  if (any(is.na(col_by))) stop("col_by does currently not support any NAs")
  
  if (!is.null(col_wise_args)) {
    if (!is.list(col_wise_args))
      stop("col_wise_args needs to be either a list or NULL")
    
    if (!all(vapply(col_wise_args, length, numeric(1)) == nlevels(col_by)))
      stop("not all elements in col_wise_args have length ", nlevels(col_by))
  }
  TRUE
}


stop_if_has_na <- function(x) {
  if (any(is.na(x))) stop(paste0(deparse(substitute(x)), " does currently not support any NAs"))
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
