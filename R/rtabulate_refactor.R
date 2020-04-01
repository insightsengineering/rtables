#' Default tabulation
#'
#'
#' This function is used when \code{\link{rtabulate}} is invoked
#'
#' @param x the \emph{already split} data being tabulated for a particular cell/set of cells
#' @param \dots passed on directly
#'
#' @details This function has the following behavior given particular types of inputs:
#' \describe{
#' \item{numeric}{calls \code{\link{mean}} on \code{x}}
#' \item{logical}{calls \code{\link{sum}} on \code{x}}
#' \item{factor}{calls \code{\link{length}} on \code{x}}
#' }
#'
#' All other classes of input currently lead to an error.
#' @export
#' @rdname rtinner
#' @author Gabriel Becker and Adrian Waddell
setGeneric("rtab_inner", function(x, ...) standardGeneric("rtab_inner"))
#' @rdname rtinner
#' @exportMethod rtab_inner
setMethod("rtab_inner", "numeric", function(x, ...) mean(x,...))
#' @rdname rtinner
#' @exportMethod rtab_inner
setMethod("rtab_inner", "logical", function(x, ...) sum(x,...))
#' @rdname rtinner
#' @exportMethod rtab_inner
setMethod("rtab_inner", "factor", function(x, ...) as.list(table(x, ...)))
#' @rdname rtinner
#' @exportMethod rtab_inner
setMethod("rtab_inner", "ANY", function(x, ...) stop("No default rtabulate behavior for class ", class(x), " please specify FUN  explicitly."))



.rtab_colby_helper <- function(lyt,
                               cbyclass,
                               var = NA_character_,
                               newtoplev = FALSE,
                               extrargs = list()) {
    ## sadly takes different argument signature...
    ## TODO fix this?
    if(identical(cbyclass, "total"))
       return(add_col_total(lyt, lbl = var))

    addfun = switch(cbyclass,
                    quartcut_df = add_colby_qrtiles,
                    cmlquartcut_df = add_colby_cmlqrtiles,
                    add_colby_varlevels)
    lyt <- addfun(lyt, var = var,
                  newtoplev = nwetoplev,
                  extrargs = extrargs)
    lyt
}
                               
                               
                               
                               
                               
                               

#' Direct Tabulation
#'
#' \code{rtablulate} provides a direct tabulation API conceptually derived from \code{\link{tapply}}.
#' 
#' In practice, a Pre-data layout is built up based on the
#' arguments using hierarchical splitting for rows and columns as necessary, then analyzing all variables in \code{x} via \code{FUN} (which defaults to \code{\link{rtab_inner}}). This layout is then applied to the full data (the combination of \code{x}, \code{col_by} and, if non-null, \code{row_by}).
#'   
#' @param x either a vector or \code{data.frame}
#' @param ... arguments passed to methods
#' 
#' @return an \code{\link{rtable}} object
#' @note For backwards compatibility, all paramters other than \code{x}, \code{col_by} and \code{FUN} appear after \dots so must be specified by full argument name.
#' 
#' @author Gabriel Becker and Adrian Waddell
#'       
#' @export
#' 
#' 
rtabulate <- function(x,
                     col_by = by_all("col_1"),
                     FUN = rtab_inner,
                     ...,
                     row_by = NULL,                   
                     format = NULL,
                     row.name = "",
                     indent = 0,
                     col_wise_args = list(),
                     total = NULL,
                     col_N = NULL
                     )  {

    if(missing(FUN) && missing(row.name)) {
        if(inherits(x, "numeric"))
            row.name = "mean"
        else if (inherits(x, "logical"))
            row.name = "count"
    } else if (!missing(FUN) && missing(row.name)) {
        row.name = paste(deparse(substitute(FUN)), collapse = ";")
    }
    if(indent != 0) {
        .Deprecated("Setting indent directly in rtabulate calls is no longer supported. Ingoring.")

    }
    usexnms = FALSE
    lyt = NULL
    
    if(!inherits(x, "data.frame")) {
        if(is(x, "list")) {
            ## gotta guess whether this is many things to summarize or one...
            ## XXX this is a fast first pass, revisit this
            
            if(length(unique(sapply(x, length))) != 1)
                x = data.frame(.xvar = I(x))
            else {
                x = as.data.frame(x, stringsAsFactors = TRUE)
                usexnms = TRUE
            }
        } else {
            ## SAF=TRUE since that is what the inner
            ## method does anyway
            x = as.data.frame(x, stringsAsFactors = TRUE)
        }
    } else {
        usexnms = TRUE
    }
    lyt = NULL
    xcols = names(x)
    fulld = as.data.frame(x)

    if(is(col_by, "PreDataTableLayouts")) {
        ## we're done already
        lyt <- col_by
    } else {
        cby_class = class(col_by)[1]
        cby_df = as.data.frame(col_by)
        cby_nms = paste0(".xxx_cby_", seq_along(cby_df))
        names(cby_df) = cby_nms
        fulld = cbind(fulld, cby_df)
        newtop = FALSE
        if(!is.null(total)) {
            lyt <- .rtab_colby_helper(lyt, "total", total)
            newtop = TRUE
        }
        
        
        ## wrap it if there's only one to preserve old behavior
        if(length(cby_nms) == 1 && length(col_wise_args) > 0)
            col_wise_args <- list(col_wise_args)

        ## columns
        for(i in seq_along(cby_nms) ){
            ex = if(length(col_wise_args) > i)
                     col_wise_args[[i]]
                 else list()
            
            lyt <- .rtab_colby_helper(lyt, cby_class,
                                      var = cby_nms[i],
                                      newtoplev = newtop,
                                      extrargs = ex)
            newtop = FALSE
        }
        
    } 
    
    if(!is.null(row_by)) {
        rdf <- as.data.frame(row_by,
                             stringsAsFactors = FALSE)
        fulld <- cbind(fulld,
                       rdf)

        for(rspl in names(rdf)) {
            lyt <- add_rowby_varlevels(lyt,
                                       rspl)
        }
    }
    ## rows
    lbls = if(usexnms) xcols else ""
    lyt <- add_analyzed_vars(lyt,
                             var = xcols,
                             lbl = lbls,
                             rowlabs = row.name,
                             afun = FUN,
                             fmt = format)
    ## XXX a way to just return the layout?
    ## but x needs to be padded with the relevant
    ## columns so not sure...
    build_table(lyt, fulld)    
}


## rtabulate_ <- function(x,
##                       col_by = by_all("col_1"),
##                       row_by = NULL,
##                       FUN, ...,
##                       format = NULL,
##                       row.name = "",
##                       indent = 0,
##                       col_wise_args = NULL) {

 


    
##   UseMethod("rtabulate")
## }
NULL
# rtabulate default for vectors
# 
# This method is used for vectors of type \code{logical} and \code{numeric}
# 
# see parameter descrition for rtabulate.numeric
#

rtabulate_default <- function(x, col_by = by_all("col_1"), FUN, ...,
                              format = NULL, row.name = "", indent  = 0,
                              col_wise_args = NULL) {
  stopifnot(is.atomic(x)) # x[rows] only works for factors, not for data.frames
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
#' rtabulate(x = iris$Species, useNA = "always")
#' rtabulate(x = factor(c("a", "a", NA, "b")), useNA = "ifany")
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
#'  
rtabulate.factor <- function(x,
                             col_by = by_all("All"), 
                             FUN = length,
                             ...,
                             useNA = c("ifany", "no", "always"),
                             format = NULL,
                             indent  = 0,
                             col_wise_args = NULL) {
  
  useNA <- match.arg(useNA)
  
  if (any("<NA>" %in% levels(x))) {
    stop("factor with level '<NA>' is not valid in rtabulate.factor") 
  }
  has_na <- any(is.na(x))
  levels(x) <- c(levels(x), "<NA>")
  x[is.na(x)] <- "<NA>"
  
  ## note that splitting with empty-string creates a un-named list element
  if (any(levels(x) == "")) {
    if ("-" %in% levels(x)) {
      stop("x currently cannot have '' and levels called -")
    }
    levels(x) <- gsub("^$", "-", levels(x)) # replace "" -> "-"
    warning("'' levels were turned into level -")
  }
  
  columns <- levels(x)
  tbl <- rtabulate(as.data.frame(x), 
            row_by = x, 
            col_by = col_by, 
            FUN = FUN, 
            format = format, 
            indent = indent, 
            col_wise_args = col_wise_args,
            ...
  )
  
  if (useNA == "always" || useNA == "ifany" && has_na) {
    tbl
  } else {
    tbl[-nrow(tbl), ]
  }

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
  
  force(FUN) #todo: remove this everywhere as it is bad style
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
