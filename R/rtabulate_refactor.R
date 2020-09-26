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
#'
#' @examples
#' rtab_inner(1:3)
#' rtab_inner(iris$Species)
#' rtab_inner(iris$Species == "setosa")
setGeneric("rtab_inner", function(x, ...) standardGeneric("rtab_inner"))

#' @rdname rtinner
#' @exportMethod rtab_inner
setMethod("rtab_inner", "numeric", function(x, ...) in_rows("Mean" = rcell(mean(x,...), format = "xx.xx")))

#' @rdname rtinner
#' @exportMethod rtab_inner
setMethod("rtab_inner", "logical", function(x, ...) in_rows("Count" = rcell(sum(x,...), format = "xx")))

#' @rdname rtinner
#' @exportMethod rtab_inner
setMethod("rtab_inner", "factor", list_wrap_x(table))

#' @rdname rtinner
#' @exportMethod rtab_inner
setMethod("rtab_inner", "ANY",
          function(x, ...) {
              stop("No default rtabulate behavior for class ", class(x), " please specify FUN  explicitly.")
          })



.rtab_colby_helper <- function(lyt,
                               cbyclass,
                               var = NA_character_,
                               nested = FALSE,
                               extra_args = list()) {
    ## sadly takes different argument signature...
    ## TODO fix this?
    if(identical(cbyclass, "total"))
       return(add_overall_col(lyt, label = var))
    stopifnot(length(cbyclass) == 1)
    addfun = switch(cbyclass,
                    quartcut_df = split_cols_by_quartiles,
                    cmlquartcut_df = function(...) split_cols_by_quartiles(..., cumulative = TRUE),
                    split_cols_by)
    lyt <- addfun(lyt, var = var,
                  nested = nested,
                  extra_args = extra_args)
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
#' @param ... arguments passed to the tabulation function
#' @inheritParams compat_args
#' @param FUN a function that processes the cell data
#' @param total string of column name of an added total column using \code{\link[rtables]{by_add_total}} to
#'   \code{col_by} and \code{\link[tern]{col_N_add_total}} to \code{col_N}. If \code{NULL} no total column is added.
#' @param col_N numeric. If non-NULL, counts to override total column counts.
#'

#'
#' @return an \code{\link{rtable}} object
#' @note For backwards compatibility, all paramters other than \code{x}, \code{col_by} and \code{FUN} appear after \dots so must be specified by full argument name.
#'
#' @author Gabriel Becker and Adrian Waddell
#'
#' @export
#'
#' @examples
#' ## logical
#' rtabulate(iris$Species == "setosa", by_all("Species"),
#'   FUN = function(x, N) c(sum(x), sum(x)/N),
#'   row.name = "n (n/N)",
#'   col_wise_args = list(N = 150),
#'   format = "xx (xx.xx%)")
#'
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
#'
#' ## numerics
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
#' # TODO: enable sprintf_format in rtabulate
#'  x <- 1:100
#'  cb <- factor(rep(LETTERS[1:3], c(20, 30, 50)))
#'
#'  rtabulate(
#'    x = x, col_by = cb,
#'    FUN = function(x, N) c(mean(x), sd(x), N),
#'    format = sprintf_format("%.2f (%.2f) and %i"),
#'    row.name = "Mean (SD) and N",
#'    col_wise_args = list(N = table(cb))
#'  )
#'
#'
#'
#' ## factors
#' rtabulate(x = iris$Species)
#'
#'  rtabulate(x = iris$Species, useNA = "always")
#'  rtabulate(x = factor(c("a", "a", NA, "b")), useNA = "ifany")
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
#'           FUN = function(dat, .N_total) {
#'     tb <- table(dat)
#'     lapply(tb, function(cell_count) {
#'      if (cell_count > 10) {
#'         cell_count * c(1, 1/.N_total)
#'      } else {
#'         rcell("-", format = "xx")
#'      }
#'      })
#'    },
#'    format = "xx (xx.xx%)")
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
#'   FUN = function(x, N) lapply(table(x), function(tbi) c(tbi, N)),
#'   col_wise_args = list(N = c(1,2))
#' )
#'
#' ## data.frames
#' df <- expand.grid(aaa = factor(c("A", "B")), bbb = factor(c("X", "Y", "Z")))
#' df <- rbind(df, df)
#' df$val <- 1:nrow(df)
#'
#'  rtabulate(
#'    x = df,
#'    row_by = df$aaa,
#'    col_by = df$bbb,
#'    FUN = function(df) {
#'       sum(df$val)
#'    }
#'  )
#'
#'  ## this is largely nonsensical, all is not really a "row by" here
#'  ## that's why we get the label row and data row
#'  rtabulate(
#'    x = iris,
#'    row_by = by_all("sum"),
#'    col_by = iris$Species,
#'    FUN = function(df) sum(df$Sepal.Length)
#'  )
#'
#' ## standard way would be this
#' rtabulate(
#'     x = iris,
#'     col_by = iris$Species,
#'     FUN = function(df) sum(df$Sepal.Length),
#'     row.name =  "sum")
#'
#'
#'  rtabulate(
#'    x = iris,
#'    row_by = iris$Species,
#'    col_by = by_all("sum"),
#'    FUN = function(df) sum(df$Sepal.Length)
#'  )
#'
#'  fsl5 <- factor(iris$Sepal.Length > 5, levels = c(TRUE, FALSE),
#'      labels = c("S.L > 5", "S.L <= 5"))
#'
#'  tbl <- rtabulate(
#'    x = iris,
#'    row_by = fsl5,
#'    col_by = iris$Species,
#'    FUN = function(df) {
#'      if (nrow(df) < 10) {
#'        rcell("-")
#'      } else {
#'        fit <- lm(Sepal.Length ~ Petal.Width, data = df)
#'
#'        rcell(list(fit), format = function(x, output) {
#'            paste("df:", x[[1]]$df.residual)
#'        })
#'      }
#'    }
#'  )
#'  tbl
#'
#'  rtabulate(
#'    x = iris,
#'    row_by = fsl5,
#'    col_by = iris$Species,
#'    FUN = function(df, N) {
#'       N
#'    },
#'    col_wise_args = list(N = c(10, 100, 200))
#'  )
#' #
#' #
#' #
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
        if(startsWith(row.name, "function("))
            row.name = ""
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
                xdf = data.frame(.xvar = I(x))
            else {
                xdf = as.data.frame(x, stringsAsFactors = TRUE)
                usexnms = TRUE
            }
        } else {
            ## SAF=TRUE since that is what the inner
            ## method does anyway
            xdf = as.data.frame(x, stringsAsFactors = TRUE)
        }
        xcols = names(xdf)
    } else {
        ## ensure there are NO NAs...
        xdf = cbind(x, ".xx_xproxy_xx." = TRUE)
        xcols = ".xx_xproxy_xx."
    }
    lyt = NULL

    fulld = xdf

    if(is(col_by, "Split")) {
        ## we're done already
        lyt <- split_cols(lyt, col_by, next_cpos(lyt, FALSE))
        if(length(col_wise_args) > 0) {
            ##awww, darn :-/
            clyt <- clayout(lyt)
            if(length(clyt) > 1)
                stop("I don't know where to put these col_wise_args. Please contact the maintainer")
            splvec <- clyt[[1]]
            tmp <- splvec[[length(splvec)]]
            split_exargs(tmp) <- c(split_exargs(tmp), col_wise_args)
            splvec[[length(splvec)]] <- tmp
            clyt[[1]] <- splvec
            clayout(lyt) <- clyt
        }
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
            ex = if(length(col_wise_args) >= i)
                     col_wise_args[[i]]
                 else list()

            lyt <- .rtab_colby_helper(lyt, cby_class,
                                      var = cby_nms[i],
                                      nested = !newtop,
                                      extra_args = ex)
            newtop = FALSE
        }

    }

    if(is(row_by, "Split")) {
        lyt <- split_rows(lyt, row_by, next_rpos(lyt, FALSE))
    } else if(!is.null(row_by)) {
        rdf <- as.data.frame(row_by,
                             stringsAsFactors = FALSE)
        fulld <- cbind(fulld,
                       rdf)

        for(rspl in names(rdf)) {
            lyt <- split_rows_by(lyt,
                                 rspl,
                                 split_label ="",
                                 child_labels = "hidden")
        }
    }
    ## rows
    labels = row.name
    if(nchar(labels) == 0 && usexnms)
       labels = xcols

    lyt <- analyze(lyt,
                   vars = xcols,
                   afun = FUN,
                   var_labels = "",
                   format = format,
                   extra_args = list(list(...)),
                   show_labels = "hidden")
    ## XXX a way to just return the layout?
    ## but x needs to be padded with the relevant
    ## columns so not sure...
    build_table(lyt, fulld)
}
