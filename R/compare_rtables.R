#' Compare two rtables
#'
#' Prints a matrix where \code{.} means cell matches, \code{X} means cell does
#' cells do not match, \code{+} cell (row) is missing, and \code{-} cell (row)
#' should not be there. If `structure` is set to `TRUE`, \code{C} indicates
#' columnar structure mismatch, \code{R} indicates row-structure mismatch, and
#' \code{S} indicates mismatch in both row and column structure.
#'
#' @param object `rtable` to test
#' @param expected `rtable` expected
#' @param tol numerical tolerance
#' @param comp.attr boolean. Compare format of cells. Other attributes are
#'  silently ignored.
#' @param structure boolean. Should structure (in the form of column and row
#'  paths to cells) be compared. Currently defaults to `FALSE`, but this is
#'  subject to change in future versions.
#'
#' @note In its current form \code{compare_rtables} does not take structure into
#' account, only row and cell position.
#'
#' @return a matrix of class \code{"rtables_diff"} representing the differences
#'  between \code{object} and \code{expected} as described above.
#' @export
#'
#' @examples
#' t1 <- rtable(header = c("A", "B"), format = "xx", rrow("row 1", 1, 2))
#' t2 <- rtable(header = c("A", "B", "C"), format = "xx", rrow("row 1", 1, 2, 3))
#'
#' compare_rtables(object = t1, expected = t2)
#'
#' if (interactive()) {
#'   Viewer(t1, t2)
#' }
#'
#' expected <- rtable(
#'   header = c("ARM A\nN=100", "ARM B\nN=200"),
#'   format = "xx",
#'   rrow("row 1", 10, 15),
#'   rrow(),
#'   rrow("section title"),
#'   rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
#' )
#'
#' expected
#'
#' object <- rtable(
#'   header = c("ARM A\nN=100", "ARM B\nN=200"),
#'   format = "xx",
#'   rrow("row 1", 10, 15),
#'   rrow("section title"),
#'   rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
#' )
#'
#' compare_rtables(object, expected, comp.attr = FALSE)
#'
#' object <- rtable(
#'   header = c("ARM A\nN=100", "ARM B\nN=200"),
#'   format = "xx",
#'   rrow("row 1", 10, 15),
#'   rrow(),
#'   rrow("section title")
#' )
#'
#' compare_rtables(object, expected)
#'
#' object <- rtable(
#'   header = c("ARM A\nN=100", "ARM B\nN=200"),
#'   format = "xx",
#'   rrow("row 1", 14, 15.03),
#'   rrow(),
#'   rrow("section title"),
#'   rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.xx, xx.xx)"))
#' )
#'
#' compare_rtables(object, expected)
#'
#' object <- rtable(
#'   header = c("ARM A\nN=100", "ARM B\nN=200"),
#'   format = "xx",
#'   rrow("row 1", 10, 15),
#'   rrow(),
#'   rrow("section title"),
#'   rrow("row colspan", rcell(c(.345543, .4432423), colspan = 2, format = "(xx.x, xx.x)"))
#' )
#'
#' compare_rtables(object, expected)
compare_rtables <- function(object, expected, tol = 0.1, comp.attr = TRUE,
                            structure = FALSE) {
  # if (identical(object, expected)) return(invisible(TRUE))

  if (!is(object, "VTableTree")) {
    stop(
      "argument object is expected to be of class TableTree or ",
      "ElementaryTable"
    )
  }
  if (!is(expected, "VTableTree")) {
    stop(
      "argument expected is expected to be of class TableTree or ",
      "ElementaryTable"
    )
  }
  dim_out <- apply(rbind(dim(object), dim(expected)), 2, max)

  X <- matrix(rep(".", dim_out[1] * dim_out[2]), ncol = dim_out[2])
  row.names(X) <- as.character(1:dim_out[1])
  colnames(X) <- as.character(1:dim_out[2])

  if (!identical(names(object), names(expected))) {
    attr(X, "info") <- "column names are not the same"
  }

  if (!comp.attr) {
    attr(X, "info") <- c(
      attr(X, "info"),
      "cell attributes have not been compared"
    )
  }
  if (!identical(row.names(object), row.names(expected))) {
    attr(X, "info") <- c(attr(X, "info"), "row labels are not the same")
  }

  nro <- nrow(object)
  nre <- nrow(expected)
  nco <- ncol(object)
  nce <- ncol(expected)

  if (nco < nce) {
    X[, seq(nco + 1, nce)] <- "-"
  } else if (nce < nco) {
    X[, seq(nce + 1, nco)] <- "+"
  }
  if (nro < nre) {
    X[seq(nro + 1, nre), ] <- "-"
  } else if (nre < nro) {
    X[seq(nre + 1, nro), ] <- "+"
  }

  orig_object <- object # nolint
  orig_expected <- expected # nolint
  if (nro != nre || nco != nce) {
    object <- object[1:min(nro, nre), 1:min(nco, nce), drop = FALSE]
    expected <- expected[1:min(nro, nre), 1:min(nco, nce), drop = FALSE]
    inner <- compare_rtables(object, expected, tol = tol, comp.attr = comp.attr, structure = structure)
    X[seq_len(nrow(object)), seq_len(ncol(object))] <- inner
    class(X) <- c("rtables_diff", class(X))
    return(X)
  }

  ## from here dimensions match!


  orows <- cell_values(object, omit_labrows = FALSE)
  erows <- cell_values(expected, omit_labrows = FALSE)
  if (nrow(object) == 1) {
    orows <- list(orows)
    erows <- list(erows)
  }
  res <- mapply(compare_rrows,
    row1 = orows, row2 = erows, tol = tol, ncol = ncol(object),
    USE.NAMES = FALSE, SIMPLIFY = FALSE
  )
  X <- do.call(rbind, res)
  rpo <- row_paths(object)
  rpe <- row_paths(expected)

  if (comp.attr) {
    ofmts <- value_formats(object)
    efmts <- value_formats(expected)
    ## dim(ofmts) <- NULL
    ## dim(efmts) <- NULL

    fmt_mismatch <- which(!mapply(identical, x = ofmts, y = efmts)) ## inherently ignores dim


    ## note the single index here!!!, no comma!!!!
    X[fmt_mismatch] <- "X"
  }


  if (structure) {
    rp_mismatches <- !mapply(identical, x = rpo, y = rpe)
    cpo <- col_paths(object)
    cpe <- col_paths(expected)
    cp_mismatches <- !mapply(identical, x = cpo, y = cpe)

    if (any(rp_mismatches)) { # P for (row or column) path do not match
      X[rp_mismatches, ] <- "R"
    }
    if (any(cp_mismatches)) {
      crep <- rep("C", nrow(X))
      if (any(rp_mismatches)) {
        crep[rp_mismatches] <- "P"
      }
      X[, cp_mismatches] <- rep(crep, sum(cp_mismatches))
    }
  }
  class(X) <- c("rtables_diff", class(X))
  X
}



## for (i in 1:dim(X)[1]) {
##   for (j in 1:dim(X)[2]) {

##     is_equivalent <- TRUE
##     if (i <= nro && i <= nre && j <= nco && j <= nce) {
##       x <- object[i,j, drop = TRUE]
##       y <- expected[i,j, drop = TRUE]

##       attr_x <- attributes(x)
##       attr_y <- attributes(y)

##       attr_x_sorted <- if (is.null(attr_x)) NULL else attr_x[order(names(attr_x))]
##       attr_y_sorted <- if (is.null(attr_y)) NULL else attr_y[order(names(attr_y))]

##       if (comp.attr && !identical(attr_x_sorted, attr_y_sorted)) {
##         is_equivalent <- FALSE
##       } else if (is.numeric(x) && is.numeric(y)) {
##         if (any(abs(na.omit(x - y)) > tol)) {
##           is_equivalent <- FALSE
##         }
##       } else {
##         if (!identical(x, y)) {
##           is_equivalent <- FALSE
##         }
##       }

##       if (!is_equivalent) {
##         X[i,j] <- "X"
##       }
##     } else if (i > nro || j > nco) {
##       ## missing in object
##       X[i, j] <- "+"
##     } else {
##       ## too many elements
##       X[i, j] <- "-"
##     }
##   }
## }
## class(X) <- c("rtable_diff", class(X))
## X
## }

compare_value <- function(x, y, tol) {
  if (identical(x, y) || (is.numeric(x) && is.numeric(y) && max(abs(x - y)) <= tol)) {
    "."
  } else {
    "X"
  }
}
compare_rrows <- function(row1, row2, tol, ncol) {
  if (length(row1) == ncol && length(row2) == ncol) {
    mapply(compare_value, x = row1, y = row2, tol = tol, USE.NAMES = FALSE)
  } else if (length(row1) == 0 && length(row2) == 0) {
    rep(".", ncol)
  } else {
    rep("X", ncol)
  }
}

## #' @export
## print.rtable_diff <- function(x, ...) {
##   print.default(unclass(x), quote = FALSE, ...)
## }
