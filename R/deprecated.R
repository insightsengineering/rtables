#nocov start

deprecation_message <- function() {
    message(paste("Note that the function", as.character(sys.call()), "is deprecated."))
}

add_analyzed_var = function(lyt, var, label = var, afun,
                            format = NULL,
                            rowlabs = "",
                            nested = FALSE,
                            inclNAs = FALSE) {
  spl = AnalyzeVarSplit(var, label,
                        afun = afun,
                        split_format = format,
                        defrowlab = rowlabs,
                        inclNAs = inclNAs)
  .Deprecated("analyze")

  if(!nested &&
     (is(last_rowsplit(lyt), "AnalyzeVarSplit") ||
      is(last_rowsplit(lyt), "AnalyzeMultiVars"))) {
    cmpnd_last_rowsplit(lyt, spl)
  } else {
    pos = next_rpos(lyt, nested)
    split_rows(lyt, spl, pos)
  }
}
# nocov end



#' Return an object with a label attribute (deprecated)
#' 
#' @param x an object
#' @param label label attribute to to attached to  \code{x}
#'
#' @export
#' @return \code{x} labeled by \code{label}. Note: the exact mechanism of labeling should be
#' considered an internal implementation detail, but the label will always be retrieved via \code{obj_label}.
#' @examples
#' x <- with_label(c(1,2,3), label = "Test")
#' obj_label(x)
with_label <- function(x, label) {
    deprecation_message()
    obj_label(x) <- label
    x
}

#' Get Label Attributes of Variables in a \code{data.frame}  (deprecated)
#'
#' Variable labels can be stored as a \code{label} attribute for each variable.
#' This functions returns a named character vector with the variable labels
#' (empty sting if not specified)
#'
#' @param x a \code{data.frame} object
#' @param fill boolean in case the \code{label} attribute does not exist if
#'   \code{TRUE} the variable names is returned, otherwise \code{NA}
#'
#' @return a named character vector with the variable labels, the names
#'   correspond to the variable names
#'
#' @export
#'
#' @examples
#' x <- iris
#' var_labels(x)
#' var_labels(x) <- paste("label for", names(iris))
#' var_labels(x)
var_labels <- function(x, fill = FALSE) {
    deprecation_message()
    
    stopifnot(is.data.frame(x))

    y <- Map(function(col, colname) {
        label <- attr(col, "label")
        
        if (is.null(label)) {
            if (fill) {
                colname
            } else {
                NA_character_
            }
        } else {
            if (!is.character(label) && !(length(label) == 1)) {
                stop("label for variable ", colname, "is not a character string")
            }
            as.vector(label)
        }
        
    }, x, colnames(x))
    
    labels <- unlist(y, recursive = FALSE, use.names = TRUE)
    
    if (!is.character(labels)) {
        stop("label extraction failed")
    }
    
    labels
    
}


#' Set Label Attributes of All Variables in a \code{data.frame} (deprecated)
#'
#' Variable labels can be stored as a \code{label} attribute for each variable.
#' This functions sets all non-missing (non-NA) variable labels in a \code{data.frame}
#'
#' @inheritParams var_labels
#' @param value new variable labels, \code{NA} removes the variable label
#'
#' @return modifies the variable labels of \code{x}
#'
#' @export
#'
#' @examples
#' x <- iris
#' var_labels(x)
#' var_labels(x) <- paste("label for", names(iris))
#' var_labels(x)
#'
#' if(interactive()){
#' View(x) # in RStudio data viewer labels are displayed
#' }
`var_labels<-` <- function(x, value) {
    deprecation_message()
    stopifnot(
        is.data.frame(x),
        is.character(value),
        ncol(x) == length(value)
    )
    
    # across columns of x
    for (j in seq_along(x)) {
        attr(x[[j]], "label") <- if (!is.na(value[j])) {
            value[j]
        } else {
            NULL
        }
    }
    
    x
}


#' Copy and Change Variable Labels of a \code{data.frame} (deprecated)
#'
#' Relabel a subset of the variables
#'
#' @inheritParams var_labels<-
#' @param ... name-value pairs, where name corresponds to a variable name in
#'   \code{x} and the value to the new variable label
#'
#' @return a copy of \code{x} with changed labels according to \code{...}
#'
#' @export
#'
#' @examples
#' x <- var_relabel(iris, Sepal.Length = "Sepal Length of iris flower")
#' var_labels(x)
#'
var_relabel <- function(x, ...) {
    deprecation_message()
    # todo: make this function more readable / code easier
    stopifnot(is.data.frame(x))
    if (missing(...)) {return(x)}
    dots <- list(...)
    varnames <- names(dots)
    stopifnot(!is.null(varnames))
    
    map_varnames <- match(varnames, colnames(x))
    
    if (any(is.na(map_varnames))) {
        stop("variables: ", paste(varnames[is.na(map_varnames)], collapse = ", "), " not found")
    }
    
    if (any(vapply(dots, Negate(is.character), logical(1)))) {
        stop("all variable labels must be of type character")
    }
    
    for (i in seq_along(map_varnames)) {
        attr(x[[map_varnames[[i]]]], "label") <-  dots[[i]]
    }
    
    x
}


#' Remove Variable Labels of a \code{data.frame} (deprecated)
#'
#' Removing labels attributes from a variables in a data frame
#'
#' @param x a \code{data.frame} object
#'
#' @return the same data frame as \code{x} stripped of variable labels
#'
#' @export
#'
#' @examples
#' x <- var_labels_remove(iris)
var_labels_remove <- function(x) {
    deprecation_message()
    stopifnot(is.data.frame(x))
    
    for (i in 1:ncol(x)) {
        attr(x[[i]], "label") <- NULL
    }
    
    x
}
