

#' Create custom analysis function wrapping existing function
#' @param fun function. The function to be wrapped in a new customized analysis fun. Should return named list.
#' @param .stats character. Names of elements to keep from \code{fun}'s full output.
#' @param .formats ANY. vector/list of formats to override any defaults applied by \code{fun}.
#' @param .labels character. Vector of labels to override defaults returned by \code{fun}
#' @param ... dots. Additional arguments to \code{fun} which effectively become new defaults. These can still be overriden by extra args within a split.
#' @return A function suitable for use in \code{\link{analyze}} with element selection, reformatting, and relabeling performed automatically.
#' @seealso [analyze()]
#' @export

make_afun <- function(fun, .stats, .formats = NULL, .labels = NULL, ...) {
    ## too clever by three-quarters (because half wasn't enough)
    ## gross scope hackery
    fun_args = list(...)
    ## so these are guaranteed to be defined for the list construction below
    ## hacky but everything else I thought of was worse.
    .N_col = NULL
    .N_total = NULL
    .ref_group = NULL
    .in_ref_col = NULL
    ret <- function() {
        custargs <- fun_args

        exargs <- list(...)
        custargs[names(exargs)] <- exargs
        sfunargs <- list(x = x,
                         .N_col = .N_col,
                         .N_total = .N_total,
                         .ref_group = .ref_group,
                         .in_ref_col = .in_ref_col)
        sfunargs <- sfunargs[names(formals(fun))]

        rawvals <- do.call(fun, c(sfunargs, custargs))

        ## note single brackets here so its a list
        ## no matter what. thats important!
        final_vals <- rawvals[.stats]
        if(are(final_vals, "CellValue"))
            rcells = final_vals
        else
            rcells = lapply(final_vals, rcell)

        if(!is.null(.labels)) {
            if(is.null(names(.labels))) {
                stopifnot(length(.labels) == length(.stats))
                names(.labels) <- names(final_vals)
            } else {
                lblpos <- match(names(.labels), names(final_vals))
                stopifnot(!anyNA(lblpos))
            }
            final_labels <- value_labels(final_vals)
            nolab <- vapply(final_labels, function(v) length(v) == 0, NA)
            final_labels[nolab] = .stats[nolab]
            names(final_labels) <- names(final_vals)
            final_labels[names(.labels)] = .labels
        }

        if(!is.null(.formats)) {
            if(is.null(names(.formats))) {
                stopifnot(length(.formats) == length(.stats))
                names(.formats) <- names(final_vals)
            } else {
                fmtpos <- match(names(.formats), names(final_vals))
                stopifnot(!anyNA(fmtpos))
            }
            final_formats <- lapply(final_vals, obj_format) ## NULL if its a raw value
            names(final_formats) <- names(final_vals)
            final_formats[names(.formats)] = .formats
        }
        rcells <- mapply(rcell, format = final_formats, x = final_vals,
                         label = final_labels,
                         SIMPLIFY = FALSE)
        in_rows(.list = rcells) ##, .labels = .labels)
    }
    formals(ret) <- formals(fun)
    ret
}


