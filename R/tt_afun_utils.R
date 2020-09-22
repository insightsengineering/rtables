

#' Create custom analysis function wrapping existing function
#'
#' @param fun function. The function to be wrapped in a new customized analysis fun. Should return named list.
#' @param .stats character. Names of elements to keep from \code{fun}'s full output.
#' @param .formats ANY. vector/list of formats to override any defaults applied by \code{fun}.
#' @param .labels character. Vector of labels to override defaults returned by \code{fun}
#' @param ... dots. Additional arguments to \code{fun} which effectively become new defaults. These can still be
#'   overriden by extra args within a split.
#'   
#' @return A function suitable for use in \code{\link{analyze}} with element selection, reformatting, and relabeling
#'   performed automatically.
#'   
#' @seealso [analyze()]
#' 
#' @export
#' 
#' @examples 
#' 
#' s_summary <- function(x) {
#'   stopifnot(is.numeric(x))
#'   
#'   list(
#'     n = sum(!is.na(x)),
#'     mean_sd = c(mean = mean(x), sd = sd(x)),
#'     min_max = range(x)
#'   )
#' }
#' 
#' s_summary(iris$Sepal.Length)
#' 
#' a_summary <- make_afun(
#'   fun = s_summary,
#'   .formats = c(n = "xx", mean_sd = "xx.xx (xx.xx)", min_max = "xx.xx - xx.xx"),
#'   .labels = c(n = "n", mean_sd = "Mean (sd)", min_max = "min - max")
#' )
#' 
#' a_summary(x = iris$Sepal.Length)
#' a_summary(x = iris$Sepal.Length, .labels = c(min_max = "Range"))
#' 
#' a_summary(x = iris$Sepal.Length, .stats = c("n", "mean_sd"))
#' 
#' a_summary(x = iris$Sepal.Length, .formats = c(mean_sd = "(xx.xxx, xx.xxx)"))
#' 
#' 
#' s_foo <- function(df, .N_col, a = 1, b = 2) {
#'    list(
#'       nrow_df = nrow(df),
#'       .N_col = .N_col,
#'       a = a,
#'       b = b
#'    ) 
#' }
#' 
#' s_foo(iris, 40)
#' 
#' a_foo <- make_afun(s_foo, b = 4,
#'  .formats = c(nrow_df = "xx.xx", ".N_col" = "xx.", a = "xx", b = "xx.x"),
#'  .labels = c(nrow_df = "Nrow df", ".N_col" = "n in cols", a = "a value", b = "b value")
#' )
#' 
#' a_foo(iris, .N_col = 40)
#' a_foo(iris, .N_col = 40, .labels = c(nrow_df = "Number of rows"))
#' 
make_afun <- function(fun, ..., .stats = NULL, .formats = NULL, .labels = NULL) {
    ## too clever by three-quarters (because half wasn't enough)
    ## gross scope hackery
    fun_extra_args = list(...)
    fun_formals <- formals(fun)
    
    labels_to_inherit <- .labels
    formats_to_inherit <- .formats
    
    ret <- function(x) { ## remember formals get clobbered here
        args <- as.list(environment(), all.names = TRUE)
        args[c(".stats", ".formats", ".labels")] <- NULL
        if ("..." %in% names(fun_formals)) 
            args <- c(args, list(...))
        
        args[names(fun_extra_args)] <- fun_extra_args # the ... from the make_afun call should probably be called earlier
        
        vals <- do.call(fun, args)
        if (!is.list(vals))
            stop("fun for make_fun is expected to return a list")
        
        if (!is.null(.stats)) {
            if (!all(.stats %in% names(vals)))
                stop("not all .stats are in the named list returned by fun")
            vals <- vals[.stats]
        }
        
        formats <- formats_to_inherit
        labels <- labels_to_inherit
        formats[names(.formats)] <- .formats
        labels[names(.labels)] <- .labels
        
        # apply formats to values
        rcell_vals <- mapply(function(xi, format) {
            if (is(xi, "CellValue")) {
                warning("make_afun should overwrite format")
                xi
            } else {
                rcell(x = xi, format = format)
            }
        }, vals, match_by_name(vals, formats), SIMPLIFY = FALSE, USE.NAMES = TRUE)
        
        in_rows(.list = rcell_vals, .labels = match_by_name(vals, labels))
    }
    formals(ret) <- c(formals(fun), list(.stats = .stats, .formats = .formats, .labels = .labels))
    ret
}


#' Match value based on name
#' 
#' 
#' @noRd
#' @examples 
#' 
#' x <- list(a = 1, b = 2)
#' 
#' match_by_name(x = x, a = NULL)
#' match_by_name(x = x, a = "to repeat")
#' match_by_name(x = x, a = c("aaa", "bbb"))
#' match_by_name(x, c(a = "aaa", b = "bbb"))
#' match_by_name(x, c(a = "aaa", b = "bbb", d = "ddd"))
#' match_by_name(x, c(a = "aaa"), default = "ccc")
#' 
#' \dontrun{
#' # errors
#' match_by_name(unname(x), c(a = "aaa", b = "bbb"))
#' match_by_name(unname(x), c(a = "aaa"))
#' }
#' 
match_by_name <- function(x, a, default) {
    if (!is.list(x))
        stop("x is required to be a list")
    
    if (is.null(names(x)))
        stop("x is required to be a named list")
    
    if (length(x) == 0) 
        return(a[character(0)])
    
    if (is.null(a))
        return(NULL)
    
    if (length(a) == 1 && is.null(names(a)))
        a <- setNames(rep(a, length(x)), names(x))
    
    if (is.null(names(a)) && length(x) == length(a)) {
        a <- setNames(a, names(x))  
    }
    
    if (!missing(default))
        a[setdiff(names(x), names(a))] <- default
    
    if (!all(names(x) %in% names(a))) 
        stop(paste("names", paste(setdiff(names(x), names(a)), collapse = ", "), "are not defined"))
    
    a[names(x)]
}





make_afun2 <- function(fun, .stats = NULL, .formats = NULL, .labels = NULL, ...) {
    ## too clever by three-quarters (because half wasn't enough)
    ## gross scope hackery
    fun_args = list(...)
    
    ## so these are guaranteed to be defined for the list construction below
    ## hacky but everything else I thought of was worse.
    .N_col = NULL
    .N_total = NULL
    .ref_group = NULL
    .in_ref_col = NULL
    
    ret <- function(x) { ## remember formals get clobbered here
        custargs <- fun_args

        exargs <- list(...) # if ret does not have an ellipsis then the exargs is equal to custargs
        custargs[names(exargs)] <- exargs
        sfunargs <- list(x = x,
                         .N_col = .N_col,
                         .N_total = .N_total,
                         .ref_group = .ref_group,
                         .in_ref_col = .in_ref_col)
        sfunargs <- sfunargs[names(formals(fun))]

        rawvals <- do.call(fun, c(sfunargs, custargs))

        if (!is.list(rawvals))
            stop("make_afun expects a function fun that always returns a list")
        
        ## note single brackets here so its a list
        ## no matter what. thats important!
        final_vals <- if (is.null(.stats)) rawvals else rawvals[.stats]
        rcells <- if (are(final_vals, "CellValue")) final_vals else lapply(final_vals, rcell)

        if (!is.null(.labels)) {
            if (is.null(names(.labels))) {
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


