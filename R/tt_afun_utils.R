#' @title Cell value constructors
#'
#' @description Construct a cell value and associate formatting, labeling,
#' indenting, and column spanning information with it.
#'
#' @inheritParams compat_args
#' @inheritParams lyt_args
#' @param x ANY. Cell value
#' @param label character(1). Label or Null. If non-null, it will be looked at when determining row labels.
#' @param colspan integer(1). Columnspan value.
#' @note currently column spanning is only supported for defining header structure.
#' @rdname rcell
#' @export
rcell = function(x, format = NULL, colspan = 1L, label = NULL, indent_mod = NULL) {
    if(is(x, "CellValue"))
        x
    else
        CellValue(val = x, format = format, colspan = colspan, label = label, indent_mod = indent_mod)
}

#' @details \code{non_ref_rcell} provides the common \emph{blank for cells in the reference
#' column, this value otherwise}, and should be passed the value of \code{.in_ref_col}
#' when it is used.
#'
#' @param  isref logical(1).  Are  we  in  the reference  column  (ie
#'     .in_ref_col shoul be passed to this argument)
#' @param refval ANY. Value to use when in the reference column. Defaults
#' to \code{NULL}
#' @rdname rcell
#' @export
non_ref_rcell = function(x, isref, format = NULL, colspan = 1L,
                         label = NULL, indent_mod = NULL,
                         refval = NULL) {
    val <- if(isref) refval else x
    rcell(val, format = format, colspan = colspan, label = label,
          indent_mod = indent_mod)
}


#' Create multiple rows in analysis or summary functions
#'
#' define the cells that get placed into multiple rows in `afun`
#'
#' @note currently the `.name` argument is not used
#'
#' @param ... single row defining expressions
#' @param .list list. list cell content, usually `rcells`, the `.list` is concatenated to `...`
#' @param .names character or NULL. Names of the returned list/structure.
#' @param .labels character or NULL. labels for the defined rows
#' @param .formats character or NULL. Formats for the values
#' @param .indent_mods integer or NULL. Indent modificatons for the defined rows.
#'
#' @export
#'
#' @seealso `analyze`
#'
#' @examples
#' in_rows(1, 2, 3, .names = c("a", "b", "c"))
#' in_rows(1, 2, 3, .labels = c("a", "b", "c"))
#' in_rows(1, 2, 3, .names = c("a", "b", "c"), .labels = c("AAA", "BBB", "CCC"))
#'
#' in_rows(.list = list(a = 1, b = 2, c = 3))
#' in_rows(1, 2, .list = list(3), .names = c("a", "b", "c"))
#'
#' basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze("AGE", afun = function(x) {
#'     in_rows(
#'        "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
#'        "Range" = rcell(range(x), format = "xx.xx - xx.xx")
#'     )
#'   }) %>%
#'   build_table(ex_adsl)
#'
in_rows <- function(..., .list = NULL, .names = NULL,
                    .labels = NULL,
                    .formats = NULL,
                    .indent_mods = NULL) {

    l <- c(list(...), .list)

    if (missing(.names) && missing(.labels)) {
        if (length(l) > 0 && is.null(names(l)))
            stop("need a named list")
    } ## else {
    ##     ## # currently .names is not supported
    ##     ## if (missing(.labels)) .labels <- .names

    ##     if (length(.labels) != length(l))
    ##         stop("dimension missmatch for cells and row names")

    ##     names(l) <- .labels
    ## }


    l2 <- mapply(rcell, x = l, label = .labels %||% list(NULL),
                 format = .formats %||% list(NULL),
                 indent_mod = .indent_mods %||% list(NULL),
                 SIMPLIFY = FALSE)
    if(!is.null(.names))
        names(l2) <- .names
    else
        names(l2) <- names(l)
    if(length(l2) == 0) NULL else l2


 ##   if (length(l) == 0) NULL else l
}



#' Create custom analysis function wrapping existing function
#'
#' @param fun function. The function to be wrapped in a new customized analysis fun. Should return named list.
#' @param .stats character. Names of elements to keep from \code{fun}'s full output.
#' @param .formats ANY. vector/list of formats to override any defaults applied by \code{fun}.
#' @param .labels character. Vector of labels to override defaults returned by \code{fun}
#' @param .ungroup_stats character. Vector of names, which must match elements of \code{.stats}
#' @param ... dots. Additional arguments to \code{fun} which effectively become new defaults. These can still be
#'   overriden by extra args within a split.
#'
#' @return A function suitable for use in \code{\link{analyze}} with element selection, reformatting, and relabeling
#'   performed automatically.
#'
#' @note setting \code{.ungroup_stats} to non-null changes the \emph{structure} of the  value(s) returned by
#' \code{fun}, rather than just labeling (\code{.labels}), formatting (\code{.formats}), and selecting amongst
#' (\code{.stats}) them. This means that subsequent \code{make_afun} calls to customize the output further
#' both can and must operate on the new structure, \emph{NOT} the original structure returned by \code{fun}.
#' See the final pair of examples below.
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
#'
#' a_summary2 <- make_afun(a_summary, .stats = c("n", "mean_sd"))
#'
#' a_summary2(x = iris$Sepal.Length)
#'
#' a_summary3 <- make_afun(a_summary, .formats = c(mean_sd = "(xx.xxx, xx.xxx)"))
#'
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
#' a_foo2 <- make_afun(a_foo, .labels = c(nrow_df = "Number of Rows"))
#' a_foo(iris, .N_col = 40)
#'
#' #grouping and further customization
#' s_grp <- function(df, .N_col, a = 1, b = 2) {
#'    list(
#'       nrow_df = nrow(df),
#'       .N_col = .N_col,
#'       letters = list(a = a,
#'                      b = b)
#'    )
#' }
#' a_grp <- make_afun(s_grp, b = 3, .labels = c(nrow_df = "row count", .N_col = "count in column"),
#'                    .formats = c(nrow_df = "xx.", .N_col = "xx."), .ungroup_stats ="letters")
#' a_grp(iris, 40)
#' a_aftergrp <- make_afun(a_grp, .stats = c("nrow_df", "b"), .formats = c(b = "xx."))
#' a_aftergrp(iris, 40)
make_afun <- function(fun, .stats = NULL, .formats = NULL, .labels = NULL, .ungroup_stats = NULL, ...) {

    ## there is a LOT more computing-on-the-language hackery in here that I would
    ## prefer, but currently this is the way I see to do everything we want to do.

    ## too clever by three-quarters (because half wasn't enough)
    ## gross scope hackery
    fun_args = force(list(...))

    ## so these are guaranteed to be defined for the list construction below
    ## hacky but everything else I thought of was worse.
    .N_col = NULL
    .N_total = NULL
    .ref_group = NULL
    .in_ref_col = NULL
    df = NULL
    ret <- function(x) { ## remember formals get clobbered here
        custargs <- fun_args

        ## values int he actual call to this function override customization
        ## done by the constructor. evalparse is to avoid a "... in wrong context" NOTE
        if("..." %in% names(formals())) {
            exargs <- eval(parse(text = "list(...)"))
            custargs[names(exargs)] <- exargs
        }
        sfunargs <- c(if("x" %in% names(formals(fun))) list(x = x) else list(df = df),
                      list(
                         .N_col = .N_col,
                         .N_total = .N_total,
                         .ref_group = .ref_group,
                         .in_ref_col = .in_ref_col))
        sfunargs <- sfunargs[names(sfunargs) %in% names(formals(fun))]
        for(var in setdiff(names(formals(fun)), c("...", names(sfunargs)))) {
            ## not missing, ie specified in the direct call, takes precedence
            if(eval(parse(text = paste0("!missing(", var, ")"))))
                sfunargs[[var]] <- get(var)
            ## not specified in the call, but specified in the constructor
            else if(var %in% names(custargs))
                sfunargs[[var]] <- custargs[[var]]
            ## else left out so we hit the original default we inherited from fun
        }

        rawvals <- do.call(fun, sfunargs)

        ## note single brackets here so its a list
        ## no matter what. thats important!
        final_vals <- if (is.null(.stats)) rawvals else rawvals[.stats]

        if (!is.list(rawvals))
            stop("make_afun expects a function fun that always returns a list")
        if(!is.null(.stats))
            stopifnot(all(.stats %in% names(rawvals)))
        else
            .stats <- names(rawvals)
        if(!is.null(.ungroup_stats) &&
           !all(.ungroup_stats %in% .stats)) {
            stop("Stats specified for ungrouping not included in non-null .stats list: ",
                 setdiff(.ungroup_stats, .stats))
        }

        if (!is.null(.labels)) {
            if (is.null(names(.labels))) {
                stopifnot(length(.labels) == length(.stats))
                names(.labels) <- names(final_vals)
            } else {
                lblpos <- match(names(.labels), names(final_vals))
                stopifnot(!anyNA(lblpos))
            }
        }

        if(!is.null(.formats)) {
            if(is.null(names(.formats))) {
                stopifnot(length(.formats) == length(.stats))
                names(.formats) <- names(final_vals)
            } else {
                fmtpos <- match(names(.formats), names(final_vals))
                stopifnot(!anyNA(fmtpos))
            }
        }

        final_labels <- value_labels(final_vals)
        final_labels[names(.labels)] <- .labels

        final_formats <- lapply(final_vals, obj_format)
        final_formats[names(.formats)] = .formats


        if(!is.null(.ungroup_stats)) {
            for(nm in .ungroup_stats) {
                tmp <- final_vals[[nm]]
                if(is(tmp, "CellValue"))
                    tmp <- tmp[[1]] ## unwrap it
                final_vals <- insert_replace(final_vals, nm, tmp)
                stopifnot(all(nzchar(names(final_vals))))

                final_labels <- insert_replace(final_labels, nm, setNames(value_labels(tmp),
                                                                          names(tmp)))
                final_formats <- insert_replace(final_formats, nm, setNames(rep(final_formats[nm], length.out = length(tmp)),
                                                                            names(tmp)))
            }
        }

        rcells <- mapply(function(x, f, l) {
            if(is(x, "CellValue")) {
                obj_label(x) <- l
                obj_format(x) <- f
                x
            }else {
                rcell(x, format =f, label = l)
            }
        }, f = final_formats, x = final_vals,
                         l = final_labels,
                         SIMPLIFY = FALSE)
        in_rows(.list = rcells) ##, .labels = .labels)
    }
    formals(ret) <- formals(fun)
    ret
}


insert_replace <- function(x, nm, newvals = x[[nm]]) {
    i <-  match(nm, names(x))
    if(is.na(i))
        stop("name not found")
    bef <- if(i > 1) 1:(i-1) else numeric()
    aft <- if(i < length(x)) (i+1) : length(x) else numeric()
    ret = c(x[bef], newvals, x[aft])
    names(ret) = c(names(x)[bef], names(newvals), names(x)[aft])
    ret
}
