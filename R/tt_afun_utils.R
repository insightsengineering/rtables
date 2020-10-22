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

    if(is(x, "CellValue")) {
        if(!is.null(label))
            obj_label(x) <- label
        if(colspan != 1L)
            cell_cspan(x) <- colspan
        if(!is.null(indent_mod))
            indent_mod(x) <- indent_mod
        if(!is.null(format))
            obj_format(x) <- format
        ret <- x
    } else {
        if(is.null(label))
            label <- obj_label(x)
        if(is.null(format))
            format <- obj_format(x)
        if(is.null(indent_mod))
            indent_mod <- indent_mod(x)
        ret <- CellValue(val = x, format = format, colspan = colspan, label = label, indent_mod = indent_mod)
    }
    ret
}

#' @details \code{non_ref_rcell} provides the common \emph{blank for cells in the reference
#' column, this value otherwise}, and should be passed the value of \code{.in_ref_col}
#' when it is used.
#'
#' @param  is_ref logical(1).  Are  we  in  the reference  column  (ie
#'     .in_ref_col shoul be passed to this argument)
#' @param refval ANY. Value to use when in the reference column. Defaults
#' to \code{NULL}
#' @rdname rcell
#' @export
non_ref_rcell = function(x, is_ref, format = NULL, colspan = 1L,
                         label = NULL, indent_mod = NULL,
                         refval = NULL) {
    val <- if(is_ref) refval else x
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
    }


    if(is.null(.formats))
        .formats <- list(NULL)
    l2 <- mapply(rcell, x = l, format = .formats, SIMPLIFY = FALSE)
    if(is.null(.labels)) {
        objlabs <- vapply(l2, function(x) obj_label(x) %||% "", "")
        if(any(nzchar(objlabs)))
            .labels <- objlabs
    }

    if(is.null(.names) && !is.null(names(l)))
        .names <- names(l)
    ret <- RowsVerticalSection(l2, names = .names,
                               labels = .labels,
                               indent_mods = .indent_mods,
                               formats = .formats)
    ## if(!is.null(.names))
    ##     names(l2) <- .names
    ## else
    ##     names(l2) <- names(l)
    if(length(ret) == 0) NULL else ret


 ##   if (length(l) == 0) NULL else l
}



.validate_nms <- function(vals, .stats, arg) {
    if (!is.null(arg)) {
        if (is.null(names(arg))) {
            stopifnot(length(arg) == length(.stats))
            names(arg) <- names(vals)
        } else {
            lblpos <- match(names(arg), names(vals))
            stopifnot(!anyNA(lblpos))
        }
    }
    arg
}


#' Create custom analysis function wrapping existing function
#'
#' @param fun function. The function to be wrapped in a new customized analysis fun. Should return named list.
#' @param .stats character. Names of elements to keep from \code{fun}'s full output.
#' @param .formats ANY. vector/list of formats to override any defaults applied by \code{fun}.
#' @param .labels character. Vector of labels to override defaults returned by \code{fun}
#' @param .indent_mods integer. Named vector of indent modifiers for the generated rows.
#' @param .ungroup_stats character. Vector of names, which must match elements of \code{.stats}
#' @param ... dots. Additional arguments to \code{fun} which effectively become new defaults. These can still be
#'   overriden by extra args within a split.
#' @param .null_ref_cells logical(1). Should cells for the reference column be NULL-ed
#' by the returned analysis function. Defaults to \code{TRUE} if \code{fun} accepts \code{.in_ref_col} as a formal argument. Note this argument occurs after \code{...} so it must be \emph{fully} specified  by name when set.
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
#'  .labels = c(nrow_df = "Nrow df", ".N_col" = "n in cols", a = "a value", b = "b value"),
#'  .indent_mods = c(nrow_df = 2L, a = 1L)
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
#'                    .formats = c(nrow_df = "xx.", .N_col = "xx."),
#'                    .indent_mod = c(letters = 1L),
#'                    .ungroup_stats ="letters")
#' a_grp(iris, 40)
#' a_aftergrp <- make_afun(a_grp, .stats = c("nrow_df", "b"), .formats = c(b = "xx."))
#' a_aftergrp(iris, 40)
#'
#' s_ref <- function(x, .in_ref_col, .ref_group) {
#'    list(
#'          mean_diff = mean(x) - mean(.ref_group)
#'        )
#' }
#'
#' a_ref <- make_afun(s_ref, .labels = c( mean_diff = "Mean Difference from Ref"))
#' a_ref(iris$Sepal.Length, .in_ref_col = TRUE, 1:10)
#' a_ref(iris$Sepal.Length, .in_ref_col = FALSE, 1:10)

make_afun <- function(fun,
                      .stats = NULL,
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = NULL,
                      .ungroup_stats = NULL,
                      ...,
                      .null_ref_cells = ".in_ref_col" %in% names(formals(fun))) {

    ## there is a LOT more computing-on-the-language hackery in here that I would
    ## prefer, but currently this is the way I see to do everything we want to do.

    ## too clever by three-quarters (because half wasn't enough)
    ## gross scope hackery
    fun_args = force(list(...))
    fun_fnames <- names(formals(fun))
    takes_inrefcol <- ".in_ref_col" %in% fun_fnames

    ret <- function(x, ...) { ## remember formals get clobbered here

        ## this helper will grab the value and wrap it in a named list if
        ## we need the variable and return list() otherwise.
        ## We define it in here so that the scoping hackery works correctly
        .if_in_formals <- function(nm, ifnot = list(), named_lwrap = TRUE) {
            val <- if(nm %in% fun_fnames) get(nm) else ifnot
            if(named_lwrap && length(val) > 0)
                setNames(list(val), nm)
            else
                val
        }

        custargs <- fun_args

        ## special handling cause I need it at the bottom as well
        in_rc_argl <- .if_in_formals(".in_ref_col")
        .in_ref_col <- if(length(in_rc_argl) > 0) in_rc_argl[[1]] else FALSE

        sfunargs <- c(
            ## these are either named lists containing the arg, or list()
            ## depending on whether fun accept the argument or not
            .if_in_formals("x"),
            .if_in_formals("df"),
            .if_in_formals(".N_col"),
            .if_in_formals(".N_total"),
            .if_in_formals(".N_row"),
            .if_in_formals(".ref_group"),
            in_rc_argl,
            .if_in_formals(".df_row"),
            .if_in_formals(".var"),
            .if_in_formals(".ref_full"))

        allvars <- setdiff(fun_fnames, c("...", names(sfunargs)))
        ## values int he actual call to this function override customization
        ## done by the constructor. evalparse is to avoid a "... in wrong context" NOTE
        if("..." %in% fun_fnames) {
            exargs <- eval(parse(text = "list(...)"))
            custargs[names(exargs)] <- exargs
            allvars <- unique(c(allvars, names(custargs)))
        }

        for(var in allvars) {
            ## not missing, ie specified in the direct call, takes precedence
            if(var %in% fun_fnames && eval(parse(text = paste0("!missing(", var, ")"))))
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

        .labels <- .validate_nms(final_vals, .stats, .labels)
        .formats <- .validate_nms(final_vals, .stats, .formats)
        .indent_mods <- .validate_nms(final_vals, .stats, .indent_mods)

        final_labels <- value_labels(final_vals)
        final_labels[names(.labels)] <- .labels

        final_formats <- lapply(final_vals, obj_format)
        final_formats[names(.formats)] = .formats

        if(is(final_vals, "RowsVerticalSection"))
            final_imods <- indent_mod(final_vals)
        else
            final_imods <- vapply(final_vals, indent_mod, 1L)
        final_imods[names(.indent_mods)] <- .indent_mods

        if(!is.null(.ungroup_stats)) {
            for(nm in .ungroup_stats) {
                tmp <- final_vals[[nm]]
                if(is(tmp, "CellValue"))
                    tmp <- tmp[[1]] ## unwrap it
                final_vals <- insert_replace(final_vals, nm, tmp)
                stopifnot(all(nzchar(names(final_vals))))

                final_labels <- insert_replace(final_labels,
                                               nm,
                                               setNames(value_labels(tmp),
                                                        names(tmp))
                                               )
                final_formats <- insert_replace(final_formats,
                                                nm,
                                                setNames(rep(final_formats[nm],
                                                             length.out = length(tmp)),
                                                         names(tmp))
                                                )
                final_imods <- insert_replace(final_imods,
                                              nm,
                                              setNames(rep(final_imods[nm],
                                                           length.out = length(tmp)),
                                                       names(tmp))
                                              )
            }
        }
        rcells <- mapply(function(x, f, l, im) {
            if(is(x, "CellValue")) {
                obj_label(x) <- l
                obj_format(x) <- f
#                indent_mod(x) <- im
                x
            } else if(.null_ref_cells) {
                non_ref_rcell(x, is_ref = .in_ref_col,
                              format =f, label = l)#, indent_mod = im)
            } else {
                rcell(x, format =f, label = l)#, indent_mod = im)
            }
        }, f = final_formats, x = final_vals,
        l = final_labels,
#        im = final_imods,
        SIMPLIFY = FALSE)
        in_rows(.list = rcells, .indent_mods = final_imods) ##, .labels = .labels)
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
