#' Cell value constructors
#'
#' Construct a cell value and associate formatting, labeling, indenting, and column spanning information with it.
#'
#' @inheritParams compat_args
#' @inheritParams lyt_args
#' @inheritParams gen_args
#' @param x (`ANY`)\cr cell value.
#' @param format (`string` or `function`)\cr the format label (string) or `formatters` function to apply to `x`.
#'   See [formatters::list_valid_format_labels()] for currently supported format labels.
#' @param label (`string` or `NULL`)\cr label. If non-`NULL`, it will be looked at when determining row labels.
#' @param colspan (`integer(1)`)\cr column span value.
#' @param footnotes (`list` or `NULL`)\cr referential footnote messages for the cell.
#' @param stat_names (`character` or `NA`)\cr names for the statistics in the cell. It can be a vector of strings.
#'   If `NA`, statistic names are not specified.
#'
#' @inherit CellValue return
#'
#' @note Currently column spanning is only supported for defining header structure.
#'
#' @examples
#' rcell(1, format = "xx.x")
#' rcell(c(1, 2), format = c("xx - xx"))
#' rcell(c(1, 2), stat_names = c("Rand1", "Rand2"))
#'
#' @rdname rcell
#' @export
rcell <- function(x,
                  format = NULL,
                  colspan = 1L,
                  label = NULL,
                  indent_mod = NULL,
                  footnotes = NULL,
                  align = NULL,
                  format_na_str = NULL,
                  stat_names = NULL,
                  round_type = valid_round_type) {
  round_type <- match.arg(round_type)
  checkmate::assert_character(stat_names, null.ok = TRUE)
  if (!is.null(align)) {
    check_aligns(align)
  }
  if (is(x, "CellValue")) {
    if (!is.null(label)) {
      obj_label(x) <- label
    }
    if (colspan != 1L) {
      cell_cspan(x) <- colspan
    }
    if (!is.null(indent_mod)) {
      indent_mod(x) <- indent_mod
    }
    if (!is.null(format)) {
      obj_format(x) <- format
    }
    if (!is.null(footnotes)) {
      cell_footnotes(x) <- lapply(footnotes, RefFootnote)
    }
    if (!is.null(format_na_str)) {
      obj_na_str(x) <- format_na_str
    }
    if (!is.null(stat_names)) {
      obj_stat_names(x) <- stat_names
    }
    ret <- x
  } else {
    if (is.null(label)) {
      label <- obj_label(x)
    }
    if (is.null(format)) {
      format <- obj_format(x)
    }
    if (is.null(indent_mod)) {
      indent_mod <- indent_mod(x)
    }
    footnotes <- lapply(footnotes, RefFootnote)
    ret <- CellValue(
      val = x,
      format = format,
      colspan = colspan,
      label = label,
      indent_mod = indent_mod,
      footnotes = footnotes,
      format_na_str = format_na_str,
      stat_names = stat_names %||% NA_character_,
      round_type = round_type
    ) # RefFootnote(footnote))
  }
  if (!is.null(align)) {
    cell_align(ret) <- align
  }
  ret
}

#' @param is_ref (`flag`)\cr whether function is being used in the reference column (i.e. `.in_ref_col` should be
#'   passed to this argument).
#' @param refval (`ANY`)\cr value to use when in the reference column. Defaults to `NULL`.
#'
#' @details
#' `non_ref_rcell` provides the common *blank for cells in the reference column, this value otherwise*, and should
#' be passed the value of `.in_ref_col` when it is used.
#'
#' @rdname rcell
#' @export
non_ref_rcell <- function(x, is_ref, format = NULL, colspan = 1L,
                          label = NULL, indent_mod = NULL,
                          refval = NULL,
                          align = "center",
                          format_na_str = NULL) {
  val <- if (is_ref) refval else x
  rcell(val,
    format = format, colspan = colspan, label = label,
    indent_mod = indent_mod, align = align,
    format_na_str = format_na_str
  )
}

#' Create multiple rows in analysis or summary functions
#'
#' Define the cells that get placed into multiple rows in `afun`.
#'
#' @param ... single row defining expressions.
#' @param .list (`list`)\cr list cell content (usually `rcells`). The `.list` is concatenated to `...`.
#' @param .names (`character` or `NULL`)\cr names of the returned list/structure.
#' @param .labels (`character` or `NULL`)\cr labels for the defined rows.
#' @param .formats (`character` or `NULL`)\cr formats for the values.
#' @param .indent_mods (`integer` or `NULL`)\cr indent modifications for the defined rows.
#' @param .cell_footnotes (`list`)\cr referential footnote messages to be associated by name with *cells*.
#' @param .row_footnotes (`list`)\cr referential footnotes messages to be associated by name with *rows*.
#' @param .aligns (`character` or `NULL`)\cr alignments for the cells. Standard for `NULL` is `"center"`.
#'   See [formatters::list_valid_aligns()] for currently supported alignments.
#' @param .format_na_strs (`character` or `NULL`)\cr NA strings for the cells.
#' @param .stat_names (`list`)\cr names for the statistics in the cells.
#'   It can be a vector of values. If `list(NULL)`, statistic names are not specified and will
#'   appear as `NA`.
#'
#' @note In post-processing, referential footnotes can also be added using row and column
#'   paths with [`fnotes_at_path<-`].
#'
#' @return A `RowsVerticalSection` object (or `NULL`). The details of this object should be considered an
#'   internal implementation detail.
#'
#' @seealso [analyze()]
#'
#' @examples
#' in_rows(1, 2, 3, .names = c("a", "b", "c"))
#' in_rows(1, 2, 3, .labels = c("a", "b", "c"))
#' in_rows(1, 2, 3, .names = c("a", "b", "c"), .labels = c("AAA", "BBB", "CCC"))
#' in_rows(
#'   .list = list(a = c(NA, NA)),
#'   .formats = "xx - xx",
#'   .format_na_strs = list(c("asda", "lkjklj"))
#' )
#' in_rows(.list = list(a = c(NA, NA)), .format_na_strs = c("asda", "lkjklj"))
#'
#' in_rows(.list = list(a = 1, b = 2, c = 3))
#' in_rows(1, 2, .list = list(3), .names = c("a", "b", "c"))
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze("AGE", afun = function(x) {
#'     in_rows(
#'       "Mean (sd)" = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
#'       "Range" = rcell(range(x), format = "xx.xx - xx.xx")
#'     )
#'   })
#'
#' tbl <- build_table(lyt, ex_adsl)
#' tbl
#'
#' @export
in_rows <- function(..., .list = NULL, .names = NULL,
                    .labels = NULL,
                    .formats = NULL,
                    .indent_mods = NULL,
                    .cell_footnotes = list(NULL),
                    .row_footnotes = list(NULL),
                    .aligns = NULL,
                    .format_na_strs = NULL,
                    .stat_names = list(NULL)) {
  if (is.function(.formats)) {
    .formats <- list(.formats)
  }

  l <- c(list(...), .list)

  if (missing(.names) && missing(.labels)) {
    if (length(l) > 0 && is.null(names(l))) {
      stop("need a named list")
    } else {
      .names <- names(l)
    }
    stopifnot(!anyNA(.names))
  }

  if (length(l) == 0) {
    if (
      length(.labels) > 0 ||
        length(.formats) > 0 ||
        length(.names) > 0 ||
        length(.indent_mods) > 0 ||
        length(.format_na_strs) > 0 ||
        (!all(is.na(.stat_names)) && length(.stat_names) > 0)
    ) {
      stop(
        "in_rows got 0 rows but length >0 of at least one of ",
        ".labels, .formats, .names, .indent_mods, .format_na_strs, .stat_names. ",
        "Does your analysis/summary function handle the 0 row ",
        "df/length 0 x case?"
      )
    }
    l2 <- list()
  } else {
    if (is.null(.formats)) {
      .formats <- list(NULL)
    }
    stopifnot(is.list(.cell_footnotes))
    if (length(.cell_footnotes) != length(l)) {
      .cell_footnotes <- c(
        .cell_footnotes,
        setNames(
          rep(list(character()),
            length.out = length(setdiff(
              names(l),
              names(.cell_footnotes)
            ))
          ),
          setdiff(
            names(l),
            names(.cell_footnotes)
          )
        )
      )
      .cell_footnotes <- .cell_footnotes[names(l)]
    }
    if (is.null(.aligns)) {
      .aligns <- list(NULL)
    }

    l2 <- mapply(rcell,
      x = l, format = .formats,
      footnotes = .cell_footnotes %||% list(NULL),
      align = .aligns,
      format_na_str = .format_na_strs %||% list(NULL),
      stat_names = .stat_names %||% list(NULL),
      SIMPLIFY = FALSE
    )
  }
  if (is.null(.labels)) {
    objlabs <- vapply(l2, function(x) obj_label(x) %||% "", "")
    if (any(nzchar(objlabs))) {
      .labels <- objlabs
    }
  }

  if (is.null(.names) && !is.null(names(l))) {
    .names <- names(l)
  }
  stopifnot(is.list(.row_footnotes))
  if (length(.row_footnotes) != length(l2)) {
    tmp <- .row_footnotes
    .row_footnotes <- vector("list", length(l2))
    pos <- match(names(tmp), .names)
    nonna <- which(!is.na(pos))
    .row_footnotes[pos] <- tmp[nonna]
    #        length(.row_footnotes) <- length(l2)
  }
  ret <- RowsVerticalSection(l2,
    names = .names,
    labels = .labels,
    indent_mods = .indent_mods,
    formats = .formats,
    footnotes = .row_footnotes,
    format_na_strs = .format_na_strs
  )
  ## if(!is.null(.names))
  ##     names(l2) <- .names
  ## else
  ##     names(l2) <- names(l)
  if (length(ret) == 0) NULL else ret

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

#' Create a custom analysis function wrapping an existing function
#'
#' @param fun (`function`)\cr the function to be wrapped in a new customized analysis function.
#'   `fun` should return a named `list`.
#' @param .stats (`character`)\cr names of elements to keep from `fun`'s full output.
#' @param .formats (`ANY`)\cr vector or list of formats to override any defaults applied by `fun`.
#' @param .labels (`character`)\cr vector of labels to override defaults returned by `fun`.
#' @param .indent_mods (`integer`)\cr named vector of indent modifiers for the generated rows.
#' @param .ungroup_stats (`character`)\cr vector of names, which must match elements of `.stats`.
#' @param ... additional arguments to `fun` which effectively become new defaults. These can still be
#'   overridden by `extra_args` within a split.
#' @param .null_ref_cells (`flag`)\cr whether cells for the reference column should be `NULL`-ed by the
#'   returned analysis function. Defaults to `TRUE` if `fun` accepts `.in_ref_col` as a formal argument. Note
#'   this argument occurs after `...` so it must be *fully* specified by name when set.
#' @param .format_na_strs (`ANY`)\cr vector/list of `NA` strings to override any defaults applied by `fun`.
#'
#' @return A function suitable for use in [analyze()] with element selection, reformatting, and relabeling
#'   performed automatically.
#'
#' @note
#' Setting `.ungroup_stats` to non-`NULL` changes the *structure* of the value(s) returned by `fun`, rather than
#' just labeling (`.labels`), formatting (`.formats`), and selecting amongst (`.stats`) them. This means that
#' subsequent `make_afun` calls to customize the output further both can and must operate on the new structure,
#' *not* the original structure returned by `fun`. See the final pair of examples below.
#'
#' @seealso [analyze()]
#'
#' @examples
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
#' s_foo <- function(df, .N_col, a = 1, b = 2) {
#'   list(
#'     nrow_df = nrow(df),
#'     .N_col = .N_col,
#'     a = a,
#'     b = b
#'   )
#' }
#'
#' s_foo(iris, 40)
#'
#' a_foo <- make_afun(s_foo,
#'   b = 4,
#'   .formats = c(nrow_df = "xx.xx", ".N_col" = "xx.", a = "xx", b = "xx.x"),
#'   .labels = c(
#'     nrow_df = "Nrow df",
#'     ".N_col" = "n in cols", a = "a value", b = "b value"
#'   ),
#'   .indent_mods = c(nrow_df = 2L, a = 1L)
#' )
#'
#' a_foo(iris, .N_col = 40)
#' a_foo2 <- make_afun(a_foo, .labels = c(nrow_df = "Number of Rows"))
#' a_foo2(iris, .N_col = 40)
#'
#' # grouping and further customization
#' s_grp <- function(df, .N_col, a = 1, b = 2) {
#'   list(
#'     nrow_df = nrow(df),
#'     .N_col = .N_col,
#'     letters = list(
#'       a = a,
#'       b = b
#'     )
#'   )
#' }
#' a_grp <- make_afun(s_grp,
#'   b = 3,
#'   .labels = c(
#'     nrow_df = "row count",
#'     .N_col = "count in column"
#'   ),
#'   .formats = c(nrow_df = "xx.", .N_col = "xx."),
#'   .indent_mods = c(letters = 1L),
#'   .ungroup_stats = "letters"
#' )
#' a_grp(iris, 40)
#' a_aftergrp <- make_afun(a_grp,
#'   .stats = c("nrow_df", "b"),
#'   .formats = c(b = "xx.")
#' )
#' a_aftergrp(iris, 40)
#'
#' s_ref <- function(x, .in_ref_col, .ref_group) {
#'   list(
#'     mean_diff = mean(x) - mean(.ref_group)
#'   )
#' }
#'
#' a_ref <- make_afun(s_ref,
#'   .labels = c(mean_diff = "Mean Difference from Ref")
#' )
#' a_ref(iris$Sepal.Length, .in_ref_col = TRUE, 1:10)
#' a_ref(iris$Sepal.Length, .in_ref_col = FALSE, 1:10)
#'
#' @export
make_afun <- function(fun,
                      .stats = NULL,
                      .formats = NULL,
                      .labels = NULL,
                      .indent_mods = NULL,
                      .ungroup_stats = NULL,
                      .format_na_strs = NULL,
                      ...,
                      .null_ref_cells = ".in_ref_col" %in% names(formals(fun))) {
  ## there is a LOT more computing-on-the-language hackery in here that I
  ## would prefer, but currently this is the way I see to do everything we
  ## want to do.

  ## too clever by three-quarters (because half wasn't enough)
  ## gross scope hackery
  fun_args <- force(list(...))
  fun_fnames <- names(formals(fun))

  ## force EVERYTHING otherwise calling this within loops is the stuff of
  ## nightmares
  force(.stats)
  force(.formats)
  force(.format_na_strs)
  force(.labels)
  force(.indent_mods)
  force(.ungroup_stats)
  force(.null_ref_cells) ## this one probably isn't needed?

  ret <- function(x, ...) { ## remember formals get clobbered here

    ## this helper will grab the value and wrap it in a named list if
    ## we need the variable and return list() otherwise.
    ## We define it in here so that the scoping hackery works correctly
    .if_in_formals <- function(nm, ifnot = list(), named_lwrap = TRUE) {
      val <- if (nm %in% fun_fnames) get(nm) else ifnot
      if (named_lwrap && !identical(val, ifnot)) {
        setNames(list(val), nm)
      } else {
        val
      }
    }

    custargs <- fun_args

    ## special handling cause I need it at the bottom as well
    in_rc_argl <- .if_in_formals(".in_ref_col")
    .in_ref_col <- if (length(in_rc_argl) > 0) in_rc_argl[[1]] else FALSE

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
      .if_in_formals(".ref_full")
    )

    allvars <- setdiff(fun_fnames, c("...", names(sfunargs)))
    ## values int he actual call to this function override customization
    ## done by the constructor. evalparse is to avoid a "... in wrong context" NOTE
    if ("..." %in% fun_fnames) {
      exargs <- eval(parser_helper(text = "list(...)"))
      custargs[names(exargs)] <- exargs
      allvars <- unique(c(allvars, names(custargs)))
    }

    for (var in allvars) {
      ## not missing, i.e. specified in the direct call, takes precedence
      if (var %in% fun_fnames && eval(parser_helper(text = paste0("!missing(", var, ")")))) {
        sfunargs[[var]] <- get(var)
      } else if (var %in% names(custargs)) { ## not specified in the call, but specified in the constructor
        sfunargs[[var]] <- custargs[[var]]
      }
      ## else left out so we hit the original default we inherited from fun
    }

    rawvals <- do.call(fun, sfunargs)

    ## note single brackets here so its a list
    ## no matter what. thats important!
    final_vals <- if (is.null(.stats)) rawvals else rawvals[.stats]

    if (!is.list(rawvals)) {
      stop("make_afun expects a function fun that always returns a list")
    }
    if (!is.null(.stats)) {
      stopifnot(all(.stats %in% names(rawvals)))
    } else {
      .stats <- names(rawvals)
    }
    if (!is.null(.ungroup_stats) && !all(.ungroup_stats %in% .stats)) {
      stop(
        "Stats specified for ungrouping not included in non-null .stats list: ",
        setdiff(.ungroup_stats, .stats)
      )
    }

    .labels <- .validate_nms(final_vals, .stats, .labels)
    .formats <- .validate_nms(final_vals, .stats, .formats)
    .indent_mods <- .validate_nms(final_vals, .stats, .indent_mods)
    .format_na_strs <- .validate_nms(final_vals, .stats, .format_na_strs)

    final_labels <- value_labels(final_vals)
    final_labels[names(.labels)] <- .labels

    final_formats <- lapply(final_vals, obj_format)
    final_formats[names(.formats)] <- .formats

    final_format_na_strs <- lapply(final_vals, obj_na_str)
    final_format_na_strs[names(.format_na_strs)] <- .format_na_strs

    if (is(final_vals, "RowsVerticalSection")) {
      final_imods <- indent_mod(final_vals)
    } else {
      final_imods <- vapply(final_vals, indent_mod, 1L)
    }
    final_imods[names(.indent_mods)] <- .indent_mods

    if (!is.null(.ungroup_stats)) {
      for (nm in .ungroup_stats) {
        tmp <- final_vals[[nm]]
        if (is(tmp, "CellValue")) {
          tmp <- tmp[[1]]
        } ## unwrap it
        final_vals <- insert_replace(final_vals, nm, tmp)
        stopifnot(all(nzchar(names(final_vals))))

        final_labels <- insert_replace(
          final_labels,
          nm,
          setNames(
            value_labels(tmp),
            names(tmp)
          )
        )
        final_formats <- insert_replace(
          final_formats,
          nm,
          setNames(
            rep(final_formats[nm],
              length.out = length(tmp)
            ),
            names(tmp)
          )
        )
        final_format_na_strs <- insert_replace(
          final_format_na_strs,
          nm,
          setNames(
            rep(final_format_na_strs[nm],
              length.out = length(tmp)
            ),
            names(tmp)
          )
        )
        final_imods <- insert_replace(
          final_imods,
          nm,
          setNames(
            rep(final_imods[nm],
              length.out = length(tmp)
            ),
            names(tmp)
          )
        )
      }
    }
    rcells <- mapply(
      function(x, f, l, na_str) {
        if (is(x, "CellValue")) {
          obj_label(x) <- l
          obj_format(x) <- f
          obj_na_str(x) <- na_str
          #                indent_mod(x) <- im
          x
        } else if (.null_ref_cells) {
          non_ref_rcell(x,
            is_ref = .in_ref_col,
            format = f, label = l,
            format_na_str = na_str
          ) # , indent_mod = im)
        } else {
          rcell(x, format = f, label = l, format_na_str = na_str) # , indent_mod = im)
        }
      },
      f = final_formats, x = final_vals,
      l = final_labels,
      na_str = final_format_na_strs,
      #        im = final_imods,
      SIMPLIFY = FALSE
    )
    in_rows(.list = rcells, .indent_mods = final_imods) ## , .labels = .labels)
  }
  formals(ret) <- formals(fun)
  ret
}

insert_replace <- function(x, nm, newvals = x[[nm]]) {
  i <- match(nm, names(x))
  if (is.na(i)) {
    stop("name not found")
  }
  bef <- if (i > 1) 1:(i - 1) else numeric()
  aft <- if (i < length(x)) (i + 1):length(x) else numeric()
  ret <- c(x[bef], newvals, x[aft])
  names(ret) <- c(names(x)[bef], names(newvals), names(x)[aft])
  ret
}

parser_helper <- function(text, envir = parent.frame(2)) {
  parse(text = text, keep.source = FALSE)
}

length_w_name <- function(x, .parent_splval) {
  in_rows(length(x),
    .names = value_labels(.parent_splval)
  )
}
