#' Trimming and pruning criteria
#'
#' Criteria functions (and constructors thereof) for trimming and pruning tables.
#'
#' @inheritParams gen_args
#'
#' @return A logical value indicating whether `tr` should be included (`TRUE`) or pruned (`FALSE`) during pruning.
#'
#' @seealso [prune_table()], [trim_rows()]
#'
#' @details `all_zero_or_na` returns `TRUE` (and thus indicates trimming/pruning) for any *non-`LabelRow`*
#'   `TableRow` which contain only any mix of `NA` (including `NaN`), `0`, `Inf` and `-Inf` values.
#'
#' @examples
#' adsl <- ex_adsl
#' levels(adsl$SEX) <- c(levels(ex_adsl$SEX), "OTHER")
#' adsl$AGE[adsl$SEX == "UNDIFFERENTIATED"] <- 0
#' adsl$BMRKR1 <- 0
#'
#' tbl_to_prune <- basic_table() %>%
#'   analyze("BMRKR1") %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("SEX") %>%
#'   summarize_row_groups() %>%
#'   split_rows_by("STRATA1") %>%
#'   summarize_row_groups() %>%
#'   analyze("AGE") %>%
#'   build_table(adsl)
#'
#' tbl_to_prune %>% prune_table(all_zero_or_na)
#'
#' @rdname trim_prune_funs
#' @export
all_zero_or_na <- function(tr) {
  if (!is(tr, "TableRow") || is(tr, "LabelRow")) {
    return(FALSE)
  }
  rvs <- unlist(unname(row_values(tr)))
  all(is.na(rvs) | rvs == 0 | !is.finite(rvs))
}

#' @details `all_zero` returns `TRUE` for any non-`LabelRow` which contains only (non-missing) zero values.
#'
#' @examples
#' tbl_to_prune %>% prune_table(all_zero)
#'
#' @rdname trim_prune_funs
#' @export
all_zero <- function(tr) {
  if (!is(tr, "TableRow") || is(tr, "LabelRow")) {
    return(FALSE)
  }
  rvs <- unlist(unname(row_values(tr)))
  isTRUE(all(rvs == 0))
}

#' Trim rows from a populated table without regard for table structure
#'
#' @inheritParams gen_args
#' @param criteria (`function`)\cr function which takes a `TableRow` object and returns `TRUE` if that row
#'   should be removed. Defaults to [all_zero_or_na()].
#'
#' @return The table with rows that have only `NA` or 0 cell values removed.
#'
#' @note
#' Visible `LabelRow`s are including in this trimming, which can lead to either all label rows being trimmed or
#' label rows remaining when all data rows have been trimmed, depending on what `criteria` returns when called on
#' a `LabelRow` object. To avoid this, use the structurally-aware [prune_table()] machinery instead.
#'
#' @details
#' This function will be deprecated in the future in favor of the more elegant and versatile [prune_table()]
#' function which can perform the same function as `trim_rows()` but is more powerful as it takes table structure
#' into account.
#'
#' @seealso [prune_table()]
#'
#' @examples
#' adsl <- ex_adsl
#' levels(adsl$SEX) <- c(levels(ex_adsl$SEX), "OTHER")
#'
#' tbl_to_trim <- basic_table() %>%
#'   analyze("BMRKR1") %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("SEX") %>%
#'   summarize_row_groups() %>%
#'   split_rows_by("STRATA1") %>%
#'   summarize_row_groups() %>%
#'   analyze("AGE") %>%
#'   build_table(adsl)
#'
#' tbl_to_trim %>% trim_rows()
#'
#' tbl_to_trim %>% trim_rows(all_zero)
#'
#' @export
trim_rows <- function(tt, criteria = all_zero_or_na) {
  rows <- collect_leaves(tt, TRUE, TRUE)
  torm <- vapply(rows, criteria,
    NA,
    USE.NAMES = FALSE
  )
  tt[!torm, ,
    keep_topleft = TRUE,
    keep_titles = TRUE,
    keep_footers = TRUE,
    reindex_refs = TRUE
  ]
}

#' @inheritParams trim_rows
#'
#' @details
#' `content_all_zeros_nas` prunes a subtable if both of the following are true:
#'
#'   * It has a content table with exactly one row in it.
#'   * `all_zero_or_na` returns `TRUE` for that single content row. In practice, when the default summary/content
#'     function is used, this represents pruning any subtable which corresponds to an empty set of the input data
#'     (e.g. because a factor variable was used in [split_rows_by()] but not all levels were present in the data).
#'
#' @examples
#' tbl_to_prune %>% prune_table(content_all_zeros_nas)
#'
#' @rdname trim_prune_funs
#' @export
content_all_zeros_nas <- function(tt, criteria = all_zero_or_na) {
  ## this will be NULL if
  ## tt is something that doesn't have a content table
  ct <- content_table(tt)
  ## NROW returns 0 for NULL.
  if (NROW(ct) == 0 || nrow(ct) > 1) {
    return(FALSE)
  }

  cr <- tree_children(ct)[[1]]
  criteria(cr)
}

#' @details
#' `prune_empty_level` combines `all_zero_or_na` behavior for `TableRow` objects, `content_all_zeros_nas` on
#' `content_table(tt)` for `TableTree` objects, and an additional check that returns `TRUE` if the `tt` has no
#' children.
#'
#' @examples
#' tbl_to_prune %>% prune_table(prune_empty_level)
#'
#' @rdname trim_prune_funs
#' @export
prune_empty_level <- function(tt) {
  if (is(tt, "TableRow")) {
    return(all_zero_or_na(tt))
  }

  if (content_all_zeros_nas(tt)) {
    return(TRUE)
  }
  kids <- tree_children(tt)
  length(kids) == 0
}

#' @details `prune_zeros_only` behaves as `prune_empty_level` does, except that like `all_zero` it prunes
#'   only in the case of all non-missing zero values.
#'
#' @examples
#' tbl_to_prune %>% prune_table(prune_zeros_only)
#'
#' @rdname trim_prune_funs
#' @export
prune_zeros_only <- function(tt) {
  if (is(tt, "TableRow")) {
    return(all_zero(tt))
  }

  if (content_all_zeros_nas(tt, criteria = all_zero)) {
    return(TRUE)
  }
  kids <- tree_children(tt)
  length(kids) == 0
}

#' @param min (`numeric(1)`)\cr (used by `low_obs_pruner` only). Minimum aggregate count value.
#'   Subtables whose combined/average count are below this threshold will be pruned.
#' @param type (`string`)\cr how count values should be aggregated. Must be `"sum"` (the default) or `"mean"`.
#'
#' @details
#' `low_obs_pruner` is a *constructor function* which, when called, returns a pruning criteria function which
#' will prune on content rows by comparing sum or mean (dictated by `type`) of the count portions of the cell
#' values (defined as the first value per cell regardless of how many values per cell there are) against `min`.
#'
#' @examples
#' min_prune <- low_obs_pruner(70, "sum")
#' tbl_to_prune %>% prune_table(min_prune)
#'
#' @rdname trim_prune_funs
#' @export
low_obs_pruner <- function(min, type = c("sum", "mean")) {
  type <- match.arg(type)
  function(tt) {
    if (is(tt, "TableRow") || NROW(ctab <- content_table(tt)) != 1) { ## note the <- in there!!!
      return(FALSE) ## only trimming on count content rows
    }
    ctr <- tree_children(ctab)[[1]]
    vals <- sapply(row_values(ctr), function(v) v[[1]])
    sumvals <- sum(vals)
    if (type == "mean") {
      sumvals <- sumvals / length(vals)
    }
    sumvals < min
  }
}

#' Recursively prune a `TableTree`
#'
#' @inheritParams gen_args
#' @param prune_func (`function`)\cr a function to be called on each subtree which returns `TRUE` if the
#'   entire subtree should be removed.
#' @param stop_depth (`numeric(1)`)\cr the depth after which subtrees should not be checked for pruning.
#'   Defaults to `NA` which indicates pruning should happen at all levels.
#' @param depth (`numeric(1)`)\cr used internally, not intended to be set by the end user.
#' @param ... named arguments to optionally be passed down to `prune_func` if it
#'   accepts them (or `...`)
#'
#' @return A `TableTree` pruned via recursive application of `prune_func`.
#'
#' @seealso [prune_empty_level()] for details on this and several other basic pruning functions included
#'   in the `rtables` package.
#'
#' @examples
#' adsl <- ex_adsl
#' levels(adsl$SEX) <- c(levels(ex_adsl$SEX), "OTHER")
#'
#' tbl_to_prune <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("SEX") %>%
#'   summarize_row_groups() %>%
#'   split_rows_by("STRATA1") %>%
#'   summarize_row_groups() %>%
#'   analyze("AGE") %>%
#'   build_table(adsl)
#'
#' tbl_to_prune %>% prune_table()
#'
#' @export
prune_table <- function(tt,
                        prune_func = prune_empty_level,
                        stop_depth = NA_real_,
                        depth = 0,
                        ...) {
  if (!is.na(stop_depth) && depth > stop_depth) {
    return(tt)
  }
  if (is(tt, "TableRow")) {
    if (prune_func(tt)) {
      tt <- NULL
    }
    return(tt)
  }

  kids <- tree_children(tt)

  more_args <- match_fun_args(prune_func, depth = depth, ...)
  torm <- vapply(kids, function(tb) {
    !is.null(tb) && do.call(prune_func, c(list(tb), more_args))
  }, NA)

  keepkids <- kids[!torm]
  keepkids <- lapply(keepkids, prune_table,
    prune_func = prune_func,
    stop_depth = stop_depth,
    depth = depth + 1,
    ...
  )

  keepkids <- keepkids[!vapply(keepkids, is.null, NA)]
  if (length(keepkids) > 0) {
    tree_children(tt) <- keepkids
  } else {
    tt <- NULL
  }
  tt
}
