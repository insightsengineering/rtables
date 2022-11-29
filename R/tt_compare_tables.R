
#' Trimming and Pruning Criteria
#'
#' Criteria functions (and constructors thereof) for trimming and pruning
#' tables.
#'
#' @inheritParams gen_args
#'
#' @details \code{all_zero_or_na} returns \code{TRUE} (and thus indicates
#'   trimming/pruning) for any \emph{non-LabelRow} \code{TableRow} which contain
#'   only any mix of \code{NA} (including \code{NaN}), \code{0}, \code{Inf} and
#'   \code{-Inf} values.
#' @rdname trim_prune_funs
#' @return A logical value indicating whether \code{tr} should be included
#'   (\code{TRUE}) or pruned (\code{FALSE}) during pruning.
#' @seealso [prune_table()], [trim_rows()]
#' @export
all_zero_or_na <- function(tr) {
    if(!is(tr, "TableRow") || is(tr, "LabelRow"))
        return(FALSE)
    rvs <- unlist(unname(row_values(tr)))
    all(is.na(rvs) | rvs == 0 | !is.finite(rvs))
}

#' @rdname trim_prune_funs
#'
#' @details \code{all_zero} returns \code{TRUE} for any non-Label row which
#'   contains only (non-missing) zero values.
#'
#' @export
all_zero <- function(tr) {
    if(!is(tr, "TableRow") || is(tr, "LabelRow"))
        return(FALSE)
    rvs <- unlist(unname(row_values(tr)))
    isTRUE(all(rvs == 0))
}
#' Trim rows from a populated table without regard for table structure
#' @inheritParams gen_args
#' @param criteria function. Function which takes a TableRow object and returns
#'   \code{TRUE} if that row should be removed. Defaults to
#'   \code{\link{all_zero_or_na}}
#' @return The table with rows that have only NA or 0 cell values removed
#' @note Visible \code{LabelRow}s are including in this trimming, which can lead
#'   to either all label rows being trimmed or label rows remaining when all
#'   data rows have been trimmed, depending on what \code{criteria} returns when
#'   called on a \code{LabelRow} object. To avoid this, use the
#'   structurally-aware \code{\link{prune_table}} machinery instead.
#' @export
#' @seealso [prune_table()]
trim_rows <- function(tt, criteria = all_zero_or_na) {
    rows <- collect_leaves(tt, TRUE, TRUE)
    torm <- vapply(rows, criteria,
                  NA, USE.NAMES = FALSE)
    tt[!torm, , keep_topleft = TRUE]
}

#' @rdname trim_prune_funs
#' @inheritParams trim_rows
#' @details \code{content_all_zeros_nas} Prunes a subtable if a) it has a
#'   content table with exactly one row in it, and b) \code{all_zero_or_na}
#'   returns \code{TRUE} for that single content row. In practice, when the
#'   default summary/content function was used, this represents pruning any
#'   subtable which corresponds to an empty set of the input data (e.g., because
#'   a factor variable was used in \code{\link{split_rows_by}} but not all
#'   levels were present in the data).
#'
#' @export
content_all_zeros_nas <- function(tt, criteria = all_zero_or_na) {
    ## this will be NULL if
    ## tt is something that doesn't have a content table
    ct <- content_table(tt)
    ## NROW returns 0 for NULL.
    if(NROW(ct) == 0 || nrow(ct) > 1)
        return(FALSE)

    cr <- tree_children(ct)[[1]]
    criteria(cr)
}


#' @details \code{prune_empty_level} combines \code{all_zero_or_na} behavior for
#'   \code{TableRow} objects, \code{content_all_zeros_nas} on
#'   \code{content_table(tt)} for \code{TableTree} objects, and an addition
#'   check that returns \code{TRUE} if the \code{tt} has no children.
#'
#' @export
#' @rdname trim_prune_funs
prune_empty_level <- function(tt) {
    if(is(tt, "TableRow"))
        return(all_zero_or_na(tt))

    if(content_all_zeros_nas(tt))
        return(TRUE)
    kids <- tree_children(tt)
    length(kids) == 0
}

#' @details \code{prune_zeros_only} behaves as \code{prune_empty_levels} does,
#'   except that like \code{all_zero} it prunes only in the case of all
#'   non-missing zero values.
#' @rdname trim_prune_funs
#' @export
prune_zeros_only <- function(tt) {
    if(is(tt, "TableRow"))
        return(all_zero(tt))

    if(content_all_zeros_nas(tt, criteria = all_zero))
        return(TRUE)
    kids <- tree_children(tt)
    length(kids) == 0
}

#' @param min numeric(1). (lob_obs_pruner only). Minimum aggregate count value.
#'   Subtables whose combined/average count are below this threshold will be
#'   pruned
#' @param type character(1). How count values should be aggregated. Must be
#'   \code{"sum"} (the default) or \code{"mean"}
#'
#' @details \code{lob_obs_pruner} is a \emph{constructor function} which, when
#'   called, returns a pruning criteria function which will prune on content
#'   rows by comparing sum or mean (dictated by \code{type})of the count count
#'   portions of the cell values (defined as the first value per cell regardless
#'   of how many values per cell there are) against \code{min}.
#'
#' @rdname trim_prune_funs
#' @export
low_obs_pruner <- function(min, type = c("sum", "mean")) {
    type <- match.arg(type)
    function(tt) {
        if(is(tt, "TableRow") ||
           ## note the <- in there!!!
           NROW(ctab <- content_table(tt)) != 1)
            return(FALSE) ## only trimming on count content rows
        ctr <- tree_children(ctab)[[1]]
        vals <- sapply(row_values(ctr), function(v) v[[1]])
        sumvals <- sum(vals)
        if(type == "mean")
            sumvals <- sumvals / length(vals)
        sumvals < min
    }
}

#' Recursively prune a TableTree
#'
#' @inheritParams gen_args
#' @param prune_func function. A Function to be called on each subtree which
#'   returns TRUE if the entire subtree should be removed.
#' @param stop_depth numeric(1). The depth after which subtrees should not be
#'   checked for pruning. Defaults to \code{NA} which indicates pruning should
#'   happen at all levels
#' @param depth numeric(1). Used internally, not intended to be set by the end
#'   user.
#'
#' @return A TableTree pruned via recursive application of \code{prune_func}.
#'
#' @export
#'
#' @seealso [prune_empty_level()]
#'
prune_table <- function(tt,
                        prune_func = prune_empty_level,
                        stop_depth = NA_real_,
                        depth = 0) {
    if(!is.na(stop_depth) && depth > stop_depth)
        return(tt)
    if(is(tt, "TableRow")) {
        if(prune_func(tt))
            tt <- NULL
        return(tt)
    }

    kids <- tree_children(tt)

    torm <- vapply(kids, function(tb) {
        !is.null(tb) && prune_func(tb)
    }, NA)

    keepkids <- kids[!torm]
    keepkids <- lapply(keepkids, prune_table,
                      prune_func = prune_func,
                      stop_depth = stop_depth,
                      depth = depth + 1)

    keepkids <- keepkids[!vapply(keepkids, is.null, NA)]
    if(length(keepkids) > 0)
        tree_children(tt) <- keepkids
    else
        tt <- NULL
    tt
}
