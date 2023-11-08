.reindex_one_pos <- function(refs, cur_idx_fun) {
  if (length(refs) == 0) {
    return(refs)
  }

  lapply(refs, function(refi) {
    ## these can be symbols, e.g. ^, † now, those are
    ## special and don't get reindexed cause they're not numbered
    ## to begin with
    idx <- ref_index(refi)
    if (is.na(idx) || !is.na(as.integer(idx))) {
      ref_index(refi) <- cur_idx_fun(refi)
    }
    refi
  })
}


setGeneric(".idx_helper", function(tr, cur_idx_fun) standardGeneric(".idx_helper"))


setMethod(
  ".idx_helper", "TableRow",
  function(tr, cur_idx_fun) {
    row_footnotes(tr) <- .reindex_one_pos(
      row_footnotes(tr),
      cur_idx_fun
    )

    cell_footnotes(tr) <- lapply(cell_footnotes(tr), ## crfs,
      .reindex_one_pos,
      cur_idx_fun = cur_idx_fun
    )
    tr
  }
)

setMethod(
  ".idx_helper", "VTableTree",
  function(tr, cur_idx_fun) {
    if (!labelrow_visible(tr)) {
      stop("got a row footnote on a non-visible label row. this should never happen") # nocov
    }
    lr <- tt_labelrow(tr)

    row_footnotes(lr) <- .reindex_one_pos(
      row_footnotes(lr),
      cur_idx_fun
    )

    tt_labelrow(tr) <- lr

    tr
  }
)

index_col_refs <- function(tt, cur_idx_fun) {
  ctree <- coltree(tt)
  ctree <- .index_col_refs_inner(ctree, cur_idx_fun)
  coltree(tt) <- ctree
  tt
}


.index_col_refs_inner <- function(ctree, cur_idx_fun) {
  col_fnotes_here(ctree) <- .reindex_one_pos(
    col_fnotes_here(ctree),
    cur_idx_fun
  )

  if (is(ctree, "LayoutColTree")) {
    tree_children(ctree) <- lapply(tree_children(ctree),
      .index_col_refs_inner,
      cur_idx_fun = cur_idx_fun
    )
  }
  ctree
  ## cfs <- col_fnotes_here(ctree)
  ## if(length(unlist(cfs)) > 0) {
  ##     col_fnotes_here(ctree) <- .reindex_one_pos(lapply(cfs,
  ##                                      function(refs) lapply(refs, function(refi) {
}

#' Update footnote indexes on a built table
#'
#' Re-indexes footnotes within a built table
#' @inheritParams gen_args
#'
#' @details  After adding or removing  referential footnotes manually,
#'     or  after subsetting  a table,  the reference  indexes (i.e.  the
#'     number associated  with specific  footnotes) may  be incorrect.
#'     This function recalculates these based on the full table.
#'
#' @note In the future this should not generally need to be called
#' manually.
#' @export
update_ref_indexing <- function(tt) {
  col_fnotes <- c(list(row_fnotes = list()), col_fnotes_here(tt))
  row_fnotes <- row_footnotes(tt)
  cell_fnotes <- cell_footnotes(tt)
  all_fns <- rbind(col_fnotes, cbind(row_fnotes, cell_fnotes))
  all_fns <- unlist(t(all_fns))
  unique_fnotes <- unique(sapply(all_fns, ref_msg))
  
  cur_index <- function(ref_fn) {
    match(ref_msg(ref_fn), unique_fnotes)
  }
  
  if (ncol(tt) > 0) {
    tt <- index_col_refs(tt, cur_index)
  } ## col_info(tt) <- index_col_refs(col_info(tt), cur_index)
  ## TODO when column refs are a thing we will
  ## still need to do those here before returning!!!
  if (nrow(tt) == 0) {
    return(tt)
  }


  rdf <- make_row_df(tt)

  rdf <- rdf[rdf$nreflines > 0, ]
  if (nrow(rdf) == 0) {
    return(tt)
  }

  for (i in seq_len(nrow(rdf))) {
    path <- unname(rdf$path[[i]])
    tt_at_path(tt, path) <-
      .idx_helper(
        tt_at_path(tt, path),
        cur_index
      )
  }
  tt
}
