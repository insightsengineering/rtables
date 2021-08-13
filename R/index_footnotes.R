
.reindex_one_pos <- function(refs, cur_idx_fun) {
    if(length(refs) == 0)
        return(refs)

    lapply(refs, function(refi) {
        ref_index(refi) <- cur_idx_fun()
        refi
    })
}


.idx_helper <- function(tr , cur_idx_fun) {
    row_footnotes(tr) <- .reindex_one_pos(row_footnotes(tr),
                                          cur_idx_fun)

    ## if(length(row_footnotes(tr)) > 0) {
    ##     row_footnotes(tr) <- .reindex_one_pos(row_footnotes(tr),
    ##                                           cur_idx_fun)
    ## }

    ## crfs <- cell_footnotes(tr)
    ## if(length(unlist(crfs)) > 0) {

        cell_footnotes(tr) <- lapply(cell_footnotes(tr), ##crfs,
                                     .reindex_one_pos,
                                     cur_idx_fun = cur_idx_fun
                                     )
    tr
}

index_col_refs <- function(tt, cur_idx_fun) {
    ctree <- coltree(tt)
    ctree <- .index_col_refs_inner(ctree, cur_idx_fun)
    coltree(tt) <- ctree
    tt
}


.index_col_refs_inner <- function(ctree, cur_idx_fun) {
    col_fnotes_here(ctree) <- .reindex_one_pos(col_fnotes_here(ctree),
                                               cur_idx_fun)

    if(is(ctree, "LayoutColTree"))
        tree_children(ctree) <- lapply(tree_children(ctree),
                                       .index_col_refs_inner,
                                       cur_idx_fun = cur_idx_fun)
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
#'     or  after subsetting  a table,  the reference  indexes (ie  the
#'     number associated  with specific  footnotes) may  be incorrect.
#'     This function recalculates these based on the full table.
#'
#' @note In the future this should not generally need to be called
#' manually.
#' @export
update_ref_indexing <- function(tt) {
    curind <- 0L
    cur_index <- function() {
        curind <<- curind + 1L
        curind
    }
    if(ncol(tt) > 0)
        tt <- index_col_refs(tt, cur_index) ##col_info(tt) <- index_col_refs(col_info(tt), cur_index)
    ## TODO when column refs are a thing we will
    ## still need to do those here before returning!!!
    if(nrow(tt) == 0)
        return(tt)


    rdf <- make_row_df(tt)

    rdf <- rdf[rdf$nreflines > 0,]
    if(nrow(rdf) == 0)
        return(tt)

    for (i in 1:nrow(rdf)) {
        path <- rdf$path[[i]]
        tt_at_path(tt, path) <-
            .idx_helper(tt_at_path(tt, path),
                        cur_index)
    }
    tt
}
