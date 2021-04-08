
.idx_helper <- function(tr , cur_idx_fun) {
    if(length(row_footnotes(tr)) > 0) {
        row_footnotes(tr) <- lapply(row_footnotes(tr),
                                    function(ref) {
            ref_index(ref) <- cur_idx_fun()
            ref
        })
    }

    crfs <- cell_footnotes(tr)
    if(length(unlist(crfs)) > 0) {

        cell_footnotes(tr) <- lapply(crfs,
                                     function(refs) lapply(refs, function(refi) {
                                                        ref_index(refi) <- cur_idx_fun()
                                                        refi
                                                    }))
    }
    tr
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
