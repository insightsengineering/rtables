all_zero_or_na = function(tr) {
    rvs = unlist(unname(row_values(tr)))
    all(is.na(rvs)) || isTRUE(all.equal(unique(rvs), 0))
}




#' Trim rows from a populated table without regard for table structure
#' @inheritParams gen_args
#' @param criteria function. Function which takes a TableRow object and returns \code{TRUE} if that row should be removed. Defaults to removing rows with all zeros or all NAs
#' @return The table with rows that have only cell values removed
#' @export
trim_rows <- function(tt, criteria = all_zero_or_na) {
    rows = collect_leaves(tt, TRUE, TRUE)
    torm = vapply(rows, criteria,
                  NA, USE.NAMES = FALSE)
    tt[!torm,]
}


## prune_by_group_summary = function(tt, criteria = all_zero_or_na, start_depth = 0, stop_depth= NA_real_, .depth = 0) {
##     kids = tree_children)
##     if(.depth >= max(start_depth, 1))
##         ctab = content_table(tt)
    


## }

recurse_prune_by_content = function(tt, criteria, stop_depth, depth) {
    if(is(tt, "TableRow") || (!is.na(stop_depth) && depth > stop_depth))
        return(tt)

    kids = tree_children(tt)
    ctabs = lapply(kids, function(kid) {
        if(!is(kid, "TableTree"))
            content_table(kid)
        else
            NULL
    })

    torm = vapply(ctabs, function(tb) {
        !is.null(tb) && criteria(tb)
    }, NA)

    keepkids = kids[!torm]
    if(depth < stop_depth && !are(keepkids, "TableRow"))
        tree_children(tt) = lapply(keepkids, recurse_prune_content,
                                   criteria = criteria,
                                   stop_depth = stop_depth,
                                   depth = depth + 1)
    else
        tree_children(tt) = keepkids
    tt
}

## }

## recurse_trim_content = function(tt, path,  trim_fun, full_recursive = FALSE) {
##     if(length(path) == 0) {
        
        

## }


