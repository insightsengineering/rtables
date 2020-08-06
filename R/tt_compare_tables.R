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

content_all_zeros_nas = function(tt) {
    ## this will be NULL if
    ## tt is something that doesn't have a content table
    ct <- content_table(tt)
    ## NROW returns 0 for NULL.
    if(NROW(ct) == 0 || nrow(ct) > 1)
        return(FALSE)

    cr <- tree_children(ct)
    all_zero_or_na(cr)
}



prune_empty_level = function(tt) {
    if(is(tt, "TableRow"))
        return(all_zero_or_na(tt))
    
    if(content_all_zeros_nas(tt))
        return(TRUE)
    kids = tree_children(tt)
    length(kids) == 0
}

recurse_prune = function(tt, prune_func = prune_empty_level, stop_depth = NA, depth = 0) {
    if(!is.na(stop_depth) && depth > stop_depth)
        return(tt)
    if(is(tt, "TableRow")) {
        if(prune_func(tt))
            tt <- NULL
        return(tt)
    }
    
    kids = tree_children(tt)
 
    torm = vapply(kids, function(tb) {
        !is.null(tb) && prune_func(tb)
    }, NA)

    keepkids = kids[!torm]
    keepkids = lapply(keepkids, recurse_prune,
                      prune_func = prune_func,
                      stop_depth = stop_depth,
                      depth = depth + 1)
    
    keepkids = keepkids[!vapply(keepkids, is.null, NA)]
    if(length(keepkids) > 0)
        tree_children(tt) = keepkids
    else
        tt = NULL
    tt
}

## }


## l <- basic_table() %>%
##     split_cols_by("ARM") %>%
##     split_rows_by("COUNTRY") %>%
##     split_rows_by("RACE") %>%
##     analyze("AGE", mean)

## sptbl <- build_table(l, dm2)
## ## recurse_trim_content = function(tt, path,  trim_fun, full_recursive = FALSE) {
## ##

        
        

## }


