
#' Listing Shenanigans
#'
#' Experimental. Not for production work. I mean it.
#' @export
#' @rdname tmp_listings
as_listing <- function(df,
                       cols = names(df),
                       key_cols = cols[1],
                       title = NULL,
                       footer = NULL) {
    o <- do.call(order, df[key_cols])
    if(is.unsorted(o)) {
        message("sorting incoming data by key columns")
        df <- df[o,]
    }

    ## reorder the full set of cols to ensure key columns are first
    cols <- c(key_cols, setdiff(cols, key_cols))
    df <- df[,cols]

    for(cnm in key_cols) {
        df[[cnm]] <- as_keycol(df[[cnm]])
    }



    class(df) <- c("listing_df", class(df))
    if(!is.null(title))
        attr(df, title) <- title
    if(!is.null(footer))
        attr(df, footer) <- footer
    attr(df, "listing_dispcols") <- cols
    df
}


#' @export
#' @rdname tmp_listings
as_keycol <- function(vec) {
    if(is.factor(vec))
        vec <- as.character(vec)
    class(vec) <- c("listing_keycol", class(vec))
    vec
}


#' @export
#' @rdname tmp_listings
is_keycol <- function(vec) {
    inherits(vec, "listing_keycol")
}



#' @export
#' @rdname tmp_listings
get_keycols <- function(df) {
    names(which(sapply(df, is_keycol)))
}

#' @export
#' @rdname tmp_listings
listing_matrix_form <- function(df) {
    cols <- attr(df, "listing_dispcols")
    listing <- df[,cols]
    attributes(listing) <- attributes(df)

    keycols <- get_keycols(listing)


    bodymat <- matrix("", nrow = nrow(listing),
                      ncol = ncol(listing))

    colnames(bodymat) <- names(listing)


    for(i in seq_along(keycols)) {
        kcol <- keycols[i]
        kcolvec <- listing[[kcol]]
        if(i == 1) {
            unqinds <- !duplicated(kcolvec)

        } else {
            spldat <- split(kcolvec, listing[keycols[1:(i-1)]])
            unqinds <- unlist(lapply(spldat, function(xi) !duplicated(xi)))
        }
        bodymat[unqinds, kcol] <- kcolvec[unqinds]
    }

    nonkeycols <- setdiff(names(listing), keycols)
    if(length(nonkeycols) > 0) {
        bodymat[,nonkeycols] <- as.matrix(listing[,nonkeycols])
    }


    fullmat <- rbind(var_labels(listing, fill = TRUE),
                     bodymat)

    structure(list(strings = fullmat,
                   spans = matrix(1, nrow = nrow(fullmat),
                                  ncol = ncol(fullmat)),
                   ref_footnotes = list(),
                   display = matrix(TRUE, nrow = nrow(fullmat),
                                    ncol = ncol(fullmat)),
                   aligns = matrix("center", nrow = nrow(fullmat),
                                   ncol = ncol(fullmat))),
              nlines_header = 1, ## XXX this is wrong!
              nrow_header = 1,
              class = c("MatrixPrintForm", "list"))
}

setOldClass("MatrixPrintForm")

setMethod("main_title", "MatrixPrintForm",
          function(obj) attr(obj, "title"))

setMethod("subtitles", "MatrixPrintForm",
          function(obj) NULL)


setMethod("main_footer", "MatrixPrintForm",
          function(obj) attr(obj, "footer"))

setMethod("prov_footer", "MatrixPrintForm",
          function(obj) NULL)


