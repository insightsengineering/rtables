
#' Listing Shenanigans
#'
#' Experimental. Not for production work. I mean it.
#'
#' @export
#' @param df data.frame. The (non-listing) data.frame to be converted to a listing
#' @param cols character. Names of columns (including but not limited to key columns)
#' which should be displayed when the listing is rendered.
#' @param key_cols character. Names of columns which should be treated as *key columns*
#' when rendering the listing.
#' @param title character or NULL. Title for the listing. Currently ignored.
#' @param footer character or NULL. Footer for the listing. Currently ignored.
#'
#' @return A `listing_df` object, sorted by the key columns.
#' @rdname tmp_listings
#' @examples
#'
#'
#' dat <- ex_adae
#'
#'
#' lsting <- as_listing(dat[1:25,], key_cols = c("USUBJID", "AESOC")) %>%
#'     add_listing_col("AETOXGR") %>%
#'     add_listing_col("BMRKR1", format = "xx.x") %>%
#'     add_listing_col("AESER / AREL", fun = function(df) paste(df$AESER, df$AREL, sep = " / "))
#'
#'
#' mat <- listing_matrix_form(lsting)
#'
#' cat(toString(mat))
as_listing <- function(df,
                       cols = key_cols,
                       key_cols = names(df)[1],
                       title = NULL,
                       footer = NULL
                       ) {
    o <- do.call(order, df[key_cols])
    if(is.unsorted(o)) {
        message("sorting incoming data by key columns")
        df <- df[o,]
    }

    ## reorder the full set of cols to ensure key columns are first
    ordercols <- c(key_cols, setdiff(names(df), key_cols))
    df <- df[,ordercols]

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
#' @param vec any. A column vector from a `listing_df`
#'
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
    atts <- attributes(df)
    atts$names <- cols
    attributes(listing) <- atts

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
        for(nonk in nonkeycols) {
            vec <- listing[[nonk]]
            if(!is.null(obj_format(vec)))
                vec <- vapply(vec, format_rcell, "", format = obj_format(vec))
            bodymat[,nonk] <- vec
        }
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

#' @export
#' @inheritParams gen_args
#' @rdname tmp_listings
setMethod("main_title", "MatrixPrintForm",
          function(obj) attr(obj, "title"))
#' @export
#' @rdname tmp_listings
setMethod("subtitles", "MatrixPrintForm",
          function(obj) NULL)

#' @export
#' @rdname tmp_listings
setMethod("main_footer", "MatrixPrintForm",
          function(obj) attr(obj, "footer"))

#' @export
#' @rdname tmp_listings
setMethod("prov_footer", "MatrixPrintForm",
          function(obj) NULL)

#' @export
#' @rdname tmp_listings
listing_dispcols <- function(df) attr(df, "listing_dispcols")

#' @export
#' @param new character. Names of columns to be added to
#' the set of display columns.
#' @rdname tmp_listings
add_listing_dispcol <- function(df, new) {
    listing_dispcols(df) <- c(listing_dispcols(df), new)
    df
}
#' @export
#' @rdname tmp_listings
`listing_dispcols<-` <-  function(df, value) {
    attr(df, "listing_dispcols") <- unique(value)
    df
}


#' @export
#' @param df listing_df. The listing to modify.
#' @param name character(1). Name of the existing or new column to be
#' displayed when the listing is rendered
#' @param fun function or NULL. A function which accepts \code{df} and
#' returns the vector for a new column, which is added to \code{df} as
#' \code{name}, or NULL if marking an existing column as
#' a listing column
#' @param format FormatSpec. A format specification (format string,
#' function, or sprintf format) for use when displaying the column
#' during rendering.
#'
#' @return `df`, with `name` created (if necessary) and marked for
#' display during rendering.
#' @rdname tmp_listings
add_listing_col <- function(df, name, fun = NULL, format = NULL) {
    if(!is.null(fun))
        df[[name]] <- fun(df)

    if(!is.null(format)) {
        vec <- df[[name]]
        attr(vec, "format") <- format
        df[[name]] <- vec
    }
    df <- add_listing_dispcol(df, name)
    df
}

print.listing_df <- function(x, ...) {
    cat(toString(listing_matrix_form(x)))
    invisible(x)
}
