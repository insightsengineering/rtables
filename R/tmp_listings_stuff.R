
setOldClass(c("listing_df", "data.frame"))
setOldClass(c("MatrixPrintForm", "list"))
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
    varlabs <- var_labels(df, fill = TRUE)
    o <- do.call(order, df[key_cols])
    if(is.unsorted(o)) {
        message("sorting incoming data by key columns")
        df <- df[o,]
    }

    ## reorder the full set of cols to ensure key columns are first
    ordercols <- c(key_cols, setdiff(names(df), key_cols))
    df <- df[,ordercols]
    var_labels(df) <- varlabs[ordercols]

    for(cnm in key_cols) {
        df[[cnm]] <- as_keycol(df[[cnm]])
    }



    class(df) <- c("listing_df", class(df))
    if(!is.null(title))
        attr(df, "main_title") <- title
    if(!is.null(footer))
        attr(df, "main_footer") <- footer
    attr(df, "listing_dispcols") <- cols
    df
}


#' @export
#' @rdname tmp_listings
as_keycol <- function(vec) {
    if(is.factor(vec)) {
        lab <- obj_label(vec)
        vec <- as.character(vec)
        obj_label(vec) <- lab
    }
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
            vec <- vapply(vec, format_rcell, "", format = obj_format(vec))
            bodymat[,nonk] <- vec
        }
    }


    fullmat <- rbind(var_labels(listing, fill = TRUE),
                     bodymat)

    keycolaligns <- rbind(rep("center", length(keycols)),
                          matrix("left", ncol = length(keycols),
                           nrow = nrow(fullmat) - 1))
    ## ret <- structure(list(strings = fullmat,
    ##                spans = matrix(1, nrow = nrow(fullmat),
    ##                               ncol = ncol(fullmat)),
    ##                ref_footnotes = list(),
    ##                display = matrix(TRUE, nrow = nrow(fullmat),
    ##                                 ncol = ncol(fullmat)),
    ##                aligns = cbind(keycolaligns,
    ##                               matrix("center", nrow = nrow(fullmat),
    ##                                      ncol = ncol(fullmat)- length(keycols)))),
    ##           nlines_header = 1, ## XXX this is wrong!
    ##           nrow_header = 1,
    ##           class = c("MatrixPrintForm", "list"))
    ## .do_mat_expand(ret, has_topleft = FALSE)
    MatrixPrintForm(strings = fullmat,
                    spans = matrix(1, nrow = nrow(fullmat),
                                   ncol = ncol(fullmat)),
                    ref_fnotes =list(),
                    aligns = cbind(keycolaligns,
                                   matrix("center", nrow = nrow(fullmat),
                                          ncol = ncol(fullmat)- length(keycols))),
                    row_info = make_row_df(df),
                    nlines_header = 1, ## XXX this is probably wrong!!!
                    nrow_header = 1,
                    has_topleft = FALSE,
                    expand_newlines = TRUE)
}



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

## #' Print a listing to the terminal
## #' @param x listing_df. the listing
## #' @param ... ANY. unused
## #' @return prints the listing object to the screen and silently returns the object
## #' @export
## setMethod("print", "listing_df",
##           function(x, ...) {
##     cat(toString(listing_matrix_form(x)))
##     invisible(x)
## })

#' @export
#' @method print listing_df
#' @rdname int_methods
print.listing_df <- function(x, ...) {
    cat(toString(listing_matrix_form(x)))
    invisible(x)
}

#' @export
#' @rdname make_row_df
setMethod("make_row_df", "listing_df",
           function(tt, colwidths = NULL, visible_only = TRUE,
                    rownum = 0,
                    indent = 0L,
                    path = character(),
                    incontent = FALSE,
                    repr_ext = 0L,
                    repr_inds = integer(),
                    sibpos = NA_integer_,
                    nsibs = NA_integer_,
                    nrowrefs = 0L,
                    ncellrefs = 0L,
                    nreflines = 0L) {

    keycols <- get_keycols(tt)
    tt$abs_rownumber <- seq_along(tt[[1]])

    tt$path <- I(lapply(1:NROW(tt),
                    function(i) {
        retpath <- character(2*length(keycols))
        for(j in seq_along(keycols)) {
            retpath[2*j - 1] <- keycols[j]
            retpath[2*j] <- tt[i, keycols[j], drop = TRUE]
        }
        retpath
    }))
    spl <- split(tt, tt[keycols])
    spl <- spl[vapply(spl, function(y) NROW(y) > 0, NA)]
    dfs <- lapply(spl, function(df) {
        df <- df[order(df$abs_rownumber),]
        ndf <- NROW(df)
        lapply(1:ndf, function(i) {
            rw <- df[i,]
            stopifnot(nrow(rw) == 1)
            pagdfrow(nm = "",
                     lab = "",
                     rnum = rw$abs_rownumber,
                     pth = NA_character_,
                     sibpos = i,
                     nsibs = ndf,
                     extent = 1L,
                     rclass = "listing_df",
                     repind = integer())
        })
    })
    ret <- do.call(rbind, unlist(dfs, recursive = FALSE))
    ret <- ret[order(ret$abs_rownumber),]
    ret
})


#' @export
#' @rdname brackets
setMethod("[", "listing_df",
          function(x, i, j, drop = FALSE) {
    xattr <- attributes(x)
    xattr$names <- xattr$names[j]
    res <- NextMethod()
    attributes(res) <- xattr
    res
})

#' @rdname title_footer
#' @export
setMethod("main_title", "listing_df",
          function(obj) attr(obj, "main_title") %||% character())

#' @rdname title_footer
#' @export
setMethod("subtitles", "listing_df",
          function(obj) attr(obj, "subtitles") %||% character())
#' @rdname title_footer
#' @export
setMethod("main_footer", "listing_df",
          function(obj) attr(obj, "main_footer") %||% character())
#' @rdname title_footer
#' @export
setMethod("prov_footer", "listing_df",
          function(obj) attr(obj, "prov_footer") %||% character())


#' @rdname paginate
#' @param lsting listing_df. The listing to paginate.
#' @export
paginate_listing <- function(lsting, lpp = 15,
                             min_siblings = 2,
                             nosplitin = character(),
                             colwidths = NULL,
                             verbose = FALSE) {

    ## XXX TODO this is duplciated form pag_tt_indices
    ## refactor so its not
    dheight <- divider_height(lsting)

    cinfo_lines <- 1L
    if(any(nzchar(all_titles(lsting)))) {
        tlines <- length(all_titles(lsting)) + dheight + 1L
    } else {
        tlines <- 0
    }
    flines <- length(all_footers(lsting))
    if(flines > 0)
        flines <- flines + dheight + 1L
    ## row lines per page
    rlpp <- lpp - cinfo_lines - tlines - flines
    pagdf <- make_row_df(lsting, colwidths)

    inds <- pag_tt_indices_inner(pagdf,
                              rlpp = rlpp,
                              min_siblings = min_siblings,
                              nosplitin = nosplitin,
                              verbose = verbose)

    lapply(inds, function(i) lsting[i,])
}
