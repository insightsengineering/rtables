## Rules for pagination
##
## 1. user defined number of lines per page
## 2. all lines have the same height
## 3. header always reprinted on all pages
## 4. "Label-rows", ie content rows above break in the nesting structure, optionaly reprinted (default TRUE)
## 5. Never (?) break on a "label"/content row
## 6. Never (?) break on the second (ie after the first) data row at a particular leaf Elementary table.
##
## Current behavior: paginate_ttree takes a TableTree object and
## returns a list of rtable (S3) objects for printing.







setMethod("nlines", "TableRow",
          function(x, colwidths) {
    fns <- sum(unlist(lapply(row_footnotes(x), nlines))) + sum(unlist(lapply(cell_footnotes(x), nlines)))
    fcells <- get_formatted_cells(x)
    rowext <- max(vapply(strsplit(c(obj_label(x), fcells), "\n", fixed = TRUE),
                         length,
                         1L))
    rowext + fns
})

setMethod("nlines", "LabelRow",
          function(x, colwidths) {
    if(labelrow_visible(x))
        length(strsplit(obj_label(x), "\n", fixed = TRUE)[[1]]) + sum(unlist(lapply(row_footnotes(x), nlines)))
    else
        0L
})

setMethod("nlines", "RefFootnote",
          function(x, colwidths) {
    1L
})


setMethod("nlines", "VTableTree",
          function(x, colwidths) {
    length(collect_leaves(x, TRUE, TRUE))
})

setMethod("nlines", "InstantiatedColumnInfo",
          function(x, colwidths) {
    lfs = collect_leaves(coltree(x))
    depths = sapply(lfs, function(l) length(pos_splits(l)))
    max(depths, length(top_left(x))) + divider_height(x)

})

col_dfrow = function(col,
                    nm = obj_name(col),
                    lab = obj_label(col),
                    cnum,
                    pth = NULL,
                    sibpos = NA_integer_,
                    nsibs = NA_integer_,
                    leaf_indices = cnum,
                    span = length(leaf_indices),
                    col_fnotes = list()
                    ) {
    if(is.null(pth))
        pth <- pos_to_path(tree_pos(col))
    data.frame(stringsAsFactors = FALSE,
               name = nm,
               label = lab,
               abs_pos = cnum,
               path = I(list(pth)),
               pos_in_siblings = sibpos,
               n_siblings = nsibs,
               leaf_indices = I(list(leaf_indices)),
               total_span = span,
               col_fnotes = I(list(col_fnotes)),
               n_col_fnotes = length(col_fnotes))
}


pos_to_path <- function(pos) {
    spls <- pos_splits(pos)
    vals <- pos_splvals(pos)

    path <- character()
    for(i in seq_along(spls)) {
        path <- c(path,
                  obj_name(spls[[i]]),
                  ##rawvalues(vals[[i]]))
                  value_names(vals[[i]]))
    }
    path
}



## ' Make row and column layout summary data.frames for use during pagination
## ' @inheritParams gen_args
## ' @param visible_only logical(1). Should only visible aspects of the table structure be reflected in this summary. Defaults to \code{TRUE}.
## ' @param incontent logical(1). Internal detail do not set manually.
## ' @param repr_ext integer(1). Internal detail do not set manually.
## ' @param repr_inds integer. Internal detail do not set manually.
## ' @param sibpos integer(1). Internal detail do not set manually.
## ' @param nsibs integer(1). Internal detail do not set manually.
## ' @param rownum numeric(1). Internal detail do not set manually.
## ' @param indent integer(1). Internal detail do not set manually.

## ' @param colwidths numeric. Internal detail do not set manually.
## ' @param nrowrefs integer(1). Internal detail do not set manually.
## ' @param ncellrefs integer(1). Internal detail do not set manually.
## ' @param nreflines integer(1). Internal detail do not set manually.
## '
## ' @details
## ' When \code{visible_only} is \code{TRUE}, the resulting data.frame will have exactly one row per visible row in the table. This is useful when reasoning about how a table will print, but does not reflect the full pathing space of the structure (though the paths which are given will all work as is).
## '
## ' When \code{visible_only} is \code{FALSE}, every structural element of the table (in row-space) will be reflected in the returned data.frame, meaning the full pathing-space will be represented but some rows in the layout summary will not represent printed rows in the table as it is displayed.
#' @inherit formatters::make_row_df
#'
#' @note the technically present root tree node is excluded from the summary returne dby
#' both \code{make_row_df} and \code{make_col_df}, as it is simply the
#' row/column structure of \code{tt} and thus not useful for pathing or pagination.
#' @export
#' @return a data.frame of row/column-structure information used by the pagination machinery.
#' @name make_row_df
#' @rdname make_row_df
#' @aliases make_row_df,VTableTree-method
# #' @exportMethod make_row_df
setMethod("make_row_df", "VTableTree",
          function(tt, colwidths = NULL, visible_only = TRUE,
                   rownum = 0,
                   indent = 0L,
                   path = character(),
                   incontent = FALSE,
                   repr_ext = 0L,
                   repr_inds = integer(),
                   sibpos = NA_integer_,
                   nsibs = NA_integer_) {

    indent <- indent + indent_mod(tt)
    orig_rownum <- rownum
    if(incontent)
        path <- c(path, "@content")
    else if (length(path) > 0 || nzchar(obj_name(tt))) ## don't add "" for root
    ##else if (length(path) > 0 && nzchar(obj_name(tt))) ## don't add "" for root
        path <- c(path, obj_name(tt))

    ret <- list()
    ## note this is the **table** not the label row
    if(!visible_only) {
        ret <- c(ret,
                 list(pagdfrow(rnum = NA,
                               nm = obj_name(tt),
                               lab = "",
                               pth = path,
                               colwidths = colwidths,
                               repext = repr_ext,
                               repind = list(repr_inds),
                               extent = 0,
                               indent = indent,
                               rclass = class(tt), sibpos = sibpos,
                               nsibs = nsibs,
                               nrowrefs = 0L,
                               ncellrefs = 0L,
                               nreflines = 0L)))
    }
    if(labelrow_visible(tt)) {
        lr = tt_labelrow(tt)
        newdf <- make_row_df(lr,
                            colwidths= colwidths,
                            visible_only = visible_only,
                            rownum = rownum,
                            indent = indent,
                            path = path,
                            incontent = TRUE,
                            repr_ext = repr_ext,
                            repr_inds = repr_inds)
        rownum <- max(newdf$abs_rownumber,na.rm = TRUE)

        ret  =  c(ret,
                  list(newdf))
        repr_ext = repr_ext + 1L
        repr_inds = c(repr_inds, rownum)
        indent <- indent + 1L
    }


    if(NROW(content_table(tt)) > 0) {
        cind <- indent + indent_mod(content_table(tt))
        contdf <-  make_row_df(content_table(tt),
                              colwidths= colwidths,
                              visible_only = visible_only,
                              rownum = rownum,
                              indent = cind,
                              path = path,
                              incontent = TRUE,
                              repr_ext = repr_ext,
                              repr_inds = repr_inds)
        crnums <- contdf$abs_rownumber
        crnums <- crnums[!is.na(crnums)]

        newrownum <- max(crnums, na.rm = TRUE)
        if(is.finite(newrownum)) {
            rownum <- newrownum
            repr_ext <- repr_ext + length(crnums)
            repr_inds <- c(repr_inds, crnums)
        }
        ret <- c(ret, list(contdf))
        indent <- cind + 1
    }


    allkids <- tree_children(tt)
    newnsibs <- length(allkids)
    for(i in seq_along(allkids)) {
        kid <- allkids[[i]]
        kiddfs <- make_row_df(kid,
                            colwidths= colwidths,
                            visible_only = visible_only,
                            rownum = force(rownum),
                            indent = indent, ## + 1,
                            path = path,
                            incontent = incontent,
                            repr_ext = repr_ext,
                            repr_inds = repr_inds,
                            nsibs = newnsibs,
                            sibpos = i)

 #       print(kiddfs$abs_rownumber)
        rownum <- max(rownum + 1L, kiddfs$abs_rownumber, na.rm = TRUE) ##max(kiddfs[[length(kiddfs)]]$abs_rownumber, na.rm = TRUE)
        ret <- c(ret, list(kiddfs))
    }

    ret <- do.call(rbind, ret)
    if(!is.na(trailing_sep(tt)))
        ret$trailing_sep[nrow(ret)] <- trailing_sep(tt)
    ret
})

                                        # #' @exportMethod make_row_df
#' @inherit formatters::make_row_df
#' @export
#' @rdname make_row_df
setMethod("make_row_df", "TableRow",
          function(tt, colwidths = NULL, visible_only = TRUE,
                   rownum = 0,
                   indent = 0L,
                   path = "root",
                   incontent = FALSE,
                   repr_ext = 0L,
                   repr_inds = integer(),
                   sibpos = NA_integer_,
                   nsibs = NA_integer_) {
    indent <- indent + indent_mod(tt)
    rownum <- rownum + 1
    rrefs <- row_footnotes(tt)
    crefs <- cell_footnotes(tt)
    reflines <- sum(sapply(c(rrefs, crefs), nlines))
    ret <- pagdfrow(row = tt,
                    rnum = rownum,
                    colwidths = colwidths,
                    sibpos = sibpos,
                    nsibs = nsibs,
                    pth = c(path, obj_name(tt)),
                    repext = repr_ext,
                    repind = repr_inds,
                    indent = indent,
                    ## these two are unlist calls cause they come in lists even with no footnotes
                    nrowrefs = length(rrefs) ,
                    ncellrefs = length(unlist(crefs)),
                    nreflines = reflines
                    )
    ret
})

# #' @exportMethod make_row_df
#' @export
#' @rdname make_row_df
setMethod("make_row_df", "LabelRow",
          function(tt, colwidths = NULL, visible_only = TRUE,
                   rownum = 0,
                   indent = 0L,
                   path = "root",
                   incontent = FALSE,
                   repr_ext = 0L,
                   repr_inds = integer(),
                   sibpos = NA_integer_,
                   nsibs = NA_integer_) {
    rownum <- rownum + 1
    indent <- indent + indent_mod(tt)
    ret <- pagdfrow(tt, rnum = rownum,
                    colwidths = colwidths,
                    sibpos = sibpos,
                    nsibs = nsibs,
                    pth = path,
                    repext = repr_ext,
                    repind = repr_inds,
                    indent = indent,
                    nrowrefs = length(row_footnotes(tt)),
                    ncellrefs = 0L,
                    nreflines = sum(vapply(row_footnotes(tt), nlines, NA_integer_)))
    if(!labelrow_visible(tt))
        ret <- ret[0,]
    ret
})


setGeneric("inner_col_df", function(ct, colwidths = NULL, visible_only = TRUE,
                                   colnum = 0L,
                                   sibpos = NA_integer_,
                                   nsibs = NA_integer_,
                                   ncolref = 0L) standardGeneric("inner_col_df"))


#' Column Layout Summary
#'
#' Used for Pagination
#' @inheritParams make_row_df
#' @rdname make_row_df
#' @export
make_col_df <-    function(tt,
                           visible_only = TRUE) {
    ctree <- coltree(tt)
    rows <- inner_col_df(ctree, ## this is a null op if its already a coltree object
                 colwidths = propose_column_widths(matrix_form(tt, indent_rownames = TRUE)),
                 visible_only = visible_only,
                 colnum = 1L,
                 sibpos = 1L,
                 nsibs = 1L)  ## nsiblings includes current so 1 means "only child"

    do.call(rbind, rows)
}

setMethod("inner_col_df", "LayoutColLeaf",
          function(ct, colwidths, visible_only,
                   colnum,
                   sibpos,
                   nsibs) {
    list(col_dfrow(col = ct,
              cnum = colnum,
              sibpos = sibpos,
              nsibs = nsibs,
              leaf_indices = colnum,
              col_fnotes = col_fnotes_here(ct)))
})


setMethod("inner_col_df", "LayoutColTree",
          function(ct, colwidths, visible_only,
                   colnum,
                   sibpos,
                   nsibs) {
    kids <- tree_children(ct)
    ret <- vector("list", length(kids))
    for(i in seq_along(kids)) {
        k <- kids[[i]]
        nleaves <- length(collect_leaves(k))
        newrows <- do.call(rbind,
                           inner_col_df(k,
                                        colnum = colnum,
                                        sibpos = i,
                                        nsibs = length(kids),
                                        visible_only = visible_only))
        colnum <- max(newrows$abs_pos, colnum, na.rm = TRUE) + 1
        ret[[i]] = newrows
    }

    if(!visible_only) {
        allindices <- unlist(lapply(ret, function(df) df$abs_pos[!is.na(df$abs_pos)]))
        thispth <- pos_to_path(tree_pos(ct))
        if(any(nzchar(thispth))) {
            thisone  <- list(col_dfrow(col = ct,
                                       cnum = NA_integer_,
                                       leaf_indices = allindices,
                                       sibpos = sibpos,
                                       nsibs = nsibs,
                                       pth = thispth,
                                       col_fnotes = col_fnotes_here(ct)))
            ret <- c(thisone, ret)
        }
    }

    ret
})


#' Pagination of a TableTree
#'
#'
#' @note This is our first take on pagination. We will refine pagination in subsequent releases. Currently only
#'   pagination in the row space work. Pagination in the column space will be added in the future.
#'
#'
#' @inheritParams gen_args
#' @param lpp numeric. Maximum lines per page including (re)printed header and context rows
#' @param min_siblings  numeric. Minimum sibling rows which must appear on either side of pagination row for a
#'   mid-subtable split to be valid. Defaults to 2.
#' @param nosplitin character. List of names of sub-tables where page-breaks are not allowed, regardless of other
#'   considerations. Defaults to none.
#'
#' @export
#' @return for \code{pag_tt_indices} a list of paginated-groups of row-indices of \code{tt}. For \code{paginate_table},
#' The subtables defined by subsetting by the indices defined by \code{pag_tt_indices}.
#' @rdname paginate
#'
#' @examples
#'
#' s_summary <- function(x) {
#'  if (is.numeric(x)) {
#'      in_rows(
#'          "n" = rcell(sum(!is.na(x)), format = "xx"),
#'          "Mean (sd)" = rcell(c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
#'                              format = "xx.xx (xx.xx)"),
#'          "IQR" = rcell(IQR(x, na.rm = TRUE), format = "xx.xx"),
#'          "min - max" = rcell(range(x, na.rm = TRUE), format = "xx.xx - xx.xx")
#'      )
#'  } else if (is.factor(x)) {
#'
#'      vs <- as.list(table(x))
#'      do.call(in_rows, lapply(vs, rcell, format = "xx"))
#'
#'  } else (
#'      stop("type not supported")
#'  )
#' }
#'
#'
#' lyt <- basic_table() %>%
#' split_cols_by(var = "ARM") %>%
#'     analyze(c("AGE", "SEX", "BEP01FL", "BMRKR1", "BMRKR2", "COUNTRY"), afun = s_summary)
#'
#' tbl <- build_table(lyt, ex_adsl)
#' tbl
#'
#' nrow(tbl)
#'
#' row_paths_summary(tbl)
#'
#' tbls <- paginate_table(tbl)
#' mf <- matrix_form(tbl, indent_rownames = TRUE)
#' w_tbls <- propose_column_widths(mf) # so that we have the same column widths
#'
#' tmp <- lapply(tbls, print, widths = w_tbls)
#'
#' tmp <- lapply(tbls, function(tbli) {
#'   cat(toString(tbli, widths = w_tbls))
#'   cat("\n\n")
#'   cat("~~~~ PAGE BREAK ~~~~")
#'   cat("\n\n")
#' })
#'
#'
pag_tt_indices = function(tt, lpp = 15,
                           min_siblings = 2,
                           nosplitin = character(),
                           colwidths = NULL,
                           verbose = FALSE) {

    dheight <- divider_height(tt)

    cinfo_lines <- nlines(col_info(tt))
    if(any(nzchar(all_titles(tt)))) {
        tlines <- length(all_titles(tt)) + dheight + 1L
    } else {
        tlines <- 0
    }
    flines <- length(all_footers(tt))
    if(flines > 0)
        flines <- flines + dheight + 1L
    ## row lines per page
    rlpp = lpp - cinfo_lines - tlines - flines
    pagdf = make_row_df(tt, colwidths)

    pag_indices_inner(pagdf, rlpp = rlpp, min_siblings = min_siblings,
                         nosplitin = nosplitin,
                         verbose = verbose)
}


copy_title_footer <- function(to, from, newptitle) {
    main_title(to) <- main_title(from)
    subtitles(to) <- subtitles(from)
    page_titles(to) <- c(page_titles(from), newptitle)
    main_footer(to) <- main_footer(from)
    prov_footer(to) <- prov_footer(from)
    to
}

pag_btw_kids <- function(tt) {
    pref <- ptitle_prefix(tt)
    lapply(tree_children(tt),
           function(tbl) {
        tbl <- copy_title_footer(tbl, tt,
                                 paste(pref, obj_label(tbl), sep = ": "))
        labelrow_visible(tbl) <- FALSE
        tbl
    })
}


do_force_paginate <- function(tt,
                              force_pag = vapply(tree_children(tt), has_force_pag, NA),
                              verbose = FALSE) {


    ## forced pagination is happening at this
    if(has_force_pag(tt)) {
        ret <- pag_btw_kids(tt)
        return(lapply(ret, do_force_paginate))

    }
    chunks <- list()
    kinds <- seq_along(force_pag)
    while(length(kinds) > 0 ) {
        if(force_pag[kinds[1]]) {
            outertbl <- copy_title_footer(tree_children(tt)[[kinds[1]]],
                                          tt,
                                          NULL)

            chunks <- c(chunks, do_force_paginate(outertbl))
            kinds <- kinds[-1]
        } else {
            tmptbl <- tt
            runend <- min(which(force_pag[kinds]), length(kinds))
            useinds <- 1:runend
            tree_children(tmptbl) <- tree_children(tt)[useinds]
            chunks <- c(chunks, tmptbl)
            kinds <- kinds[-useinds]
        }
    }
    chunks
}




#' @export
#' @aliases paginate_table
#' @rdname paginate
paginate_table = function(tt, lpp = 15,
                          min_siblings = 2,
                          nosplitin = character(),
                          colwidths = NULL,
                          verbose = FALSE) {

    force_pag <- vapply(tree_children(tt), has_force_pag, TRUE)
    if(has_force_pag(tt) || any(force_pag)) {
        if(is.null(colwidths)) {
            colwidths <- propose_column_widths(matrix_form(tt))
        }

        spltabs <- do_force_paginate(tt, verbose = verbose)
        spltabs <- unlist(spltabs, recursive = TRUE)
        ret <- lapply(spltabs, paginate_table,
                      lpp = lpp,
                      min_siblings = min_siblings,
                      nosplitin = nosplitin,
                      colwidths = colwidths,
                      verbose = verbose)
        return(unlist(ret, recursive = TRUE))

    }
    inds = pag_tt_indices(tt, lpp = lpp,
                          min_siblings = min_siblings,
                          nosplitin = nosplitin,
                          colwidths = colwidths,
                          verbose = verbose)
    lapply(inds, function(x) tt[x,,keep_topleft = TRUE,
                                keep_titles = TRUE,
                                reindex_refs = FALSE])
}



#' @export
#' @aliases paginate_table
#' @inheritParams formatters::vert_pag_indices
#' @rdname paginate
vpaginate_table <- function(tt, cpp = 40, verbose = FALSE) {
    inds <- vert_pag_indices(tt, cpp = cpp, verbose = verbose)

    lapply(inds, function(j) tt[,j, keep_topleft = TRUE,
                                keep_titles = TRUE,
                                reindex_refs = FALSE])

}
