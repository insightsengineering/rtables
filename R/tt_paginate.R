## Rules for pagination
##
## 1. user defined number of lines per page
## 2. all lines have the same height
## 3. header always reprinted on all pages
## 4. "Label-rows", i.e. content rows above break in the nesting structure, optionally reprinted (default TRUE)
## 5. Never (?) break on a "label"/content row
## 6. Never (?) break on the second (i.e. after the first) data row at a particular leaf Elementary table.
##
## Current behavior: paginate_ttree takes a TableTree object and
## returns a list of rtable (S3) objects for printing.

#' @inheritParams formatters::nlines
#'
#' @rdname formatters_methods
#' @aliases nlines,TableRow-method
#' @exportMethod nlines
setMethod(
  "nlines", "TableRow",
  function(x, colwidths, max_width, fontspec, col_gap = 3) {
    fns <- sum(unlist(lapply(row_footnotes(x), nlines, max_width = max_width, fontspec = fontspec))) +
      sum(unlist(lapply(cell_footnotes(x), nlines, max_width = max_width, fontspec = fontspec)))
    fcells <- as.vector(get_formatted_cells(x))
    spans <- row_cspans(x)
    have_cw <- !is.null(colwidths)
    ## handle spanning so that the projected word-wrapping from nlines is correct
    if (any(spans > 1)) {
      new_fcells <- character(length(spans))
      new_colwidths <- numeric(length(spans))
      cur_fcells <- fcells
      cur_colwidths <- colwidths[-1] ## not the row labels they can't span
      for (i in seq_along(spans)) {
        spi <- spans[i]
        new_fcells[i] <- cur_fcells[1] ## 1 cause we're trimming it every loop
        new_colwidths[i] <- sum(head(cur_colwidths, spi)) + col_gap * (spi - 1)
        cur_fcells <- tail(cur_fcells, -1 * spi)
        cur_colwidths <- tail(cur_colwidths, -1 * spi)
      }
      if (have_cw) {
        colwidths <- c(colwidths[1], new_colwidths)
      }
      fcells <- new_fcells
    }

    ## rowext <- max(vapply(strsplit(c(obj_label(x), fcells), "\n", fixed = TRUE),
    ##                      length,
    ##                      1L))
    rowext <- max(
      unlist(
        mapply(
          function(s, w) {
            nlines(strsplit(s, "\n", fixed = TRUE), max_width = w, fontspec = fontspec)
          },
          s = c(obj_label(x), fcells),
          w = (colwidths %||% max_width) %||% vector("list", length(c(obj_label(x), fcells))),
          SIMPLIFY = FALSE
        )
      )
    )

    rowext + fns
  }
)

#' @export
#' @rdname formatters_methods
setMethod(
  "nlines", "LabelRow",
  function(x, colwidths, max_width, fontspec = fontspec, col_gap = NULL) {
    if (labelrow_visible(x)) {
      nlines(strsplit(obj_label(x), "\n", fixed = TRUE)[[1]], max_width = colwidths[1], fontspec = fontspec) +
        sum(unlist(lapply(row_footnotes(x), nlines, max_width = max_width, fontspec = fontspec)))
    } else {
      0L
    }
  }
)

#' @export
#' @rdname formatters_methods
setMethod(
  "nlines", "RefFootnote",
  function(x, colwidths, max_width, fontspec, col_gap = NULL) {
    nlines(format_fnote_note(x), colwidths = colwidths, max_width = max_width, fontspec = fontspec)
  }
)

#' @export
#' @rdname formatters_methods
setMethod(
  "nlines", "InstantiatedColumnInfo",
  function(x, colwidths, max_width, fontspec, col_gap = 3) {
    h_rows <- .do_tbl_h_piece2(x)
    tl <- top_left(x) %||% rep("", length(h_rows))
    main_nls <- vapply(
      seq_along(h_rows),
      function(i) {
        max(
          nlines(h_rows[[i]],
            colwidths = colwidths,
            fontspec = fontspec,
            col_gap = col_gap
          ),
          nlines(tl[i],
            colwidths = colwidths[1],
            fontspec = fontspec
          )
        )
      },
      1L
    )

    ## lfs <- collect_leaves(coltree(x))
    ## depths <- sapply(lfs, function(l) length(pos_splits(l)))

    coldf <- make_col_df(x, colwidths = colwidths)
    have_fnotes <- length(unlist(coldf$col_fnotes)) > 0
    ## ret <- max(depths, length(top_left(x))) +
    ##     divider_height(x)
    ret <- sum(main_nls, divider_height(x))
    if (have_fnotes) {
      ret <- sum(
        ret,
        vapply(unlist(coldf$col_fnotes),
          nlines,
          1,
          max_width = max_width,
          fontspec = fontspec
        ),
        2 * divider_height(x)
      )
    }
    ret
  }
)

col_dfrow <- function(col,
                      nm = obj_name(col),
                      lab = obj_label(col),
                      cnum,
                      pth = NULL,
                      sibpos = NA_integer_,
                      nsibs = NA_integer_,
                      leaf_indices = cnum,
                      span = length(leaf_indices),
                      col_fnotes = list(),
                      col_count = facet_colcount(col, NULL),
                      ccount_visible = disp_ccounts(col),
                      ccount_format = colcount_format(col),
                      ccount_na_str,
                      global_cc_format) {
  if (is.null(pth)) {
    pth <- pos_to_path(tree_pos(col))
  }
  data.frame(
    stringsAsFactors = FALSE,
    name = nm,
    label = lab,
    abs_pos = cnum,
    path = I(list(pth)),
    pos_in_siblings = sibpos,
    n_siblings = nsibs,
    leaf_indices = I(list(leaf_indices)),
    total_span = span,
    col_fnotes = I(list(col_fnotes)),
    n_col_fnotes = length(col_fnotes),
    col_count = col_count,
    ccount_visible = ccount_visible,
    ccount_format = ccount_format %||% global_cc_format,
    ccount_na_str = ccount_na_str
  )
}

pos_to_path <- function(pos) {
  spls <- pos_splits(pos)
  vals <- pos_splvals(pos)

  path <- character()
  for (i in seq_along(spls)) {
    nm <- obj_name(spls[[i]])
    val_i <- value_names(vals[[i]])
    path <- c(
      path,
      obj_name(spls[[i]]),
      ## rawvalues(vals[[i]]))
      if (!is.na(val_i)) val_i
    )
  }
  path
}


add_sect_div_path <- function(df, path) {
  df[["sect_div_from_path"]] <- I(list(path))
  df
}

# make_row_df ---------------------------------------------------------------

## We now extend the rowdf beyond what is (currently as of writing) defined in
## formatters with the following columns to support section divider shenanigans:
## - self_section_div - the section divider set on that exact object (row, or table if visible_only=FALSE)
## - sect_div_from_path - the path to the element `trailing_section_div` was
## inherited from (or NA_character_ for label rows which aren't pathable and
## can only receive section divs from themselves).
## This should probably be migrated down into formatters for the next release but
## its fine here for the rtables patch release because nothing in formatters
## (ie printing or pagination) needs or uses this new info.
## TODO: migrate above down to formatters

#' @inherit formatters::make_row_df
#'
# #' @note The technically present root tree node is excluded from the summary returned by both `make_row_df` and
# #'   `make_col_df`, as it is simply the row/column structure of `tt` and thus not useful for pathing or pagination.
# #'
# #' @return a data.frame of row/column-structure information used by the pagination machinery.
# #'
# #' @export
# #' @name make_row_df
# #' @rdname make_row_df
# #' @aliases make_row_df,VTableTree-method
#' @rdname formatters_methods
#' @exportMethod make_row_df
setMethod(
  "make_row_df", "VTableTree",
  function(tt,
           colwidths = NULL,
           visible_only = TRUE,
           rownum = 0,
           indent = 0L,
           path = character(),
           incontent = FALSE,
           repr_ext = 0L,
           repr_inds = integer(),
           sibpos = NA_integer_,
           nsibs = NA_integer_,
           max_width = NULL,
           fontspec = NULL,
           col_gap = 3) {
    
    new_dev <- open_font_dev(fontspec)
    if (new_dev) {
      on.exit(close_font_dev())
    }
    
    indent <- indent + indent_mod(tt)
    ## retained for debugging info
    orig_rownum <- rownum # nolint
    if (incontent) {
      path <- c(path, "@content")
    } else if (length(path) > 0 || nzchar(obj_name(tt))) { ## don't add "" for root
      ## else if (length(path) > 0 && nzchar(obj_name(tt))) ## don't add "" for root # nolint
      path <- c(path, obj_name(tt))
    }
    ret <- list()

    ## note this is the **table** not the label row
    if (!visible_only) {
      tabrdf <- pagdfrow(
        rnum = NA,
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
        nreflines = 0L,
        fontspec = fontspec,
        trailing_sep = trailing_section_div(tt)
      )
      tabrdf <- add_sect_div_path(tabrdf, path)
      tabrdf$self_section_div <- trailing_section_div(tt)
      ret <- c(
        ret,
        list(tabrdf)
      )
    }
    if (labelrow_visible(tt)) {
      lr <- tt_labelrow(tt)
      newdf <- make_row_df(lr,
        colwidths = colwidths,
        visible_only = visible_only,
        rownum = rownum,
        indent = indent,
        path = path,
        incontent = TRUE,
        repr_ext = repr_ext,
        repr_inds = repr_inds,
        max_width = max_width,
        fontspec = fontspec
      )
      rownum <- max(newdf$abs_rownumber, na.rm = TRUE)
      # newdf <- add_sect_div_path(newdf, NA_character_) ## label rows aren't pathable...

      ret <- c(
        ret,
        list(newdf)
      )
      repr_ext <- repr_ext + 1L
      repr_inds <- c(repr_inds, rownum)
      indent <- indent + 1L
    }

    if (NROW(content_table(tt)) > 0) {
      ct_tt <- content_table(tt)
      cind <- indent + indent_mod(ct_tt)
      ## this isn't right, we can display content and label rows at the
      ## same time (though by default we don't) and they could, in theory
      ## have different trailing section divs...
      ## trailing_section_div(ct_tt) <- trailing_section_div(tt_labelrow(tt))
      contdf <- make_row_df(ct_tt,
        colwidths = colwidths,
        visible_only = visible_only,
        rownum = rownum,
        indent = cind,
        path = path,
        incontent = TRUE,
        repr_ext = repr_ext,
        repr_inds = repr_inds,
        max_width = max_width,
        fontspec = fontspec
      )
      crnums <- contdf$abs_rownumber
      crnums <- crnums[!is.na(crnums)]

      newrownum <- max(crnums, na.rm = TRUE)
      if (is.finite(newrownum)) {
        rownum <- newrownum
        repr_ext <- repr_ext + length(crnums)
        repr_inds <- c(repr_inds, crnums)
      }
      ## if someone attached a trailing separator to a content table somehow
      ## weird but not /technically/ impossible, it overrides its last row's div,
      ## as always
      if (!is.na(trailing_section_div(ct_tt))) {
        contdf$trailing_section_div[nrow(contdf)] <- trailing_section_div(ct_tt)
        contdf$sect_div_from_path[[nrow(contdf)]] <- c(path, "@content")
      }
      ret <- c(ret, list(contdf))
      indent <- cind + 1
    }

    allkids <- tree_children(tt)
    newnsibs <- length(allkids)
    for (i in seq_along(allkids)) {
      kid <- allkids[[i]]
      kiddfs <- make_row_df(kid,
        colwidths = colwidths,
        visible_only = visible_only,
        rownum = force(rownum),
        indent = indent, ## + 1,
        path = path,
        incontent = incontent,
        repr_ext = repr_ext,
        repr_inds = repr_inds,
        nsibs = newnsibs,
        sibpos = i,
        max_width = max_width,
        fontspec = fontspec
      )

      #       print(kiddfs$abs_rownumber)
      rownum <- max(rownum + 1L, kiddfs$abs_rownumber, na.rm = TRUE)
      ret <- c(ret, list(kiddfs))
    }

    ret <- do.call(rbind, ret)

    ## Case where it has Elementary table or VTableTree section_div it is overridden
    ## precedence is least specific -> most specific, so the last row of any
    ## subtable is overridden by the subtable's div, we are calling make_row_df
    ## recursively so this achieves that even when levels of structure are skipped
    ## e.g. grandparent has a section div but section div doesn't (we now have
    ## a test for this)
    if (!is.na(trailing_section_div(tt))) {
      ret$trailing_sep[nrow(ret)] <- trailing_section_div(tt)
      ret$sect_div_from_path[[nrow(ret)]] <- path
    }
    ret
  }
)

# #' @exportMethod make_row_df
#' @inherit formatters::make_row_df
#'
#' @export
#' @rdname formatters_methods
setMethod(
  "make_row_df", "TableRow",
  function(tt, colwidths = NULL, visible_only = TRUE,
           rownum = 0,
           indent = 0L,
           path = "root",
           incontent = FALSE,
           repr_ext = 0L,
           repr_inds = integer(),
           sibpos = NA_integer_,
           nsibs = NA_integer_,
           max_width = NULL,
           fontspec,
           col_gap = 3) {
    indent <- indent + indent_mod(tt)
    rownum <- rownum + 1
    rrefs <- row_footnotes(tt)
    crefs <- cell_footnotes(tt)
    reflines <- sum(
      sapply(
        c(rrefs, crefs),
        nlines,
        colwidths = colwidths,
        max_width = max_width,
        fontspec = fontspec,
        col_gap = col_gap
      )
    ) ## col_gap not strictly necessary as these aren't rows, but why not
    self_path <- c(path, unname(obj_name(tt)))
    ret <- pagdfrow(
      row = tt,
      rnum = rownum,
      colwidths = colwidths,
      sibpos = sibpos,
      nsibs = nsibs,
      pth = self_path,
      repext = repr_ext,
      repind = repr_inds,
      indent = indent,
      extent = nlines(tt, colwidths = colwidths, max_width = max_width, fontspec = fontspec, col_gap = col_gap),
      ## these two are unlist calls cause they come in lists even with no footnotes
      nrowrefs = length(rrefs),
      ncellrefs = length(unlist(crefs)),
      nreflines = reflines,
      trailing_sep = trailing_section_div(tt),
      fontspec = fontspec
    )
    ret <- add_sect_div_path(ret, self_path)
    ret$self_section_div <- trailing_section_div(tt)
    ret
  }
)

# #' @exportMethod make_row_df
#' @export
#' @rdname formatters_methods
setMethod(
  "make_row_df", "LabelRow",
  function(tt, colwidths = NULL, visible_only = TRUE,
           rownum = 0,
           indent = 0L,
           path = "root",
           incontent = FALSE,
           repr_ext = 0L,
           repr_inds = integer(),
           sibpos = NA_integer_,
           nsibs = NA_integer_,
           max_width = NULL,
           fontspec,
           col_gap = 3) {
    rownum <- rownum + 1
    indent <- indent + indent_mod(tt)
    ret <- pagdfrow(tt,
      extent = nlines(tt,
        colwidths = colwidths,
        max_width = max_width,
        fontspec = fontspec,
        col_gap = col_gap
      ),
      rnum = rownum,
      colwidths = colwidths,
      sibpos = sibpos,
      nsibs = nsibs,
      pth = path,
      repext = repr_ext,
      repind = repr_inds,
      indent = indent,
      nrowrefs = length(row_footnotes(tt)),
      ncellrefs = 0L,
      nreflines = sum(vapply(row_footnotes(tt), nlines, NA_integer_,
        colwidths = colwidths,
        max_width = max_width,
        fontspec = fontspec,
        col_gap = col_gap
      )),
      trailing_sep = trailing_section_div(tt),
      fontspec = fontspec
    )
    ret <- add_sect_div_path(ret, NA_character_)
    ret$self_section_div <- trailing_section_div(tt)
    if (!labelrow_visible(tt)) {
      ret <- ret[0, , drop = FALSE]
    }
    ret
  }
)

setGeneric("inner_col_df", function(ct,
                                    colwidths = NULL,
                                    visible_only = TRUE,
                                    colnum = 0L,
                                    sibpos = NA_integer_,
                                    nsibs = NA_integer_,
                                    ncolref = 0L,
                                    na_str,
                                    global_cc_format) {
  standardGeneric("inner_col_df")
})

#' Column layout summary
#'
#' Used for pagination. Generate a structural summary of the columns of an `rtables` table and return it as a
#' `data.frame`.
#'
#' @inheritParams formatters::make_row_df
#' @param ccount_format (`FormatSpec`)\cr The format to be used by default for
#'   column counts if one is not specified for an individual column count.
#' @param na_str (`character(1)`)\cr The string to display when a column count is NA. Users should not need to set this.
#' @export
make_col_df <- function(tt,
                        colwidths = NULL,
                        visible_only = TRUE,
                        na_str = "",
                        ccount_format = colcount_format(tt) %||% "(N=xx)") {
  ctree <- coltree(tt, ccount_format = colcount_format(tt)) ## this is a null op if its already a coltree object
  rows <- inner_col_df(ctree,
    ## colwidths is currently unused anyway...  propose_column_widths(matrix_form(tt, indent_rownames=TRUE)),
    colwidths = colwidths,
    visible_only = visible_only,
    colnum = 1L,
    sibpos = 1L,
    nsibs = 1L,
    na_str = na_str,
    global_cc_format = ccount_format
  ) ## nsiblings includes current so 1 means "only child"

  do.call(rbind, rows)
}

setMethod(
  "inner_col_df", "LayoutColLeaf",
  function(ct, colwidths, visible_only,
           colnum,
           sibpos,
           nsibs,
           na_str,
           global_cc_format) {
    list(col_dfrow(
      col = ct,
      cnum = colnum,
      sibpos = sibpos,
      nsibs = nsibs,
      leaf_indices = colnum,
      col_fnotes = col_footnotes(ct),
      ccount_na_str = na_str,
      global_cc_format = global_cc_format
    ))
  }
)

setMethod(
  "inner_col_df", "LayoutColTree",
  function(ct, colwidths, visible_only,
           colnum,
           sibpos,
           nsibs,
           na_str,
           global_cc_format) {
    kids <- tree_children(ct)
    ret <- vector("list", length(kids))
    for (i in seq_along(kids)) {
      k <- kids[[i]]
      newrows <- do.call(
        rbind,
        inner_col_df(k,
          colnum = colnum,
          sibpos = i,
          nsibs = length(kids),
          visible_only = visible_only,
          na_str = na_str,
          global_cc_format = global_cc_format
        )
      )
      colnum <- max(newrows$abs_pos, colnum, na.rm = TRUE) + 1
      ret[[i]] <- newrows
    }

    if (!visible_only) {
      allindices <- unlist(lapply(ret, function(df) df$abs_pos[!is.na(df$abs_pos)]))
      thispth <- pos_to_path(tree_pos(ct))
      if (any(nzchar(thispth))) {
        thisone <- list(col_dfrow(
          col = ct,
          cnum = NA_integer_,
          leaf_indices = allindices,
          sibpos = sibpos,
          nsibs = nsibs,
          pth = thispth,
          col_fnotes = col_footnotes(ct),
          ccount_na_str = na_str,
          global_cc_format = global_cc_format
        ))
        ret <- c(thisone, ret)
      }
    }

    ret
  }
)

## THIS INCLUDES BOTH "table stub" (i.e. column label and top_left) AND
## title/subtitle!!!!!
.header_rep_nlines <- function(tt, colwidths, max_width, fontspec, verbose = FALSE) {
  cinfo_lines <- nlines(col_info(tt), colwidths = colwidths, max_width = max_width, fontspec = fontspec)
  if (any(nzchar(all_titles(tt)))) {
    ## +1 is for blank line between subtitles and divider
    tlines <- sum(nlines(all_titles(tt),
      colwidths = colwidths,
      max_width = max_width,
      fontspec = fontspec
    )) + divider_height(tt) + 1L
  } else {
    tlines <- 0
  }
  ret <- cinfo_lines + tlines
  if (verbose) {
    message(
      "Lines required for header content: ",
      ret, " (col info: ", cinfo_lines, ", titles: ", tlines, ")"
    )
  }
  ret
}

## this is ***only*** lines that are expected to be repeated on  multiple pages:
## main footer, prov footer, and referential footnotes on **columns**

.footer_rep_nlines <- function(tt, colwidths, max_width, have_cfnotes, fontspec, verbose = FALSE) {
  flines <- nlines(main_footer(tt),
    colwidths = colwidths,
    max_width = max_width - table_inset(tt),
    fontspec = fontspec
  ) +
    nlines(prov_footer(tt), colwidths = colwidths, max_width = max_width, fontspec = fontspec)
  if (flines > 0) {
    dl_contrib <- if (have_cfnotes) 0 else divider_height(tt)
    flines <- flines + dl_contrib + 1L
  }

  if (verbose) {
    message(
      "Determining lines required for footer content",
      if (have_cfnotes) " [column fnotes present]",
      ": ", flines, " lines"
    )
  }

  flines
}

# Pagination ---------------------------------------------------------------

#' Pagination of a `TableTree`
#'
#' Paginate an `rtables` table in the vertical and/or horizontal direction, as required for the specified page size.
#'
#' @inheritParams gen_args
#' @inheritParams paginate_table
#' @param lpp (`numeric(1)`)\cr maximum lines per page including (re)printed header and context rows.
#' @param min_siblings (`numeric(1)`)\cr minimum sibling rows which must appear on either side of pagination row for a
#'   mid-subtable split to be valid. Defaults to 2.
#' @param nosplitin (`character`)\cr names of sub-tables where page-breaks are not allowed, regardless of other
#'   considerations. Defaults to none.
#'
#' @return
#' * `pag_tt_indices` returns a list of paginated-groups of row-indices of `tt`.
#' * `paginate_table` returns the subtables defined by subsetting by the indices defined by `pag_tt_indices`.
#'
#' @details
#' `rtables` pagination is context aware, meaning that label rows and row-group summaries (content rows) are repeated
#' after (vertical) pagination, as appropriate. This allows the reader to immediately understand where they are in the
#' table after turning to a new page, but does also mean that a rendered, paginated table will take up more lines of
#' text than rendering the table without pagination would.
#'
#' Pagination also takes into account word-wrapping of title, footer, column-label, and formatted cell value content.
#'
#' Vertical pagination information (pagination `data.frame`) is created using (`make_row_df`).
#'
#' Horizontal pagination is performed by creating a pagination data frame for the columns, and then applying the same
#' algorithm used for vertical pagination to it.
#'
#' If physical page size and font information are specified, these are used to derive lines-per-page (`lpp`) and
#' characters-per-page (`cpp`) values.
#'
#' The full multi-direction pagination algorithm then is as follows:
#'
#' 0. Adjust `lpp` and `cpp` to account for rendered elements that are not rows (columns):
#'   - titles/footers/column labels, and horizontal dividers in the vertical pagination case
#'   - row-labels, table_inset, and top-left materials in the horizontal case
#' 1. Perform 'forced pagination' representing page-by row splits, generating 1 or more tables.
#' 2. Perform vertical pagination separately on each table generated in (1).
#' 3. Perform horizontal pagination **on the entire table** and apply the results to each table
#'    page generated in (1)-(2).
#' 4. Return a list of subtables representing full bi-directional pagination.
#'
#' Pagination in both directions is done using the *Core Pagination Algorithm* implemented in the `formatters` package:
#'
#' @inheritSection formatters::pagination_algo Pagination Algorithm
#'
#' @examples
#' s_summary <- function(x) {
#'   if (is.numeric(x)) {
#'     in_rows(
#'       "n" = rcell(sum(!is.na(x)), format = "xx"),
#'       "Mean (sd)" = rcell(c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
#'         format = "xx.xx (xx.xx)"
#'       ),
#'       "IQR" = rcell(IQR(x, na.rm = TRUE), format = "xx.xx"),
#'       "min - max" = rcell(range(x, na.rm = TRUE), format = "xx.xx - xx.xx")
#'     )
#'   } else if (is.factor(x)) {
#'     vs <- as.list(table(x))
#'     do.call(in_rows, lapply(vs, rcell, format = "xx"))
#'   } else {
#'     (
#'       stop("type not supported")
#'     )
#'   }
#' }
#'
#' lyt <- basic_table() %>%
#'   split_cols_by(var = "ARM") %>%
#'   analyze(c("AGE", "SEX", "BEP01FL", "BMRKR1", "BMRKR2", "COUNTRY"), afun = s_summary)
#'
#' tbl <- build_table(lyt, ex_adsl)
#' tbl
#'
#' nrow(tbl)
#'
#' row_paths_summary(tbl)
#'
#' tbls <- paginate_table(tbl, lpp = 15)
#' mf <- matrix_form(tbl, indent_rownames = TRUE)
#' w_tbls <- propose_column_widths(mf) # so that we have the same column widths
#'
#'
#' tmp <- lapply(tbls, function(tbli) {
#'   cat(toString(tbli, widths = w_tbls))
#'   cat("\n\n")
#'   cat("~~~~ PAGE BREAK ~~~~")
#'   cat("\n\n")
#' })
#'
#' @rdname paginate
#' @export
pag_tt_indices <- function(tt,
                           lpp = 15,
                           min_siblings = 2,
                           nosplitin = character(),
                           colwidths = NULL,
                           max_width = NULL,
                           fontspec = NULL,
                           col_gap = 3,
                           verbose = FALSE) {
  dheight <- divider_height(tt)

  #  cinfo_lines <- nlines(col_info(tt), colwidths = colwidths, max_width = max_width)
  coldf <- make_col_df(tt, colwidths)
  have_cfnotes <- length(unlist(coldf$col_fnotes)) > 0

  hlines <- .header_rep_nlines(tt,
    colwidths = colwidths, max_width = max_width,
    verbose = verbose,
    fontspec = fontspec
  )
  ## if(any(nzchar(all_titles(tt)))) {
  ##     tlines <- sum(nlines(all_titles(tt), colwidths = colwidths, max_width = max_width)) +
  ##       length(wrap_txt(all_titles(tt), max_width = max_width)) +
  ##         dheight + 1L
  ## } else {
  ##     tlines <- 0
  ## }
  ## flines <- nlines(main_footer(tt), colwidths = colwidths,
  ##                  max_width = max_width - table_inset(tt)) +
  ##     nlines(prov_footer(tt), colwidths = colwidths, max_width = max_width)
  ## if(flines > 0) {
  ##     dl_contrib <- if(have_cfnotes) 0 else dheight
  ##     flines <- flines + dl_contrib + 1L
  ## }
  flines <- .footer_rep_nlines(tt,
    colwidths = colwidths,
    max_width = max_width,
    have_cfnotes = have_cfnotes,
    fontspec = fontspec,
    verbose = verbose
  )
  ## row lines per page
  rlpp <- lpp - hlines - flines
  if (verbose) {
    message(
      "Adjusted Lines Per Page: ",
      rlpp, " (original lpp: ", lpp, ")"
    )
  }
  pagdf <- make_row_df(tt, colwidths, max_width = max_width)

  pag_indices_inner(pagdf,
    rlpp = rlpp, min_siblings = min_siblings,
    nosplitin = nosplitin,
    verbose = verbose,
    have_col_fnotes = have_cfnotes,
    div_height = dheight,
    col_gap = col_gap,
    has_rowlabels = TRUE
  )
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
  lapply(
    tree_children(tt),
    function(tbl) {
      tbl <- copy_title_footer(
        tbl, tt,
        paste(pref, obj_label(tbl), sep = ": ")
      )
      labelrow_visible(tbl) <- FALSE
      tbl
    }
  )
}

force_paginate <- function(tt,
                           force_pag = vapply(tree_children(tt), has_force_pag, NA),
                           verbose = FALSE) {
  ## forced pagination is happening at this
  if (has_force_pag(tt)) {
    ret <- pag_btw_kids(tt)
    return(unlist(lapply(ret, force_paginate)))
  }
  chunks <- list()
  kinds <- seq_along(force_pag)
  while (length(kinds) > 0) {
    if (force_pag[kinds[1]]) {
      outertbl <- copy_title_footer(
        tree_children(tt)[[kinds[1]]],
        tt,
        NULL
      )

      chunks <- c(chunks, force_paginate(outertbl))
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
  unlist(chunks, recursive = TRUE)
}

#' @importFrom formatters do_forced_paginate
setMethod(
  "do_forced_paginate", "VTableTree",
  function(obj) force_paginate(obj)
)

non_null_na <- function(x) !is.null(x) && is.na(x)

#' @inheritParams formatters::vert_pag_indices
#' @inheritParams formatters::page_lcpp
#' @inheritParams formatters::toString
#' @param cpp (`numeric(1)` or `NULL`)\cr width (in characters) of the pages for horizontal pagination.
#'   `NA` (the default) indicates `cpp` should be inferred from the page size; `NULL` indicates no horizontal
#'   pagination should be done regardless of page size.
#'
#' @rdname paginate
#' @aliases paginate_table
#' @export
paginate_table <- function(tt,
                           page_type = "letter",
                           font_family = "Courier",
                           font_size = 8,
                           lineheight = 1,
                           landscape = FALSE,
                           pg_width = NULL,
                           pg_height = NULL,
                           margins = c(top = .5, bottom = .5, left = .75, right = .75),
                           lpp = NA_integer_,
                           cpp = NA_integer_,
                           min_siblings = 2,
                           nosplitin = character(),
                           colwidths = NULL,
                           tf_wrap = FALSE,
                           max_width = NULL,
                           fontspec = font_spec(font_family, font_size, lineheight),
                           col_gap = 3,
                           verbose = FALSE) {
  new_dev <- open_font_dev(fontspec)
  if (new_dev) {
    on.exit(close_font_dev())
  }

  if ((non_null_na(lpp) || non_null_na(cpp)) &&
    (!is.null(page_type) || (!is.null(pg_width) && !is.null(pg_height)))) { # nolint
    pg_lcpp <- page_lcpp(
      page_type = page_type,
      font_family = font_family,
      font_size = font_size,
      lineheight = lineheight,
      pg_width = pg_width,
      pg_height = pg_height,
      margins = margins,
      landscape = landscape,
      fontspec = fontspec
    )

    if (non_null_na(lpp)) {
      lpp <- pg_lcpp$lpp
    }
    if (is.na(cpp)) {
      cpp <- pg_lcpp$cpp
    }
  } else {
    if (non_null_na(cpp)) {
      cpp <- NULL
    }
    if (non_null_na(lpp)) {
      lpp <- 70
    }
  }

  if (is.null(colwidths)) {
    colwidths <- propose_column_widths(
      matrix_form(
        tt,
        indent_rownames = TRUE,
        fontspec = fontspec,
        col_gap = col_gap
      ),
      fontspec = fontspec
    )
  }

  if (!tf_wrap) {
    if (!is.null(max_width)) {
      warning("tf_wrap is FALSE - ignoring non-null max_width value.")
    }
    max_width <- NULL
  } else if (is.null(max_width)) {
    max_width <- cpp
  } else if (identical(max_width, "auto")) {
    ## XXX this 3 is column sep width!!!!!!!
    max_width <- sum(colwidths) + col_gap * (length(colwidths) - 1)
  }
  if (!is.null(cpp) && !is.null(max_width) && max_width > cpp) {
    warning("max_width specified is wider than characters per page width (cpp).")
  }

  ## taken care of in vert_pag_indices now
  ## if(!is.null(cpp))
  ##     cpp <- cpp - table_inset(tt)

  force_pag <- vapply(tree_children(tt), has_force_pag, TRUE)
  if (has_force_pag(tt) || any(force_pag)) {
    spltabs <- do_forced_paginate(tt)
    spltabs <- unlist(spltabs, recursive = TRUE)
    ret <- lapply(spltabs, paginate_table,
      lpp = lpp,
      cpp = cpp,
      min_siblings = min_siblings,
      nosplitin = nosplitin,
      colwidths = colwidths,
      tf_wrap = tf_wrap,
      max_width = max_width,
      fontspec = fontspec,
      verbose = verbose,
      col_gap = col_gap
    )
    return(unlist(ret, recursive = TRUE))
  }

  inds <- paginate_indices(tt,
    page_type = page_type,
    fontspec = fontspec,
    ## font_family = font_family,
    ## font_size = font_size,
    ## lineheight = lineheight,
    landscape = landscape,
    pg_width = pg_width,
    pg_height = pg_height,
    margins = margins,
    lpp = lpp,
    cpp = cpp,
    min_siblings = min_siblings,
    nosplitin = nosplitin,
    colwidths = colwidths,
    tf_wrap = tf_wrap,
    max_width = max_width,
    col_gap = col_gap,
    verbose = verbose
  ) ## paginate_table apparently doesn't accept indent_size

  res <- lapply(
    inds$pag_row_indices,
    function(ii) {
      subt <- tt[ii, drop = FALSE, keep_titles = TRUE, keep_topleft = TRUE, reindex_refs = FALSE]
      lapply(
        inds$pag_col_indices,
        function(jj) {
          subt[, jj, drop = FALSE, keep_titles = TRUE, keep_topleft = TRUE, reindex_refs = FALSE]
        }
      )
    }
  )
  res <- unlist(res, recursive = FALSE)
  res
}
