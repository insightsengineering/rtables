
# toString ----

#' @export
setGeneric("toString", function(x,...) standardGeneric("toString"))

## preserve S3 behavior
setMethod("toString", "ANY", base::toString)

#' @export
setMethod("print", "ANY", base::print)


#' Convert an `rtable` object to a string
#'
#'
#' @param x table object
#' @param widths widths of row.name and columns columns
#' @param col_gap gap between columns
#' @exportMethod toString
#'
#' @return a string representation of \code{x} as it appears when printed.
#'
#' @examples
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' l <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#'
#' tbl <- build_table(l, iris2)
#'
#' cat(toString(tbl, col_gap = 3))
#' @rdname tostring
setMethod("toString", "VTableTree", function(x,
                                             widths = NULL,
                                             col_gap = 3) {
    toString(matrix_form(x, indent_rownames = TRUE),
             widths = widths, col_gap = col_gap)
})

#' @rdname tostring
#' @exportMethod toString
setMethod("toString", "MatrixPrintForm", function(x,
                                                  widths = NULL,
                                                  col_gap = 3) {
    mat <- x

  ## we create a matrix with the formatted cell contents
##  mat <- matrix_form(x, indent_rownames = TRUE)


  if (is.null(widths)) {
    widths <- propose_column_widths(mat_form = mat)
  }
  stopifnot(length(widths) == ncol(mat$strings))

  # format the to ASCII
  body <- mat$strings
  aligns <- mat$aligns
  keep_mat <- mat$display
  spans <- mat$spans
#    ri <- mat$row_info
    ref_fnotes <- mat$ref_footnotes


  # xxxx <- 1
  # # get rowgap position
  # insert_gap_before <- which(ri$depth == sort(ri$depth)[seq_len(xxxx)])
  # # which(ri$rowtype == "LabelRow" & ri$depth == min(ri$depth))
  # insert_gap_before <- remove_consecutive_numbers(insert_gap_before)
  # insert_gap_before <- insert_gap_before[insert_gap_before != 1]

  nr <- nrow(body)
  nl_header <- attr(mat, "nlines_header")

  cell_widths_mat <- matrix(rep(widths, nr), nrow = nr, byrow = TRUE)
  nc <- ncol(cell_widths_mat)

  for (i in seq_len(nrow(body))) {
    if (any(!keep_mat[i, ])) { # any spans?
      j <- 1
      while (j <= nc) {
        nj <- spans[i, j]
        j <- if (nj > 1) {
          js <- seq(j, j + nj - 1)
          cell_widths_mat[i, js] <- sum(cell_widths_mat[i, js]) + col_gap * (nj - 1)
          j + nj
        } else {
          j + 1
        }
      }
    }
  }

  # Are the total characters per row the same?
  # A <- cell_widths_mat
  # A[!keep_mat] <- 0
  # apply(A, 1, sum)

  content <- matrix(mapply(padstr, body, cell_widths_mat, aligns), ncol = ncol(body))
  content[!keep_mat] <- NA
  # apply(content, 1, function(x) sum(nchar(x), na.rm = TRUE))

  gap_str <- strrep(" ", col_gap)

  ncchar <-  sum(widths) + (length(widths) - 1) * col_gap
  div <- substr(strrep(linesep, ncchar), 1, ncchar)

  txt_head <- apply(head(content, nl_header), 1, .paste_no_na, collapse = gap_str)
  txt_body <- apply(tail(content, -nl_header), 1, .paste_no_na, collapse = gap_str)

  allts <- all_titles(x)
  titles_txt <- if(any(nzchar(allts))) c(allts, "", div)  else NULL
    ## TODO make titles affect width...

    allfoots <- all_footers(x)


    footer_txt <- c(if(length(ref_fnotes) > 0) c(div, "", ref_fnotes),
                    if(any(nzchar(allfoots))) c(div, "", allfoots))
  paste0(paste(c(titles_txt, txt_head, div, txt_body, footer_txt), collapse = "\n"), "\n")

})

pad_vert_center <- function(x, len) {
    needed <- len - length(x)
    if(needed < 0) stop("got vector already longer than target length this shouldn't happen")
    if(needed > 0) {
        bf <- ceiling(needed/2)
        af <- needed - bf
        x <- c(if(bf > 0) rep("", bf), x, if(af > 0) rep("", af))
    }
    x
}

pad_vert_top <- function(x, len) {
    c(x, rep("", len - length(x)))
}

pad_vert_bottom <- function(x, len) {
    c(rep("", len - length(x)), x)
}

pad_vec_to_len <- function(vec, len, cpadder = pad_vert_top, rlpadder = cpadder) {
    dat <- unlist(lapply(vec[-1], cpadder, len = len))
    dat <- c(rlpadder(vec[[1]], len = len), dat)
    matrix(dat,  nrow = len)
}

rep_vec_to_len <- function(vec, len, ...) {
    matrix(unlist(lapply(vec, rep, times = len)),
           nrow = len)
}


safe_strsplit <- function(x, split, ...) {
    ret <- strsplit(x, split, ...)
    lapply(ret, function(reti) if(length(reti) == 0) "" else reti)
}

.expand_mat_rows_inner <- function(i, mat, row_nlines, expfun, ...) {
    leni <- row_nlines[i]
    rw <- mat[i,]
    if(is.character(rw))
        rw <- safe_strsplit(rw, "\n", fixed = TRUE)
    expfun(rw, len = leni, ...)
}

expand_mat_rows <- function(mat, row_nlines = apply(mat, 1, nlines), expfun = pad_vec_to_len, ...) {

    rinds <- 1:nrow(mat)
    exprows <- lapply(rinds, .expand_mat_rows_inner,
                      mat = mat,
                      row_nlines = row_nlines,
                      expfun = expfun,
                      ...)
    do.call(rbind, exprows)

}



spans_to_viscell <- function(spans) {
    if(!is.vector(spans))
        spans <- as.vector(spans)
    myrle <- rle(spans)
    unlist(mapply(function(vl, ln) rep(c(TRUE, rep(FALSE, vl - 1L)),
                                                                  times = ln/vl),
                                             SIMPLIFY = FALSE,
                                             vl = myrle$values,
                                             ln = myrle$lengths),
                                      recursive = FALSE)
}



#' Transform rtable to a list of matrices which can be used for outputting
#'
#' Although rtables are represented as a tree data structure when outputting the table to ASCII or HTML it is useful to
#' map the rtable to an in between state with the formatted cells in a matrix form.
#'
#' @inheritParams gen_args
#' @param indent_rownames logical(1), if TRUE the column with the row names in the `strings` matrix of has indented row
#'   names (strings pre-fixed)
#'
#' @export
#'
#' @details
#'
#' The strings in the return object are defined as follows: row labels are those determined by \code{summarize_rows} and cell values are determined using \code{get_formatted_cells}. (Column labels are calculated using a non-exported internal funciton.
#'
#'@return A list with the following elements:
#' \describe{
#' \item{strings}{The content, as it should be printed, of the top-left material, column headers, row labels , and cell values of \code{tt}}
#' \item{spans}{The column-span information for each print-string in the strings matrix}
#' \item{aligns}{The text alignment for each print-string in the strings matrix}
#' \item{display}{Whether each print-string in the strings matrix should be printed or not}.
#' \item{row_info}{the data.frame generated by \code{summarize_rows(tt)}}
#' }
#'
#' With an additional \code{nrow_header} attribute indicating the number of pseudo "rows"  the
#' column structure defines.
#' @examples
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' l <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#'
#' l
#'
#' tbl <- build_table(l, iris2)
#'
#' matrix_form(tbl)
matrix_form <- function(tt, indent_rownames = FALSE) {

  stopifnot(is(tt, "VTableTree"))

  header_content <- .tbl_header_mat(tt) # first col are for row.names

    ##sr <- summarize_rows(tt)
    sr <- make_row_df(tt)

  body_content_strings <- if (NROW(sr) == 0) {
    character()
  } else {
    cbind(as.character(sr$label), get_formatted_cells(tt))
  }

  tsptmp <- lapply(collect_leaves(tt, TRUE, TRUE), function(rr) {
    sp <- row_cspans(rr)
    rep(sp, times = sp)
  })

  ## the 1 is for row labels
  body_spans <- if (nrow(tt) > 0) {
    cbind(1L, do.call(rbind, tsptmp))
  } else {
    matrix(1, nrow = 0, ncol = ncol(tt) + 1)
  }

  body <- rbind(header_content$body, body_content_strings)
  spans <- rbind(header_content$span, body_spans)
  row.names(spans) <- NULL

  space <- matrix(rep(0, length(body)), nrow = nrow(body))
  aligns <- matrix(rep("center", length(body)), nrow = nrow(body))
  aligns[, 1] <- "left" # row names

  ## if (any(apply(body, c(1, 2), function(x) grepl("\n", x, fixed = TRUE))))
  ##   stop("no \\n allowed at the moment")

  display <- matrix(rep(TRUE, length(body)), ncol = ncol(body))

    print_cells_mat <- spans == 1L
    if(!all(print_cells_mat)) {
        display_rws  <- lapply(seq_len(nrow(spans)),
                               function(i) {
            print_cells <- print_cells_mat[i,]
            row <- spans[i,]
            ##         display <- t(apply(spans, 1, function(row) {
            ## print_cells <- row == 1

            if (!all(print_cells)) {
                ## need to calculate which cell need to be printed
                myrle <- rle(row)
                print_cells <- spans_to_viscell(row) ## unlist(mapply(function(vl, ln) rep(c(TRUE, rep(FALSE, vl - 1L)),
                                      ##                             times = ln/vl),
                                      ##        SIMPLIFY = FALSE,
                                      ##        vl = myrle$values,
                                      ##        ln = myrle$lengths),
                                      ## recursive = FALSE)

                ## tmp <- 0
                ## for (i in seq_along(print_cells)) {
                ##   if (!print_cells[i] && tmp <= 0) {
                ##     print_cells[i] <- TRUE
                ##     tmp <- row[i] - 1
                ##   } else {
                ##     tmp <- tmp - 1
                ##   }
                ## }
            }
            print_cells
        })
        display <- do.call(rbind, display_rws)
    }


    nr_header <- nrow(header_content$body)
    if (indent_rownames) {
        body[, 1] <- indent_string(body[, 1], c(rep(0, nr_header), sr$indent))
    }

    col_ref_strs <- matrix(vapply(header_content$footnotes, function(x) {
        if(length(x) == 0)
            ""
        else
            paste(vapply(x, format_fnote_ref, ""), collapse = " ")
    }, ""), ncol = ncol(body))
    body_ref_strs <- get_ref_matrix(tt)

    body <- matrix(paste0(body,
                         rbind(col_ref_strs,
                               body_ref_strs)),
                   nrow = nrow(body),
                   ncol = ncol(body))

  ##   row_nlines <- apply(body, 1, nlines)
  ##   nrows <- nrow(body)
  ##   if (any(row_nlines > 1)) {
  ##       hdr_inds <- 1:nr_header
  ##       ## groundwork for sad haxx to get tl to not be messed up
  ##       tl <- body[hdr_inds, 1]
  ##       body <- rbind(expand_mat_rows(body[hdr_inds, , drop = FALSE], row_nlines[hdr_inds], cpadder = pad_vert_bottom),
  ##                     expand_mat_rows(body[-1*hdr_inds,, drop = FALSE], row_nlines[-hdr_inds]))
  ##       spans <- expand_mat_rows(spans, row_nlines, rep_vec_to_len)
  ##       aligns <- expand_mat_rows(aligns, row_nlines, rep_vec_to_len)
  ##       display <- expand_mat_rows(display, row_nlines, rep_vec_to_len)
  ##       nlines_header <- sum(row_nlines[1:nr_header])
  ##       ## sad haxx :(
  ## ##      if(length(tl) != nr_header) {
  ##       body[1:nr_header,1] <- c(tl, rep("", nr_header - length(tl)))

  ##   } else {
  ##       nlines_header <- nr_header
  ##   }

    ref_fnotes <- get_formatted_fnotes(tt)
  ret <- structure(
    list(
      strings = body,
      spans = spans,
      aligns = aligns,
      display = display,
      row_info = sr,
      line_grouping = 1:nrow(body), # this is done for real in .do_mat_expand now
      ref_footnotes = ref_fnotes
    ),
    nlines_header = nr_header, ## this is done for real in .do_mat_expand nownlines_header,
    nrow_header = nr_header,
    class = c("MatrixPrintForm", "list"))
    .do_mat_expand(ret)
}


.do_mat_expand <- function(matform, has_topleft = TRUE) {

    row_nlines <- apply(matform$strings, 1, nlines)
    nlines_header <- attr(matform, "nlines_header")
    nr_header <- attr(matform, "nrow_header")
    nrows <- nrow(matform$strings)
    if (any(row_nlines > 1)) {
        hdr_inds <- 1:nr_header
        ## groundwork for sad haxx to get tl to not be messed up
        if(has_topleft)
            tl <- matform$strings[hdr_inds, 1]
        else
            tl <- character()
        matform$strings <- rbind(expand_mat_rows(matform$strings[hdr_inds, , drop = FALSE], row_nlines[hdr_inds], cpadder = pad_vert_bottom),
                      expand_mat_rows(matform$strings[-1*hdr_inds,, drop = FALSE], row_nlines[-hdr_inds]))
        matform$spans <- expand_mat_rows(matform$spans, row_nlines, rep_vec_to_len)
        matform$aligns <- expand_mat_rows(matform$aligns, row_nlines, rep_vec_to_len)
        matform$display <- expand_mat_rows(matform$display, row_nlines, rep_vec_to_len)
        attr(matform, "nlines_header") <- sum(row_nlines[1:nr_header])
        ## sad haxx :(
  ##      if(length(tl) != nr_header) {
        matform$strings[1:nr_header,1] <- c(tl, rep("", nr_header - length(tl)))
        matform$line_grouping <- rep(1:nrows, times = row_nlines)

    }

    matform
}





format_fnote_ref <- function(fn) {
    if(length(fn) == 0 || (is.list(fn) && all(vapply(fn, function(x) length(x) == 0, TRUE))))
        return("")
    else if(is.list(fn) && all(vapply(fn, is.list, TRUE)))
        return(vapply(fn, format_fnote_ref, ""))
    if(is.list(fn)) {
        inds <- unlist(lapply(unlist(fn), function(x) if(is(x, "RefFootnote")) x@index else NULL))
    } else {
        inds <- fn@index
    }
    if(length(inds) > 0) {
        paste0(" {", paste(inds, collapse = ", "), "}")
    } else {
        ""
    }
}


format_fnote_note <- function(fn) {
    if(length(fn) == 0 || (is.list(fn) && all(vapply(fn, function(x) length(x) == 0, TRUE))))
        return(character())
    if(is.list(fn)) {
        return(unlist(lapply(unlist(fn), format_fnote_note)))
    }

    if(is(fn, "RefFootnote")) {
        paste0("{", fn@index, "} - ", fn@value)
    } else {
        NULL
    }
}

.fn_ind_extractor <- function(strs) {
    res <- suppressWarnings(as.numeric(gsub("\\{([[:digit:]]+)\\}.*", "\\1", strs)))
    if(!(sum(is.na(res)) %in% c(0L, length(res))))
        stop("Got NAs mixed with non-NAS for extracted footnote indices. This should not happen")
    res
}

.colref_mat_helper <- function(vals, span) {
        val <- paste(lapply(vals, format_fnote_ref), collapse = " ")
        if(length(val) == 0)
            val <- ""
        rep(val, times = span)
}

get_colref_matrix <- function(tt) {
    cdf <- make_col_df(tt, visible_only=FALSE)
    objs <- cdf$col_fnotes
    spans <- cdf$total_span
    vals <- mapply(.colref_mat_helper,
                   vals = objs,
                   span = spans)
    vals
}

get_ref_matrix <- function(tt) {
    if(ncol(tt) == 0 || nrow(tt) == 0) {
        return(matrix("", nrow = nrow(tt), ncol = ncol(tt) + 1L))
    }
    rows <- collect_leaves(tt, incl.cont = TRUE, add.labrows = TRUE)
    lst <- unlist(lapply(rows, cell_footnotes), recursive = FALSE)
    cstrs <- unlist(lapply(lst, format_fnote_ref))
    bodymat <- matrix(cstrs,
                      byrow = TRUE,
                      nrow = nrow(tt),
                      ncol = ncol(tt))
    cbind(vapply(rows, function(rw) format_fnote_ref(row_footnotes(rw)), ""), bodymat)
}

get_formatted_fnotes <- function(tt) {
    colresfs <- unlist(make_col_df(tt, visible_only = FALSE)$col_fnotes)
    colstrs <- unlist(lapply(colresfs, format_fnote_note))
    rows <- collect_leaves(tt, incl.cont = TRUE, add.labrows = TRUE)
    lst <- unlist(lapply(rows, cell_footnotes), recursive = FALSE)
    cellstrs <- unlist(lapply(lst, format_fnote_note))
    rstrs <- unlist(lapply(rows, function(rw) format_fnote_note(row_footnotes(rw))))
    allstrs <- c(colstrs, rstrs, cellstrs)
    inds <- .fn_ind_extractor(allstrs)
    allstrs[order(inds)]
}



## print depths (not to be confused with tree depths)
.cleaf_depths <- function(ctree = coltree(cinfo), depth = 1, cinfo) {
    if(is(ctree, "LayoutColLeaf"))
        return(depth)
    unlist(lapply(tree_children(ctree), .cleaf_depths, depth = depth + 1))
}


## .do_tbl_h_piece <- function(ct, padding = 0, span = 1) {

##     if(is(ct, "LayoutColLeaf")) {
##       ##   padcells <- if(padding > 0) list(rep(list(rcell("", colspan = 1)), padding))

##         return(c(list(rcell(obj_label(ct), colspan = 1))))
##     }


##     nleafs <- length(collect_leaves(ct))
##     padcells <- if(padding > 0) list(rep(list(rcell("", colspan = 1)), padding))

##     kids <- tree_children(ct)
##     cdepths <- vapply(kids, function(k) max(.cleaf_depths(k)), 1)
##     pieces <- mapply(.do_tbl_h_piece,
##                      ct = kids, padding = max(cdepths) - cdepths,
##                      SIMPLIFY= FALSE)
##     ## listpieces <- vapply(pieces, function(x) {
##     ##     any(vapply(x, function(y) is.list(y) && !is(y, "CellValue"), NA))
##     ##     }, NA)
##     ## if(any(listpieces))
##     ##     pieces[listpieces] <- lapply(pieces[listpieces], unlist)
##     lpieces <- vapply(pieces, length, 1L)

##     nmcell <- list(rcell(obj_label(ct), colspan = nleafs))

##     stopifnot(length(unique(lpieces)) == 1)
##     rowparts <- lapply(1:max(lpieces),
##                        function(i) {
##         res = lapply(pieces, `[[`, i = i)
##         if(!are(res, "CellValue"))
##             res = unlist(res, recursive = FALSE)
##         res
##     })


##     c(padcells,
##       nmcell,
##       rowparts)
## }

.do_tbl_h_piece2 <- function(tt) {
    coldf <- make_col_df(tt, visible_only = FALSE)
    remain <- seq_len(nrow(coldf))
    chunks <- list()
    cur <- 1
    retmat <- NULL

    ## each iteration of this loop identifies
    ## all rows corresponding to one top-level column
    ## label and its children, then processes those
    ## with .do_header_chunk
    while(length(remain) > 0) {
        rw <- remain[1]
        inds <- coldf$leaf_indices[[rw]]
        endblock <- which(coldf$abs_pos == max(inds))

        stopifnot(endblock >= rw)
        chunks[[cur]] <- .do_header_chunk(coldf[rw:endblock,])
        remain <- remain[remain > endblock]
        cur <- cur + 1
    }
    chunks <- .pad_tops(chunks)
    lapply(seq_len(length(chunks[[1]])),
           function(i) {
        DataRow(unlist(lapply(chunks, `[[`, i), recursive = FALSE))
    })
}
.pad_end <- function(lst, padto, ncols) {
    curcov <- sum(vapply(lst, cell_cspan, 0L))
    if(curcov == padto)
        return(lst)

    c(lst, list(rcell("", colspan = padto - curcov)))
}


.pad_tops <- function(chunks) {
    lens <- vapply(chunks, length, 1L)
    padto <- max(lens)
    needpad <- lens != padto
    if(all(!needpad))
        return(chunks)

    chunks[needpad] <- lapply(chunks[needpad],
                              function(chk) {
        span <- sum(vapply(chk[[length(chk)]], cell_cspan, 1L))
        needed <- padto - length(chk)
        c(replicate(rcell("", colspan = span),
                    n = needed),
          chk)
    })
    chunks

}

.do_header_chunk <- function(coldf) {
    ## hard assumption that coldf is a section
    ## of a column dataframe summary that was
    ## created with visible_only=FALSE
    nleafcols <- length(coldf$leaf_indices[[1]])

    spldfs <- split(coldf, lengths(coldf$path))
    toret <- lapply(seq_along(spldfs),
                    function(i) {
        rws <- spldfs[[i]]

        thisbit <- lapply(seq_len(nrow(rws)),
                          function(ri) {
            rcell(rws[ri, "label", drop = TRUE], colspan = rws$total_span[ri],
                  footnotes = rws[ri, "col_fnotes", drop = TRUE][[1]])
        })
        .pad_end(thisbit, nleafcols)
    })

    toret
}



.tbl_header_mat <- function(tt) {

    clyt <- coltree(tt)
    rows <- .do_tbl_h_piece2(tt) ##(clyt)
    cinfo <- col_info(tt)

    nc <- ncol(tt)
    body <- matrix(rapply(rows, function(x) {
        cs <- row_cspans(x)
        if (is.null(cs)) cs <- rep(1, ncol(x))
        rep(row_values(x), cs)
    }), ncol = nc, byrow = TRUE)

    span <- matrix(rapply(rows, function(x) {
        cs <- row_cspans(x)
        if (is.null(cs)) cs <- rep(1, ncol(x))
        rep(cs, cs)
    }), ncol = nc, byrow = TRUE)

    fnote <- do.call(rbind,
                           lapply(rows, function(x) {
                              cell_footnotes(x)
                           }))



    if (disp_ccounts(cinfo)) {
        counts <- col_counts(cinfo)
        cformat <- colcount_format(cinfo)
        body <- rbind(body, vapply(counts, format_rcell, character(1), cformat))
        span <- rbind(span, rep(1, nc))
        fnote <- rbind(fnote, rep(list(list()), nc))
    }

    tl <- top_left(cinfo)
    lentl <- length(tl)
    nli <- nrow(body)
    if(lentl == 0)
        tl <- rep("", nli)
    else if(lentl > nli) {
        npad <- lentl - nli
        body <- rbind(matrix("", nrow = npad, ncol = ncol(body)), body)
        span <- rbind(matrix(1, nrow = npad, ncol = ncol(span)), span)
        fnote <- rbind(matrix(list(), nrow = npad, ncol = ncol(body)), fnote)
    } else if (lentl < nli)
        tl <- c(tl, rep("", nli - lentl))

    list(body = cbind(tl, body, deparse.level = 0), span = cbind(1, span),
         footnotes = cbind(list(list()), fnote))
}



# get formatted cells ----

#' get formatted cells
#'
#' @return the formatted print-strings for all (body) cells in \code{obj}.
#' @export
#' @inheritParams gen_args
#' @examples
#'
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' tbl <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx") %>%
#'   build_table(iris2)
#'
#' get_formatted_cells(tbl)
#' @rdname gfc
setGeneric("get_formatted_cells", function(obj) standardGeneric("get_formatted_cells"))
#' @rdname gfc
setMethod("get_formatted_cells", "TableTree",
          function(obj) {

    lr <- get_formatted_cells(tt_labelrow(obj))

    ct <- get_formatted_cells(content_table(obj))

    els <- lapply(tree_children(obj), get_formatted_cells)

    ## TODO fix ncol problem for rrow()
    if (ncol(ct) == 0 && ncol(lr) != ncol(ct)) {
        ct <- lr[NULL, ]
    }

    do.call(rbind, c(list(lr), list(ct),  els))
})

#' @rdname gfc
setMethod("get_formatted_cells", "ElementaryTable",
          function(obj) {
    lr <- get_formatted_cells(tt_labelrow(obj))
    els <- lapply(tree_children(obj), get_formatted_cells)
    do.call(rbind, c(list(lr), els))
})

#' @rdname gfc
setMethod("get_formatted_cells", "TableRow",
          function(obj) {
            default_format <- if (is.null(obj_format(obj))) "xx" else obj_format(obj)
            format <- lapply(row_cells(obj), function(x) {
                format <- obj_format(x)
                if (is.null(format))
                    default_format
                else
                    format
            })

            matrix(unlist(Map(function(val, format, spn) {
                stopifnot(is(spn, "integer"))
                rep(paste(format_rcell(val, format), collapse = ", "), spn)
            }, row_values(obj), format, row_cspans(obj))), ncol = ncol(obj))
})

#' @rdname gfc
setMethod("get_formatted_cells", "LabelRow",
          function(obj) {
    nc <- ncol(obj) # TODO note rrow() or rrow("label") has the wrong ncol
    if (labelrow_visible(obj)) {
        matrix(rep("", nc), ncol = nc)
    } else {
              matrix(character(0), ncol = nc)
    }
})


#' Propose Column Widths of an `rtable` object
#'
#' The row names are also considered a column for the output
#'
#' @param x `rtable` object
#' @param mat_form object as created with `matrix_form`
#'
#' @export
#' @return a vector of column widths based on the content of \code{x} (or \code{mat_form} if explictly provided)
#' for use in printing and, in the future, in pagination.
#' @examples
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' l <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#'
#' tbl <- build_table(l, iris2)
#'
#' propose_column_widths(tbl)
propose_column_widths <- function(x, mat_form = matrix_form(x, indent_rownames = TRUE)) {

    ##stopifnot(is(x, "VTableTree"))

  body <- mat_form$strings
  spans <- mat_form$spans
  aligns <- mat_form$aligns
  display <- mat_form$display

  chars <- nchar(body)

  # first check column widths without colspan
  has_spans <- spans != 1
  chars_ns <- chars
  chars_ns[has_spans] <- 0
  widths <- apply(chars_ns, 2, max)

  # now check if the colspans require extra width
  if (any(has_spans)) {
    has_row_spans <- apply(has_spans, 1, any)

    chars_sp <- chars[has_row_spans, , drop = FALSE]
    spans_sp <- spans[has_row_spans, , drop = FALSE]
    disp_sp <- display[has_row_spans, , drop = FALSE]

    nc <- ncol(spans)
    for (i in seq_len(nrow(chars_sp))) {
      for (j in seq_len(nc)) {
        if (disp_sp[i, j] && spans_sp[i, j] != 1) {
          i_cols <- seq(j, j + spans_sp[i, j] - 1)

          nchar_i <- chars_sp[i, j]
          cw_i <- widths[i_cols]
          available_width <- sum(cw_i)

          if (nchar_i > available_width) {
            # need to update widths to fit content with colspans
            # spread width among columns
            widths[i_cols] <- cw_i + spread_integer(nchar_i - available_width, length(cw_i))
          }
        }
      }
    }
  }
  widths
}

# utility functions ----

#' from sequence remove numbers where diff == 1
#'
#' numbers need to be sorted
#'
#' @noRd
#'
#' @examples
#' remove_consecutive_numbers(x = c(2, 4, 9))
#' remove_consecutive_numbers(x = c(2, 4, 5, 9))
#' remove_consecutive_numbers(x = c(2, 4, 5, 6, 9))
#' remove_consecutive_numbers(x = 4:9)
remove_consecutive_numbers <- function(x) {

  # actually should be integer
  stopifnot(is.wholenumber(x), is.numeric(x), !is.unsorted(x))

  if (length(x) == 0) return(integer(0))
  if (!is.integer(x)) x <- as.integer(x)

  sel <- rep(TRUE, length(x))

  x[c(TRUE, diff(x)  != 1)]
}


#' insert an empty string
#'
#' @noRd
#'
#' @examples
#' empty_string_after(letters[1:5], 2)
#' empty_string_after(letters[1:5], c(2, 4))
empty_string_after <- function(x, indices) {

  if (length(indices) > 0) {
    offset <- 0
    for (i in sort(indices)) {
      x <- append(x, "", i + offset)
      offset <- offset + 1
    }
  }
  x
}

#' spread x into len elements
#'
#' @noRd
#'
#' @examples
#' spread_integer(3, 1)
#' spread_integer(0, 3)
#' spread_integer(1, 3)
#' spread_integer(2, 3)
#' spread_integer(3, 3)
#' spread_integer(4, 3)
#' spread_integer(5, 3)
#' spread_integer(6, 3)
#' spread_integer(7, 3)
spread_integer <- function(x, len) {
  stopifnot(
    is.wholenumber(x), length(x) == 1, x >= 0,
    is.wholenumber(len), length(len) == 1, len >= 0,
    !(len == 0 && x > 0)
  )


  if (len == 0) {
    integer(0)
  } else {
    y <- rep(floor(x/len), len)
    i <- 1
    while (sum(y) < x) {
      y[i] <- y[i] + 1
      if (i == len) {
        i <- 1
      } else {
        i <- i + 1
      }
    }
    y
  }
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

#' Indent Strings
#'
#' Used in rtables to indent row names for the ASCII output.
#'
#' @param x a character vector
#' @param indent a vector of length \code{length(x)} with non-negative integers
#' @param incr non-negative integer: number of spaces per indent level
#' @param including_newline boolean: should newlines also be indented
#'
#' @export
#' @return \code{x} indented by left-padding with code{indent*incr} white-spaces.
#' @examples
#' indent_string("a", 0)
#' indent_string("a", 1)
#' indent_string(letters[1:3], 0:2)
#' indent_string(paste0(letters[1:3], "\n", LETTERS[1:3]), 0:2)
#'
indent_string <- function(x, indent = 0, incr = 2, including_newline = TRUE) {

  if (length(x) > 0) {
    indent <- rep_len(indent, length.out = length(x))
    incr <- rep_len(incr, length.out = length(x))
  }

  indent_str <- strrep(" ", (indent > 0) * indent * incr)

  if (including_newline) {
    x <- unlist(mapply(function(xi, stri) {
      gsub("\n", stri, xi, fixed = TRUE)
    }, x, paste0("\n", indent_str)))
  }

  paste0(indent_str, x)
}

.paste_no_na <- function(x, ...) {
  paste(na.omit(x), ...)
}


#' Pad a string and align within string
#'
#' @param x string
#' @param n number of character of the output string, if `n < nchar(x)` an error is thrown
#'
#' @noRd
#'
#' @examples
#'
#' padstr("abc", 3)
#' padstr("abc", 4)
#' padstr("abc", 5)
#' padstr("abc", 5, "left")
#' padstr("abc", 5, "right")
#'
#' if(interactive()){
#' padstr("abc", 1)
#' }
#'
padstr <- function(x, n, just = c("center", "left", "right")) {

  just <- match.arg(just)

  if (length(x) != 1) stop("length of x needs to be 1 and not", length(x))
  if (is.na(n) || !is.numeric(n) || n < 0) stop("n needs to be numeric and > 0")

  if (is.na(x)) x <- "<NA>"

  nc <- nchar(x)

  if (n < nc) stop("\"", x, "\" has more than ", n, " characters")

  switch(
    just,
    center = {
      pad <- (n - nc)/2
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    left = paste0(x, spaces(n - nc)),
    right = paste0(spaces(n - nc), x)
  )
}

spaces <- function(n) {
  strrep(" ", n)
}


#' Convert Matrix of Strings into a String with Aligned Columns
#'
#' Note that this function is intended to print simple rectangular
#' matrices and not rtables.
#'
#' @param mat a matrix of strings
#' @param nheader number of header rows
#' @param colsep string that separates the columns
#' @param linesep character to build line separator
#' 
#' @noRd
#'
#' @return a string
#'
#' @examples
#'
#' mat <- matrix(c("A", "B", "C", "a", "b", "c"), nrow = 2, byrow = TRUE)
#' cat(rtables:::mat_as_string(mat)); cat("\n")
mat_as_string <- function(mat, nheader = 1, colsep = "    ", linesep = "\u2014") {
  colwidths <- apply(apply(mat, c(1, 2), nchar), 2, max)

  rows_formatted <- apply(mat, 1, function(row) {
    paste(unlist(mapply(padstr, row, colwidths, "left")), collapse = colsep)
  })

  header_rows <- seq_len(nheader)
  paste(c(rows_formatted[header_rows], 
          substr(strrep(linesep, nchar(rows_formatted[1])), 1, nchar(rows_formatted[1])),
          rows_formatted[-header_rows]), collapse = "\n")
}

