#' @import formatters
#' @importMethodsFrom formatters toString matrix_form nlines
NULL

# toString ----

## #' @export
## setGeneric("toString", function(x,...) standardGeneric("toString"))

## ## preserve S3 behavior
## setMethod("toString", "ANY", base::toString)

## #' @export
## setMethod("print", "ANY", base::print)

#' Convert an `rtable` object to a string
#'
#' @inheritParams formatters::toString
#' @inheritParams gen_args
#' @inherit formatters::toString
#'
#' @return A string representation of `x` as it appears when printed.
#'
#' @examplesIf require(dplyr)
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary), format = "xx.xx")
#'
#' tbl <- build_table(lyt, iris2)
#'
#' cat(toString(tbl, col_gap = 3))
#'
#' @rdname tostring
#' @aliases tostring toString,VTableTree-method
#' @exportMethod toString
setMethod("toString", "VTableTree", function(x,
                                             widths = NULL,
                                             col_gap = 3,
                                             hsep = horizontal_sep(x),
                                             indent_size = 2,
                                             tf_wrap = FALSE,
                                             max_width = NULL,
                                             fontspec = font_spec(),
                                             ttype_ok = FALSE,
                                             round_type = obj_round_type(x)) {
  toString(
    matrix_form(x,
      indent_rownames = TRUE,
      indent_size = indent_size,
      fontspec = fontspec,
      col_gap = col_gap,
      round_type = round_type
    ),
    widths = widths, col_gap = col_gap,
    hsep = hsep,
    tf_wrap = tf_wrap,
    max_width = max_width,
    fontspec = fontspec,
    ttype_ok = ttype_ok
  )
})

#' Table shells
#'
#' A table shell is a rendering of the table which maintains the structure, but does not display the values, rather
#' displaying the formatting instructions for each cell.
#'
#' @inheritParams formatters::toString
#' @inheritParams gen_args
#'
#' @return
#' * `table_shell` returns `NULL`, as the function is called for the side effect of printing the shell to the console.
#' * `table_shell_str` returns the string representing the table shell.
#'
#' @seealso [value_formats()] for a matrix of formats for each cell in a table.
#'
#' @examplesIf require(dplyr)
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary), format = "xx.xx")
#'
#' tbl <- build_table(lyt, iris2)
#' table_shell(tbl)
#'
#' @export
table_shell <- function(tt, widths = NULL, col_gap = 3, hsep = default_hsep(),
                        tf_wrap = FALSE, max_width = NULL) {
  cat(table_shell_str(
    tt = tt, widths = widths, col_gap = col_gap, hsep = hsep,
    tf_wrap = tf_wrap, max_width = max_width
  ))
}

## XXX consider moving to formatters, its really just a function
## of the MatrixPrintForm
#' @rdname table_shell
#' @export
table_shell_str <- function(tt, widths = NULL, col_gap = 3, hsep = default_hsep(),
                            tf_wrap = FALSE, max_width = NULL) {
  matform <- matrix_form(tt, indent_rownames = TRUE)
  format_strs <- vapply(
    as.vector(matform$formats),
    function(x) {
      if (inherits(x, "function")) {
        "<fnc>"
      } else if (inherits(x, "character")) {
        x
      } else {
        stop("Don't know how to make a shell with formats of class: ", class(x))
      }
    }, ""
  )

  format_strs_mat <- matrix(format_strs, ncol = ncol(matform$strings))
  format_strs_mat[, 1] <- matform$strings[, 1]
  nlh <- mf_nlheader(matform)
  format_strs_mat[seq_len(nlh), ] <- matform$strings[seq_len(nlh), ]

  matform$strings <- format_strs_mat
  if (is.null(widths)) {
    widths <- propose_column_widths(matform)
  }
  toString(matform,
    widths = widths, col_gap = col_gap, hsep = hsep,
    tf_wrap = tf_wrap, max_width = max_width
  )
}

#' Transform an `rtable` to a list of matrices which can be used for outputting
#'
#' Although `rtables` are represented as a tree data structure when outputting the table to ASCII or HTML
#' it is useful to map the `rtable` to an in-between state with the formatted cells in a matrix form.
#'
#' @inheritParams gen_args
#' @inheritParams formatters::format_value
#' @param indent_rownames (`flag`)\cr if `TRUE`, the column with the row names in the `strings` matrix of the output
#'   has indented row names (strings pre-fixed).
#' @param expand_newlines (`flag`)\cr whether the matrix form generated should expand rows whose values contain
#'   newlines into multiple 'physical' rows (as they will appear when rendered into ASCII). Defaults to `TRUE`.
#' @param fontspec (`font_spec`)\cr The font that should be used by default when
#'   rendering this `MatrixPrintForm` object, or NULL (the default).
#' @param col_gap (`numeric(1)`)]\cr The number of spaces (in the font specified
#'  by `fontspec`) that should be placed between columns when the table
#'  is rendered directly to text (e.g., by `toString` or `export_as_txt`). Defaults
#'  to `3`.
#'
#' @details
#' The strings in the return object are defined as follows: row labels are those determined by `make_row_df` and cell
#' values are determined using `get_formatted_cells`. (Column labels are calculated using a non-exported internal
#' function.
#'
#' @return A list with the following elements:
#'   \describe{
#'     \item{`strings`}{The content, as it should be printed, of the top-left material, column headers, row labels,
#'       and cell values of `tt`.}
#'     \item{`spans`}{The column-span information for each print-string in the `strings` matrix.}
#'     \item{`aligns`}{The text alignment for each print-string in the `strings` matrix.}
#'     \item{`display`}{Whether each print-string in the strings matrix should be printed.}
#'     \item{`row_info`}{The `data.frame` generated by `make_row_df`.}
#'   }
#'
#' With an additional `nrow_header` attribute indicating the number of pseudo "rows" that the column structure defines.
#'
#' @examplesIf require(dplyr)
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"),
#'     afun = list_wrap_x(summary), format = "xx.xx"
#'   )
#'
#' lyt
#'
#' tbl <- build_table(lyt, iris2)
#'
#' matrix_form(tbl)
#'
#' @export
setMethod(
  "matrix_form", "VTableTree",
  function(obj,
           indent_rownames = FALSE,
           expand_newlines = TRUE,
           indent_size = 2,
           fontspec = NULL,
           col_gap = 3L,
           round_type = obj_round_type(obj)) {
    
    new_dev <- open_font_dev(fontspec)
    if (new_dev) {
      on.exit(close_font_dev())
    }

    stopifnot(is(obj, "VTableTree"))
    check_ccount_vis_ok(obj)
    header_content <- .tbl_header_mat(obj) # first col are for row.names

    sr <- make_row_df(obj, fontspec = fontspec)

    body_content_strings <- if (NROW(sr) == 0) {
      character()
    } else {
      cbind(as.character(sr$label), get_formatted_cells(obj, round_type = round_type))
    }

    formats_strings <- if (NROW(sr) == 0) {
      character()
    } else {
      cbind("", get_formatted_cells(obj, shell = TRUE))
    }

    tsptmp <- lapply(collect_leaves(obj, TRUE, TRUE), function(rr) {
      sp <- row_cspans(rr)
      rep(sp, times = sp)
    })

    ## the 1 is for row labels
    body_spans <- if (nrow(obj) > 0) {
      cbind(1L, do.call(rbind, tsptmp))
    } else {
      matrix(1, nrow = 0, ncol = ncol(obj) + 1)
    }

    body_aligns <- if (NROW(sr) == 0) {
      character()
    } else {
      cbind("left", get_cell_aligns(obj))
    }

    body <- rbind(header_content$body, body_content_strings)

    hdr_fmt_blank <- matrix("",
      nrow = nrow(header_content$body),
      ncol = ncol(header_content$body)
    )
    if (disp_ccounts(obj)) {
      hdr_fmt_blank[nrow(hdr_fmt_blank), ] <- c("", rep(colcount_format(obj), ncol(obj)))
    }

    formats <- rbind(hdr_fmt_blank, formats_strings)

    spans <- rbind(header_content$span, body_spans)
    row.names(spans) <- NULL

    aligns <- rbind(
      matrix(rep("center", length(header_content$body)),
        nrow = nrow(header_content$body)
      ),
      body_aligns
    )

    aligns[, 1] <- "left" # row names and topleft (still needed for topleft)

    nr_header <- nrow(header_content$body)
    if (indent_rownames) {
      body[, 1] <- indent_string(body[, 1], c(rep(0, nr_header), sr$indent),
        incr = indent_size
      )
      # why also formats?
      formats[, 1] <- indent_string(formats[, 1], c(rep(0, nr_header), sr$indent),
        incr = indent_size
      )
    } else if (NROW(sr) > 0) {
      sr$indent <- rep(0, NROW(sr))
    }

    col_ref_strs <- matrix(vapply(header_content$footnotes, function(x) {
      if (length(x) == 0) {
        ""
      } else {
        paste(vapply(x, format_fnote_ref, ""), collapse = " ")
      }
    }, ""), ncol = ncol(body))
    body_ref_strs <- get_ref_matrix(obj)

    body <- matrix(
      paste0(
        body,
        rbind(
          col_ref_strs,
          body_ref_strs
        )
      ),
      nrow = nrow(body),
      ncol = ncol(body)
    )

    ref_fnotes <- get_formatted_fnotes(obj) # pagination will not count extra lines coming from here
    pag_titles <- page_titles(obj)

    MatrixPrintForm(
      strings = body,
      spans = spans,
      aligns = aligns,
      formats = formats,
      ## display = display, purely a function of spans, handled in constructor now
      row_info = sr,
      colpaths = make_col_df(obj)[["path"]],
      ## line_grouping handled internally now line_grouping = 1:nrow(body),
      ref_fnotes = ref_fnotes,
      nlines_header = nr_header, ## this is fixed internally
      nrow_header = nr_header,
      expand_newlines = expand_newlines,
      has_rowlabs = TRUE,
      has_topleft = TRUE,
      main_title = main_title(obj),
      subtitles = subtitles(obj),
      page_titles = pag_titles,
      main_footer = main_footer(obj),
      prov_footer = prov_footer(obj),
      table_inset = table_inset(obj),
      header_section_div = header_section_div(obj),
      horizontal_sep = horizontal_sep(obj),
      indent_size = indent_size,
      fontspec = fontspec,
      col_gap = col_gap,
      round_type = round_type
    )
  }
)


check_ccount_vis_ok <- function(tt) {
  ctree <- coltree(tt)
  tlkids <- tree_children(ctree)
  lapply(tlkids, ccvis_check_subtree)
  invisible(NULL)
}

ccvis_check_subtree <- function(ctree) {
  kids <- tree_children(ctree)
  if (is.null(kids)) {
    return(invisible(NULL))
  }
  vals <- vapply(kids, disp_ccounts, TRUE)
  if (length(unique(vals)) > 1) {
    unmatch <- which(!duplicated(vals))[1:2]
    stop(
      "Detected different colcount visibility among sibling facets (those ",
      "arising from the same split_cols_by* layout instruction). This is ",
      "not supported.\n",
      "Set count values to NA if you want a blank space to appear as the ",
      "displayed count for particular facets.\n",
      "First disagreement occured at paths:\n",
      .path_to_disp(pos_to_path(tree_pos(kids[[unmatch[1]]]))), "\n",
      .path_to_disp(pos_to_path(tree_pos(kids[[unmatch[2]]])))
    )
  }
  lapply(kids, ccvis_check_subtree)
  invisible(NULL)
}

.resolve_fn_symbol <- function(fn) {
  if (!is(fn, "RefFootnote")) {
    return(NULL)
  }
  ret <- ref_symbol(fn)
  if (is.na(ret)) {
    ret <- as.character(ref_index(fn))
  }
  ret
}

format_fnote_ref <- function(fn) {
  if (length(fn) == 0 || (is.list(fn) && all(vapply(fn, function(x) length(x) == 0, TRUE)))) {
    return("")
  } else if (is.list(fn) && all(vapply(fn, is.list, TRUE))) {
    return(vapply(fn, format_fnote_ref, ""))
  }
  if (is.list(fn)) {
    inds <- unlist(lapply(unlist(fn), .resolve_fn_symbol))
  } else {
    inds <- .resolve_fn_symbol(fn)
  }
  if (length(inds) > 0) {
    paste0(" {", paste(unique(inds), collapse = ", "), "}")
  } else {
    ""
  }
}

format_fnote_note <- function(fn) {
  if (length(fn) == 0 || (is.list(fn) && all(vapply(fn, function(x) length(x) == 0, TRUE)))) {
    return(character())
  }
  if (is.list(fn)) {
    return(unlist(lapply(unlist(fn), format_fnote_note)))
  }

  if (is(fn, "RefFootnote")) {
    paste0("{", .resolve_fn_symbol(fn), "} - ", ref_msg(fn))
  } else {
    NULL
  }
}

.fn_ind_extractor <- function(strs) {
  res <- suppressWarnings(as.numeric(gsub("\\{([[:digit:]]+)\\}.*", "\\1", strs)))
  res[res == "NA"] <- NA_character_
  ## these mixing is allowed now with symbols
  ## if(!(sum(is.na(res)) %in% c(0L, length(res))))
  ##     stop("Got NAs mixed with non-NAS for extracted footnote indices. This should not happen")
  res
}

get_ref_matrix <- function(tt) {
  if (ncol(tt) == 0 || nrow(tt) == 0) {
    return(matrix("", nrow = nrow(tt), ncol = ncol(tt) + 1L))
  }
  rows <- collect_leaves(tt, incl.cont = TRUE, add.labrows = TRUE)
  lst <- unlist(lapply(rows, cell_footnotes), recursive = FALSE)
  cstrs <- unlist(lapply(lst, format_fnote_ref))
  bodymat <- matrix(cstrs,
    byrow = TRUE,
    nrow = nrow(tt),
    ncol = ncol(tt)
  )
  cbind(vapply(rows, function(rw) format_fnote_ref(row_footnotes(rw)), ""), bodymat)
}

get_formatted_fnotes <- function(tt) {
  colresfs <- unlist(make_col_df(tt, visible_only = FALSE)$col_fnotes)
  rows <- collect_leaves(tt, incl.cont = TRUE, add.labrows = TRUE)
  lst <- c(
    colresfs,
    unlist(
      lapply(rows, function(r) unlist(c(row_footnotes(r), cell_footnotes(r)), recursive = FALSE)),
      recursive = FALSE
    )
  )

  inds <- vapply(lst, ref_index, 1L)
  ord <- order(inds)
  lst <- lst[ord]
  syms <- vapply(lst, ref_symbol, "")
  keep <- is.na(syms) | !duplicated(syms)
  lst <- lst[keep]
  unique(vapply(lst, format_fnote_note, ""))

  ##                    , recursive = FALSE)
  ## rlst <- unlist(lapply(rows, row_footnotes))
  ## lst <-
  ## syms <- vapply(lst, ref_symbol, "")
  ## keep <- is.na(syms) | !duplicated(syms)
  ## lst <- lst[keep]
  ## inds <- vapply(lst, ref_index, 1L)
  ## cellstrs <- unlist(lapply(lst, format_fnote_note))
  ## rstrs <- unlist(lapply(rows, function(rw) format_fnote_note(row_footnotes(rw))))
  ## allstrs <- c(colstrs, rstrs, cellstrs)
  ## inds <- .fn_ind_extractor(allstrs)
  ## allstrs[order(inds)]
}

.do_tbl_h_piece2 <- function(tt) {
  coldf <- make_col_df(tt, visible_only = FALSE)
  remain <- seq_len(nrow(coldf))
  chunks <- list()
  cur <- 1
  na_str <- colcount_na_str(tt)

  ## XXX this would be better as the facet-associated
  ## format but I don't know that we need to
  ## support that level of differentiation anyway...
  cc_format <- colcount_format(tt)
  ## each iteration of this loop identifies
  ## all rows corresponding to one top-level column
  ## label and its children, then processes those
  ## with .do_header_chunk
  while (length(remain) > 0) {
    rw <- remain[1]
    inds <- coldf$leaf_indices[[rw]]
    endblock <- which(coldf$abs_pos == max(inds))

    stopifnot(endblock >= rw)
    chunk_res <- .do_header_chunk(coldf[rw:endblock, ], cc_format, na_str = na_str)
    chunk_res <- unlist(chunk_res, recursive = FALSE)
    chunks[[cur]] <- chunk_res
    remain <- remain[remain > endblock]
    cur <- cur + 1
  }
  chunks <- .pad_tops(chunks)
  lapply(
    seq_len(length(chunks[[1]])),
    function(i) {
      DataRow(unlist(lapply(chunks, `[[`, i), recursive = FALSE))
    }
  )
}

.pad_end <- function(lst, padto, ncols) {
  curcov <- sum(vapply(lst, cell_cspan, 0L))
  if (curcov == padto) {
    return(lst)
  }

  c(lst, list(rcell("", colspan = padto - curcov)))
}

.pad_tops <- function(chunks) {
  lens <- vapply(chunks, length, 1L)
  padto <- max(lens)
  needpad <- lens != padto
  if (all(!needpad)) {
    return(chunks)
  }

  for (i in seq_along(lens)) {
    if (lens[i] < padto) {
      chk <- chunks[[i]]
      span <- sum(vapply(chk[[length(chk)]], cell_cspan, 1L))
      chunks[[i]] <- c(
        replicate(list(list(rcell("", colspan = span))),
          n = padto - lens[i]
        ),
        chk
      )
    }
  }
  chunks
}

.do_header_chunk <- function(coldf, cc_format, na_str) {
  ## hard assumption that coldf is a section
  ## of a column dataframe summary that was
  ## created with visible_only=FALSE
  nleafcols <- length(coldf$leaf_indices[[1]])

  spldfs <- split(coldf, lengths(coldf$path))
  toret <- lapply(
    seq_along(spldfs),
    function(i) {
      rws <- spldfs[[i]]
      thisbit_vals <- lapply(
        seq_len(nrow(rws)),
        function(ri) {
          cellii <- rcell(rws[ri, "label", drop = TRUE],
            colspan = rws$total_span[ri],
            footnotes = rws[ri, "col_fnotes", drop = TRUE][[1]]
          )
          cellii
        }
      )
      ret <- list(.pad_end(thisbit_vals, padto = nleafcols))
      anycounts <- any(rws$ccount_visible)
      if (anycounts) {
        thisbit_ns <- lapply(
          seq_len(nrow(rws)),
          function(ri) {
            vis_ri <- rws$ccount_visible[ri]
            val <- if (vis_ri) rws$col_count[ri] else NULL
            fmt <- rws$ccount_format[ri]
            if (is.character(fmt)) {
              cfmt_dim <- names(which(sapply(formatters::list_valid_format_labels(), function(x) any(x == fmt))))
              if (cfmt_dim == "2d") {
                if (grepl("%", fmt)) {
                  val <- c(val, 1) ## XXX This is the old behavior but it doesn't take into account parent counts...
                } else {
                  stop(
                    "This 2d format is not supported for column counts. ",
                    "Please choose a 1d format or a 2d format that includes a % value."
                  )
                }
              } else if (cfmt_dim == "3d") {
                stop("3d formats are not supported for column counts.")
              }
            }
            cellii <- rcell(
              val,
              colspan = rws$total_span[ri],
              format = fmt, # cc_format,
              format_na_str = na_str
            )
            cellii
          }
        )
        ret <- c(ret, list(.pad_end(thisbit_ns, padto = nleafcols)))
      }
      ret
    }
  )
  toret
}

.tbl_header_mat <- function(tt) {
  rows <- .do_tbl_h_piece2(tt) ## (clyt)
  cinfo <- col_info(tt)

  nc <- ncol(tt)
  body <- matrix(rapply(rows, function(x) {
    cs <- row_cspans(x)
    strs <- get_formatted_cells(x)
    strs
  }), ncol = nc, byrow = TRUE)

  span <- matrix(rapply(rows, function(x) {
    cs <- row_cspans(x)
    if (is.null(cs)) cs <- rep(1, ncol(x))
    rep(cs, cs)
  }), ncol = nc, byrow = TRUE)

  fnote <- do.call(
    rbind,
    lapply(rows, function(x) {
      cell_footnotes(x)
    })
  )

  tl <- top_left(cinfo)
  lentl <- length(tl)
  nli <- nrow(body)
  if (lentl == 0) {
    tl <- rep("", nli)
  } else if (lentl > nli) {
    tl_tmp <- paste0(tl, collapse = "\n")
    tl <- rep("", nli)
    tl[length(tl)] <- tl_tmp
  } else if (lentl < nli) {
    # We want topleft alignment that goes to the bottom!
    tl <- c(rep("", nli - lentl), tl)
  }
  list(
    body = cbind(tl, body, deparse.level = 0), span = cbind(1, span),
    footnotes = cbind(list(list()), fnote)
  )
}

# get formatted cells ----

#' Get formatted cells
#'
#' @inheritParams gen_args
#' @inheritParams formatters::format_value
#' @param shell (`flag`)\cr whether the formats themselves should be returned instead of the values with formats
#'   applied. Defaults to `FALSE`.
#'
#' @return The formatted print-strings for all (body) cells in `obj`.
#'
#' @examplesIf require(dplyr)
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
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary), format = "xx.xx") %>%
#'   build_table(iris2)
#'
#' get_formatted_cells(tbl)
#'
#' @export
#' @rdname gfc
setGeneric(
  "get_formatted_cells",
  function(obj, shell = FALSE, round_type = valid_round_type) standardGeneric("get_formatted_cells")
)

#' @rdname gfc
setMethod(
  "get_formatted_cells", "TableTree",
  function(obj, shell = FALSE, round_type = obj_round_type(obj)) {
    lr <- get_formatted_cells(tt_labelrow(obj), shell = shell, round_type = round_type)

    ct <- get_formatted_cells(content_table(obj), shell = shell, round_type = round_type)

    els <- lapply(tree_children(obj), get_formatted_cells, shell = shell, round_type = round_type)

    ## TODO fix ncol problem for rrow()
    if (ncol(ct) == 0 && ncol(lr) != ncol(ct)) {
      ct <- lr[NULL, ]
    }

    do.call(rbind, c(list(lr), list(ct), els))
  }
)

#' @rdname gfc
setMethod(
  "get_formatted_cells", "ElementaryTable",
  function(obj, shell = FALSE, round_type = obj_round_type(obj)) {
    lr <- get_formatted_cells(tt_labelrow(obj), shell = shell, round_type = round_type)
    els <- lapply(tree_children(obj), get_formatted_cells, shell = shell, round_type = round_type)
    do.call(rbind, c(list(lr), els))
  }
)

#' @rdname gfc
setMethod(
  "get_formatted_cells", "TableRow",
  function(obj, shell = FALSE, round_type = obj_round_type(obj)) {
    # Parent row format and na_str
    pr_row_format <- if (is.null(obj_format(obj))) "xx" else obj_format(obj)
    pr_row_na_str <- obj_na_str(obj) %||% "NA"

    matrix(
      unlist(Map(function(val, spn, shelli) {
        stopifnot(is(spn, "integer"))

        out <- format_rcell(val,
          pr_row_format = pr_row_format,
          pr_row_na_str = pr_row_na_str,
          shell = shelli,
          round_type = round_type
        )
        if (!is.function(out) && is.character(out)) {
          out <- paste(out, collapse = ", ")
        }

        rep(list(out), spn)
      }, val = row_cells(obj), spn = row_cspans(obj), shelli = shell)),
      ncol = ncol(obj)
    )
  }
)

#' @rdname gfc
setMethod(
  "get_formatted_cells", "LabelRow",
  function(obj, shell = FALSE, round_type = valid_round_type) {
    round_type <- match.arg(round_type)
    nc <- ncol(obj) # TODO note rrow() or rrow("label") has the wrong ncol
    vstr <- if (shell) "-" else ""
    if (labelrow_visible(obj)) {
      matrix(rep(vstr, nc), ncol = nc)
    } else {
      matrix(character(0), ncol = nc)
    }
  }
)

#' @rdname gfc
setGeneric("get_cell_aligns", function(obj) standardGeneric("get_cell_aligns"))

#' @rdname gfc
setMethod(
  "get_cell_aligns", "TableTree",
  function(obj) {
    lr <- get_cell_aligns(tt_labelrow(obj))

    ct <- get_cell_aligns(content_table(obj))

    els <- lapply(tree_children(obj), get_cell_aligns)

    ## TODO fix ncol problem for rrow()
    if (ncol(ct) == 0 && ncol(lr) != ncol(ct)) {
      ct <- lr[NULL, ]
    }

    do.call(rbind, c(list(lr), list(ct), els))
  }
)

#' @rdname gfc
setMethod(
  "get_cell_aligns", "ElementaryTable",
  function(obj) {
    lr <- get_cell_aligns(tt_labelrow(obj))
    els <- lapply(tree_children(obj), get_cell_aligns)
    do.call(rbind, c(list(lr), els))
  }
)

#' @rdname gfc
setMethod(
  "get_cell_aligns", "TableRow",
  function(obj) {
    als <- vapply(row_cells(obj), cell_align, "")
    spns <- row_cspans(obj)

    matrix(rep(als, times = spns),
      ncol = ncol(obj)
    )
  }
)

#' @rdname gfc
setMethod(
  "get_cell_aligns", "LabelRow",
  function(obj) {
    nc <- ncol(obj) # TODO note rrow() or rrow("label") has the wrong ncol
    if (labelrow_visible(obj)) {
      matrix(rep("center", nc), ncol = nc)
    } else {
      matrix(character(0), ncol = nc)
    }
  }
)

# utility functions ----

#' From a sorted sequence of numbers, remove numbers where diff == 1
#'
#' @examples
#' remove_consecutive_numbers(x = c(2, 4, 9))
#' remove_consecutive_numbers(x = c(2, 4, 5, 9))
#' remove_consecutive_numbers(x = c(2, 4, 5, 6, 9))
#' remove_consecutive_numbers(x = 4:9)
#'
#' @noRd
remove_consecutive_numbers <- function(x) {
  # actually should be integer
  stopifnot(is.wholenumber(x), is.numeric(x), !is.unsorted(x))

  if (length(x) == 0) {
    return(integer(0))
  }
  if (!is.integer(x)) x <- as.integer(x)

  x[c(TRUE, diff(x) != 1)]
}

#' Insert an empty string
#'
#' @examples
#' empty_string_after(letters[1:5], 2)
#' empty_string_after(letters[1:5], c(2, 4))
#'
#' @noRd
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

#' Indent strings
#'
#' Used in rtables to indent row names for the ASCII output.
#'
#' @param x (`character`)\cr a character vector.
#' @param indent (`numeric`)\cr a vector of non-negative integers of length `length(x)`.
#' @param incr (`integer(1)`)\cr a non-negative number of spaces per indent level.
#' @param including_newline (`flag`)\cr whether newlines should also be indented.
#'
#' @return `x`, indented with left-padding with `indent * incr` white-spaces.
#'
#' @examples
#' indent_string("a", 0)
#' indent_string("a", 1)
#' indent_string(letters[1:3], 0:2)
#' indent_string(paste0(letters[1:3], "\n", LETTERS[1:3]), 0:2)
#'
#' @export
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

## .paste_no_na <- function(x, ...) {
##   paste(na.omit(x), ...)
## }

## #' Pad a string and align within string
## #'
## #' @param x string
## #' @param n number of character of the output string, if `n < nchar(x)` an error is thrown
## #'
## #' @noRd
## #'
## #' @examples
## #'
## #' padstr("abc", 3)
## #' padstr("abc", 4)
## #' padstr("abc", 5)
## #' padstr("abc", 5, "left")
## #' padstr("abc", 5, "right")
## #'
## #' if(interactive()){
## #' padstr("abc", 1)
## #' }
## #'
## padstr <- function(x, n, just = c("center", "left", "right")) {

##   just <- match.arg(just)

##   if (length(x) != 1) stop("length of x needs to be 1 and not", length(x))
##   if (is.na(n) || !is.numeric(n) || n < 0) stop("n needs to be numeric and > 0")

##   if (is.na(x)) x <- "<NA>"

##   nc <- nchar(x)

##   if (n < nc) stop("\"", x, "\" has more than ", n, " characters")

##   switch(
##     just,
##     center = {
##       pad <- (n - nc)/2
##       paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
##     },
##     left = paste0(x, spaces(n - nc)),
##     right = paste0(spaces(n - nc), x)
##   )
## }

## spaces <- function(n) {
##   strrep(" ", n)
## }

#' Convert matrix of strings into a string with aligned columns
#'
#' Note that this function is intended to print simple rectangular matrices and not `rtable`s.
#'
#' @param mat (`matrix`)\cr a matrix of strings.
#' @param nheader (`integer(1)`)\cr number of header rows.
#' @param colsep (`string`)\cr a string that separates the columns.
#' @param hsep (`character(1)`)\cr character to build line separator.
#'
#' @return A string.
#'
#' @examples
#' mat <- matrix(c("A", "B", "C", "a", "b", "c"), nrow = 2, byrow = TRUE)
#' cat(mat_as_string(mat))
#' cat("\n")
#'
#' @noRd
mat_as_string <- function(mat, nheader = 1, colsep = "    ", hsep = default_hsep()) {
  colwidths <- apply(apply(mat, c(1, 2), nchar), 2, max)

  rows_formatted <- apply(mat, 1, function(row) {
    paste(unlist(mapply(padstr, row, colwidths, "left")), collapse = colsep)
  })

  header_rows <- seq_len(nheader)
  nchwidth <- nchar(rows_formatted[1])
  paste(c(
    rows_formatted[header_rows],
    substr(strrep(hsep, nchwidth), 1, nchwidth),
    rows_formatted[-header_rows]
  ), collapse = "\n")
}
