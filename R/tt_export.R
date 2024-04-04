#' @importFrom tools file_ext
NULL

#' Create enriched flat value table with paths
#'
#' This function creates a flat tabular file of cell values and corresponding paths via [path_enriched_df()]. It then
#' writes that data frame out as a `tsv` file.
#'
#' By default (i.e. when `value_func` is not specified, list columns where at least one value has length > 1 are
#' collapsed to character vectors by collapsing the list element with `"|"`.
#'
#' @note
#' There is currently no round-trip capability for this type of export. You can read values exported this way back in
#' via `import_from_tsv` but you will receive only the `data.frame` version back, NOT a `TableTree`.
#'
#' @inheritParams gen_args
#' @inheritParams data.frame_export
#' @param file (`character(1)`)\cr the path of the file to written to or read from.
#'
#' @return
#' * `export_as_tsv` returns `NULL` silently.
#' * `import_from_tsv` returns a `data.frame` with re-constituted list values.
#'
#' @seealso [path_enriched_df()] for the underlying function that does the work.
#'
#' @importFrom utils write.table read.table
#' @rdname tsv_io
#' @export
export_as_tsv <- function(tt, file = NULL, path_fun = collapse_path,
                          value_fun = collapse_values) {
  df <- path_enriched_df(tt, path_fun = path_fun, value_fun = value_fun)
  write.table(df, file, sep = "\t")
}

#' @rdname tsv_io
#' @export
import_from_tsv <- function(file) {
  rawdf <- read.table(file, header = TRUE, sep = "\t")
  as.data.frame(lapply(
    rawdf,
    function(col) {
      if (!any(grepl(.collapse_char, col, fixed = TRUE))) {
        col
      } else {
        I(strsplit(col, split = .collapse_char_esc))
      }
    }
  ))
}

### Migrated to formatters ----

#' @importFrom formatters export_as_txt
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' cat(export_as_txt(tbl, file = NULL, paginate = TRUE, lpp = 8))
#'
#' \dontrun{
#' tf <- tempfile(fileext = ".txt")
#' export_as_txt(tbl, file = tf)
#' system2("cat", tf)
#' }
#'
#' @export
formatters::export_as_txt

# data.frame output ------------------------------------------------------------

#' Generate a result data frame
#'
#' Collection of utilities to extract `data.frame` objects from `TableTree` objects.
#'
#' @inheritParams gen_args
#' @param spec (`character(1)`)\cr the specification to use to extract the result data frame. See Details below.
#' @param simplify (`flag`)\cr whether the result data frame should only have labels and result columns visible.
#' @param ... additional arguments passed to spec-specific result data frame conversion function. Currently it can be
#'   one or more of the following parameters (valid only for `v0_experimental` spec. for now):
#'   - `expand_colnames`: when `TRUE`, the result data frame will have expanded column names above the usual
#'     output. This is useful when the result data frame is used for further processing.
#'   - `simplify`: when `TRUE`, the result data frame will have only visible labels and result columns.
#'   - `as_strings`: when `TRUE`, the result data frame will have all values as strings, as they appear
#'     in the final table (it can also be retrieved from `matrix_form(tt)$strings`). This is also true for
#'     column counts if `expand_colnames = TRUE`.
#'   - `as_viewer`: when `TRUE`, the result data frame will have all values as they appear in the final table,
#'     i.e. with the same precision and numbers, but in easy-to-use numeric form.
#'   - `keep_label_rows`: when `TRUE`, the result data frame will have all labels as they appear in the
#'     final table.
#'   - `as_is`: when `TRUE`, the result data frame will have all the values as they appear in the final table,
#'     but without information about the row structure. Row labels will be assigned to rows so to work well
#'     with [df_to_tt()].
#'
#' @details `as_result_df()`: Result data frame specifications may differ in the exact information
#' they include and the form in which they represent it. Specifications whose names end in "_experimental"
#' are subject to change without notice, but specifications without the "_experimental"
#' suffix will remain available *including any bugs in their construction* indefinitely.
#'
#' @return
#' * `as_result_df` returns a result `data.frame`.
#'
#' @seealso [df_to_tt()] when using `as_is = TRUE` and [make_row_df()] to have a comprehensive view of the
#'   hierarchical structure of the rows.
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("STRATA1") %>%
#'   analyze(c("AGE", "BMRKR2"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#' as_result_df(tbl)
#'
#' @name data.frame_export
#' @export
as_result_df <- function(tt, spec = "v0_experimental", simplify = FALSE, ...) {
  checkmate::assert_class(tt, "VTableTree")
  checkmate::assert_string(spec)
  checkmate::assert_flag(simplify)

  if (nrow(tt) == 0) {
    return(sanitize_table_struct(tt))
  }

  result_df_fun <- lookup_result_df_specfun(spec)
  out <- result_df_fun(tt, ...)

  if (simplify) {
    out <- .simplify_result_df(out)
  }

  out
}

# Function that selects specific outputs from the result data frame
.simplify_result_df <- function(df) {
  col_df <- colnames(df)
  row_names_col <- which(col_df == "row_name")
  result_cols <- seq(which(col_df == "node_class") + 1, length(col_df))

  df[, c(row_names_col, result_cols)]
}

# Not used in rtables
# .split_colwidths <- function(ptabs, nctot, colwidths) {
#   ret <- list()
#   i <- 1L
#
#   rlw <- colwidths[1]
#   colwidths <- colwidths[-1]
#   donenc <- 0
#   while (donenc < nctot) {
#     curnc <- NCOL(ptabs[[i]])
#     ret[[i]] <- c(rlw, colwidths[seq_len(curnc)])
#     colwidths <- colwidths[-1 * seq_len(curnc)]
#     donenc <- donenc + curnc
#     i <- i + 1
#   }
#   ret
# }

#' @describeIn data.frame_export A list of functions that extract result data frames from `TableTree`s.
#'
#' @return
#' * `result_df_specs()` returns a named list of result data frame extraction functions by "specification".
#'
#' @examples
#' result_df_specs()
#'
#' @export
result_df_specs <- function() {
  list(v0_experimental = result_df_v0_experimental)
}

lookup_result_df_specfun <- function(spec) {
  if (!(spec %in% names(result_df_specs()))) {
    stop(
      "unrecognized result data frame specification: ",
      spec,
      "If that specification is correct you may need to update your version of rtables"
    )
  }
  result_df_specs()[[spec]]
}

result_df_v0_experimental <- function(tt,
                                      as_viewer = FALSE,
                                      as_strings = FALSE,
                                      expand_colnames = FALSE,
                                      keep_label_rows = FALSE,
                                      as_is = FALSE) {
  checkmate::assert_flag(as_viewer)
  checkmate::assert_flag(as_strings)
  checkmate::assert_flag(expand_colnames)
  checkmate::assert_flag(keep_label_rows)
  checkmate::assert_flag(as_is)

  if (as_is) {
    keep_label_rows <- TRUE
    expand_colnames <- FALSE
  }

  raw_cvals <- cell_values(tt)
  ## if the table has one row and multiple columns, sometimes the cell values returns a list of the cell values
  ## rather than a list of length 1 representing the single row. This is bad but may not be changeable
  ## at this point.
  if (nrow(tt) == 1 && length(raw_cvals) > 1) {
    raw_cvals <- list(raw_cvals)
  }

  # Flatten the list of lists (rows) of cell values into a data frame
  cellvals <- as.data.frame(do.call(rbind, raw_cvals))
  row.names(cellvals) <- NULL

  if (nrow(tt) == 1 && ncol(tt) == 1) {
    colnames(cellvals) <- names(raw_cvals)
  }

  if (as_viewer || as_strings) {
    # we keep previous calculations to check the format of the data
    mf_tt <- matrix_form(tt)
    mf_result_chars <- mf_strings(mf_tt)[-seq_len(mf_nlheader(mf_tt)), -1]
    mf_result_chars <- .remove_empty_elements(mf_result_chars)
    mf_result_numeric <- as.data.frame(
      .make_numeric_char_mf(mf_result_chars)
    )
    mf_result_chars <- as.data.frame(mf_result_chars)
    if (!setequal(dim(mf_result_numeric), dim(cellvals)) || !setequal(dim(mf_result_chars), dim(cellvals))) {
      stop(
        "The extracted numeric data.frame does not have the same dimension of the",
        " cell values extracted with cell_values(). This is a bug. Please report it."
      ) # nocov
    }
    if (as_strings) {
      colnames(mf_result_chars) <- colnames(cellvals)
      cellvals <- mf_result_chars
    } else {
      colnames(mf_result_numeric) <- colnames(cellvals)
      cellvals <- mf_result_numeric
    }
  }

  rdf <- make_row_df(tt)

  df <- rdf[, c("name", "label", "abs_rownumber", "path", "reprint_inds", "node_class")]
  # Removing initial root elements from path (out of the loop -> right maxlen)
  df$path <- lapply(df$path, .remove_root_elems_from_path,
    which_root_name = c("root", "rbind_root"),
    all = TRUE
  )
  maxlen <- max(lengths(df$path))

  # Loop for metadata (path and details from make_row_df)
  metadf <- do.call(
    rbind.data.frame,
    lapply(
      seq_len(NROW(df)),
      function(ii) {
        handle_rdf_row(df[ii, ], maxlen = maxlen)
      }
    )
  )

  # Should we keep label rows with NAs instead of values?
  if (keep_label_rows) {
    cellvals_mat_struct <- as.data.frame(
      matrix(NA, nrow = nrow(rdf), ncol = ncol(cellvals))
    )
    colnames(cellvals_mat_struct) <- colnames(cellvals)
    cellvals_mat_struct[metadf$node_class != "LabelRow", ] <- cellvals
    ret <- cbind(metadf, cellvals_mat_struct)
  } else {
    ret <- cbind(
      metadf[metadf$node_class != "LabelRow", ],
      cellvals
    )
  }

  # If we want to expand colnames
  if (expand_colnames) {
    col_name_structure <- .get_formatted_colnames(clayout(tt))
    number_of_non_data_cols <- which(colnames(ret) == "node_class")
    if (NCOL(ret) - number_of_non_data_cols != NCOL(col_name_structure)) {
      stop(
        "When expanding colnames structure, we were not able to find the same",
        " number of columns as in the result data frame. This is a bug. Please report it."
      ) # nocov
    }

    buffer_rows_for_colnames <- matrix(
      rep("<only_for_column_names>", number_of_non_data_cols * NROW(col_name_structure)),
      nrow = NROW(col_name_structure)
    )

    header_colnames_matrix <- cbind(buffer_rows_for_colnames, data.frame(col_name_structure))
    colnames(header_colnames_matrix) <- colnames(ret)

    count_row <- NULL
    if (disp_ccounts(tt)) {
      ccounts <- col_counts(tt)
      if (as_strings) {
        ccounts <- mf_strings(mf_tt)[mf_nlheader(mf_tt), ]
        ccounts <- .remove_empty_elements(ccounts)
      }
      count_row <- c(rep("<only_for_column_counts>", number_of_non_data_cols), ccounts)
      header_colnames_matrix <- rbind(header_colnames_matrix, count_row)
    }
    ret <- rbind(header_colnames_matrix, ret)
  }

  # Using only labels for row names and losing information about paths
  if (as_is) {
    tmp_rownames <- ret$label_name
    ret <- ret[, -seq_len(which(colnames(ret) == "node_class"))]
    if (length(unique(tmp_rownames)) == length(tmp_rownames)) {
      rownames(ret) <- tmp_rownames
    } else {
      ret <- cbind("label_name" = tmp_rownames, ret)
      rownames(ret) <- NULL
    }
  } else {
    rownames(ret) <- NULL
  }

  ret
}

.remove_empty_elements <- function(char_df) {
  if (is.null(dim(char_df))) {
    return(char_df[nzchar(char_df, keepNA = TRUE)])
  }

  apply(char_df, 2, function(col_i) col_i[nzchar(col_i, keepNA = TRUE)])
}

# Helper function to make the character matrix numeric
.make_numeric_char_mf <- function(char_df) {
  if (is.null(dim(char_df))) {
    return(as.numeric(stringi::stri_extract_all(char_df, regex = "\\d+.\\d+|\\d+")))
  }

  ret <- apply(char_df, 2, function(col_i) {
    lapply(
      stringi::stri_extract_all(col_i, regex = "\\d+.\\d+|\\d+"),
      as.numeric
    )
  })

  do.call(cbind, ret)
}

make_result_df_md_colnames <- function(maxlen) {
  spllen <- floor((maxlen - 2) / 2)
  ret <- character()
  if (spllen > 0) {
    ret <- paste(c("spl_var", "spl_value"), rep(seq_len(spllen), rep(2, spllen)), sep = "_")
  }
  ret <- c(ret, c("avar_name", "row_name", "label_name", "row_num", "is_group_summary", "node_class"))
}

do_label_row <- function(rdfrow, maxlen) {
  pth <- rdfrow$path[[1]]
  # Adjusting for the fact that we have two columns for each split
  extra_nas_from_splits <- floor((maxlen - length(pth)) / 2) * 2

  # Special cases with hidden labels
  if (length(pth) %% 2 == 1) {
    extra_nas_from_splits <- extra_nas_from_splits + 1
  }

  c(
    as.list(pth[seq_len(length(pth) - 1)]),
    as.list(replicate(extra_nas_from_splits, list(NA_character_))),
    as.list(tail(pth, 1)),
    list(
      label_name = rdfrow$label,
      row_num = rdfrow$abs_rownumber,
      content = FALSE,
      node_class = rdfrow$node_class
    )
  )
}

do_content_row <- function(rdfrow, maxlen) {
  pth <- rdfrow$path[[1]]
  contpos <- which(pth == "@content")

  seq_before <- seq_len(contpos - 1)

  c(
    as.list(pth[seq_before]),
    as.list(replicate(maxlen - contpos, list(NA_character_))),
    list(tail(pth, 1)),
    list(
      label_name = rdfrow$label,
      row_num = rdfrow$abs_rownumber,
      content = TRUE,
      node_class = rdfrow$node_class
    )
  )
}

do_data_row <- function(rdfrow, maxlen) {
  pth <- rdfrow$path[[1]]
  pthlen <- length(pth)
  ## odd means we have a multi-analsysis step in the path, we dont' want that in the result data frame
  if (pthlen %% 2 == 1) {
    pth <- pth[-1 * (pthlen - 2)]
  }
  pthlen_new <- length(pth)
  if (maxlen == 1) pthlen_new <- 3
  c(
    as.list(pth[seq_len(pthlen_new - 2)]),
    replicate(maxlen - pthlen, list(NA_character_)),
    as.list(tail(pth, 2)),
    list(
      label_name = rdfrow$label,
      row_num = rdfrow$abs_rownumber,
      content = FALSE,
      node_class = rdfrow$node_class
    )
  )
}

.remove_root_elems_from_path <- function(path, which_root_name = c("root", "rbind_root"), all = TRUE) {
  any_root_paths <- path[1] %in% which_root_name
  if (any_root_paths) {
    if (isTRUE(all)) {
      # Selecting the header grouping of root and rbind_root (we do not want to remove other root labels-path later)
      root_indices <- which(path %in% which_root_name)
      if (any(diff(root_indices) > 1)) { # integer(0) for diff means FALSE
        end_point_root_headers <- which(diff(root_indices) > 1)[1]
      } else {
        end_point_root_headers <- length(root_indices)
      }
      root_path_to_remove <- seq_len(end_point_root_headers)
    } else {
      root_path_to_remove <- 1
    }
    path <- path[-root_path_to_remove]
  }

  # Fix for very edge case where we have only root elements
  if (length(path) == 0) {
    path <- which_root_name[1]
  }

  path
}

handle_rdf_row <- function(rdfrow, maxlen) {
  nclass <- rdfrow$node_class

  ret <- switch(nclass,
    LabelRow = do_label_row(rdfrow, maxlen),
    ContentRow = do_content_row(rdfrow, maxlen),
    DataRow = do_data_row(rdfrow, maxlen),
    stop("Unrecognized node type in row dataframe, unable to generate result data frame")
  )
  setNames(ret, make_result_df_md_colnames(maxlen))
}

# Helper recurrent function to get the column names for the result data frame from the VTableTree
.get_formatted_colnames <- function(clyt) {
  ret <- obj_label(clyt)
  if (!nzchar(ret)) {
    ret <- NULL
  }
  if (is.null(tree_children(clyt))) {
    return(ret)
  } else {
    ret <- rbind(ret, do.call(cbind, lapply(tree_children(clyt), .get_formatted_colnames)))
    colnames(ret) <- NULL
    rownames(ret) <- NULL
    return(ret)
  }
}

#' @describeIn data.frame_export Transform a `TableTree` object to a path-enriched `data.frame`.
#'
#' @param path_fun (`function`)\cr function to transform paths into single-string row/column names.
#' @param value_fun (`function`)\cr function to transform cell values into cells of a `data.frame`. Defaults to
#'   `collapse_values`, which creates strings where multi-valued cells are collapsed together, separated by `|`.
#'
#' @return
#' * `path_enriched_df()` returns a `data.frame` of `tt`'s cell values (processed by `value_fun`, with columns named by
#'   the full column paths (processed by `path_fun` and an additional `row_path` column with the row paths (processed
#'   by `path_fun`).
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#' path_enriched_df(tbl)
#'
#' @export
path_enriched_df <- function(tt, path_fun = collapse_path, value_fun = collapse_values) {
  rdf <- make_row_df(tt)
  cdf <- make_col_df(tt)
  cvs <- as.data.frame(do.call(rbind, cell_values(tt)))
  cvs <- as.data.frame(lapply(cvs, value_fun))
  row.names(cvs) <- NULL
  colnames(cvs) <- path_fun(cdf$path)
  preppaths <- path_fun(rdf[rdf$node_class != "LabelRow", ]$path)
  cbind.data.frame(row_path = preppaths, cvs)
}

.collapse_char <- "|"
.collapse_char_esc <- "\\|"

collapse_path <- function(paths) {
  if (is.list(paths)) {
    return(vapply(paths, collapse_path, ""))
  }
  paste(paths, collapse = .collapse_char)
}

collapse_values <- function(colvals) {
  if (!is.list(colvals)) { ## || all(vapply(colvals, length, 1L) == 1))
    return(colvals)
  } else if (all(vapply(colvals, length, 1L) == 1)) {
    return(unlist(colvals))
  }
  vapply(colvals, paste, "", collapse = .collapse_char)
}

# pdf output -------------------------------------------------------------------

### Export as PDF - migrated to formatters

#' @importFrom formatters export_as_pdf
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' \dontrun{
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(tbl, file = tf, pg_height = 4)
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(tbl, file = tf, lpp = 8)
#' }
#'
#' @export
formatters::export_as_pdf

# only used in pagination
.tab_to_colpath_set <- function(tt) {
  vapply(
    collect_leaves(coltree(tt)),
    function(y) paste(pos_to_path(tree_pos(y)), collapse = " "),
    ""
  )
}
.figure_out_colinds <- function(subtab, fulltab) {
  match(
    .tab_to_colpath_set(subtab),
    .tab_to_colpath_set(fulltab)
  )
}

# Flextable and docx -----------------------------------------------------------

#' Export as word document
#'
#' From a table, produce a self-contained word document or attach it to a template word
#' file (`template_file`). This function is based on the [tt_to_flextable()] transformer and
#' the `officer` package.
#'
#' @inheritParams gen_args
#' @param file (`character(1)`)\cr string that indicates the final file output. Must have `.docx` extension.
#' @param doc_metadata (`list` of `character(1)`)\cr any value that can be used as metadata by
#'   `?officer::set_doc_properties`. Important text values are `title`, `subject`, `creator`, and `description`,
#'   while `created` is a date object.
#' @inheritParams tt_to_flextable
#' @param template_file (`character(1)`)\cr template file that `officer` will use as a starting point for the final
#'   document. Document attaches the table and uses the defaults defined in the template file.
#' @param section_properties (`officer::prop_section`)\cr an [officer::prop_section()] object which sets margins and
#'   page size.
#'
#' @note `export_as_docx()` has few customization options available. If you require specific formats and details,
#'   we suggest that you use [tt_to_flextable()] prior to `export_as_docx`. Only the `title_as_header` and
#'   `footer_as_text` parameters must be re-specified if the table is changed first using [tt_to_flextable()].
#'
#' @seealso [tt_to_flextable()]
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' # See how section_properties_portrait function is built for custom
#' \dontrun{
#' tf <- tempfile(fileext = ".docx")
#' export_as_docx(tbl, file = tf, section_properties = section_properties_portrait())
#' }
#'
#' @export
export_as_docx <- function(tt,
                           file,
                           doc_metadata = NULL,
                           titles_as_header = FALSE,
                           footers_as_text = TRUE,
                           template_file = NULL,
                           section_properties = NULL) {
  # Checks
  check_required_packages(c("flextable", "officer"))
  if (inherits(tt, "VTableTree")) {
    flex_tbl <- tt_to_flextable(tt,
      titles_as_header = titles_as_header,
      footers_as_text = footers_as_text
    )
    if (isFALSE(titles_as_header) || isTRUE(footers_as_text)) {
      # Ugly but I could not find a getter for font.size
      font_sz <- flex_tbl$header$styles$text$font.size$data[1, 1]
      font_sz_footer <- flex_tbl$header$styles$text$font.size$data[1, 1] - 1
      font_fam <- flex_tbl$header$styles$text$font.family$data[1, 1]

      # Set the test as the tt
      fpt <- officer::fp_text(font.family = font_fam, font.size = font_sz)
      fpt_footer <- officer::fp_text(font.family = font_fam, font.size = font_sz_footer)
    }
  } else {
    flex_tbl <- tt
  }
  if (!is.null(template_file) && !file.exists(template_file)) {
    template_file <- NULL
  }

  # Create a new empty Word document
  if (!is.null(template_file)) {
    doc <- officer::read_docx(template_file)
  } else {
    doc <- officer::read_docx()
  }

  if (!is.null(section_properties)) {
    doc <- officer::body_set_default_section(doc, section_properties)
  }

  # Extract title
  if (isFALSE(titles_as_header) && inherits(tt, "VTableTree")) {
    ts_tbl <- all_titles(tt)
    if (length(ts_tbl) > 0) {
      doc <- add_text_par(doc, ts_tbl, fpt)
    }
  }

  # Add the table to the document
  doc <- flextable::body_add_flextable(doc, flex_tbl, align = "left")

  # add footers as paragraphs
  if (isTRUE(footers_as_text) && inherits(tt, "VTableTree")) {
    # Adding referantial footer line separator if present
    # (this is usually done differently, i.e. inside footnotes)
    matform <- matrix_form(tt, indent_rownames = TRUE)
    if (length(matform$ref_footnotes) > 0) {
      doc <- add_text_par(doc, matform$ref_footnotes, fpt_footer)
    }
    # Footer lines
    if (length(all_footers(tt)) > 0) {
      doc <- add_text_par(doc, all_footers(tt), fpt_footer)
    }
  }

  if (!is.null(doc_metadata)) {
    # Checks for values rely on officer function
    doc <- do.call(officer::set_doc_properties, c(list("x" = doc), doc_metadata))
  }

  # Save the Word document to a file
  print(doc, target = file)
}

# Shorthand to add text paragraph
add_text_par <- function(doc, chr_v, text_format) {
  for (ii in seq_along(chr_v)) {
    cur_fp <- officer::fpar(officer::ftext(chr_v[ii], prop = text_format))
    doc <- officer::body_add_fpar(doc, cur_fp)
  }
  doc
}

#' @describeIn export_as_docx Helper function that defines standard portrait properties for tables.
#' @export
section_properties_portrait <- function() {
  officer::prop_section(
    page_size = officer::page_size(
      orient = "portrait",
      width = 8.5, height = 11
    ),
    type = "continuous",
    page_margins = margins_potrait()
  )
}

#' @describeIn export_as_docx Helper function that defines standard landscape properties for tables.
#' @export
section_properties_landscape <- function() {
  officer::prop_section(
    page_size = officer::page_size(
      orient = "landscape",
      width = 8.5, height = 11
    ),
    type = "continuous",
    page_margins = margins_landscape()
  )
}

#' @describeIn export_as_docx Helper function that defines standard portrait margins for tables.
#' @export
margins_potrait <- function() {
  officer::page_mar(bottom = 0.98, top = 0.95, left = 1.5, right = 1, gutter = 0)
}
#' @describeIn export_as_docx Helper function that defines standard landscape margins for tables.
#' @export
margins_landscape <- function() {
  officer::page_mar(bottom = 1, top = 1.5, left = 0.98, right = 0.95, gutter = 0)
}

#' Create a `flextable` from an `rtables` table
#'
#' Principally used for export ([export_as_docx()]), this function produces a `flextable`
#' from an `rtables` table. If `theme = NULL`, `rtables`-like style will be used. Otherwise,
#' [theme_docx_default()] will produce a `.docx`-friendly table.
#'
#' @inheritParams gen_args
#' @inheritParams paginate_table
#' @param theme (`function` or `NULL`)\cr A theme function that is designed internally as a function of a `flextable`
#'   object to change its layout and style. If `NULL`, it will produce a table similar to `rtables` default. Defaults
#'   to `theme_docx_default(tt)`.
#' @param border (`officer` border object)\cr defaults to `officer::fp_border(width = 0.5)`.
#' @param indent_size (`integer(1)`)\cr if `NULL`, the default indent size of the table (see [matrix_form()]
#'   `indent_size`) is used. To work with `docx`, any size is multiplied by 2 mm (5.67 pt) by default.
#' @param titles_as_header (`logical(1)`)\cr defaults to `TRUE` for [tt_to_flextable()], so the table is self-contained
#'   as it makes additional header rows for [main_title()] string and [subtitles()] character vector (one per element).
#'   `FALSE` is suggested for [export_as_docx()]. This adds titles and subtitles as a text paragraph above the table.
#'   The same style is applied.
#' @param footers_as_text (`flag`)\cr defaults to `FALSE` for [tt_to_flextable()], so the table is self-contained with
#'   the `flextable` definition of footnotes. `TRUE` is used for [export_as_docx()] to add the footers as a new
#'   paragraph after the table. The same style is applied, but with a smaller font.
#' @param counts_in_newline (`flag`)\cr defaults to `FALSE`. In `rtables` text printing ([formatters::toString()]),
#'   the column counts, i.e. `(N=xx)`, are always on a new line. For `docx` exports it could be necessary to print it
#'   on the same line.
#' @param paginate (`flag`)\cr when exporting `.docx` documents using `export_as_docx`, we suggest relying on the
#'   Microsoft Word pagination system. If `TRUE`, this option splits `tt` into different "pages" as multiple
#'   `flextables`. Cooperation between the two mechanisms is not guaranteed. Defaults to `FALSE`.
#' @param total_width (`numeric(1)`)\cr total width (in inches) for the resulting flextable(s). Defaults to 10.
#'
#' @return A `flextable` object.
#'
#' @seealso [export_as_docx()]
#'
#' @examples
#' analysisfun <- function(x, ...) {
#'   in_rows(
#'     row1 = 5,
#'     row2 = c(1, 2),
#'     .row_footnotes = list(row1 = "row 1 - row footnote"),
#'     .cell_footnotes = list(row2 = "row 2 - cell footnote")
#'   )
#' }
#'
#' lyt <- basic_table(
#'   title = "Title says Whaaaat", subtitles = "Oh, ok.",
#'   main_footer = "ha HA! Footer!"
#' ) %>%
#'   split_cols_by("ARM") %>%
#'   analyze("AGE", afun = analysisfun)
#'
#' tbl <- build_table(lyt, ex_adsl)
#' # rtables style
#' tt_to_flextable(tbl, theme = NULL)
#'
#' tt_to_flextable(tbl, theme = theme_docx_default(tbl, font_size = 7))
#'
#' @export
tt_to_flextable <- function(tt,
                            theme = theme_docx_default(tt),
                            border = flextable::fp_border_default(width = 0.5),
                            indent_size = NULL,
                            titles_as_header = TRUE,
                            footers_as_text = FALSE,
                            counts_in_newline = FALSE,
                            paginate = FALSE,
                            lpp = NULL,
                            cpp = NULL,
                            ...,
                            colwidths = propose_column_widths(matrix_form(tt, indent_rownames = TRUE)),
                            tf_wrap = !is.null(cpp),
                            max_width = cpp,
                            total_width = 10) {
  check_required_packages("flextable")
  if (!inherits(tt, "VTableTree")) {
    stop("Input table is not an rtables' object.")
  }
  checkmate::assert_flag(titles_as_header)
  checkmate::assert_flag(footers_as_text)
  checkmate::assert_flag(counts_in_newline)

  ## if we're paginating, just call -> pagination happens also afterwards if needed
  if (paginate) {
    if (is.null(lpp)) {
      stop("lpp must be specified when calling tt_to_flextable with paginate=TRUE")
    }
    tabs <- paginate_table(tt, lpp = lpp, cpp = cpp, tf_wrap = tf_wrap, max_width = max_width, ...)
    cinds <- lapply(tabs, function(tb) c(1, .figure_out_colinds(tb, tt) + 1L))
    return(mapply(tt_to_flextable,
      tt = tabs, colwidths = cinds,
      MoreArgs = list(paginate = FALSE, total_width = total_width),
      SIMPLIFY = FALSE
    ))
  }

  # Calculate the needed colwidths
  final_cwidths <- total_width * colwidths / sum(colwidths) # xxx to fix
  # xxx FIXME missing transformer from character based widths to mm or pt

  # Extract relevant information
  matform <- matrix_form(tt, indent_rownames = TRUE)
  body <- mf_strings(matform) # Contains header
  spans <- mf_spans(matform) # Contains header
  mpf_aligns <- mf_aligns(matform) # Contains header
  hnum <- mf_nlheader(matform) # Number of lines for the header
  rdf <- make_row_df(tt) # Row-wise info

  # decimal alignment pre-proc
  if (any(grepl("dec", mpf_aligns))) {
    body <- decimal_align(body, mpf_aligns)
    # Coercion for flextable
    mpf_aligns[mpf_aligns == "decimal"] <- "center"
    mpf_aligns[mpf_aligns == "dec_left"] <- "left"
    mpf_aligns[mpf_aligns == "dec_right"] <- "right"
  }

  # Fundamental content of the table
  content <- as.data.frame(body[-seq_len(hnum), , drop = FALSE])
  flx <- flextable::qflextable(content) %>%
    # Default rtables if no footnotes
    remove_hborder(part = "body", w = "bottom")

  # Header addition -> NB: here we have a problem with (N=xx)
  hdr <- body[seq_len(hnum), , drop = FALSE]

  # IMPORTANT: Fix of (N=xx) which is by default on a new line but we usually do not
  # want this, and it depends on the size of the table, it is not another
  # row with different columns -> All of this should be fixed at source (in toString)
  if (hnum > 1) { # otherwise nothing to do
    det_nclab <- apply(hdr, 2, grepl, pattern = "\\(N=[0-9]+\\)$")
    has_nclab <- apply(det_nclab, 1, any)
    if (isFALSE(counts_in_newline) && any(has_nclab)) {
      whsnc <- which(has_nclab) # which rows have it
      what_is_nclab <- det_nclab[whsnc, ]

      # condition for popping the interested row by merging the upper one
      hdr[whsnc, what_is_nclab] <- paste(hdr[whsnc - 1, what_is_nclab],
        hdr[whsnc, what_is_nclab],
        sep = " "
      )
      hdr[whsnc - 1, what_is_nclab] <- ""

      # We can remove the row if they are all ""
      row_to_pop <- whsnc - 1
      if (all(!nzchar(hdr[row_to_pop, ]))) {
        hdr <- hdr[-row_to_pop, , drop = FALSE]
        spans <- spans[-row_to_pop, , drop = FALSE]
        body <- body[-row_to_pop, , drop = FALSE]
        mpf_aligns <- mpf_aligns[-row_to_pop, , drop = FALSE]
        hnum <- hnum - 1
      }
    }
  }

  flx <- flx %>%
    flextable::set_header_labels( # Needed bc headers must be unique
      values = setNames(
        as.vector(hdr[hnum, , drop = TRUE]),
        names(content)
      )
    )
  # If there are more rows
  if (hnum > 1) {
    for (i in seq(hnum - 1, 1)) {
      sel <- spans_to_viscell(spans[i, ])
      flx <- flextable::add_header_row(
        flx,
        top = TRUE,
        values = as.vector(hdr[i, sel]),
        colwidths = as.integer(spans[i, sel]) # xxx to fix
      )
    }
  }

  # Polish the inner horizontal borders from the header
  flx <- flx %>%
    remove_hborder(part = "header", w = "all") %>%
    add_hborder("header", ii = c(0, hnum), border = border)

  # ALIGNS
  flx <- flx %>%
    apply_alignments(mpf_aligns[seq_len(hnum), , drop = FALSE], "header") %>%
    apply_alignments(mpf_aligns[-seq_len(hnum), , drop = FALSE], "body")

  # Rownames indentation
  checkmate::check_int(indent_size, null.ok = TRUE)
  if (is.null(indent_size)) {
    indent_size <- matform$indent_size * word_mm_to_pt(2) # default is 2mm (5.7pt)
  }
  for (i in seq_len(NROW(tt))) {
    flx <- flextable::padding(flx,
      i = i, j = 1,
      padding.left = indent_size * rdf$indent[[i]] + word_mm_to_pt(0.1), # 0.1 mmm in pt
      padding.right = word_mm_to_pt(0.1) # 0.1 mmm in pt (so not to touch the border)
    )
  }

  # Adding referantial footer line separator if present
  if (length(matform$ref_footnotes) > 0 && isFALSE(footers_as_text)) {
    flx <- flextable::add_footer_lines(flx, values = matform$ref_footnotes) %>%
      add_hborder(part = "body", ii = nrow(tt), border = border)
  }

  # Footer lines
  if (length(all_footers(tt)) > 0 && isFALSE(footers_as_text)) {
    flx <- flextable::add_footer_lines(flx, values = all_footers(tt))
  }

  flx <- flextable::width(flx, width = final_cwidths) # xxx to fix

  if (!is.null(theme)) {
    flx <- theme(flx)
  }

  # Title lines (after theme for problems with lines)
  if (titles_as_header && length(all_titles(tt)) > 0 && any(nzchar(all_titles(tt)))) {
    real_titles <- all_titles(tt)
    real_titles <- real_titles[nzchar(real_titles)]
    flx <- flextable::add_header_lines(flx, values = real_titles, top = TRUE) %>%
      # Remove the added borders
      remove_hborder(part = "header", w = c("inner", "top")) %>%
      # Re-add the separator between titles and real headers
      add_hborder(
        part = "header", ii = length(real_titles),
        border = border
      ) %>%
      # Remove vertical borders added by theme eventually
      remove_vborder(part = "header", ii = seq_along(real_titles))
  }

  # These final formatting need to work with colwidths
  flx <- flextable::set_table_properties(flx, layout = "autofit", align = "left") # xxx to fix
  # NB: autofit or fixed may be switched if widths are correctly staying in the page
  flx <- flextable::fix_border_issues(flx) # Fixes some rendering gaps in borders

  flx
}

#' @describeIn tt_to_flextable Main theme function for [export_as_docx()]
#'
#' @inheritParams export_as_docx
#' @param font (`character(1)`)\cr defaults to `"Arial"`. If the font is not available, `flextable` default is used.
#' @param font_size (`integer(1)`)\cr font size. Defaults to 9.
#' @param bold (`character`)\cr parts of the table text that should be in bold. Can be any combination of
#'   `c("header", "content_rows", "label_rows")`. The first one renders all column names bold (not `topleft` content).
#'   The second and third option use [rtables::make_row_df()] to render content or/and label rows as bold.
#' @param bold_manual (named `list` or `NULL`)\cr list of index lists. See example for needed structure. Accepted
#'   groupings/names are `c("header", "body")`.
#'
#' @seealso [export_as_docx()]
#'
#' @examples
#' # Custom theme
#' special_bold <- list(
#'   "header" = list("i" = 1, "j" = c(1, 3)),
#'   "body" = list("i" = c(1, 2), "j" = 1)
#' )
#' custom_theme <- theme_docx_default(tbl,
#'   font_size = 10,
#'   font = "Brush Script MT",
#'   border = flextable::fp_border_default(color = "pink", width = 2),
#'   bold = NULL,
#'   bold_manual = special_bold
#' )
#' tt_to_flextable(tbl,
#'   border = flextable::fp_border_default(color = "pink", width = 2),
#'   theme = custom_theme
#' )
#'
#' @export
theme_docx_default <- function(tt = NULL, # Option for more complicated stuff
                               font = "Arial",
                               font_size = 9,
                               bold = c("header", "content_rows", "label_rows"),
                               bold_manual = NULL,
                               border = flextable::fp_border_default(width = 0.5)) {
  function(flx) {
    check_required_packages("flextable")
    if (!inherits(flx, "flextable")) {
      stop(sprintf(
        "Function `%s` supports only flextable objects.",
        "theme_box()"
      ))
    }
    if (!is.null(tt) && !inherits(tt, "VTableTree")) {
      stop("Input table is not an rtables' object.")
    }
    checkmate::assert_int(font_size, lower = 1)
    checkmate::assert_string(font)
    checkmate::assert_subset(bold,
      eval(formals(theme_docx_default)$bold),
      empty.ok = TRUE
    )

    # Font setting
    flx <- flextable::fontsize(flx, size = font_size, part = "all") %>%
      flextable::fontsize(size = font_size - 1, part = "footer") %>%
      flextable::font(fontname = font, part = "all")

    # Vertical borders
    flx <- flx %>%
      flextable::border_outer(part = "body", border = border) %>%
      flextable::border_outer(part = "header", border = border)

    # Vertical alignment -> all top for now, we will set it for the future
    flx <- flx %>%
      flextable::valign(j = 2:(NCOL(tt) + 1), valign = "top", part = "body") %>%
      flextable::valign(j = 1, valign = "top", part = "body") %>%
      flextable::valign(j = 2:(NCOL(tt) + 1), valign = "top", part = "header")

    # Bold settings
    if (any(bold == "header")) {
      flx <- flextable::bold(flx, j = 2:(NCOL(tt) + 1), part = "header") # Done with theme
    }
    # Content rows are effectively our labels in row names
    if (any(bold == "content_rows")) {
      if (is.null(tt)) stop('bold = "content_rows" needs the original rtables object (tt).')
      rdf <- make_row_df(tt)
      which_body <- which(rdf$node_class == "ContentRow")
      flx <- flextable::bold(flx, j = 1, i = which_body, part = "body")
    }
    if (any(bold == "label_rows")) {
      if (is.null(tt)) stop('bold = "content_rows" needs the original rtables object (tt).')
      rdf <- make_row_df(tt)
      which_body <- which(rdf$node_class == "LabelRow")
      flx <- flextable::bold(flx, j = 1, i = which_body, part = "body")
    }
    # If you want specific cells to be bold
    if (!is.null(bold_manual)) {
      checkmate::assert_list(bold_manual)
      valid_sections <- c("header", "body") # Only valid values
      checkmate::assert_subset(names(bold_manual), valid_sections)
      for (bi in seq_along(bold_manual)) {
        bld_tmp <- bold_manual[[bi]]
        checkmate::assert_list(bld_tmp)
        if (!all(c("i", "j") %in% names(bld_tmp)) || !all(vapply(bld_tmp, checkmate::test_integerish, logical(1)))) {
          stop(
            "Found an allowed section for manual bold (", names(bold_manual)[bi],
            ") that was not a named list with i (row) and j (col) integer vectors."
          )
        }
        flx <- flextable::bold(flx,
          i = bld_tmp$i, j = bld_tmp$j,
          part = names(bold_manual)[bi]
        )
      }
    }

    # vertical padding is manual atm and respect doc std
    flx <- flx %>%
      # flextable::padding(j = 2:(NCOL(tt) + 1), padding.top = , part = "body") %>% # not specified
      flextable::padding(j = 1, padding.top = 1, padding.bottom = 1, part = "body") %>%
      flextable::padding(j = 2:(NCOL(tt) + 1), padding.top = 0, padding.bottom = 3, part = "header")

    # single line spacing (for safety) -> space = 1
    flx <- flextable::line_spacing(flx, space = 1, part = "all")

    flx
  }
}

# Padding helper functions to transform mm to pt and viceversa
# # General note for word: 1pt -> 0.3527777778mm -> 0.013888888888889"
word_inch_to_pt <- function(inch) { # nocov
  inch / 0.013888888888889 # nocov
}

word_mm_to_pt <- function(mm) {
  mm / 0.3527777778
}

# Polish horizontal borders
remove_hborder <- function(flx, part, w = c("top", "bottom", "inner")) {
  # If you need to remove all of them
  if (length(w) == 1 && w == "all") {
    w <- eval(formals(remove_hborder)$w)
  }

  if (any(w == "top")) {
    flx <- flextable::hline_top(flx,
      border = flextable::fp_border_default(width = 0),
      part = part
    )
  }
  if (any(w == "bottom")) {
    flx <- flextable::hline_bottom(flx,
      border = flextable::fp_border_default(width = 0),
      part = part
    )
  }
  # Inner horizontal lines removal
  if (any(w == "inner")) {
    flx <- flextable::border_inner_h(
      flx,
      border = flextable::fp_border_default(width = 0),
      part = part
    )
  }
  flx
}

# Remove vertical borders from both sides (for titles)
remove_vborder <- function(flx, part, ii) {
  flx <- flextable::border(flx,
    i = ii, part = part,
    border.left = flextable::fp_border_default(width = 0),
    border.right = flextable::fp_border_default(width = 0)
  )
}

# Add horizontal border
add_hborder <- function(flx, part, ii, border) {
  if (any(ii == 0)) {
    flx <- flextable::border(flx, i = 1, border.top = border, part = part)
    ii <- ii[!(ii == 0)]
  }
  if (length(ii) > 0) {
    flx <- flextable::border(flx, i = ii, border.bottom = border, part = part)
  }
  flx
}

apply_alignments <- function(flx, aligns_df, part) {
  # List of characters you want to search for
  search_chars <- unique(c(aligns_df))

  # Loop through each character and find its indexes
  for (char in search_chars) {
    indexes <- which(aligns_df == char, arr.ind = TRUE)
    tmp_inds <- as.data.frame(indexes)
    flx <- flx %>%
      flextable::align(
        i = tmp_inds[["row"]],
        j = tmp_inds[["col"]],
        align = char,
        part = part
      )
  }

  flx
}
