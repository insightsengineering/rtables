# as_result_df ------------------------------------------------------------
#' Generate a result data frame
#'
#' Collection of utilities to extract `data.frame` objects from `TableTree` objects.
#'
#' @inheritParams gen_args
#' @param spec (`function`)\cr function that generates the result data frame from a table (`TableTree`).
#'   It defaults to `NULL`, for standard processing.
#' @param expand_colnames (`flag`)\cr when `TRUE`, the result data frame will have expanded column
#'   names above the usual output. This is useful when the result data frame is used for further processing.
#' @param data_format (`string`)\cr the format of the data in the result data frame. It can be one value
#'   between `"full_precision"` (default), `"strings"`, and `"numeric"`. The last two values show the numeric
#'   data with the visible precision.
#' @param make_ard (`flag`)\cr when `TRUE`, the result data frame will have only one statistic per row.
#' @param keep_label_rows (`flag`)\cr when `TRUE`, the result data frame will have all labels
#'   as they appear in the final table.
#' @param simplify (`flag`)\cr when `TRUE`, the result data frame will have only visible labels and
#'   result columns. Consider showing also label rows with `keep_label_rows = TRUE`. This output can be
#'   used again to create a `TableTree` object with [df_to_tt()].
#' @param ... additional arguments passed to spec-specific result data frame function (`spec`).
#'
#' @return
#' * `as_result_df` returns a result `data.frame`.
#'
#' @seealso [df_to_tt()] when using `simplify = TRUE` and [formatters::make_row_df()] to have a
#'   comprehensive view of the hierarchical structure of the rows.
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_rows_by("STRATA1") %>%
#'   analyze(c("AGE", "BMRKR2"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#' as_result_df(tbl, simplify = TRUE)
#'
#' @name data.frame_export
#' @export
as_result_df <- function(tt, spec = NULL,
                         data_format = c("full_precision", "strings", "numeric"),
                         make_ard = FALSE,
                         expand_colnames = FALSE,
                         keep_label_rows = FALSE,
                         simplify = FALSE,
                         ...) {
  data_format <- data_format[[1]]
  checkmate::assert_class(tt, "VTableTree")
  checkmate::assert_function(spec, null.ok = TRUE)
  checkmate::assert_choice(data_format[[1]], choices = eval(formals(as_result_df)[["data_format"]]))
  checkmate::assert_flag(make_ard)
  checkmate::assert_flag(expand_colnames)
  checkmate::assert_flag(keep_label_rows)
  checkmate::assert_flag(simplify)

  if (nrow(tt) == 0) {
    return(sanitize_table_struct(tt))
  }

  if (make_ard) {
    simplify <- FALSE
    expand_colnames <- TRUE
    keep_label_rows <- FALSE
  }

  if (is.null(spec)) {
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

    if (data_format %in% c("strings", "numeric")) {
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
      if (data_format == "strings") {
        colnames(mf_result_chars) <- colnames(cellvals)
        cellvals <- mf_result_chars
      } else {
        colnames(mf_result_numeric) <- colnames(cellvals)
        cellvals <- mf_result_numeric
      }
    }

    rdf <- make_row_df(tt)
    cinfo_df <- col_info(tt)
    ci_coltree <- coltree(cinfo_df)
    column_split_names <- .get_column_split_name(ci_coltree) # used only in make_ard

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
        if (data_format == "strings") {
          ccounts <- mf_strings(mf_tt)[mf_nlheader(mf_tt), ]
          ccounts <- .remove_empty_elements(ccounts)
        }
        count_row <- c(rep("<only_for_column_counts>", number_of_non_data_cols), ccounts)
        header_colnames_matrix <- rbind(header_colnames_matrix, count_row)
      }
      ret <- rbind(header_colnames_matrix, ret)
    }

    # ARD part for one stat per row
    if (make_ard) {
      # Unnecessary columns
      ret_tmp <- ret[, !colnames(ret) %in% c("row_num", "is_group_summary", "node_class")]

      # Indexes of real columns (visible in the output, but no row names)
      only_col_indexes <- seq(which(colnames(ret_tmp) == "label_name") + 1, ncol(ret_tmp))

      # Core row names
      col_label_rows <- grepl("<only_for_column_*", ret_tmp$avar_name)
      core_row_names <- ret_tmp[!col_label_rows, -only_col_indexes]

      # Moving colnames to rows (flattening)
      ret_w_cols <- NULL
      # col_i <- only_col_indexes[1]
      for (col_i in only_col_indexes) {
        tmp_ret_by_col_i <- cbind(
          group1 = column_split_names[[ret_tmp[, col_i][[1]]]],
          group1_level = ret_tmp[, col_i][[1]],
          # instead of avar_name  row_name       label_name
          setNames(core_row_names, c("variable", "variable_level", "variable_label")), # missing stat_name xxx
          stat = I(setNames(ret_tmp[!col_label_rows, col_i], NULL))
        )

        ret_w_cols <- rbind(ret_w_cols, tmp_ret_by_col_i)
      }

      ret <- ret_w_cols
    }

    out <- if (simplify) {
      .simplify_result_df(ret)
    } else {
      ret
    }
  } else {
    # Applying specs
    out <- spec(tt, ...)
  }

  out
}

# Helper function to get column split names
.get_column_split_name <- function(ci_coltree) {
  # ci stands for column information
  if (is(ci_coltree, "LayoutAxisTree")) {
    kids <- tree_children(ci_coltree)
    return(unlist(lapply(kids, .get_column_split_name)))
  }
  sapply(pos_splits(tree_pos(ci_coltree)), spl_payload)
}

# Function that selects specific outputs from the result data frame
.simplify_result_df <- function(df) {
  col_df <- colnames(df)
  if (!all(c("row_name", "node_class") %in% col_df)) {
    stop("Please simplify the result data frame only when it has 'row_name' and 'node_class' columns.")
  }
  row_names_col <- which(col_df == "label_name")
  result_cols <- seq(which(col_df == "node_class") + 1, length(col_df))

  df[, c(row_names_col, result_cols)]
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
# path_enriched_df ------------------------------------------------------------
#
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
