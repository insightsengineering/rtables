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
#' @param add_tbl_name_split (`flag`)\cr when `TRUE` and when the table has more than one
#'   `analyze(table_names = "<diff_names>")`, the table names will be present as a group split named
#'   `"<analysis_spl_tbl_name>"`.
#' @param verbose (`flag`)\cr when `TRUE`, the function will print additional information for
#'   `data_format != "full_precision"`.
#' @param ... additional arguments passed to spec-specific result data frame function (`spec`). When
#'   using `make_ard = TRUE`, it is possible to turn off the extraction of the exact string decimals
#'   printed by the table with `add_tbl_str_decimals = FALSE`.
#'
#' @return
#' * `as_result_df` returns a result `data.frame`.
#'
#' @seealso [df_to_tt()] when using `simplify = TRUE` and [formatters::make_row_df()] to have a
#'   comprehensive view of the hierarchical structure of the rows.
#'
#' @note When `parent_name` is used when constructing a layout to directly control
#'   the name of subtables in a table, that will be reflected in the 'group' values
#'   returned in the result dataframe/ard. When automatic de-duplication of sibling names
#'   is performed by `rtables`, that is automatically undone during the result
#'   df creation process, so the group values will be as if the relevant siblings
#'   had identical names.
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
                         add_tbl_name_split = FALSE,
                         simplify = FALSE,
                         verbose = FALSE,
                         round_type = obj_round_type(tt),
                         ...) {
  data_format <- data_format[[1]]
  checkmate::assert_class(tt, "VTableTree")
  checkmate::assert_function(spec, null.ok = TRUE)
  checkmate::assert_choice(data_format[[1]], choices = eval(formals(as_result_df)[["data_format"]]))
  checkmate::assert_flag(make_ard)
  checkmate::assert_flag(expand_colnames)
  checkmate::assert_flag(keep_label_rows)
  checkmate::assert_flag(simplify)
  checkmate::assert_flag(add_tbl_name_split)
  checkmate::assert_flag(verbose)

  if (nrow(tt) == 0) {
    return(sanitize_table_struct(tt))
  }

  if (make_ard) {
    simplify <- FALSE
    expand_colnames <- TRUE
    keep_label_rows <- FALSE
  }

  if (is.null(spec)) {
    # raw values
    rawvals <- cell_values(tt)
    cellvals_init <- .make_df_from_raw_data(rawvals, nr = nrow(tt), nc = ncol(tt))

    if (data_format %in% c("strings", "numeric")) {
      # we keep previous calculations to check the format of the data
      mf_tt <- matrix_form(tt, round_type = round_type)
      mf_result_chars <- mf_strings(mf_tt)[-seq_len(mf_nlheader(mf_tt)), -1, drop = FALSE]
      is_not_label_rows <- make_row_df(tt)$node_class != "LabelRow"
      mf_result_chars <- .remove_empty_elements(mf_result_chars, is_not_label_rows)
      mf_result_numeric <- .make_numeric_char_mf(mf_result_chars)
      mf_result_chars <- as.data.frame(mf_result_chars)
      mf_result_numeric <- as.data.frame(mf_result_numeric)
      cond1 <- !setequal(dim(mf_result_chars), dim(cellvals_init))
      cond2 <- !setequal(dim(mf_result_numeric), dim(cellvals_init))
      if (cond1 || cond2) {
        stop(
          "The extracted numeric data.frame does not have the same dimension of the",
          " cell values extracted with cell_values(). This is a bug. Please report it."
        ) # nocov
      }

      colnames(mf_result_chars) <- colnames(cellvals_init)
      colnames(mf_result_numeric) <- colnames(cellvals_init)
      if (data_format == "strings") {
        cellvals <- mf_result_chars
        if (isTRUE(make_ard)) {
          stop("make_ard = TRUE is not compatible with data_format = 'strings'")
        }
      } else if (data_format == "numeric") {
        if (isTRUE(make_ard)) {
          cellvals <- .convert_to_character(mf_result_numeric)
        } else {
          cellvals <- mf_result_numeric
        }
      }
      diff_in_cellvals <- length(unlist(cellvals_init)) - length(unlist(cellvals))
      if (make_ard && abs(diff_in_cellvals) > 0) {
        warning_msg <- paste0(
          "Found ", abs(diff_in_cellvals), " cell values that differ from ",
          "printed values. This is possibly related to conditional formatting. "
        )

        # number of values difference mask between cellvals and cellvals_init (TRUE if different)
        dmc <- .lengths_with_nulls(unlist(cellvals, recursive = FALSE)) !=
          .lengths_with_nulls(unlist(cellvals_init, recursive = FALSE))
        dmc <- matrix(dmc, nrow = nrow(cellvals), ncol = ncol(cellvals))

        # Mainly used for debugging
        warning_msg <- if (verbose) { # beware that \n will break this (use make_row_df(tt)$self_extent for fix)
          selected_rows_to_print <- mf_strings(mf_tt)[-seq_len(mf_nlheader(mf_tt)), , drop = FALSE]
          selected_rows_to_print <- selected_rows_to_print[!make_row_df(tt)$node_class == "LabelRow", , drop = FALSE]
          selected_rows_to_print <- cbind(
            which(apply(dmc, 1, any, simplify = TRUE)),
            as.data.frame(selected_rows_to_print[apply(dmc, 1, any), , drop = FALSE])
          )
          colnames(selected_rows_to_print) <- c("row_number", "row_name", colnames(cellvals_init))
          paste0(
            warning_msg,
            "\n",
            "The following row names were modified: ",
            paste(selected_rows_to_print$row_name, sep = ", ", collapse = ", "),
            "\n"
          )
        } else {
          paste0(warning_msg, "To see the affected row names use `verbose = TRUE`.")
        }
        warning(warning_msg)
        cellvals[dmc] <- cellvals_init[dmc]
      }
    } else {
      cellvals <- cellvals_init
    }

    rdf <- make_row_df(tt)

    df <- rdf[, c("name", "label", "abs_rownumber", "path", "reprint_inds", "node_class")]
    # Removing initial root elements from path (out of the loop -> right maxlen)
    df$path <- lapply(df$path, .fix_raw_row_path,
      which_root_name = c("root", "rbind_root"),
      all = TRUE
    )

    # Correcting maxlen for even number of paths (only multianalysis diff table names)
    maxlen <- max(lengths(df$path))
    if (maxlen %% 2 != 0) {
      maxlen <- if (add_tbl_name_split) {
        maxlen + 1
      } else {
        maxlen - 1
      }
    }

    # Loop for metadata (path and details from make_row_df)
    metadf <- do.call(
      rbind.data.frame,
      lapply(
        seq_len(NROW(df)),
        function(ii) {
          handle_rdf_row(df[ii, ], maxlen = maxlen, add_tbl_name_split = add_tbl_name_split)
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

    # Fix for content rows analysis variable label
    if (any(ret$node_class == "ContentRow")) {
      where_to <- which(ret$node_class == "ContentRow")
      for (crow_i in where_to) {
        # For each Content row, extract the row split that is used as analysis variable
        tmp_tbl <- ret[crow_i, , drop = FALSE]
        na_labels <- lapply(tmp_tbl, is.na) %>% unlist(use.names = FALSE)
        group_to_take <- colnames(tmp_tbl[, !na_labels])
        group_to_take <- group_to_take[grep("^group[0-9]+$", group_to_take)]

        # Final assignment of each Content row to its correct analysis label
        ret$avar_name[crow_i] <- ret[[group_to_take[length(group_to_take)]]][crow_i]
      }
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

    # make_ard -----------------------------------------------------------------
    # ARD part for one stat per row
    if (make_ard) {
      cinfo_df <- col_info(tt)
      ci_coltree <- coltree(cinfo_df)
      column_split_names <- .get_column_split_name(ci_coltree) # used only in make_ard

      # Unnecessary columns
      ret_tmp <- ret[, !colnames(ret) %in% c("row_num", "is_group_summary", "node_class")]
      n_row_groups <- sapply(colnames(ret), function(x) {
        if (grepl("^group", x)) {
          # Extract the number after "group" using regex
          as.numeric(sub("group(\\d+).*", "\\1", x))
        } else {
          0 # Return 0 if no "group" is found
        }
      }) %>%
        max()

      # Indexes of real columns (visible in the output, but no row names)
      only_col_indexes <- seq(which(colnames(ret_tmp) == "label_name") + 1, ncol(ret_tmp))

      # Core row names
      col_label_rows <- grepl("<only_for_column_*", ret_tmp$avar_name)
      number_of_col_splits <- sum(grepl("<only_for_column_names>", ret_tmp$avar_name))
      core_row_names <- ret_tmp[!col_label_rows, -only_col_indexes]
      colnames_to_rename <- colnames(core_row_names) %in% c("avar_name", "row_name", "label_name")
      # instead of avar_name  row_name  label_name ("variable_label" is not present in ARDs)
      colnames(core_row_names)[colnames_to_rename] <- c("variable", "variable_level", "variable_label")

      # Adding stats_names if present
      raw_stat_names <- .get_stat_names_from_table(tt, add_labrows = keep_label_rows)
      cell_stat_names <- .make_df_from_raw_data(raw_stat_names, nr = nrow(tt), nc = ncol(tt))

      # Moving colnames to rows (flattening)
      ret_w_cols <- NULL
      # Looping on statistical columns
      for (col_i in only_col_indexes) {
        # Making row splits into row specifications (group1 group1_level)
        current_col_split_level <- unlist(ret_tmp[seq_len(number_of_col_splits), col_i], use.names = FALSE)
        col_split_names <- column_split_names[[1]][[1]] # cross section of the column split names (not values)
        more_than_one_name_in_csn <- sapply(col_split_names, length) > 1

        # Correction for cases where there is split_cols_by_multivar
        if (any(more_than_one_name_in_csn)) {
          col_split_names[more_than_one_name_in_csn] <- lapply(
            seq(sum(more_than_one_name_in_csn)),
            function(i) {
              paste0("multivar_split", i)
            }
          )
        }

        # Alternated association of split names and values (along with group split)
        flattened_cols_names <- .c_alternated(col_split_names, current_col_split_level)
        names(flattened_cols_names) <- .c_alternated(
          paste0("group", seq_along(col_split_names) + n_row_groups),
          paste0("group", seq_along(current_col_split_level) + n_row_groups, "_level")
        )

        if (n_row_groups > 0) {
          tmp_core_ret_by_col_i <- cbind(
            core_row_names[, seq(n_row_groups * 2)],
            t(data.frame(flattened_cols_names)),
            core_row_names[, -seq(n_row_groups * 2)],
            row.names = NULL
          )
        } else {
          tmp_core_ret_by_col_i <- cbind(
            t(data.frame(flattened_cols_names)),
            core_row_names,
            row.names = NULL
          )
        }

        # retrieving stat names and stats
        stat_name <- setNames(cell_stat_names[, col_i - min(only_col_indexes) + 1, drop = TRUE], NULL)
        stat <- setNames(ret_tmp[!col_label_rows, col_i, drop = TRUE], NULL)
        necessary_stat_lengths <- lapply(stat, length)
        stat[!lengths(stat) > 0] <- NA

        # Truncating or adding NA if stat names has more or less elements than stats
        stat_name <- lapply(seq_along(stat_name), function(sn_i) {
          unlist(stat_name[[sn_i]], use.names = FALSE)[seq_len(necessary_stat_lengths[[sn_i]])]
        })
        stat_name[!nzchar(stat_name)] <- NA
        stat_name[!lengths(stat_name) > 0] <- NA

        # unnesting stat_name and stat
        tmp_ret_by_col_i <- NULL
        for (row_i in seq_along(stat)) {
          tmp_ret_by_col_i <- rbind(
            tmp_ret_by_col_i,
            cbind(
              tmp_core_ret_by_col_i[row_i, ],
              stat_name = stat_name[[row_i]],
              stat = stat[[row_i]],
              row.names = NULL
            )
          )
        }

        ret_w_cols <- rbind(ret_w_cols, tmp_ret_by_col_i)
      }

      # If add_tbl_str_decimals is not present, we need to call the function again to keep precision
      add_tbl_str_decimals <- list(...)$add_tbl_str_decimals
      if (is.null(add_tbl_str_decimals) || isTRUE(add_tbl_str_decimals)) {
        # Trying to extract strings as numeric for comparison
        tryCatch(
          {
            stat_string_ret <- as_result_df(
              tt = tt, spec = spec, data_format = "numeric",
              make_ard = TRUE, add_tbl_str_decimals = FALSE, verbose = verbose
            )
            ret_w_cols <- cbind(ret_w_cols, "stat_string" = stat_string_ret$stat)
          },
          error = function(e) {
            warning("Could not add 'stat_string' column to the result data frame. Error: ", e$message)
          }
        )
      }

      ret <- ret_w_cols
    }

    # Simplify the result data frame
    out <- if (simplify) {
      .simplify_result_df(ret)
    } else {
      ret
    }

    # take out rownames
    rownames(out) <- NULL
  } else {
    # Applying specs
    out <- spec(tt, ...)
  }

  out
}

.lengths_with_nulls <- function(lst) {
  sapply(lst, function(x) if (is.null(x)) 1 else length(x))
}


# Helper function used to structure the raw values into a dataframe
.make_df_from_raw_data <- function(rawvals, nr, nc) {
  ## if the table has one row and multiple columns, sometimes the cell values returns a list of the cell values
  ## rather than a list of length 1 representing the single row. This is bad but may not be changeable
  ## at this point.
  if (nr == 1 && length(rawvals) > 1) {
    rawvals <- list(rawvals)
  }

  # Flatten the list of lists (rows) of cell values into a data frame
  cellvals <- as.data.frame(do.call(rbind, rawvals))

  if (nr == 1 && nc == 1) {
    if (length(unlist(rawvals)) > 1) { # This happens only with nr = nc = 1 for raw values
      cellvals <- as.data.frame(I(rawvals))
    }
    colnames(cellvals) <- names(rawvals)
  }

  row.names(cellvals) <- NULL
  cellvals
}

# Is there a better alternative?
.c_alternated <- function(v1, v2) {
  unlist(mapply(c, v1, v2, SIMPLIFY = FALSE))
}

# Amazing helper function to get the statistic names from row cells!
.get_stat_names_from_table <- function(tt, add_labrows = FALSE) {
  # omit_labrows # omit label rows
  rows <- collect_leaves(tt, incl.cont = TRUE, add.labrows = add_labrows)
  lapply(rows, function(ri) {
    lapply(row_cells(ri), obj_stat_names)
  })
}

# Helper function to get column split names
.get_column_split_name <- function(ci_coltree) {
  # ci stands for column information
  if (is(ci_coltree, "LayoutAxisTree")) {
    kids <- tree_children(ci_coltree)
    return(lapply(kids, .get_column_split_name))
  }

  lapply(pos_splits(tree_pos(ci_coltree)), function(x) {
    pl <- spl_payload(x)
    if (!is.null(pl)) { # it is null when all obs (1 column)
      pl
    } else {
      x@name
    }
  })
}

# Function that selects specific outputs from the result data frame
.simplify_result_df <- function(df) {
  col_df <- colnames(df)
  if (!all(c("label_name", "node_class") %in% col_df)) {
    stop("Please simplify the result data frame only when it has 'label_name' and 'node_class' columns.")
  }
  label_names_col <- which(col_df == "label_name")
  result_cols <- seq(which(col_df == "node_class") + 1, length(col_df))

  df[, c(label_names_col, result_cols)]
}

.remove_empty_elements <- function(char_df, is_not_label_rows) {
  if (is.null(dim(char_df))) {
    return(char_df[nzchar(char_df, keepNA = TRUE)])
  }
  rows_to_remove <- apply(char_df, 1, function(row_i) all(!nzchar(row_i, keepNA = TRUE)), simplify = TRUE)
  char_df[!rows_to_remove | is_not_label_rows, , drop = FALSE]
}

# Helper function to make the character matrix numeric
.make_numeric_char_mf <- function(char_df) {
  if (is.null(dim(char_df))) {
    ret <- lapply(char_df[[1]], function(x) {
      as.numeric(stringi::stri_extract_all(x, regex = "\\d+.\\d+|\\d+")[[1]])
    }) # keeps the list (single element) for data.frame
    return(I(ret))
  }

  ret <- apply(char_df, 2, function(col_i) {
    out <- lapply(
      stringi::stri_extract_all(col_i, regex = "\\d+.\\d+|\\d+"),
      as.numeric
    )
    if (all(dim(char_df) == c(1, 1)) && is.list(out[[1]])) {
      unlist(out, use.names = FALSE)
    } else {
      out
    }
  }, simplify = FALSE)

  do.call(cbind, ret)
}

make_result_df_md_colnames <- function(maxlen) {
  spllen <- floor((maxlen - 2) / 2)
  ret <- character()
  if (spllen > 0) {
    ret <- paste("group", rep(seq_len(spllen), each = 2), c("", "_level"), sep = "")
  }
  ret <- c(ret, c("avar_name", "row_name", "label_name", "row_num", "is_group_summary", "node_class"))
}

do_label_row <- function(rdfrow, maxlen, add_tbl_name_split = FALSE) {
  pth <- rdfrow$path[[1]]
  # Adjusting for the fact that we have two columns for each split
  extra_nas_from_splits <- floor((maxlen - length(pth)) / 2) * 2

  # Special cases with hidden labels
  if (length(pth) %% 2 == 1) {
    extra_nas_from_splits <- extra_nas_from_splits + 1
  } else {
    if (isTRUE(add_tbl_name_split)) {
      pth <- c("<analysis_spl_tbl_name>", pth)
      extra_nas_from_splits <- extra_nas_from_splits - 1
    } else {
      pth <- pth[-1]
      extra_nas_from_splits <- extra_nas_from_splits + 1
    }
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

do_data_row <- function(rdfrow, maxlen, add_tbl_name_split = FALSE) {
  pth <- rdfrow$path[[1]]
  pthlen <- length(pth)
  ## odd means we have a multi-analsysis step in the path, we do not want this in the result
  if (pthlen %% 2 == 1 && pthlen > 1) {
    # we remove the last element, as it is a fake split (tbl_name from analyse)
    # pth <- pth[-1 * (pthlen - 2)]
    if (isTRUE(add_tbl_name_split)) {
      pth <- c("<analysis_spl_tbl_name>", pth)
    } else {
      pth <- pth[-1]
    }
  }
  pthlen_new <- length(pth)
  if (pthlen_new == 1) {
    pthlen_new <- 3 # why?
  }
  c(
    as.list(pth[seq_len(pthlen_new - 2)]),
    replicate(ifelse((maxlen - pthlen_new) > 0, maxlen - pthlen_new, 0), list(NA_character_)),
    as.list(tail(pth, 2)),
    list(
      label_name = rdfrow$label,
      row_num = rdfrow$abs_rownumber,
      content = FALSE,
      node_class = rdfrow$node_class
    )
  )
}

deuniqify_path_elements <- function(path) {
  gsub("\\[[[:digit:]]+\\]$", "", path)
}

.fix_raw_row_path <- function(path, which_root_name = c("root", "rbind_root"), all = TRUE) {
  path <- deuniqify_path_elements(path)
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

handle_rdf_row <- function(rdfrow, maxlen, add_tbl_name_split = FALSE) {
  nclass <- rdfrow$node_class

  ret <- switch(nclass,
    LabelRow = do_label_row(rdfrow, maxlen, add_tbl_name_split = add_tbl_name_split),
    ContentRow = do_content_row(rdfrow, maxlen),
    DataRow = do_data_row(rdfrow, maxlen, add_tbl_name_split = add_tbl_name_split),
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
    ret
  } else {
    ret <- rbind(ret, do.call(cbind, lapply(tree_children(clyt), .get_formatted_colnames)))
    colnames(ret) <- NULL
    rownames(ret) <- NULL
    ret
  }
}

# Function to convert all elements to character while preserving structure
.convert_to_character <- function(df) {
  # Apply transformation to each column
  df_converted <- lapply(df, function(col) {
    if (is.list(col)) {
      # For columns with vector cells, convert each vector to a character vector
      I(lapply(col, as.character))
    } else {
      # For regular columns, directly convert to character
      as.character(col)
    }
  })
  # Return the transformed data frame
  data.frame(df_converted, stringsAsFactors = FALSE)
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
