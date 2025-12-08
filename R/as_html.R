insert_brs <- function(vec) {
  if (length(vec) == 1) {
    ret <- list(vec)
  } else {
    nout <- length(vec) * 2 - 1
    ret <- vector("list", nout)
    for (i in seq_along(vec)) {
      ret[[2 * i - 1]] <- vec[i]
      if (2 * i < nout) {
        ret[[2 * i]] <- tags$br()
      }
    }
  }
  ret
}

div_helper <- function(lst, class) {
  do.call(tags$div, c(list(class = paste(class, "rtables-container"), lst)))
}

#' Convert an `rtable` object to a `shiny.tag` HTML object
#'
#' The returned HTML object can be immediately used in `shiny` and `rmarkdown`.
#'
#' @param x (`VTableTree`)\cr a `TableTree` object.
#' @param class_table (`character`)\cr class for `table` tag.
#' @param class_tr (`character`)\cr class for `tr` tag.
#' @param class_th (`character`)\cr class for `th` tag.
#' @param width (`character`)\cr a string to indicate the desired width of the table. Common input formats include a
#'   percentage of the viewer window width (e.g. `"100%"`) or a distance value (e.g. `"300px"`). Defaults to `NULL`.
#' @param link_label (`character`)\cr link anchor label (not including `tab:` prefix) for the table.
#' @param bold (`character`)\cr elements in table output that should be bold. Options are `"main_title"`,
#'   `"subtitles"`, `"header"`, `"row_names"`, `"label_rows"`, and `"content_rows"` (which includes any non-label
#'   rows). Defaults to `"header"`.
#' @param header_sep_line (`flag`)\cr whether a black line should be printed to under the table header. Defaults
#'   to `TRUE`.
#' @param no_spaces_between_cells (`flag`)\cr whether spaces between table cells should be collapsed. Defaults
#'   to `FALSE`.
#' @param expand_newlines (`flag`)\cr Defaults to `FALSE`, relying on `html` output to solve newline characters (`\n`).
#'   Doing this keeps the structure of the cells but may depend on the output device.
#' @param round_type (`"iec"`, `"iec_mod"` or `"sas"`)\cr the type of rounding to perform.
#' See [round_fmt()] for details.
#'
#' @importFrom htmltools tags
#'
#' @return A `shiny.tag` object representing `x` in HTML.
#'
#' @examples
#' tbl <- rtable(
#'   header = LETTERS[1:3],
#'   format = "xx",
#'   rrow("r1", 1, 2, 3),
#'   rrow("r2", 4, 3, 2, indent = 1),
#'   rrow("r3", indent = 2)
#' )
#'
#' as_html(tbl)
#'
#' as_html(tbl, class_table = "table", class_tr = "row")
#'
#' as_html(tbl, bold = c("header", "row_names"))
#'
#' \dontrun{
#' Viewer(tbl)
#' }
#'
#' @export
as_html <- function(x,
                    width = NULL,
                    class_table = "table table-condensed table-hover",
                    class_tr = NULL,
                    class_th = NULL,
                    link_label = NULL,
                    bold = c("header"),
                    header_sep_line = TRUE,
                    no_spaces_between_cells = FALSE,
                    expand_newlines = FALSE,
                    round_type = if (is(x, "VTableTree")) obj_round_type(x) else valid_round_type) {
  if (is.null(x)) {
    return(tags$p("Empty Table"))
  }

  stopifnot(is(x, "VTableTree"))

  mat <- matrix_form(x, indent_rownames = TRUE, expand_newlines = expand_newlines, round_type = round_type)

  nlh <- mf_nlheader(mat)
  nc <- ncol(x) + 1
  nr <- length(mf_lgrouping(mat))

  # Structure is a list of lists with rows (one for each line grouping) and cols as dimensions
  cells <- matrix(rep(list(list()), (nr * nc)), ncol = nc)

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      curstrs <- mf_strings(mat)[i, j]
      curspn <- mf_spans(mat)[i, j]
      algn <- mf_aligns(mat)[i, j]

      inhdr <- i <= nlh
      tagfun <- if (inhdr) tags$th else tags$td
      cells[i, j][[1]] <- tagfun(
        class = if (inhdr) class_th else class_tr,
        style = paste0("text-align: ", algn, ";"),
        style = if (inhdr && !"header" %in% bold) "font-weight: normal;",
        style = if (i == nlh && header_sep_line) "border-bottom: 1px solid black;",
        colspan = if (curspn != 1) curspn,
        insert_brs(curstrs)
      )
    }
  }

  if (header_sep_line) {
    cells[nlh][[1]] <- htmltools::tagAppendAttributes(
      cells[nlh, 1][[1]],
      style = "border-bottom: 1px solid black;"
    )
  }

  # Create a map between line numbers and line groupings, adjusting abs_rownumber with nlh
  map <- data.frame(lines = seq_len(nr), abs_rownumber = mat$line_grouping)
  row_info_df <- data.frame(indent = mat$row_info$indent, abs_rownumber = mat$row_info$abs_rownumber + nlh)
  map <- merge(map, row_info_df, by = "abs_rownumber")

  # add indent values for headerlines
  map <- rbind(data.frame(abs_rownumber = 1:nlh, indent = 0, lines = 0), map)


  # Row labels style
  for (i in seq_len(nr)) {
    indent <- ifelse(any(map$lines == i), map$indent[map$lines == i][1], -1)

    # Apply indentation
    if (indent > 0) {
      cells[i, 1][[1]] <- htmltools::tagAppendAttributes(
        cells[i, 1][[1]],
        style = paste0("padding-left: ", indent * 3, "ch;")
      )
    }

    # Apply bold font weight if "row_names" is in 'bold'
    if ("row_names" %in% bold) {
      cells[i, 1][[1]] <- htmltools::tagAppendAttributes(
        cells[i, 1][[1]],
        style = "font-weight: bold;"
      )
    }
  }

  # label rows style
  if ("label_rows" %in% bold) {
    which_lbl_rows <- which(mat$row_info$node_class == "LabelRow")
    cells[which_lbl_rows + nlh, ] <- lapply(
      cells[which_lbl_rows + nlh, ],
      htmltools::tagAppendAttributes,
      style = "font-weight: bold;"
    )
  }

  # content rows style
  if ("content_rows" %in% bold) {
    which_cntnt_rows <- which(mat$row_info$node_class %in% c("ContentRow", "DataRow"))
    cells[which_cntnt_rows + nlh, ] <- lapply(
      cells[which_cntnt_rows + nlh, ],
      htmltools::tagAppendAttributes,
      style = "font-weight: bold;"
    )
  }

  if (any(!mat$display)) {
    # Check that expansion kept the same display info
    check_expansion <- c()
    for (ii in unique(mat$line_grouping)) {
      rows <- which(mat$line_grouping == ii)
      check_expansion <- c(
        check_expansion,
        apply(mat$display[rows, , drop = FALSE], 2, function(x) all(x) || all(!x))
      )
    }

    if (!all(check_expansion)) {
      stop(
        "Found that a group of rows have different display options even if ",
        "they belong to the same line group. This should not happen. Please ",
        "file an issue or report to the maintainers."
      ) # nocov
    }

    for (ii in unique(mat$line_grouping)) {
      rows <- which(mat$line_grouping == ii)
      should_display_col <- apply(mat$display[rows, , drop = FALSE], 2, any)
      cells[ii, !should_display_col] <- NA_integer_
    }
  }

  rows <- apply(cells, 1, function(row) {
    tags$tr(
      class = class_tr,
      style = "white-space: pre;",
      Filter(function(x) !identical(x, NA_integer_), row)
    )
  })

  hsep_line <- tags$hr(class = "solid")

  hdrtag <- div_helper(
    class = "rtables-titles-block",
    list(
      div_helper(
        class = "rtables-main-titles-block",
        lapply(main_title(x), if ("main_title" %in% bold) tags$b else tags$p,
          class = "rtables-main-title"
        )
      ),
      div_helper(
        class = "rtables-subtitles-block",
        lapply(subtitles(x), if ("subtitles" %in% bold) tags$b else tags$p,
          class = "rtables-subtitle"
        )
      )
    )
  )

  tabletag <- do.call(
    tags$table,
    c(
      rows,
      list(
        class = class_table,
        style = paste(
          if (no_spaces_between_cells) "border-collapse: collapse;",
          if (!is.null(width)) paste("width:", width)
        ),
        tags$caption(sprintf("(\\#tag:%s)", link_label),
          style = "caption-side: top;",
          .noWS = "after-begin"
        )
      )
    )
  )

  rfnotes <- div_helper(
    class = "rtables-ref-footnotes-block",
    lapply(mat$ref_footnotes, tags$p,
      class = "rtables-referential-footnote"
    )
  )

  mftr <- div_helper(
    class = "rtables-main-footers-block",
    lapply(main_footer(x), tags$p,
      class = "rtables-main-footer"
    )
  )

  pftr <- div_helper(
    class = "rtables-prov-footers-block",
    lapply(prov_footer(x), tags$p,
      class = "rtables-prov-footer"
    )
  )

  ## XXX this omits the divs entirely if they are empty. Do we want that or do
  ## we want them to be there but empty??
  ftrlst <- list(
    if (length(mat$ref_footnotes) > 0) rfnotes,
    if (length(mat$ref_footnotes) > 0) hsep_line,
    if (length(main_footer(x)) > 0) mftr,
    if (length(main_footer(x)) > 0 && length(prov_footer(x)) > 0) tags$br(), # line break
    if (length(prov_footer(x)) > 0) pftr
  )

  if (!is.null(unlist(ftrlst))) ftrlst <- c(list(hsep_line), ftrlst)
  ftrlst <- ftrlst[!vapply(ftrlst, is.null, TRUE)]

  ftrtag <- div_helper(
    class = "rtables-footers-block",
    ftrlst
  )

  div_helper(
    class = "rtables-all-parts-block",
    list(
      tags$head(
        tags$style(
          ".rtables-all-parts-block table tr {    border-top: 1px solid #ddd;}"
        )
      ),
      hdrtag,
      tabletag,
      ftrtag
    )
  )
}
