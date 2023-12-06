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

#' Convert an `rtable` object to a `shiny.tag` html object
#'
#' The returned `html` object can be immediately used in `shiny` and `rmarkdown`.
#'
#' @param x `rtable` object
#' @param class_table class for `table` tag
#' @param class_tr class for `tr` tag
#' @param class_th class for `th` tag
#' @param width a string to indicate the desired width of the table. Common input formats include a 
#'   percentage of the viewer window width (e.g. `"100%"`) or a distance value (e.g. `"300px"`). 
#'   Defaults to `NULL`.
#' @param link_label link anchor label (not including `tab:` prefix) for the table.
#' @param bold elements in table output that should be bold. Options are `"main_title"`, `"subtitles"`,
#'   `"header"`, `"row_names"`, `"label_rows"`, and `"content_rows"` (which includes any non-label rows).
#'   Defaults to `"header"`.
#' @param header_sep_line whether a black line should be printed to under the table header. Defaults to `TRUE`.
#' @param no_spaces_between_cells whether spaces between table cells should be collapsed. Defaults to `FALSE`.
#'
#' @return A `shiny.tag` object representing `x` in HTML.
#'
#' @examples
#'
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
#' @importFrom htmltools tags
#' @export
as_html <- function(x,
                    width = NULL,
                    class_table = "table table-condensed table-hover",
                    class_tr = NULL,
                    class_th = NULL,
                    link_label = NULL,
                    bold = c("header"),
                    header_sep_line = TRUE,
                    no_spaces_between_cells = FALSE) {
  if (is.null(x)) {
    return(tags$p("Empty Table"))
  }

  stopifnot(is(x, "VTableTree"))

  mat <- matrix_form(x)

  nlh <- mf_nlheader(mat)
  nc <- ncol(x) + 1

  # Structure is a list of lists with rows (one for each line grouping) and cols as dimensions
  cells <- matrix(rep(list(list()), (nlh + nrow(x)) * (nc)), ncol = nc)

  for (i in seq_len(nrow(mat$strings))) {
    for (j in seq_len(ncol(mat$strings))) {
      curstrs <- mat$strings[i, j]
      curspn <- mat$spans[i, j]
      algn <- mat$aligns[i, j]

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

  # row labels style
  for (i in seq_len(nrow(x))) {
    indent <- mat$row_info$indent[i]
    if (indent > 0) { # indentation
      cells[i + nlh, 1][[1]] <- htmltools::tagAppendAttributes(cells[i + nlh, 1][[1]],
        style = paste0("padding-left: ", indent * 3, "ch;")
      )
    }
    if ("row_names" %in% bold) { # font weight
      cells[i + nlh, 1][[1]] <- htmltools::tagAppendAttributes(
        cells[i + nlh, 1][[1]],
        style = paste0("font-weight: bold;")
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
      hdrtag,
      tabletag,
      ftrtag
    )
  )
}
