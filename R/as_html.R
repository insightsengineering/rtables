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
#' @param class_td class for `td` tag
#' @param class_th class for `th` tag
#' @param width width
#' @param link_label link anchor label (not including \code{tab:} prefix) for the table.
#' @param bold_main_title whether the main title should be in bold. Defaults to `FALSE`.
#'
#' @return A \code{shiny.tag} object representing \code{x} in HTML.
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
#' as_html(tbl, class_td = "aaa")
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
                    class_td = NULL,
                    class_th = NULL,
                    link_label = NULL,
                    bold_main_title = FALSE) {
  if (is.null(x)) {
    return(tags$p("Empty Table"))
  }

  stopifnot(is(x, "VTableTree"))

  mat <- matrix_form(x)

  nrh <- mf_nrheader(mat)
  nc <- ncol(x) + 1

  # Structure is a list of lists with rows (one for each line grouping) and cols as dimensions
  cells <- matrix(rep(list(list()), (nrh + nrow(x)) * (nc)), ncol = nc)

  for (i in unique(mat$line_grouping)) {
    rows <- which(mat$line_grouping == i)
    for (j in seq_len(ncol(mat$strings))) {
      curstrs <- mat$strings[rows, j]
      curspans <- mat$spans[rows, j]
      curaligns <- mat$aligns[rows, j]

      curspn <- unique(curspans)
      stopifnot(length(curspn) == 1)
      inhdr <- i <= nrh
      tagfun <- if (inhdr) tags$th else tags$td
      algn <- unique(curaligns)
      stopifnot(length(algn) == 1)
      cells[i, j][[1]] <- tagfun(
        class = if (inhdr) class_th else class_tr,
        class = if (j > 1 || i > nrh) paste0("text-", algn),
        colspan = if (curspn != 1) curspn,
        insert_brs(curstrs)
      )
    }
  }

  ## special casing hax for top_left. We probably want to do this better someday
  cells[1:nrh, 1] <- mapply(
    FUN = function(x, algn) {
      tags$th(x, class = class_th, style = "white-space:pre;")
    },
    x = mat$strings[1:nrh, 1],
    algn = mat$aligns[1:nrh, 1],
    SIMPLIFY = FALSE
  )

  # indent row names
  for (i in seq_len(nrow(x))) {
    indent <- mat$row_info$indent[i]
    if (indent > 0) {
      cells[i + nrh, 1][[1]] <- htmltools::tagAppendAttributes(cells[i + nrh, 1][[1]],
        style = paste0("padding-left: ", indent * 3, "ch")
      )
    }
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
      style = "white-space:pre;",
      Filter(function(x) !identical(x, NA_integer_), row)
    )
  })

  hsep_line <- tags$hr(class = "solid")

  hdrtag <- div_helper(
    class = "rtables-titles-block",
    list(
      div_helper(
        class = "rtables-main-titles-block",
        lapply(main_title(x), if (bold_main_title) tags$b else tags$p,
          class = "rtables-main-title"
        )
      ),
      div_helper(
        class = "rtables-subtitles-block",
        lapply(subtitles(x), tags$p,
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
        tags$caption(sprintf("(\\#tag:%s)", link_label),
          style = "caption-side:top;",
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
    if (length(main_footer(x)) > 0 && length(prov_footer(x)) > 0) tags$br(),
    if (length(prov_footer(x)) > 0) pftr
  )

  if (length(ftrlst) > 0) ftrlst <- c(list(hsep_line), ftrlst)
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
