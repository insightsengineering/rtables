
### XXX is the stuff in this file correct or should we be exporting *formatted* values to
### meet the needs of consumers of this? Do we ened to support both?

#' Create Enriched flat value table with paths
#'
#'
#' This function creates a flat tabular file of cell values and
#' corresponding paths via \code{\link{path_enriched_df}}. I then
#' writes that data.frame out as a tsv file.
#'
#' By default (ie when \code{value_func} is not specified,
#' List columns where at least one value has length > 1 are collapsed
#' to character vectors by collapsing the list element with \code{"|"}.
#'
#' @note There is currently no round-trip capability for this type of export.
#' You can read values exported this way back in via \code{import_from_tsv}
#' but you will receive only the data.frame version back, NOT a \code{TableTree}.
#'
#' @inheritParams gen_args
#' @param file character(1). The path of the file to written to or read from.
#' @inheritParams path_enriched_df
#' @return \code{NULL} silently for \code{export_as_tsv}, a data.frame with
#' re-constituted list values for \code{export_as_tsv}.
#' @export
#' @rdname tsv_io
#' @importFrom utils write.table read.table

export_as_tsv <- function(tt, file = NULL, path_fun = collapse_path,
                          value_fun = collapse_values) {
    df <- path_enriched_df(tt, path_fun = path_fun, value_fun = value_fun)
    write.table(df, file, sep = "\t")
}


.collapse_char <- "|"
.collapse_char_esc <- "\\|"

##' @export
##' @rdname tsv_io
import_from_tsv <- function(file) {
    rawdf <- read.table(file, header = TRUE, sep = "\t")
    as.data.frame(lapply(rawdf,
                         function(col) {
        if(!any(grepl(.collapse_char, col, fixed = TRUE)))
            col
        else
            I(strsplit(col, split = .collapse_char_esc))
    }))


}

collapse_path <- function(paths) {
    if(is.list(paths))
        return(vapply(paths, collapse_path, ""))
    paste(paths, collapse = .collapse_char)
}

collapse_values <- function(colvals) {
    if(!is.list(colvals) || all(vapply(colvals, length, 1L) == 1))
        return(colvals)
    vapply(colvals, paste, "", collapse = .collapse_char)
}

#' Transform TableTree object to Path-Enriched data.frame
#'
#' @inheritParams gen_args
#' @param path_fun function. Function to transform paths into single-string row/column names.
#' @param value_fun function. Functiont to transform cell values into cells of the data.frame. Defaults to \code{collapse_values} which
#' creates strings where multi-valued cells are collapsed together, separated by \code{|}.
#' @export
#' @return A data frame of \code{tt}'s cell values (processed by \code{value_fun},
#' with columns named by the full column
#' paths (processed by \code{path_fun} and an additional \code{row_path} column
#' with the row paths (processed by by \code{path_fun}).
#' @examples
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#' path_enriched_df(tbl)

path_enriched_df <- function(tt, path_fun = collapse_path, value_fun = collapse_values) {
    rdf <- make_row_df(tt)
    cdf <- make_col_df(tt)
    cvs <- as.data.frame(do.call(rbind, cell_values(tt)))
    cvs <- as.data.frame(lapply(cvs, value_fun))
    row.names(cvs) <- NULL
    colnames(cvs) <- path_fun(cdf$path)
    preppaths <- path_fun(rdf[rdf$node_class != "LabelRow",]$path)
    cbind.data.frame(row_path = preppaths, cvs)

}

#' Export as plain text with page break symbol
#'
#' @inheritParams gen_args
#' @param file character(1). File to write.
#' @param paginate logical(1). Should \code{tt} be paginated before writing the file.
#' @param \dots Passed directly to \code{\link{paginate_table}}
#' @param page_break character(1). Page break symbol (defaults to outputting \code{"\\s"}).
#' @return \code{file} (this function is called for the side effect of writing the file.
#'
#'
#' @export
#'
#' @seealso `export_as_pdf`
#'
#' @examples
#'
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
export_as_txt <- function(tt, file = NULL, paginate = FALSE, ..., page_break = "\\s\\n") {

    colwidths <- propose_column_widths(tt)

    if(paginate) {
        tbls <- paginate_table(tt, ...)
    } else {
        tbls <- list(tt)
    }

    res <- paste(sapply(tbls, toString, widths = colwidths), collapse = page_break)

    if(!is.null(file))
        cat(res, file = file)
    else
        res
}


#' Create a FlexTable object representing an rtables TableTree
#'
#' @inheritParams gen_args
#' @return a flextable object
#' @export
#' @examples
#' analysisfun <- function(x, ...) {
#'     in_rows(row1 = 5,
#'             row2 = c(1, 2),
#'             .row_footnotes = list(row1 = "row 1 - row footnote"),
#'             .cell_footnotes = list(row2 = "row 2 - cell footnote"))
#' }
#'
#' lyt <- basic_table(title = "Title says Whaaaat", subtitles = "Oh, ok.",
#'                main_footer = "ha HA! Footer!") %>%
#' split_cols_by("ARM") %>%
#' analyze("AGE", afun = analysisfun)
#'
#' tbl <-  build_table(lyt, ex_adsl)
#' ft <- tt_to_flextable(tbl)
#' ft

tt_to_flextable <- function(tt) {
    if(!requireNamespace("flextable") || !requireNamespace("officer")) {
        stop("this function requires the flextable and officer packages. Please install them if you wish to use it")
    }
    matform <- matrix_form(tt, indent_rownames = TRUE)

    hnum <- attr(matform, "nrow_header")

    content <- as.data.frame(matform$strings[-(1:hnum), , drop = FALSE])

    rdf <- make_row_df(tt)

    hdr <- matform$strings[1:hnum, , drop = FALSE]

    flx <- flextable::qflextable(content)

    flx <- flextable::set_header_labels(flx, values = setNames(as.vector(hdr[hnum, , drop = TRUE]), names(content)))

    if(hnum > 1) {
        for(i in (hnum-1):1) {
            sel <- spans_to_viscell(matform$spans[i,])
            flx <- flextable::add_header_row(flx, top=TRUE, values = as.vector(hdr[i,sel]),
                                  colwidths = as.integer(matform$spans[i,sel]))
        }

    }
    flx <- flextable::align(flx, j = 2:(NCOL(tt) +1), align = "center", part = "header")
    flx <- flextable::align(flx, j = 2:(NCOL(tt) +1),  align = "center", part = "body")
    for(i in seq_len(NROW(tt))) {
        flx <- flextable::padding(flx, i = i, j = 1, padding.left = 10*rdf$indent[[i]])
    }
    if(length(matform$ref_footnotes) >0) {
        flx <- flextable::add_footer_lines(flx, values = matform$ref_footnotes)
    }

    if(length(all_titles(tt)) > 0) {
        flx <- flextable::hline(flx, i = 1L, border = officer::fp_border(), part = "header")
        ## rev is because add_header_lines(top=TRUE) seems to put them in backwards!!! AAHHHH
        flx <- flextable::add_header_lines(flx, values = rev(all_titles(tt)), top = TRUE)
    }

    if(length(all_footers(tt)) > 0) {
        flx <- flextable::hline(flx, i = length(matform$ref_footnotes), border = officer::fp_border(), part = "footer")
        flx <- flextable::add_footer_lines(flx, values = all_footers(tt))
    }

    flextable::set_table_properties(flx, layout = "autofit")

}


#' Export as PDF
#'
#' The PDF output is based on the ASCII output created with `toString`
#'
#' @inheritParams export_as_txt
#' @inheritParams grid::plotViewport
#' @inheritParams paginate_table
#' @param file file to write, must have `.pdf` extension
#' @param width the width and height of the graphics region in inches
#' @param height the width and height of the graphics region in inches
#' @param fontsize the size of text (in points)
#' @param ... arguments passed on to `paginate_table`
#'
#' @importFrom grDevices pdf
#' @importFrom grid textGrob grid.newpage gpar pushViewport plotViewport unit grid.draw
#'   convertWidth convertHeight grobHeight grobWidth
#'
#' @seealso `export_as_txt`
#'
#'
#' @importFrom grid textGrob get.gpar
#' @importFrom grDevices dev.off
#' @export
#'
#' @examples
#' lyt <- basic_table(title = ) %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2", "COUNTRY"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#'
#' \dontrun{
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(tab, file = tf, height = 4)
#' tf <- tempfile(fileext = ".pdf")
#' export_as_pdf(tbl, file = tf, lpp = 8)
#' }
#'
export_as_pdf <- function(tt,
                          file, width = 11.7, height = 8.3, # passed to pdf()
                          margins = c(4, 4, 4, 4), fontsize = 8,  # grid parameters
                          paginate = TRUE, lpp = NULL, ... # passed to paginate_table
) {
    stopifnot(tools::file_ext(file) != ".pdf")

    gp_plot <- gpar(fontsize = fontsize, fontfamily = "mono")

    pdf(file = file, width = width, height = height)
    grid.newpage()
    pushViewport(plotViewport(margins = margins, gp = gp_plot))

    colwidths <- propose_column_widths(tt)
    tbls <- if (paginate) {

        if (is.null(lpp)) {
            cur_gpar <-  get.gpar()
            lpp <- floor(convertHeight(unit(1, "npc"), "lines", valueOnly = TRUE) /
                             (cur_gpar$cex * cur_gpar$lineheight))
        }

        paginate_table(tt, lpp = lpp, ...)
    } else {
        list(tt)
    }

    stbls <- lapply(lapply(tbls, toString, widths = colwidths), function(xi) substr(xi, 1, nchar(xi) - nchar("\n")))

    gtbls <- lapply(stbls, function(txt) {
        textGrob(
            label = txt,
            x = unit(0, "npc"), y = unit(1, "npc"),
            just = c("left", "top")
        )
    })

    npages <- length(gtbls)
    exceeds_width = rep(FALSE, npages)
    exceeds_height = rep(FALSE, npages)

    for (i in seq_along(gtbls)) {
        g <- gtbls[[i]]

        if (i > 1) {
            grid.newpage()
            pushViewport(plotViewport(margins = margins, gp = gp_plot))
        }

        if (convertHeight(grobHeight(g), "inches", valueOnly = TRUE) >
            convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)) {
            exceeds_height[i] <- TRUE
            warning("height of page ", i, " exceeds the available space")
        }
        if (convertWidth(grobWidth(g), "inches", valueOnly = TRUE) >
            convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)) {
            exceeds_width[i] <- TRUE
            warning("width of page ", i, " exceeds the available space")
        }

        grid.draw(g)
    }
    dev.off()

    list(file = file, npages = npages, exceeds_width = exceeds_width, exceeds_height = exceeds_height, lpp = lpp)
}
