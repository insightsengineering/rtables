
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

#' export as plain text with page break symbol
#' @inheritParams gen_args
#' @param file character(1). File to write.
#' @param paginate logical(1). Should \code{tt} be paginated before writing the file.
#' @param \dots Passed directly to \code{\link{paginate_table}}
#' @param page_break character(1). Page break symbol (defualts to outputting \code{"\\s"}).
#' @return \code{file} (this function is called for the side effect of writing the file.
#' @export
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("AGE", "BMRKR2"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#' ## this just displays it
#' export_as_txt(tbl, file = NULL)
#' \dontrun{
#' tf <- tempfile(file.ext = ".txt")
#' export_as_txt(tt, file = tf)
#' }
export_as_txt <- function(tt, file = NULL, paginate = FALSE, ..., page_break = "\\s") {
    if(paginate)
        tbls <- paginate_table(tt, ...)
    else
        tbls <- list(tt)

    ## toString seems to take care of newline?
    res <- paste(sapply(tbls, toString),
                 collapse = paste0(page_break, "\n"))
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
