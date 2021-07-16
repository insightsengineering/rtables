
### XXX is the stuff in this file correct or should we be exporting *formatted* values to
### meet the needs of consumers of this? Do we ened to support both?

#' Create Enriched flat value table with paths
#'
#'
#' This function creates a flat tabular file of cell values and
#' corresponding paths.
#'
#' List columns where at least one value has length > 1 are collapsed
#' to character vectors by collapsing the list element with \code{"|"}.
#'
#' @note There is currently no round-trip capability for this type of export.
#' You can read values exported this way back in via \code{import_from_tsv}
#' but you will receive only the data.frame version back, NOT a \code{TableTree}.
#'
#' @inheritParams gen_args
#' @param file character(1). The path of the file to written to or read from.
#' @param pathproc function. Internal detail,  not intended for use by end users.
#' @return \code{NULL} silently for \code{export_as_tsv}, a data.frame with
#' re-constituted list values for \code{export_as_tsv}.
#' @export
#' @rdname tsv_io
#' @importFrom utils write.table read.table

export_as_tsv <- function(tt, file = NULL, pathproc = collapse_path) {
    df <- path_enriched_df(tt, pathproc = pathproc)
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

path_enriched_df <- function(tt, pathproc = collapse_path) {
    rdf <- make_row_df(tt)
    cdf <- make_col_df(tt)
    cvs <- as.data.frame(do.call(rbind, cell_values(tt)))
    cvs <- as.data.frame(lapply(cvs, collapse_values))
    row.names(cvs) <- NULL
    colnames(cvs) <- pathproc(cdf$path)
    preppaths <- pathproc(rdf[rdf$node_class != "LabelRow",]$path)
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
export_as_txt <- function(tt, file = NULL, paginate = FALSE, ..., page_break = "\n\\s\n") {
    if(paginate)
        tbls <- paginate_table(tt, ...)
    else
        tbls <- list(tt)

    res <- paste(sapply(tbls, toString), collapse = page_break)
    
    if(!is.null(file))
        cat(res, file = file)
    else
        res
}


#' Export as PDF
#' 
#' The PDF output is based on the ASCII output created with `toString`
#' 
#' @inheritParams export_as_txt
#' @inheritParams grid::plotViewport
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
#' @importFrom grid textGrob
#' @export
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
#' export_as_pdf(tbl, file = tf,  lpp = 8)
#' }
#' 
export_as_pdf <- function(tt, 
                          file, width = 11.7, height = 8.3, # passed to pdf()
                          margins = c(4, 4, 4, 4), fontsize = 8,  # grid parameters
                          paginate = TRUE, ... # passed to paginate_table
) {
    stopifnot(tools::file_ext(file) != ".pdf")
    
    colwidths <- propose_column_widths(tt)
    tbls <- if (paginate)
        paginate_table(tt, ...)
    else 
        list(tt)
    
    stbls <- lapply(tbls, toString, widths = colwidths)
    
    gtbls <- lapply(stbls, function(txt) {
        textGrob(
            label = txt,
            x = unit(0, "npc"), y = unit(1, "npc"), 
            just = c("left", "top"),
            gp = gpar(fontsize = fontsize, fontfamily = "mono")
        )
    })
    
    npages <- length(gtbls)
    exceeds_width = rep(FALSE, npages)
    exceeds_height = rep(FALSE, npages)
    
    pdf(file = file, width = width, height = height)
    for (i in seq_along(gtbls)) {
        g <- gtbls[[i]]
    
        grid.newpage()
        pushViewport(plotViewport(margins = margins))
        
        if (convertHeight(grobHeight(g), "inches", valueOnly = TRUE) > 
            convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE)) {
            exceeds_height[i] <- TRUE
            message("height of page", i, "exceeds the available space")
        }
        if (convertWidth(grobWidth(g), "inches", valueOnly = TRUE) > 
            convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE)) {
            exceeds_width[i] <- TRUE
            message("width of page", i, "exceeds the available space")
        }
        
        grid.draw(g)
    }
    dev.off()
    
    list(file = file, npages = npages, exceeds_width = exceeds_width, exceeds_height = exceeds_height)
}
