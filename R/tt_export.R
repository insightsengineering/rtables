#' @importFrom tools file_ext
NULL
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
    if(!is.list(colvals)) ## || all(vapply(colvals, length, 1L) == 1))
        return(colvals)
    else if(all(vapply(colvals, length, 1L) == 1))
        return(unlist(colvals))
    vapply(colvals, paste, "", collapse = .collapse_char)
}

#' Transform TableTree object to Path-Enriched data.frame
#'
#' @inheritParams gen_args
#' @param path_fun function. Function to transform paths into single-string
#'   row/column names.
#' @param value_fun function. Function to transform cell values into cells of
#'   the data.frame. Defaults to \code{collapse_values} which creates strings
#'   where multi-valued cells are collapsed together, separated by \code{|}.
#' @export
#' @return A data frame of \code{tt}'s cell values (processed by
#'   \code{value_fun}, with columns named by the full column paths (processed by
#'   \code{path_fun} and an additional \code{row_path} column with the row paths
#'   (processed by by \code{path_fun}).
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
    preppaths <- path_fun(rdf[rdf$node_class != "LabelRow", ]$path)
    cbind.data.frame(row_path = preppaths, cvs)

}
