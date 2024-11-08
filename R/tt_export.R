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
#' @param file (`string`)\cr the path of the file to written to or read from.
#' @param sep (`string`)\cr defaults to `\t`. See [utils::write.table()] for more details.
#' @param ... (`any`)\cr additional arguments to be passed to [utils::write.table()].
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
                          value_fun = collapse_values, sep = "\t", ...) {
  df <- path_enriched_df(tt, path_fun = path_fun, value_fun = value_fun)
  write.table(df, file, sep = sep, ...)
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
# txt (formatters) --------------------------------------------------------------------
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

# pdf (formatters) ----------------------------------------------------------
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


