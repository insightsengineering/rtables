#' Change indentation of all `rrows` in an `rtable`
#'
#' Change indentation of all `rrows` in an `rtable`
#'
#' @param x \code{\link{rtable}} object
#' @param by integer to increase indentation of rows. Can be negative. If final indentation is smaller than 0 then the
#'   indentation is set to 0.
#'
#' @export
#' @return \code{x} with its indent modifier incremented by \code{by}.
#' @examples
#' is_setosa <- iris$Species == "setosa"
#' m_tbl <- rtable(
#'   header = rheader(
#'     rrow(row.name = NULL, rcell("Sepal.Length", colspan = 2), rcell("Petal.Length", colspan = 2)),
#'     rrow(NULL, "mean", "median", "mean", "median")
#'   ),
#'   rrow(
#'     row.name = "All Species",
#'     mean(iris$Sepal.Length), median(iris$Sepal.Length),
#'     mean(iris$Petal.Length), median(iris$Petal.Length),
#'     format = "xx.xx"
#'   ),
#'   rrow(
#'     row.name = "Setosa",
#'     mean(iris$Sepal.Length[is_setosa]), median(iris$Sepal.Length[is_setosa]),
#'     mean(iris$Petal.Length[is_setosa]), median(iris$Petal.Length[is_setosa]),
#'     format = "xx.xx"
#'   )
#' )
#' indent(m_tbl)
#' indent(m_tbl, 2)
#'
indent <- function(x, by = 1) {
  if (nrow(x) == 0 || by == 0) {
    return(x)
  }

  indent_mod(x) <- indent_mod(x) + by
  x
}

#' Clear All Indent Mods from a Table
#' @inheritParams gen_args
#' @return The same class as \code{tt}, with all indent mods set to zero.
#' @examples
#' lyt1 <- basic_table() %>%
#'   summarize_row_groups("STUDYID", label_fstr = "overall summary") %>%
#'   split_rows_by("AEBODSYS", child_labels = "visible") %>%
#'   summarize_row_groups("STUDYID", label_fstr = "subgroup summary") %>%
#'   analyze("AGE", indent_mod = -1L)
#'
#' tbl1 <- build_table(lyt1, ex_adae)
#' tbl1
#' clear_indent_mods(tbl1)
#' @export
#' @rdname clear_imods
setGeneric("clear_indent_mods", function(tt) standardGeneric("clear_indent_mods"))
#' @export
#' @rdname clear_imods
setMethod(
  "clear_indent_mods", "VTableTree",
  function(tt) {
    ct <- content_table(tt)
    if (!is.null(ct)) {
      content_table(tt) <- clear_indent_mods(ct)
    }
    tree_children(tt) <- lapply(tree_children(tt), clear_indent_mods)
    indent_mod(tt) <- 0L
    tt
  }
)
#' @export
#' @rdname clear_imods
setMethod(
  "clear_indent_mods", "TableRow",
  function(tt) {
    indent_mod(tt) <- 0L
    tt
  }
)
