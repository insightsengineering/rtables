#' Change indentation of all `rrows` in an `rtable`
#'
#' Change indentation of all `rrows` in an `rtable`
#'
#' @param x (`TableTree`)\cr an `rtable` object.
#' @param by (`integer`)\cr number to increase indentation of rows by. Can be negative. If final indentation is
#'   less than 0, the indentation is set to 0.
#'
#' @return `x` with its indent modifier incremented by `by`.
#'
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
#' @export
indent <- function(x, by = 1) {
  if (nrow(x) == 0 || by == 0) {
    return(x)
  }

  indent_mod(x) <- indent_mod(x) + by
  x
}

#' Clear all indent modifiers from a table
#'
#' @inheritParams gen_args
#'
#' @return The same class as `tt`, with all indent modifiers set to zero.
#'
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
#'
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
