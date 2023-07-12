

# paths summary ----

#' @rdname make_col_row_df
#'
#' @title Return List with Table Row/Col Paths
#'
#' @param x an rtable object
#'
#' @examples
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("SEX", "AGE"))
#'
#' tbl <- build_table(lyt, ex_adsl)
#' tbl
#'
#' row_paths(tbl)
#' col_paths(tbl)
#'
#' cell_values(tbl, c("AGE", "Mean"),  c("ARM", "B: Placebo"))
#'
#' @return a list of paths to each row/column within \code{x}
#'
#' @seealso [cell_values()], [`fnotes_at_path<-`],
#'   [row_paths_summary()], [col_paths_summary()]
#'
#' @export
row_paths <- function(x) {
  stopifnot(is_rtable(x))
  make_row_df(x, visible_only = TRUE)$path
}


#' @rdname make_col_row_df
#' @export
col_paths <- function(x) {
    if(!is(coltree(x), "LayoutColTree"))
        stop("I don't know how to extract the column paths from an object of class ", class(x))
  make_col_df(x, visible_only = TRUE)$path
}


#' Print Row/Col Paths Summary
#'
#' @param x an rtable object
#'
#' @export
#' @return A data.frame summarizing the row- or column-structure of \code{x}.
#' @examples
#'
#' library(dplyr)
#'
#' ex_adsl_MF <- ex_adsl %>% filter(SEX %in% c("M", "F"))
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_cols_by("SEX", split_fun = drop_split_levels) %>%
#'   analyze(c("AGE", "BMRKR2"))
#'
#' tbl <- build_table(lyt, ex_adsl_MF)
#' tbl
#'
#' df <- row_paths_summary(tbl)
#' df
#'
#' col_paths_summary(tbl)
#'
#' # manually constructed table
#' tbl2 <- rtable(
#'    rheader(
#'      rrow("row 1", rcell("a", colspan = 2),
#'      rcell("b", colspan = 2)
#'    ),
#'    rrow("h2", "a", "b", "c", "d")),
#'    rrow("r1", 1, 2, 1, 2), rrow("r2", 3, 4, 2,1)
#' )
#' col_paths_summary(tbl2)
row_paths_summary <- function(x) {
  stopifnot(is_rtable(x))

  if (nrow(x) == 0)
    return("rowname     node_class       path\n---------------------\n")

  pagdf <- make_row_df(x, visible_only = TRUE)
  row.names(pagdf) <- NULL

  mat <- rbind(
    c("rowname", "node_class", "path"),
    t(apply(pagdf, 1, function(xi) {
      c(
        indent_string(xi$label, xi$indent),
        xi$node_class,
        paste(xi$path, collapse = ", ")
      )
    }))
  )

  txt <- mat_as_string(mat)
  cat(txt)
  cat("\n")

  invisible(pagdf[, c("label", "indent", "node_class", "path")])
}


#' @rdname row_paths_summary
#' @export
col_paths_summary <- function(x) {
  stopifnot(is_rtable(x))

  pagdf <- make_col_df(x, visible_only = FALSE)
  row.names(pagdf) <- NULL

  mat <- rbind(
    c("label", "path"),
    t(apply(pagdf, 1, function(xi) {
      c(
        indent_string(xi$label, floor(length(xi$path) / 2 - 1)),
        paste(xi$path, collapse = ", ")
      )
    }))
  )

  txt <- mat_as_string(mat)
  cat(txt)
  cat("\n")

  invisible(pagdf[, c("label", "path")])
}


# Rows ----
# . Summarize Rows ----



# summarize_row_df <-
#     function(name,
#              label,
#              indent,
#              depth,
#              rowtype,
#              indent_mod,
#              level) {
#         data.frame(
#             name = name,
#             label = label,
#             indent = indent,
#             depth = level,
#             rowtype = rowtype,
#             indent_mod = indent_mod,
#             level = level,
#             stringsAsFactors = FALSE
#         )
#     }

summarize_row_df_unclassed <- function(name,
                                       label,
                                       indent,
                                       depth,
                                       rowtype,
                                       indent_mod,
                                       level,
                                       ## footnotes = list()
                                       num_row_refs = 0L,
                                       num_cell_refs = 0L) {
    list(name = name,
         label = label,
         indent = indent,
         depth = level,
         rowtype = rowtype,
         indent_mod = indent_mod,
         level = level,
         num_row_refs = num_row_refs,
         num_cell_refs = num_cell_refs)
    ##  footnotes = footnotes)##,
            ## stringsAsFactors = FALSE)
}

srow_df_names <- names(summarize_row_df_unclassed("hi", "hi", 0L, 0L, "hi", 0L, 0L))

summarize_row_df_empty <- NULL

fast_rsummry_bind <- function(lst) {
    if(length(lst) == 0) {
        return(summarize_row_df_empty)
    } else if(length(lst) == 1L) {
        return(as.data.frame(lst[[1]]))
    } else {
        res <- lapply(seq_along(srow_df_names),
                 function(i) {
                     do.call(c, lapply(lst, function(x) {
                         if(length(x) > 0) x[[i]] else x
                     }))
                 })
        names(res) <- srow_df_names
        res <- as.data.frame(res)
    }
    res
}



#' summarize_rows
#'
#' `summarize_rows` is deprecated in favor of `make_row_df`.
#' @param obj `VTableTree`.
#' @return A data.frame summarizing the rows in \code{obj}.
#' @export
summarize_rows <- function(obj) {
    .Deprecated("make_row_df")
    rows <- summarize_rows_inner(obj, 0, 0)
    fast_rsummry_bind(rows)
}

#' Summarize Rows
#'
#' @inheritParams gen_args
#' @param depth numeric(1). Depth.
#' @param indent numeric(1). Indent.
#'
#' @examples
#'
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary),
#'           format = "xx.xx")
#'
#' tbl <- build_table(lyt, iris2)

#' @rdname int_methods
setGeneric("summarize_rows_inner", function(obj, depth = 0, indent = 0)
    standardGeneric("summarize_rows_inner"))
#' @rdname int_methods
setMethod("summarize_rows_inner", "TableTree",
          function(obj, depth = 0, indent = 0) {

    indent <- max(0L, indent + indent_mod(obj))

    lr <- summarize_rows_inner(tt_labelrow(obj), depth, indent)
    if(!is.null(lr))
        ret <- list(lr)
    else
        ret <- list()

    indent <- indent + (!is.null(lr))

    ctab <- content_table(obj)
    if(NROW(ctab)) {
        ct <- summarize_rows_inner(ctab, depth = depth,
                                   indent = indent + indent_mod(ctab))
        ret <- c(ret, ct)
        indent <- indent + (length(ct) > 0) * (1 + indent_mod(ctab))
    }

    kids <- tree_children(obj)
    els <- lapply(tree_children(obj), summarize_rows_inner,
                 depth = depth + 1, indent = indent)
    if(!are(kids, "TableRow")) {
        if(!are(kids, "VTableTree")) {
            ## hatchet job of a hack, wrap em just so we can unlist em all at
            ## the same level
            rowinds <- vapply(kids, is, NA, class2 = "TableRow")
            els[rowinds] <- lapply(els[rowinds], function(x) list(x))
        }
        els <- unlist(els, recursive = FALSE)
    }
    ret <- c(ret, els)
    ret
    ## df <- do.call(rbind, c(list(lr), list(ct), els))

            ## row.names(df) <- NULL
            ## df

          })
#' @rdname int_methods
setMethod("summarize_rows_inner", "ElementaryTable",
          function(obj, depth = 0, indent = 0) {

    indent <- max(0L, indent + indent_mod(obj))
    lr <- summarize_rows_inner(tt_labelrow(obj), depth, indent)
    if(!is.null(lr)) {
        ret <- list(lr)
        indent <- indent + 1
    } else {
        ret <- list()
    }

    els <- lapply(tree_children(obj), summarize_rows_inner, depth = depth + 1,
                  indent = indent)

    c(ret, els)
            ## df <- do.call(rbind, c(list(lr), els))
            ## row.names(df) <- NULL
            ## df

          })
.num_cell_refs <- function(tr) {
    sum(vapply(cell_footnotes(tr),
               function(cfn) length(cfn) > 0,
               FALSE))
}

#' @rdname int_methods
setMethod("summarize_rows_inner", "TableRow",
          function(obj, depth = 0, indent = 0) {

            indent <- max(0L, indent + indent_mod(obj))

            summarize_row_df_unclassed(
              name = obj_name(obj),
              label = obj_label(obj),
              indent = indent,
              depth = depth,
              rowtype = "TableRow",
              indent_mod = indent_mod(obj),
              level = tt_level(obj),
              num_row_refs = length(row_footnotes(obj)),
              num_cell_refs = .num_cell_refs(obj)
            )

          })
#' @rdname int_methods
setMethod("summarize_rows_inner", "LabelRow",
          function(obj, depth = 0, indent = 0) {

            indent <- max(0L, indent + indent_mod(obj))

            if (labelrow_visible(obj)) {
              summarize_row_df_unclassed(
                name = obj_name(obj),
                label = obj_label(obj),
                indent = indent,
                depth = depth,
                rowtype = "LabelRow",
                indent_mod = indent_mod(obj),
                level = tt_level(obj),
                num_row_refs = length(row_footnotes(obj)),
                num_cell_refs = 0
              )
            } else {
              summarize_row_df_empty
            }
          })



# Print Table Structure ----

#' Summarize Table
#'
#' @param x a table object
#' @param detail either `row` or `subtable`
#'
#' @export
#'
#' @return currently no return value. Called for the side-effect of printing a
#'   row- or subtable-structure summary of \code{x}.
#' @examples
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' lyt <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary),
#'           format = "xx.xx")
#'
#' tbl <- build_table(lyt, iris2)
#' tbl
#'
#' row_paths(tbl)
#'
#' table_structure(tbl)
#'
#' table_structure(tbl, detail = "row")
table_structure <- function(x, detail = c("subtable", "row")) {

  detail <- match.arg(detail)

  switch(
    detail,
    subtable = treestruct(x),
    row =  table_structure_inner(x),
    stop("unsupported level of detail ", detail)
  )

}


#' @rdname int_methods
#'
#' @param obj a table object
#' @param depth depth in tree
#' @param indent indent
#' @param print_indent indent for print
setGeneric("table_structure_inner",
           function(obj,
                    depth = 0,
                    indent = 0,
                    print_indent = 0) {
               standardGeneric("table_structure_inner")
           })


scat <- function(..., indent = 0, newline = TRUE) {

  txt <- paste(..., collapse = "", sep = "")

  cat(indent_string(txt, indent))

  if (newline) cat("\n")
}

## helper functions
obj_visible <- function(x) {
  x@visible
}

is_empty_labelrow <- function(x) {
  obj_label(x) == "" && !labelrow_visible(x)
}

is_empty_ElementaryTable <- function(x) {
  length(tree_children(x)) == 0 && is_empty_labelrow(tt_labelrow(x))
}

#' @export
setGeneric("str", function(object, ...)
    standardGeneric("str"))

#' @rdname int_methods
#' @param max.level numeric(1). Passed to `utils::str`. Defaults to 3 for the
#' `VTableTree` method, unlike the underlying default of `NA`. `NA` is *not*
#' appropriate for `VTableTree` objects.
#' @export
setMethod("str", "VTableTree",
          function(object, max.level = 3L, ...){
    utils::str(object, max.level = max.level, ...)
    warning("str provides a low level, implementation-detail-specific description of the TableTree object structure. ",
            "See table_structure(.) for a summary of table struture intended for end users.",
            call. = FALSE)
    invisible(NULL)
})

#' @rdname int_methods
#' @inheritParams table_structure_inner
setMethod("table_structure_inner", "TableTree",
          function(obj, depth = 0, indent = 0, print_indent = 0) {

            indent <- indent + indent_mod(obj)

            scat("TableTree: ", "[", obj_name(obj), "] (",
                 obj_label(obj), ")", indent = print_indent)

            table_structure_inner(tt_labelrow(obj), depth, indent,
                                  print_indent + 1)

            ctab <- content_table(obj)
            visible_content <- if (is_empty_ElementaryTable(ctab)) {
              # scat("content: -", indent = print_indent + 1)
              FALSE
            } else {
              scat("content:", indent = print_indent + 1)
              table_structure_inner(ctab, depth = depth,
                              indent = indent + indent_mod(ctab),
                              print_indent = print_indent + 2)
            }

            if (length(tree_children(obj)) == 0) {
              scat("children: - ", indent = print_indent + 1)
            } else {
              scat("children: ", indent = print_indent + 1)
              lapply(tree_children(obj), table_structure_inner,
                     depth = depth + 1,
                     indent = indent + visible_content * (1 + indent_mod(ctab)),
                     print_indent = print_indent + 2)
            }

          invisible(NULL)

          })

#' @rdname int_methods
setMethod("table_structure_inner", "ElementaryTable",
          function(obj, depth = 0, indent = 0, print_indent = 0) {

            scat("ElementaryTable: ", "[", obj_name(obj),
                 "] (", obj_label(obj), ")", indent = print_indent)


            indent <- indent + indent_mod(obj)

            table_structure_inner(tt_labelrow(obj), depth,
                                  indent, print_indent + 1)


            if (length(tree_children(obj)) == 0) {
              scat("children: - ", indent = print_indent + 1)
            } else {
              scat("children: ", indent = print_indent + 1)
              lapply(tree_children(obj), table_structure_inner,
                     depth = depth + 1, indent = indent,
                     print_indent = print_indent + 2)
            }

            invisible(NULL)

          })

#' @rdname int_methods
setMethod("table_structure_inner", "TableRow",
          function(obj, depth = 0, indent = 0, print_indent = 0) {

            scat(class(obj), ": ", "[", obj_name(obj), "] (",
                 obj_label(obj), ")", indent = print_indent)

            indent <- indent + indent_mod(obj)

            invisible(NULL)
          })

#' @rdname int_methods
setMethod("table_structure_inner", "LabelRow",
          function(obj, depth = 0, indent = 0, print_indent = 0) {

            indent <- indent + indent_mod(obj)

            txtvis <- if (!obj_visible(obj)) " - <not visible>" else ""

            scat("labelrow: ", "[", obj_name(obj), "] (", obj_label(obj), ")",
                 txtvis,  indent = print_indent)

            obj_visible(obj)
          })
