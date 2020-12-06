

# paths summary ----

#' @rdname make_col_row_df
#' 
#' @title Return List with Table Row/Col Paths
#' 
#' @param x an rtable object
#' 
#' @export
#' 
#' @examples 
#' tbl <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(c("SEX", "AGE")) %>%
#'   build_table(ex_adsl)
#' 
#' tbl
#' 
#' row_paths(tbl)
#' col_paths(tbl)
#' 
#' \dontrun{
#' cell_values(tbl, c("AGE", "Mean"),  c("ARM", "B: Placebo")) 
#' cell_values(tbl)
#' }
#' 
row_paths <- function(x) {
  stopifnot(is_rtable(x))
  make_row_df(x, visible_only = TRUE)$path
}


#' @rdname make_col_row_df
#' @export
col_paths <- function(x) {
  stopifnot(is_rtable(x))
  make_col_df(x, visible_only = TRUE)$path
}


#' Print Row/Col Paths Summary
#' 
#' @param x an rtable object
#' 
#' @export
#' 
#' @examples 
#' 
#' library(dplyr)
#' 
#' ex_adsl_MF <- ex_adsl %>% filter(SEX %in% c("M", "F"))
#' 
#' tbl <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   split_cols_by("SEX", split_fun = drop_split_levels) %>%
#'   analyze(c("AGE", "BMRKR2")) %>%
#'   build_table(ex_adsl_MF)
#' 
#' tbl
#' 
#' df <- row_paths_summary(tbl)
#' 
#' df
#' 
#' col_paths_summary(tbl)
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
        paste(xi$path[-1], collapse = ", ")
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

  pagdf <- make_col_df(x, visible_only = TRUE)
  row.names(pagdf) <- NULL
  
  mat <- rbind(
    c("label", "path"),
    t(apply(pagdf, 1, function(xi) {
      .GlobalEnv$xi <- xi
      c(
        xi$label,
        paste(xi$path[-1], collapse = ", ")
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

summarize_row_df <- function(name, label, indent, depth, rowtype, indent_mod, level) {
  data.frame(name = name, label = label, indent = indent, depth = level, rowtype = rowtype, indent_mod = indent_mod, level = level,
             stringsAsFactors = FALSE)
}

summarize_row_df_empty <- function(...) {
  data.frame(name = character(0), label = character(0), indent = integer(0), depth = integer(0), rowtype = character(0), indent_mod = integer(0), level = integer(0))
}

#' Summarize Rows
#'
#' @inheritParams gen_args
#' @param depth numeric(1). Depth.
#' @param indent numeric(1). Indent.
#' 
#' @noRd
#' @examples
#'
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' l <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#'
#' tbl <- build_table(l, iris2)
#'
#' summarize_rows(tbl)
#'
setGeneric("summarize_rows", function(obj, depth = 0, indent = 0) standardGeneric("summarize_rows"))
#' @rdname int_methods
setMethod("summarize_rows", "TableTree",
          function(obj, depth = 0, indent = 0) {

            indent <- max(0L, indent + indent_mod(obj))

            lr <- summarize_rows(tt_labelrow(obj), depth, indent)

            indent <- indent + (nrow(lr) > 0)

            ctab <- content_table(obj)
            ct <- summarize_rows(ctab, depth = depth, indent = indent + indent_mod(ctab))

            els <- lapply(tree_children(obj), summarize_rows,
                          depth = depth + 1, indent = indent + (nrow(ct) > 0) * (1 + indent_mod(ctab)))

            df <- do.call(rbind, c(list(lr), list(ct), els))

            row.names(df) <- NULL
            df

          })
#' @rdname int_methods
setMethod("summarize_rows", "ElementaryTable",
          function(obj, depth = 0, indent = 0) {

            indent <- max(0L, indent + indent_mod(obj))

            lr <- summarize_rows(tt_labelrow(obj), depth, indent)

            els <- lapply(tree_children(obj), summarize_rows, depth = depth + 1, indent = indent + (nrow(lr) > 0))

            df <- do.call(rbind, c(list(lr), els))
            row.names(df) <- NULL
            df

          })

#' @rdname int_methods
setMethod("summarize_rows", "TableRow",
          function(obj, depth = 0, indent = 0) {

            indent <- max(0L, indent + indent_mod(obj))

            summarize_row_df(
              name = obj_name(obj),
              label = obj_label(obj),
              indent = indent,
              depth = depth,
              rowtype = "TableRow",
              indent_mod = indent_mod(obj),
              level = tt_level(obj)
            )

          })
#' @rdname int_methods
setMethod("summarize_rows", "LabelRow",
          function(obj, depth = 0, indent = 0) {

            indent <- max(0L, indent + indent_mod(obj))

            if (labelrow_visible(obj)) {
              summarize_row_df(
                name = obj_name(obj),
                label = obj_label(obj),
                indent = indent,
                depth = depth,
                rowtype = "LabelRow",
                indent_mod = indent_mod(obj),
                level = tt_level(obj)
              )
            } else {
              summarize_row_df_empty()
            }
          })



# Print Table Structure ----

#' Summarize Table
#' 
#' 
#' @inheritParams gen_args
#' @param depth numeric(1).
#' @param indent numeric(1).
#' @param print_indent numeric(1)
#' 
#' @export
#' @examples 
#' 
#' library(dplyr)
#'
#' iris2 <- iris %>%
#'   group_by(Species) %>%
#'   mutate(group = as.factor(rep_len(c("a", "b"), length.out = n()))) %>%
#'   ungroup()
#'
#' l <- basic_table() %>%
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#'
#' tbl <- build_table(l, iris2)
#' 
#' row_paths(tbl)
#' 
#' table_structure(tbl)
#' 
setGeneric("table_structure", function(obj, depth = 0, indent = 0, print_indent = 0) standardGeneric("table_structure"))


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

#' @rdname int_methods
#' @inheritParams table_structure
setMethod("table_structure", "TableTree",
          function(obj, depth = 0, indent = 0, print_indent = 0) {

            indent <- indent + indent_mod(obj)

            scat("TableTree: ", "[", obj_name(obj), "] (", obj_label(obj), ")", indent = print_indent)

            table_structure(tt_labelrow(obj), depth, indent, print_indent + 1)

            ctab <- content_table(obj)
            visible_content <- if (is_empty_ElementaryTable(ctab)) {
              # scat("content: -", indent = print_indent + 1)
              FALSE
            } else {
              scat("content:", indent = print_indent + 1)
              table_structure(ctab, depth = depth,
                              indent = indent + indent_mod(ctab),
                              print_indent = print_indent + 2)
            }

            if (length(tree_children(obj)) == 0) {
              scat("children: - ", indent = print_indent + 1)
            } else {
              scat("children: ", indent = print_indent + 1)
              lapply(tree_children(obj), table_structure, 
                     depth = depth + 1, 
                     indent = indent + visible_content * (1 + indent_mod(ctab)),
                     print_indent = print_indent + 2)
            }

          invisible(NULL)

          })

#' @rdname int_methods
setMethod("table_structure", "ElementaryTable",
          function(obj, depth = 0, indent = 0, print_indent = 0) {

            scat("ElementaryTable: ", "[", obj_name(obj), "] (", obj_label(obj), ")", indent = print_indent)


            indent <- indent + indent_mod(obj)
            
            table_structure(tt_labelrow(obj), depth, indent, print_indent + 1)
            
            
            if (length(tree_children(obj)) == 0) {
              scat("children: - ", indent = print_indent + 1)
            } else {
              scat("children: ", indent = print_indent + 1)
              lapply(tree_children(obj), table_structure, depth = depth + 1, indent = indent, print_indent = print_indent + 2)
            }

            invisible(NULL)

          })

#' @rdname int_methods
setMethod("table_structure", "TableRow",
          function(obj, depth = 0, indent = 0, print_indent = 0) {

            scat(class(obj), ": ", "[", obj_name(obj), "] (", obj_label(obj), ")", indent = print_indent)

            indent <- indent + indent_mod(obj)

            invisible(NULL)
          })

#' @rdname int_methods
setMethod("table_structure", "LabelRow",
          function(obj, depth = 0, indent = 0, print_indent = 0) {

            indent <- indent + indent_mod(obj)
            
            txtvis <- if (!obj_visible(obj)) " - <not visible>" else ""
            
            scat("labelrow: ", "[", obj_name(obj), "] (", obj_label(obj), ")", txtvis,  indent = print_indent)
            
            obj_visible(obj)
          })
