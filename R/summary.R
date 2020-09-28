

setGeneric("summary", function(object,...) standardGeneric("summary"))
## preserve S3 behavior
#' @export
setMethod("summary", "ANY", base:::summary)


#' Show Row and Column summary of a TableTree
#' 
#' @param object an object of class \code{TableTree} which is usually created with \code{\link{build_table}}
#' 
#' @examples 
#' library(dplyr)
#' 
#' l <- basic_table() %>% 
#'     split_cols_by("Species") %>%
#'     split_rows_by("RND") %>%
#'     analyze(c("Sepal.Length", "Petal.Length"), afun = list_wrap_x(summary) , format = "xx.xx")
#' l
#' 
#' iris2 <- iris %>% mutate(RND = sample(c("A", "B"), 150, replace = TRUE))
#' 
#' tbl <- build_table(l, iris2)
#' tbl
#' 
#' summary(tbl)
#' @rdname summarymeths
setMethod("summary", "VTableTree", function(object, depth = 0, indent = 0, row_type = "", ...) {
  
  if (indent == 0) {
    cat_row(0, "name", "label", TRUE, "type  |")
    cat("----------------------------------------------------------\n")
  }
  
  # note to follow the logic of get_formatted_rows
  
  lr <- summary(tt_labelrow(object), depth, indent)

  indent <- indent + !is.null(lr)
  
  summary(content_table(object), depth = depth, indent = indent, row_type = "cont. |")
  lapply(tree_children(object), summary, depth = depth + 1, indent = indent + (length(content_table(object)) > 0), row_type = "leaf  |")
  
  
  if (indent == 0) {
    cat("\n* indicates that row is not visible\n")
  }
  invisible(NULL)
})



#' Summary method for elementary table
#' 
#' @examples 
#' tbl <- rtabulate(iris$Sepal.Length, iris$Species, list_wrap_x(summary))
#'
#' @rdname summarymeths
setMethod("summary", "ElementaryTable", function(object, depth = 0, indent = 0, row_type) {

  lr <- summary(tt_labelrow(object), depth, indent)
  
  lapply(tree_children(object), summary, depth = depth + 1, indent = indent + !is.null(lr), row_type)
  
  invisible(NULL)
})

#' @rdname summarymeths
setMethod("summary", "TableRow", function(object, depth = 0, indent = 0, row_type) {
  cat_row(indent, obj_name(object), obj_label(object), TRUE, row_type)
})
#' @rdname summarymeths
setMethod("summary", "LabelRow", function(object, depth = 0, indent = 0, ...) {

  if (labelrow_visible(object)) {
    cat_row(indent, obj_name(object), obj_label(object), object@visible, "label |")
    TRUE
  } else {
    NULL    
  }
  
})

cat_row <- function(indent, name, label, visible, content) {
  if (is.null(label)) label <- "<no label>"
  if (is.null(name)) name <- "<no name>"
  
  if (requireNamespace("crayon", quietly = TRUE)) {
    cat(
      if (visible) " " else "*",
      crayon::silver(content),
      indent_string(
        paste(
          crayon::blue(name),
          crayon::black(label),
          sep = " - "
        ),
        indent
      ),
      "\n"
    )
  } else {
    cat(
      if (visible) " " else "*",
      content,
      indent_string(
        paste(
          name,
          label,
          sep = " - "
        ),
        indent
      ),
      "\n"
    )
  }
}


# rtables:::treestruct(tbl)
# rtables:::make_pagdf(tbl)
# cell_value(tbl, c("B", "Sepal.Length", "Median"), "setosa") # names, not labels

# Column Structure:
# name                label              col_count
# - setosa            Setosa Flower         10
# - versicolor        Versicolor Flower     20
# - virginica         Viginica Flower       32
# (don't) show col counts
# 
# Row Structure:
# name             label          visible    type 
# - Sepal.Length   Sepal.Length    TRUE      label
#   - Min.         Min.            TRUE      content
#   - 1st Qu.      ...             TRUE
#   - Median       ...             TRUE
#   - Mean         ...             TRUE
#   - 3rd. Qu.     ...             TRUE
#   - Max.         ...             TRUE
# - Petal.Length   ...             TRUE
#   - Min.         ...             TRUE
#   - 1st Qu.      ...             TRUE
#   - Median       ...             TRUE
#   - Mean         ...             TRUE
#   - 3rd. Qu.     ...             TRUE
#   - Max.         ...             TRUE
# 


# SOC
#  TERM           ARM A           ARM B
# -------------------------------------
# 


summarize_row_df <- function(name, label, indent, depth, rowtype, indent_mod, level) {
  data.frame(name = name, label = label, indent = indent, depth = level, rowtype = rowtype, indent_mod = indent_mod, level = level,
             stringsAsFactors = FALSE)
}

summarize_row_df_empty <- function(...) {
  data.frame(name = character(0), label = character(0), indent = integer(0), depth = integer(0), rowtype = character(0), indent_mod = integer(0), level = integer(0))
}

#' Summarize Rows
#' 
#' 
#' @export
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
            
            indent <- indent + indent_mod(obj)
            
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
            
            indent <- indent + indent_mod(obj)
            
            lr <- summarize_rows(tt_labelrow(obj), depth, indent)
            
            els <- lapply(tree_children(obj), summarize_rows, depth = depth + 1, indent = indent + (nrow(lr) > 0))
            
            df <- do.call(rbind, c(list(lr), els))
            row.names(df) <- NULL
            df
            
          })

#' @rdname int_methods
setMethod("summarize_rows", "TableRow",
          function(obj, depth = 0, indent = 0) {
            
            indent <- indent + indent_mod(obj)
            
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
            
            indent <- indent + indent_mod(obj)
            
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



#' Summarize Table
#' 
#' 
#' @export
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
#' l <- basic_table() %>% 
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#' 
#' tbl <- build_table(l, iris2)
#' 
#' summarize_table(tbl)
#' 
setGeneric("summarize_table", function(obj, depth = 0, indent = 0, print_indent = 0) standardGeneric("summarize_table"))


scat <- function(..., indent = 0, newline = TRUE) {
  
  txt <- paste(..., collapse = "", sep = "")
  
  cat(indent_string(txt, indent))
  
  if (newline) cat("\n")
}

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
setMethod("summarize_table", "TableTree",
          function(obj, depth = 0, indent = 0, print_indent = 0) {
            
            indent <- indent + indent_mod(obj)
            
            scat("TableTree: ", "[", obj_name(obj), "] (", obj_label(obj), ")", indent = print_indent)
            
            visible <- if (is_empty_labelrow(tt_labelrow(obj))) {
              scat("labelrow: -", indent = print_indent + 1)
              FALSE
            } else {
              scat("labelrow:", indent = print_indent + 1)
              summarize_table(tt_labelrow(obj), depth, indent, print_indent + 2)
            }
            
            indent <- indent + visible
            
            ctab <- content_table(obj)
            visible_content <- if (is_empty_ElementaryTable(ctab)) {
              scat("content: -", indent = print_indent + 1)
              FALSE
            } else {
              scat("content:", indent = print_indent + 1)
              summarize_table(ctab, depth = depth,
                              indent = indent + indent_mod(ctab),
                              print_indent = print_indent + 2)
            }
            
            if (length(tree_children(obj)) == 0) {
              scat("children: - ", indent = print_indent + 1)
            } else {
              scat("children: ", indent = print_indent + 1)
              lapply(tree_children(obj), summarize_table, 
                     depth = depth + 1, 
                     indent = indent + visible_content * (1 + indent_mod(ctab)),
                     print_indent = print_indent + 2)
            }
          
          invisible(NULL)
            
          })
#' @rdname int_methods
setMethod("summarize_table", "ElementaryTable",
          function(obj, depth = 0, indent = 0, print_indent = 0) {
            
            scat("ElementaryTable: ", "[", obj_name(obj), "] (", obj_label(obj), ")", indent = print_indent)
            
            
            indent <- indent + indent_mod(obj)
            
            visible <- if (is_empty_labelrow(tt_labelrow(obj))) {
              scat("labelrow: -", indent = print_indent + 1)
              FALSE
            } else {
              scat("labelrow:", indent = print_indent + 1)
              summarize_table(tt_labelrow(obj), depth, indent, print_indent + 2)
            }
            
            
            if (length(tree_children(obj)) == 0) {
              scat("children: - ", indent = print_indent + 1)
            } else {
              scat("children: ", indent = print_indent + 1)
              lapply(tree_children(obj), summarize_table, depth = depth + 1, indent = indent, print_indent = print_indent + 2)
            }
            
            invisible(NULL)
            
          })

#' @rdname int_methods
setMethod("summarize_table", "TableRow",
          function(obj, depth = 0, indent = 0, print_indent = 0) {
            
            scat(class(obj), ": ", "[", obj_name(obj), "] (", obj_label(obj), ")", indent = print_indent)
            
            indent <- indent + indent_mod(obj)
            
            invisible(NULL)
          })
#' @rdname int_methods
setMethod("summarize_table", "LabelRow",
          function(obj, depth = 0, indent = 0, print_indent = 0) {
          
            indent <- indent + indent_mod(obj)
            
            txtvis <- if (!obj_visible(obj)) "- <not visible>" else ""
            scat("LabelRow: ", "[", obj_name(obj), "] (", obj_label(obj), ")", txtvis,  indent = print_indent)
            
            obj_visible(obj)
          })
