

setGeneric("summary", function(object,...) standardGeneric("summary"))
## preserve S3 behavior
#' @export
setMethod("summary", "ANY", base:::summary)


#' Show Row and Column summary of a TableTree
#' 
#' @param object an object of class \code{TableTree} which is usually created with \code{\url{build_table}}
#' 
#' @examples 
#' library(dplyr)
#' 
#' l <- basic_table() %>% 
#'     split_cols_by("Species") %>%
#'     split_rows_by("RND") %>%
#'     analyze(c("Sepal.Length", "Petal.Length"), afun = lstwrapx(summary) , fmt = "xx.xx")
#' l
#' 
#' iris2 <- iris %>% mutate(RND = sample(c("A", "B"), 150, replace = TRUE))
#' 
#' tbl <- build_table(l, iris2)
#' tbl
#' 
#' summary(tbl)
setMethod("summary", "TableTree", function(object, depth = 0, indent = 0, row_type = "", ...) {
  
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
#' tbl <- rtabulate(iris$Sepal.Length, iris$Species, lstwrapx(summary))
#' 
setMethod("summary", "ElementaryTable", function(object, depth = 0, indent = 0, row_type) {

  lr <- summary(tt_labelrow(object), depth, indent)
  
  lapply(tree_children(object), summary, depth = depth + 1, indent = indent + !is.null(lr), row_type)
  
  invisible(NULL)
})


setMethod("summary", "TableRow", function(object, depth = 0, indent = 0, row_type) {
  cat_row(indent, obj_name(object), obj_label(object), TRUE, row_type)
})

setMethod("summary", "LabelRow", function(object, depth = 0, indent = 0, ...) {

  if (lblrow_visible(object)) {
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





summarize_row_df <- function(name, label, indent, depth, rowtype, level) {
  data.frame(name = name, label = label, indent = indent, depth = level, rowtype = rowtype, level = level,
             stringsAsFactors = FALSE)
}

summarize_row_df_empty <- function(...) {
  data.frame(name = character(0), label = character(0), indent = integer(0), depth = integer(0), rowtype = character(0), level = integer(0))
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
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = lstwrapx(summary) , fmt = "xx.xx")
#' 
#' tbl <- build_table(l, iris2)
#' 
#' y <- summarize_rows(tbl)
#' 
setGeneric("summarize_rows", function(obj, depth = 0, indent = 0) standardGeneric("summarize_rows"))

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

setMethod("summarize_rows", "ElementaryTable",
          function(obj, depth = 0, indent = 0) {
            
            indent <- indent + indent_mod(obj)
            
            lr <- summarize_rows(tt_labelrow(obj), depth, indent)
            
            els <- lapply(tree_children(obj), summarize_rows, depth = depth + 1, indent = indent + (nrow(lr) > 0))
            
            df <- do.call(rbind, c(list(lr), els))
            row.names(df) <- NULL
            df
            
          })


setMethod("summarize_rows", "TableRow",
          function(obj, depth = 0, indent = 0) {
            
            indent <- indent + indent_mod(obj)
            
            summarize_row_df(
              name = obj_name(obj),
              label = obj_label(obj),
              indent = indent,
              depth = depth,
              rowtype = "TableRow",
              level = tt_level(obj)
            ) 
            
          })

setMethod("summarize_rows", "LabelRow",
          function(obj, depth = 0, indent = 0) {
            
            indent <- indent + indent_mod(obj)
            
            if (lblrow_visible(obj)) {
              summarize_row_df(
                name = obj_name(obj),
                label = obj_label(obj),
                indent = indent,
                depth = depth,
                rowtype = "LabelRow",
                level = tt_level(obj)
              ) 
            } else {
              summarize_row_df_empty()         
            } 
          })




