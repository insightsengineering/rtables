

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

