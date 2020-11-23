

# paths summary ----

#' Return List with Table Row Paths
#' 
#' @param x an rtable object
#' 
#' @export
#' 
#' @examples 
#' row_paths(get_example_tables()[[1]])
row_paths <- function(x) {
  make_pagdf(x, visible_only = TRUE)$path
}


#' Return List with Table Column Paths
#' 
#' @param x an rtable object
#' 
#' @export
#' 
#' @examples 
#' col_paths(get_example_tables()[[1]])
col_paths <- function(x) {
  
  # TODO: replace with make_col_pagedf
  lapply(lapply(unlist(get_col_paths(x)), get_unit), `[[`, "path")
}

# class useful for not unlisting vectors
setClass("Unit", representation(unit = "ANY"))
get_unit <- function(x) x@unit

get_col_paths <- function(x, path = character(0)) {

  if (is(x, "TableTree") || is(x, "ElementaryTable"))
    x <- col_info(x)@tree_layout

  if (is(x, "LayoutColTree")) {
    mapply(function(xi, namei) {
      get_col_paths(xi, path = c(path, namei))
    }, tree_children(x), names(tree_children(x)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
          
  } else if (is(x, "LayoutColLeaf")) {
    
    new("Unit", unit = list(path = path, label = x@label))
  } else {
    stop("---")
  }

}



#' Print Row Paths Summary
#' 
#' @param x an rtable object
#' 
#' @export
#' 
#' @examples 
#' tbls <- get_example_tables()
#' 
#' row_paths_summary(tbls$demographic)
row_paths_summary <- function(x) {
  stopifnot(is(x, "TableTree"))
  
  if (nrow(x) == 0)
    return("rowname     rowtype       path\n---------------------\n")
  
  # make_pagdf has currently issues with indent
  pagdf <- make_pagdf(x, visible_only = TRUE)
  sr <- summarize_rows(x)
  if (nrow(sr) != nrow(pagdf) && !all(pagdf$name == sr$name) && !(all(pagdf$label == sr$label)) )
    stop("internal error")
  
  sr$path <- pagdf$path
  sr$type <- pagdf$node_class
  
  mat <- rbind(
    c("rowname", "type", "path"),
    t(apply(sr, 1, function(xi) {
      c(
        indent_string(xi$label, xi$indent),
        xi$type,
        paste(xi$path[-1], collapse = ", ")
      )
    }))
  )
  
  txt <- mat_as_string(mat)
  
  cat(txt)
  cat("\n")
  
  invisible(NULL)
  
}

#' Print Column Paths Summary
#' 
#' @param x an rtable object
#' 
#' @export
#' 
#' @examples 
#' tbls <- get_example_tables()
#' 
#' col_paths_summary(tbls$demographic)
#' 
#' col_paths_summary(tbls$handed)
col_paths_summary <- function(x) {
  objs <- lapply(unlist(get_col_paths(x)), get_unit)
 
  labels <- vapply(objs, `[[`, character(1), "label")
  paths <- lapply(objs, `[[`, "path")
  
  col_labels <- unlist(Map(indent_string, labels, vapply(paths, length, numeric(1))-1))
  col_paths <- vapply(paths, paste, character(1), collapse = ", ")
  
  mat <- cbind(c("column name", col_labels), c("path", col_paths))
  
  cat(mat_as_string(mat))
  cat("\n")
  
  invisible(NULL)
}

# 

# 


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


# .  Print Row Paths ----
#' Print the row paths
#' 
#' @param  x rtable object
#' 
#' @export
#' 
#' @examples 
#' ADSL <- ex_adsl
#' tbl <- basic_table() %>%
#'   split_cols_by("ARM") %>%
#'   analyze(vars = c("AGE", "BMRKR1"), afun = function(x) {
#'     in_rows(
#'       mean_sd = rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
#'       range = rcell(range(x), format = "xx.xx - xx.xx"),
#'       .labels = c(mean_sd = "Mean (Sd)", range = "Range")
#'     )
#'   }) %>%
#'   build_table(ADSL)
#' 
#' 
#' row_paths(tbl)
row_paths3 <- function(x) {

  invisible(sr$path)
}






setGeneric("summary", function(object,...) standardGeneric("summary"))
## preserve S3 behavior
#' @export
setMethod("summary", "ANY", base:::summary)


#' Show Row and Column summary of a TableTree
#' 
#' @param object an object of class \code{TableTree} which is usually created with \code{\link{build_table}}
#' @param row_type character(1).
#' @param \dots \dots.
#' @param depth numeric(1). Depth.
#' @param indent numeric(1). Indent.
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
