
# toString ----

#' @export
setGeneric("toString", function(x,...) standardGeneric("toString"))

## preserve S3 behavior
setMethod("toString", "ANY", base::toString)

setMethod("show", "VTableTree", function(object) {
  cat(toString(object))
})

#' @export
setMethod("print", "ANY", base::print)
setMethod("print", "VTableTree", function(x, ...) {
  cat(toString(x, ...))
})


#' Convert an `rtable` object to a string
#' 
#' 
#' @param x table object
#' @param widths widths of rowname and colmns columns
#' @param column_gap gap between columns
#' 
#' @exportMethod toString
#' 
#' @examples 
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
#' toString(tbl, gap = 3)
#' 
setMethod("toString", "VTableTree", function(x, widths = NULL, column_gap = 3, row_gap_at_depth = 1) {
  
  browser()
  ## we create a matrix with the formatted cell contents
  mat <- matrix_form(x)
  
  if (is.null(widths)) {
    widths <- propose_column_widths(x, mat_form = mat)
  }
  stopifnot(length(widths) == ncol(mat$strings))
  
  # format the to ASCII
  body <- mat$strings
  aligns <- mat$aligns
  keep_mat <- mat$display
  spans <- mat$spans
  ri <- mat$row_info
  
  # indent rownames
  body[, 1] <- indent_string(body[, 1], c(rep(0, attr(mat, "nrow_header")), ri$indent))
  
  # get rowgap position
  insert_gap_before <- which(mat$row_info$rowtype == "LabelRow" & mat$row_info$depth %in% row_gap_at_depth)
  insert_gap_before <- insert_gap_before[insert_gap_before != 1] # also remove the consecutive (i.e. 4,5, 8, 12 remove 5)
  
  
  nr <- nrow(body)
  nr_header <- attr(mat, "nrow_header")
  
  cell_widths_mat <- matrix(rep(widths, nr), nrow = nr, byrow = TRUE) 
  nc <- ncol(cell_widths_mat)
  
  for (i in seq_len(nrow(body))) {
    if (any(!keep_mat[i, ])) { # any spans?
      j <- 1
      while (j <= nc) {
        nj <- spans[i, j]
        j <- if (nj > 1) {
          js <- seq(j, j + nj - 1)
          cell_widths_mat[i, js] <- sum(cell_widths_mat[i, js]) + column_gap * (nj - 1)
          j + nj
        } else {
          j + 1
        }
      }
    }
  }
  
  # Are the total characters per row the same?
  # A <- cell_widths_mat
  # A[!keep_mat] <- 0
  # apply(A, 1, sum)
  
  content <- matrix(mapply(padstr, body, cell_widths_mat, aligns), ncol = ncol(body))
  content[!keep_mat] <- NA
  # apply(content, 1, function(x) sum(nchar(x), na.rm = TRUE))
  
  gap_str <- strrep(" ", column_gap)
  
  div <- strrep("-", sum(widths) + (length(widths) - 1) * column_gap)
  
  txt_head <- apply(head(content, nr_header), 1, .paste_no_na, collapse = gap_str)
  txt_body <- empty_string_after(apply(tail(content, -nr_header), 1, .paste_no_na, collapse = gap_str), insert_gap_before - 1)
  
  paste0(paste(c(txt_head, div, txt_body), collapse = "\n"), "\n")
  
})


#' insert an empty string
#' 
#' @noRd
#' 
#' @examples 
#' empty_string_after(letters[1:5], 2)
#' empty_string_after(letters[1:5], c(2, 4))
empty_string_after <- function(x, indices) {
  
  if (length(indices) > 0) {
    offset <- 0
    for (i in sort(indices)) {
      x <- append(x, "", i + offset)
      offset <- offset + 1
    }
  }
  x
}


#' Transform rtable to a list of matrices which can be used for outputting
#' 
#' Although rtables are represented as a tree data structure when outputting the table to ASCII or HTML it is useful to
#' map the rtable to an in between state with the formatted cells in a matrix form.
#' 
#' 
#' @note 
#' TODO: Look into `gt` representation
#' 
#' @export
#' 
#' @examples 
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
#' l
#' 
#' tbl <- build_table(l, iris2)
#' 
#' matrix_form(tbl)
matrix_form <- function(x) {
  
  stopifnot(is(x, "VTableTree"))
  
  header_content <- .tbl_header_mat(x) # first col are for row.names
  
  sr <- summarize_rows(x)
  
  body_content_strings <- if (nrow(sr) == 0) {
    ""
  } else {
    cbind(sr$label, get_formatted_cells(x))
  }
  
  tsptmp <- lapply(collect_leaves(x, TRUE, TRUE), function(rr) {
    sp <- row_cspans(rr)
    rep(sp, times = sp)
  })
  ## the 1 is for row labels
  body_spans <- cbind(1L, do.call(rbind, tsptmp))
  
  body <- rbind(header_content$body, body_content_strings)
  spans <- rbind(header_content$span, body_spans)
  row.names(spans) <- NULL
  
  space <- matrix(rep(0, length(body)), nrow = nrow(body))
  aligns <- matrix(rep("center", length(body)), nrow = nrow(body))
  aligns[, 1] <- "left" # row names
  
  if (any(apply(body, c(1, 2), function(x) grepl("\n", x, fixed = TRUE))))
    stop("no \\n allowed at the moment")
  
  display <- matrix(rep(TRUE, length(body)), ncol = ncol(body))
  
  display <- t(apply(spans, 1, function(row) {
    print_cells <- row == 1
    
    if (!all(print_cells)) {
      # need to calculate which cell need to be printed
      tmp <- 0
      for (i in seq_along(print_cells)) {
        if (!print_cells[i] && tmp <= 0) {
          print_cells[i] <- TRUE
          tmp <- row[i] - 1
        } else {
          tmp <- tmp - 1
        }
      }
    }
    print_cells
  }))
  
  
  structure(
    list(
      strings = body,
      spans = spans,
      aligns = aligns,
      display = display,
      row_info = sr
    ),
    nrow_header = nrow(header_content$body)
  )
}



.tbl_header_mat <- function(tt) {
  
  clyt <- coltree(tt)
  
  atrow <- 1
  rows <- list()
  kids <- tree_children(clyt)
  dispcounts <- disp_ccounts(tt)
  while(length(kids) > 0 && atrow < 1000) { #we will break from this
    
      labs <- names(kids)
      #TODO: XXX remove reliance on old_cell and old_rrowl!!!
    cells <- lapply(names(kids), function(x) {
      old_rcell(x, colspan = length(collect_leaves(kids[[x]], incl.cont = FALSE)))
    })
    rows[[atrow]] <- old_rrowl(row.name = "",
                              cells)
    atrow <- atrow + 1
    ## otherwise its pasted with the next level...
    ## XXX todo figure out a better way to do this
    names(kids) <- NULL
    kids <- unlist(lapply(kids,
                         function(k) {
                           if(is(k, "LayoutColLeaf"))
                             NULL
                           else
                             tree_children(k)
                         }),
                  recursive = FALSE)
  }
  
  nc <- ncol(tt)
  body <- matrix(rapply(rows, function(x) {
    cs <- attr(x, "colspan")
    if (is.null(cs)) cs <- 1
    rep(as.vector(x), cs)
  }), ncol = nc, byrow = TRUE)
  
  span <- matrix(rapply(rows, function(x) {
    cs <- attr(x, "colspan")
    if (is.null(cs)) cs <- 1
    rep(cs, cs)
  }), ncol = nc, byrow = TRUE)
  
  
  if (col_info(tt)@display_columncounts) {
    counts <- col_info(tt)@counts
    cformat <- col_info(tt)@columncount_format
    body <- rbind(body, vapply(counts, format_rcell, character(1), cformat))
    span <- rbind(span, rep(1, nc))
  }
  
  list(body = cbind("", body), span = cbind(1, span))
}



# get formatted cells ----

#' get formatted cells
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
#' tbl <- basic_table() %>% 
#'   split_cols_by("Species") %>%
#'   split_cols_by("group") %>%
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = lstwrapx(summary) , fmt = "xx.xx") %>%
#'   build_table(iris2)
#' 
#' get_formatted_cells(tbl)
#' 
setGeneric("get_formatted_cells", function(obj) standardGeneric("get_formatted_cells"))

setMethod("get_formatted_cells", "TableTree",
          function(obj) {
            
            lr <- get_formatted_cells(tt_labelrow(obj))
            
            ct <- get_formatted_cells(content_table(obj))
            
            els <- lapply(tree_children(obj), get_formatted_cells)
            
            do.call(rbind, c(list(lr), list(ct),  els))
          })

setMethod("get_formatted_cells", "ElementaryTable",
          function(obj) {
            
            lr <- get_formatted_cells(tt_labelrow(obj))
            
            els <- lapply(tree_children(obj), get_formatted_cells)
            
            do.call(rbind, c(list(lr), els))
          })


setMethod("get_formatted_cells", "TableRow",
          function(obj) {
            default_format <- if (is.null(obj_fmt(obj))) "xx" else obj_fmt(obj)
            
            format <- lapply(row_cells(obj), function(x) {
              fmt <- obj_fmt(x)
              if (is.null(fmt))
                default_format
              else
                fmt
            })
            
            matrix(unlist(Map(function(val, fmt, spn) {
              stopifnot(is(spn, "integer"))
              rep(paste(format_rcell(val, fmt), collapse = ", "), spn)
            }, row_values(obj), format, row_cspans(obj))), ncol = ncol(obj))
            
          })

setMethod("get_formatted_cells", "LabelRow",
          function(obj) {
            nc <- ncol(obj)
            if (lblrow_visible(obj)) {
              matrix(rep("", nc), ncol = nc)
            } else {
              matrix(character(0), ncol = nc)
            }
            
          })


#' Propose Column Widths of an `rtable` object
#' 
#' The row names are also considered a column for the output
#' 
#' @param x `rtable` object
#' @param matrix_form object as created with `matrix_form`
#' 
#' @export
#' 
#' @examples 
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
#' propose_column_widths(tbl)
propose_column_widths <- function(x, mat_form = matrix_form(x)) {
  stopifnot(is(x, "VTableTree"))
  
  body <- mat_form$strings
  spans <- mat_form$spans
  aligns <- mat_form$aligns
  display <- mat_form$display
  
  chars <- nchar(body)
  
  # first check column widths without colspan
  has_spans <- spans != 1
  chars_ns <- chars
  chars_ns[has_spans] <- 0
  widths <- apply(chars_ns, 2, max)
  
  # now check if the colspans require extra width
  if (any(has_spans)) {
    has_row_spans <- apply(has_spans, 1, any)
    
    chars_sp <- chars[has_row_spans, , drop = FALSE]
    spans_sp <- spans[has_row_spans, , drop = FALSE]
    disp_sp <- display[has_row_spans, , drop = FALSE]
    
    nc <- ncol(spans)
    for (i in seq_len(nrow(chars_sp))) {
      for (j in seq_len(nc)) {
        if (disp_sp[i, j] && spans_sp[i, j] != 1) {
          i_cols <- seq(j, j + spans_sp[i, j] - 1)
          
          nchar_i <- chars_sp[i, j]
          cw_i <- widths[i_cols]
          available_width <- sum(cw_i)
          
          if (nchar_i > available_width) {
            # need to update widths to fit content with colspans
            # spread width among columns
            widths[i_cols] <- cw_i + spread_integer(nchar_i - available_width, length(cw_i))
          }
        }
      }
    }
  }
  widths
}

# utility functions ----

#' spread x into len elements
#' 
#' @noRd
#' 
#' @examples 
#' spread_integer(3, 1)
#' spread_integer(0, 3)
#' spread_integer(1, 3)
#' spread_integer(2, 3)
#' spread_integer(3, 3)
#' spread_integer(4, 3)
#' spread_integer(5, 3)
#' spread_integer(6, 3)
#' spread_integer(7, 3)
spread_integer <- function(x, len) {
  stopifnot(
    is.wholenumber(x), length(x) == 1, x >= 0,
    is.wholenumber(len), length(len) == 1, len >= 0,
    !(len == 0 && x > 0)
  )
  

  if (len == 0) {
    integer(0)
  } else {
    y <- rep(floor(x/len), len)
    i <- 1
    while (sum(y) < x) {
      y[i] <- y[i] + 1
      if (i == len) {
        i <- 1
      } else {
        i <- i + 1
      }
    }
    y
  }
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}

indent_string <- function(x, indent = 0, incr = 2) {
  
  if (length(x) > 0) {
    indent <- rep_len(indent, length.out = length(x))
    incr <- rep_len(incr, length.out = length(x))
  }
  
  paste0(strrep(" ", (indent > 0) * indent * incr), x)
}

.paste_no_na <- function(x, ...) {
  paste(na.omit(x), ...)
}


#' Pad a string and align within string
#' 
#' @param x string
#' @param n number of character of the output string, if `n < nchar(x)` an error is thrown
#' 
#' @noRd
#' 
#' @examples 
#' 
#' padstr("abc", 3)
#' padstr("abc", 4)
#' padstr("abc", 5)
#' padstr("abc", 5, "left")
#' padstr("abc", 5, "right")
#' 
#' \dontrun{
#' padstr("abc", 1)
#' }
#' 
padstr <- function(x, n, just = c("center", "left", "right")) {
  
  just <- match.arg(just)
  
  if (length(x) != 1) stop("length of x needs to be 1 and not", length(x))
  if (is.na(n) || !is.numeric(n) || n < 0) stop("n needs to be numeric and > 0")
  
  if (is.na(x)) x <- "<NA>"
  
  nc <- nchar(x)
  
  if (n < nc) stop("\"", x, "\" has more than ", n, " characters")
  
  switch(
    just,
    center = {
      pad <- (n - nc)/2  
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    left = paste0(x, spaces(n - nc)),
    right = paste0(spaces(n - nc), x)
  )
}

spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}

