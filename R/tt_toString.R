
# toString ----

#' @export
setGeneric("toString", function(x,...) standardGeneric("toString"))

## preserve S3 behavior
setMethod("toString", "ANY", base::toString)

#' @export
setMethod("print", "ANY", base::print)


#' Convert an `rtable` object to a string
#'
#'
#' @param x table object
#' @param widths widths of row.name and columns columns
#' @param col_gap gap between columns
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
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#'
#' tbl <- build_table(l, iris2)
#'
#' cat(toString(tbl, col_gap = 3))
setMethod("toString", "VTableTree", function(x, widths = NULL, col_gap = 3) {

  ## we create a matrix with the formatted cell contents
  mat <- matrix_form(x)

  # indent rownames
  mat$strings[, 1] <- indent_string(mat$strings[, 1], c(rep(0, attr(mat, "nrow_header")), mat$row_info$indent))

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


  # xxxx <- 1
  # # get rowgap position
  # insert_gap_before <- which(ri$depth == sort(ri$depth)[seq_len(xxxx)])
  # # which(ri$rowtype == "LabelRow" & ri$depth == min(ri$depth))
  # insert_gap_before <- remove_consecutive_numbers(insert_gap_before)
  # insert_gap_before <- insert_gap_before[insert_gap_before != 1]

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
          cell_widths_mat[i, js] <- sum(cell_widths_mat[i, js]) + col_gap * (nj - 1)
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

  gap_str <- strrep(" ", col_gap)

  div <- strrep("-", sum(widths) + (length(widths) - 1) * col_gap)

  txt_head <- apply(head(content, nr_header), 1, .paste_no_na, collapse = gap_str)
  txt_body <- apply(tail(content, -nr_header), 1, .paste_no_na, collapse = gap_str)

  paste0(paste(c(txt_head, div, txt_body), collapse = "\n"), "\n")

})





#' Transform rtable to a list of matrices which can be used for outputting
#'
#' Although rtables are represented as a tree data structure when outputting the table to ASCII or HTML it is useful to
#' map the rtable to an in between state with the formatted cells in a matrix form.
#'
#' @inheritParams gen_args
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
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
#'
#' l
#'
#' tbl <- build_table(l, iris2)
#'
#' matrix_form(tbl)
matrix_form <- function(tt) {

  stopifnot(is(tt, "VTableTree"))

  header_content <- .tbl_header_mat(tt) # first col are for row.names

  sr <- summarize_rows(tt)

  body_content_strings <- if (nrow(sr) == 0) {
    ""
  } else {
    cbind(sr$label, get_formatted_cells(tt))
  }

  tsptmp <- lapply(collect_leaves(tt, TRUE, TRUE), function(rr) {
    sp <- row_cspans(rr)
    rep(sp, times = sp)
  })
    ## the 1 is for row labels
    if(nrow(tt) > 0)
        body_spans <- cbind(1L, do.call(rbind, tsptmp))
    else
        body_spans <- matrix(1, nrow = 1, ncol = ncol(tt) + 1)
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

## print depths (not to be confused with tree depths)
.cleaf_depths <- function(ctree = coltree(cinfo), depth = 1, cinfo) {
    if(is(ctree, "LayoutColLeaf"))
        return(depth)
    unlist(lapply(tree_children(ctree), .cleaf_depths, depth = depth + 1))
}


.do_tbl_h_piece <- function(ct, padding = 0) {
    if(is(ct, "LayoutColLeaf")) {
        return(list(rcell(obj_name(ct), colspan = 1)))
    }

    nleafs <- length(collect_leaves(ct))
    kids <- tree_children(ct)
    kidnms <- vapply(kids, obj_name, "")
    cdepths <- vapply(kids, function(k) max(.cleaf_depths(k)), 1)
    pieces <- mapply(.do_tbl_h_piece,
    ct = kids, padding = max(cdepths) - cdepths,
    SIMPLIFY= FALSE)
    lpieces <- vapply(pieces, length, 1L)

    padcells <- if(padding > 0) list(rep(list(rcell("", colspan = nleafs)), padding))
    ##nmcell <- if(nchar(obj_name(ct)) > 0) rcell(obj_name(ct), colspan = nleafs) else NULL
    nmcell <- list(rcell(obj_name(ct), colspan = nleafs))

    stopifnot(length(unique(lpieces)) == 1)
    rowparts <- lapply(1:max(lpieces),
                       function(i) {
        res = lapply(pieces, `[[`, i = i)
        if(!are(res, "CellValue"))
            res = unlist(res, recursive = FALSE)
        res
    })


    c(padcells,
      nmcell,
      rowparts)
}


.tbl_header_mat <- function(tt) {

    clyt <- coltree(tt)
    rowvals <- .do_tbl_h_piece(clyt)
    rowvals <- rowvals[sapply(rowvals, function(x) any(nzchar(unlist(x))))]
    rows <- lapply(rowvals, DataRow, cinfo = col_info(tt))



    nc <- ncol(tt)
    body <- matrix(rapply(rows, function(x) {
        cs <- row_cspans(x) ##attr(x, "colspan")
        if (is.null(cs)) cs <- rep(1, ncol(x))
        rep(row_values(x), cs) ##as.vector(x), cs)
    }), ncol = nc, byrow = TRUE)

    span <- matrix(rapply(rows, function(x) {
        cs <- row_cspans(x) ##attr(x, "colspan")
        if (is.null(cs)) cs <- rep(1, ncol(x))
        rep(cs, cs) ##as.vector(x), cs)
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
#' @inheritParams gen_args
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
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx") %>%
#'   build_table(iris2)
#'
#' get_formatted_cells(tbl)
#' @rdname gfc
setGeneric("get_formatted_cells", function(obj) standardGeneric("get_formatted_cells"))
#' @rdname gfc
setMethod("get_formatted_cells", "TableTree",
          function(obj) {

            lr <- get_formatted_cells(tt_labelrow(obj))

            ct <- get_formatted_cells(content_table(obj))

            els <- lapply(tree_children(obj), get_formatted_cells)

            # TODO fix ncol problem for rrow()
            if (ncol(ct) == 0 && ncol(lr) != ncol(ct)) {
              ct <- lr[NULL, ]
            }

            do.call(rbind, c(list(lr), list(ct),  els))
          })
#' @rdname gfc
setMethod("get_formatted_cells", "ElementaryTable",
          function(obj) {

            lr <- get_formatted_cells(tt_labelrow(obj))

            els <- lapply(tree_children(obj), get_formatted_cells)

            do.call(rbind, c(list(lr), els))
          })

#' @rdname gfc
setMethod("get_formatted_cells", "TableRow",
          function(obj) {
            default_format <- if (is.null(obj_format(obj))) "xx" else obj_format(obj)

            format <- lapply(row_cells(obj), function(x) {
              format <- obj_format(x)
              if (is.null(format))
                default_format
              else
                format
            })

            matrix(unlist(Map(function(val, format, spn) {
              stopifnot(is(spn, "integer"))
              rep(paste(format_rcell(val, format), collapse = ", "), spn)
            }, row_values(obj), format, row_cspans(obj))), ncol = ncol(obj))

          })
#' @rdname gfc
setMethod("get_formatted_cells", "LabelRow",
          function(obj) {
            nc <- ncol(obj) # TODO note rrow() or rrow("label") has the wrong ncol
            if (labelrow_visible(obj)) {
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
#' @param mat_form object as created with `matrix_form`
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
#'   analyze(c("Sepal.Length", "Petal.Width"), afun = list_wrap_x(summary) , format = "xx.xx")
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

#' from sequence remove numbers where diff == 1
#'
#' numbers need to be sorted
#'
#' @noRd
#'
#' @examples
#' remove_consecutive_numbers(x = c(2, 4, 9))
#' remove_consecutive_numbers(x = c(2, 4, 5, 9))
#' remove_consecutive_numbers(x = c(2, 4, 5, 6, 9))
#' remove_consecutive_numbers(x = 4:9)
remove_consecutive_numbers <- function(x) {

  # actually should be integer
  stopifnot(is.wholenumber(x), is.numeric(x), !is.unsorted(x))

  if (length(x) == 0) return(integer(0))
  if (!is.integer(x)) x <- as.integer(x)

  sel <- rep(TRUE, length(x))

  x[c(TRUE, diff(x)  != 1)]
}


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
  strrep(" ", n)
}

