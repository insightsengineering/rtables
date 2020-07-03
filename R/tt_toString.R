
# toString ----

#' @export
setGeneric("toString", function(x,...) standardGeneric("toString"))

## preserve S3 behavior
setMethod("toString", "ANY", base:::toString)

setMethod("show", "VTableTree", function(object) {
  cat(toString(object))
})


#' Convert an `rtable` object to a string
#' 
#' @noRd
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
#' toString(tbl, gap = 3)
#' 
setMethod("toString", "VTableTree", function(x, gap = 3) {
  
 # browser()
  
  ## we create a matrix with the formatted cell contents
  
  header_content <- .tbl_header_mat(x) # first col are for row.names
  
  ## table body
  body_content <- unname(unlist(get_formatted_rows(x)))
  if(is.null(body_content))
    body_content <- ""
  body_content_strings <- matrix(body_content, ncol = ncol(x) + 1, byrow = TRUE)
  
  tsptmp <- lapply(collect_leaves(x, TRUE, TRUE), function(rr) {
    sp <- row_cspans(rr)
    rep(sp, times = sp)
  })
  ## the 1 is for row labels
  body_spans <- cbind(1L, do.call(rbind, tsptmp))
  
  body <- rbind(header_content$body, body_content_strings)
  spans <- rbind(header_content$span, body_spans)
  
  space <- matrix(rep(0, length(body)), nrow = nrow(body))
  aligns <- matrix(rep("center", length(body)), nrow = nrow(body))
  aligns[, 1] <- "left" # row names
  
  if (any(apply(body, c(1, 2), function(x) grepl("\n", x, fixed = TRUE))))
    stop("no \\n allowed at the moment")
  
  space <- nchar(body)/spans
  col_widths <- ceiling(apply(space, 2, max))
  
  col_widths_mat <- matrix(rep(col_widths, nrow(body)), ncol = ncol(body), byrow = TRUE)
  
  nc <- ncol(col_widths_mat)
  cell_widths_mat <- col_widths_mat
  keep_mat <- matrix(rep(TRUE, length(cell_widths_mat)), ncol = nc)
  
  for (i in 1:nrow(cell_widths_mat)) {
    j <- 1
    while (j <= nc) {
      sp <- spans[i, j]
      if (sp > 1) {
        j_rng <- seq(j, j + sp - 1)
        cell_widths_mat[i, j_rng] <- sum(col_widths[j_rng]) + (sp - 1) * gap
        keep_mat[i, tail(j_rng, -1)] <- FALSE
        j <- j + sp 
      } else {
        j <- j + 1
      }
    }
  }
  
  content <- matrix(mapply(padstr, body, cell_widths_mat, aligns), ncol = ncol(body))
  
  content[!keep_mat] <- NA
  
  gap_str <- strrep(" ", gap)
  
  div <- strrep("-", sum(col_widths) + (length(col_widths) - 1) * gap)
  
  txt_head <- apply(head(content, nrow(header_content$body)), 1, .paste_no_na, collapse = gap_str)
  txt_body <- apply(tail(content, -nrow(header_content$body)), 1, .paste_no_na, collapse = gap_str)
  
  paste0(paste(c(txt_head, div, txt_body), collapse = "\n"), "\n")
  
})


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


# get_formatted_rows ----

#' Get formatted rows
#' @inheritParams gen_args
#' @inheritParams compat_args
#' @param depth Depth we are currently at. Not intended to  be set by end users.
#' @rdname gfr
setGeneric("get_formatted_rows", function(obj, depth = 0, indent = 0) standardGeneric("get_formatted_rows"))


## TableTree objects (can) have content Rows
## process the content, then the children by recursive call
#' @rdname gfr
#' @exportMethod get_formatted_rows
setMethod("get_formatted_rows", "TableTree",
          function(obj, depth = 0, indent = 0) {

    indent <- indent + indent_mod(obj)
    lr <- get_formatted_rows(tt_labelrow(obj), depth, indent)
                             
    indent <- indent + !is.null(lr)
    ctab <- content_table(obj)
    ct <- unlist(get_formatted_rows(ctab, depth = depth, indent = indent + indent_mod(ctab)))
    els <- lapply(tree_children(obj), get_formatted_rows, depth = depth + 1, indent = indent + (length(ct) > 0) *(1 + indent_mod(ctab)))
    list(lr, ct, els)
})


## this will hit all Content tables as well
## as any subtrees that happen to be
## Elementary
#' @rdname gfr
#' @exportMethod get_formatted_rows
setMethod("get_formatted_rows", "ElementaryTable",
          function(obj, depth = 0, indent = 0) {
    indent <- indent + indent_mod(obj)

    lr <- get_formatted_rows(obj@labelrow, depth, indent)
    els <- lapply(tree_children(obj), get_formatted_rows, depth = depth + 1, indent = indent + !is.null(lr))

    list(lr, els)
})

#' @rdname gfr
#' @exportMethod get_formatted_rows
setMethod("get_formatted_rows", "TableRow",
          function(obj, depth = 0, indent = 0) {
    indent <- indent + indent_mod(obj)
    ## stopifnot(all(row_cspans(obj) == 1)) # Second assertion depends on first
    ## stopifnot(length(row_values(obj)) == ncol(obj))
    
    default_format <- if (is.null(obj_fmt(obj))) "xx" else obj_fmt(obj)
    
    format <- lapply(row_cells(obj), function(x) {
        fmt <- obj_fmt(x)
        if (is.null(fmt))
            default_format
        else
            fmt
    })
    c(indent_string(obj_label(obj), indent),
      unlist(Map(function(val, fmt, spn) {
          stopifnot(is(spn, "integer"))
          rep(paste(format_rcell(val, fmt), collapse = ", "),
              spn)
      }, row_values(obj), format, row_cspans(obj)),
      recursive = FALSE))
})

#' @rdname gfr
#' @exportMethod get_formatted_rows
setMethod("get_formatted_rows", "LabelRow",
          function(obj, depth = 0, indent = 0) {
    indent <- indent + indent_mod(obj)
    if (lblrow_visible(obj))
        c(indent_string(obj_label(obj), indent), rep("", ncol(obj)))
    else
        NULL 
})


#' Calculate the Column Widths of an `rtable`
#' 
#' @export
#' 
#' @examples 
#' 
column_widhts <- function(x, column_gap = 3) {
  stopifnot(is(x, "rtable"))
  
}

# utility functions ----

indent_string <- function(x, indent = 0, incr = 2) {
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
  
  if (is.na(x)) x <- ""
  
  nc <- nchar(x)
  
  if (n < nc) stop("\"", x, "\" has more than ", n, " characters")
  
  switch(
    just,
    center = {
      pad <- (n-nc)/2  
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    left = paste0(x, spaces(n-nc)),
    right = paste0(spaces(n-nc), x)
  )
}

spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}

