## temporary compatability layer to leverage existing
## ascii-rendering functionality
##
## We will want to replace this is if the S4 approach is
## adopted fully



setGeneric("toString", function(x,...) standardGeneric("toString"))
## preserve S3 behavior
setMethod("toString", "ANY", base:::toString)

setMethod("show", "VTableTree", function(object) {
  cat(toString(object))
})

setMethod("toString", "VTableTree", function(x, gap = 3) {
  
  tmp <- .tbl_header_mat(x)
  
  hbody <- tmp$body
  hspans <- tmp$span
    
    ## table body
    matdata <- unname(unlist(get_formatted_rows(x)))
    if(is.null(matdata))
        matdata <- ""
  tbody <- matrix(matdata, ncol = ncol(x) + 1, byrow = TRUE)
  tspans <- matrix(rep(1, length(tbody)), nrow = nrow(tbody))
  
  body <- rbind(hbody, tbody)
  spans <- rbind(hspans, tspans)
  
  space <- matrix(rep(0, length(body)), nrow = nrow(body))
  aligns <- matrix(rep("center", length(body)), nrow = nrow(body))
  aligns[, 1] <- "left" # row names
  
  if (any(apply(body, c(1, 2), function(x) grepl("\n", x, fixed = TRUE))))
    stop("no \\n allowed at the moment")
  
  space <- matrix(mapply(body, spans, FUN = function(txt, span) {
    max(vapply(txt, nchar, numeric(1))) / span
  }), ncol = ncol(body))
  
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
  
  txt_head <- apply(head(content, nrow(hbody)), 1, .paste_no_na, collapse = gap_str)
  txt_body <- apply(tail(content, -nrow(hbody)), 1, .paste_no_na, collapse = gap_str)
  
  paste0(paste(c(txt_head, div, txt_body), collapse = "\n"),
           "\n")
  
})

.paste_no_na <- function(x, ...) {
  paste(na.omit(x), ...)
}


.tbl_header_mat = function(tt) {
  
  clyt = coltree(tt)
  
  atrow = 1
  rows = list()
  kids = tree_children(clyt)
  dispcounts = disp_ccounts(tt)
  while(length(kids) > 0 && atrow < 1000) { #we will break from this
    
    labs = names(kids)
    cells = lapply(names(kids), function(x) {
      old_rcell(x, colspan = length(collect_leaves(kids[[x]], incl.cont = FALSE)))
    })
    rows[[atrow]] = old_rrowl(row.name = "",
                              cells)
    atrow = atrow + 1
    ## otherwise its pasted with the next level...
    ## XXX todo figure out a better way to do this
    names(kids) = NULL
    kids = unlist(lapply(kids,
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



#' Get formatted rows
#' @inheritParams argument_conventions
#' @rdname gfr
#' @export
setGeneric("get_formatted_rows", function(obj, depth = 0, indent = 0) standardGeneric("get_formatted_rows"))

## TableTree objects (can) have content Rows
## process the content, then the children by recursive call
#' @rdname gfr
#' @exportMethod get_formatted_rows
setMethod("get_formatted_rows", "TableTree",
          function(obj, depth = 0, indent = 0) {
            
            lr <- get_formatted_rows(tt_labelrow(obj), depth, indent)
            
            indent <- indent + !is.null(lr)
            
            ct <- unlist(get_formatted_rows(content_table(obj), depth = depth, indent = indent))
            els <- lapply(tree_children(obj), get_formatted_rows, depth = depth + 1, indent = indent + (length(ct) > 0))
            
            list(lr, ct, els)
          })


## this will hit all Content tables as well
## as any subtrees that happen to be
## Elementary
#' @rdname gfr
#' @exportMethod get_formatted_rows
setMethod("get_formatted_rows", "ElementaryTable",
          function(obj, depth = 0, indent = 0) {
            
            lr <- get_formatted_rows(obj@labelrow, depth, indent)
            #lr <- NULL
            
            els <- lapply(tree_children(obj), get_formatted_rows, depth = depth + 1, indent = indent + !is.null(lr))
            
            list(lr, els)
            
          })

#' @rdname gfr
#' @exportMethod get_formatted_rows
setMethod("get_formatted_rows", "TableRow",
          function(obj, depth = 0, indent = 0) {
            
            stopifnot(all(row_cspans(obj) == 1)) # Second assertion depends on first
            stopifnot(length(row_values(obj)) == ncol(obj))
            
            default_format <- if (is.null(obj_fmt(obj))) "xx" else obj_fmt(obj)
            
            format <- lapply(row_values(obj), function(x) {
              fmt <- attr(x, "format")
              if (is.null(fmt))
                default_format
              else
                fmt
            })

            c(indent_string(obj_label(obj), indent), unlist(Map(function(val, fmt)
              paste(format_rcell(val, fmt), collapse = ", "), row_values(obj), format)))
            
          })
#' @rdname gfr
#' @exportMethod get_formatted_rows

setMethod("get_formatted_rows", "LabelRow",
          function(obj, depth = 0, indent = 0) {
            
            if (obj@visible) 
              c(indent_string(obj_label(obj), indent), rep("", ncol(obj)))
            else
              NULL 
          })


indent_string <- function(x, indent = 0, incr = 2) {
  paste0(strrep(" ", (indent > 0) * indent * incr), x)
}

