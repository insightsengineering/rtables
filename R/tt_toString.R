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
  tbody <- matrix(unname(unlist(get_formated_rows(x))), ncol = ncol(tab) + 1, byrow = TRUE)
  tspans <- matrix(rep(1, length(tbody)), nrow = nrow(tbody))
  
  body <- rbind(hbody, tbody)
  spans <- rbind(hspans, tspans)
  
  space <- matrix(rep(0, length(body)), nrow = nrow(body))
  aligns <- matrix(rep("center", length(body)), nrow = nrow(body))
  aligns[, 1] <- "left" # row names
  
  if (any(apply(body, c(1, 2), function(x) grepl("\n", x, fixed = TRUE))))
    stop("no \n allowed at the moment")
  
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
  
  paste(c(txt_head, div, txt_body), collapse = "\n")
  
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
  
  list(body = cbind("", body), span = cbind(1, span))
}



#' @exportMethod get_formated_rows
setGeneric("get_formated_rows", function(obj, depth = 0, indent = 0) standardGeneric("get_formated_rows"))

## TableTree objects (can) have content Rows
## process the content, then the children by recursive call

setMethod("get_formated_rows", "TableTree",
          function(obj, depth = 0, indent = 0) {
            
            lr <- get_formated_rows(obj@labelrow, depth, indent)
            
            indent <- indent + !is.null(lr)
            
            ct <- unlist(get_formated_rows(content_table(obj), depth = depth, indent = indent))
            els <- lapply(tree_children(obj), get_formated_rows, depth = depth + 1, indent = indent + (length(ct) > 0))
            
            list(lr, ct, els)
          })


## this will hit all Content tables as well
## as any subtrees that happen to be
## Elementary
setMethod("get_formated_rows", "ElementaryTable",
          function(obj, depth = 0, indent = 0) {
            
            lr <- get_formated_rows(obj@labelrow, depth, indent)
            #lr <- NULL
            
            els <- lapply(tree_children(obj), get_formated_rows, depth = depth + 1, indent = indent + !is.null(lr))
            
            list(lr, els)
            
          })


setMethod("get_formated_rows", "TableRow",
          function(obj, depth = 0, indent = 0) {
            
            stopifnot(all(row_cspans(obj) == 1)) # Second assertion depends on first
            stopifnot(length(row_values(obj)) == ncol(obj))
            
            # why?
            format <- if (is.null(obj_fmt(obj))) "xx" else obj_fmt(obj)
            
            format <- rep(format, length.out = ncol(obj))
            
            c(indent_string(obj_label(obj), indent), unlist(Map(format_rcell, row_values(obj), format)))
            
          })

setMethod("get_formated_rows", "LabelRow",
          function(obj, depth = 0, indent = 0) {
            
            if (obj@visible) 
              c(indent_string(obj_label(obj), indent), rep("", ncol(obj)))
            else
              NULL 
          })


indent_string <- function(x, indent = 0, incr = 2) {
  paste0(strrep(" ", (indent > 0) * indent * incr), x)
}




# setGeneric("to_s3compat", function(obj, ...) standardGeneric("to_s3compat"))
# setMethod("to_s3compat", "TableRow",
#           function(obj, ...)
#           {
#             cells = mapply( function(val, span) {
#               if(is.null(span))
#                 span = 1L #XXX
#               fmt = attr(val, "format")
#               if(is.null(fmt))
#                 fmt = obj_fmt(obj)
#               old_rcell(val, format = fmt, colspan = span)
#             }, val = row_values(obj),
#             span = row_cspans(obj),
#             SIMPLIFY = FALSE)
#             old_rrowl(row.name = obj_label(obj),
#                       cells, 
#                       indent = tt_level(obj),
#                       format = obj_fmt(obj))
#             
#           })
# 
# setMethod("to_s3compat", "LabelRow",
#           function(obj, ...)
#           {
#             old_rrowl(row.name = obj_label(obj),
#                       list(), 
#                       indent = tt_level(obj),
#                       format = obj_fmt(obj))
#             
#           })
# 
# setMethod("to_s3compat", "VTableTree",
#           function(obj, ...) {
#             header = .make_s3_header2(obj)
#             rows = lapply(collect_leaves(obj, incl.cont = TRUE, add.labrows = TRUE),
#                           to_s3compat)
#             old_rtablel(header = header, rows, format = obj_fmt(obj))
#           })
# 
# 
# .collapse_cspans = function(rvals, breakpts) {
#   n = length(rvals)
#   ## cumsum trick to build a factor that increments
#   ## anytime a logical vector is true, thus creating
#   ## categorical variable from TRUE/FALSE event data
#   ##
#   ## the meaning of inds is
#   ## (2nd different from first, 3rd diff from as second, ...)
#   breaklgl = !mapply(identical, x = rvals[-1], y = rvals[-n])
#   ## add ones we've already found
#   breaklgl[breakpts - 1] = TRUE
#   #0 combines with first run
#   inds = c(0,cumsum(breaklgl)) 
#   breakpts = which(breaklgl) + 1 
#   
#   spl = split(rvals, inds)
#   cells = lapply(spl,
#                  function(x) old_rcell(x[[1]], colspan = length(x),
#                                        format = NULL))
#   
#   
#   list(cells = cells, breakpts = breakpts)
#   
#   
# }
# 
# 
# .make_s3_header2 = function(tt) {
#   clyt = coltree(tt)
#   
#   atrow = 1
#   rows = list()
#   kids = tree_children(clyt)
#   dispcounts = disp_ccounts(tt)
#   while(length(kids) > 0 && atrow < 1000) { #we will break from this
#     
#     labs = names(kids)
#     cells = lapply(names(kids), function(x) {
#       old_rcell(x, colspan = length(collect_leaves(kids[[x]], incl.cont = FALSE)))
#     })
#     rows[[atrow]] = old_rrowl(row.name = "",
#                               cells)
#     atrow = atrow + 1
#     ## otherwise its pasted with the next level...
#     ## XXX todo figure out a better way to do this
#     names(kids) = NULL
#     kids = unlist(lapply(kids,
#                          function(k) {
#                            if(is(k, "LayoutColLeaf"))
#                              NULL
#                            else
#                              tree_children(k)
#                          }),
#                   recursive = FALSE)
#   }
#   ## add column counts row if necessary
#   if(dispcounts) {
#     ccounts = col_counts(tt)
#     countcells = lapply(ccounts, old_rcell,
#                         format = colcount_fmt(tt),
#                         colspan = 1L)                            
#     rows[[length(rows) + 1L]] = do.call(old_rrow, c(countcells, row.name = "", format = NULL))
#   }
#   old_rheaderl(rows)
# }
# 



# ttrow_to_str = function(ttrow, nchar_rownames = 20, nchar_col = 5, gap =1 , indent.unit = 1L,  ...) {
# 
#     indent_lvl = length(pos_splvals(ttrow))
#     row.name <-  if (nzchar(obj_label(ttrow))) {
#                      paste(c(rep(" ", indent_lvl *indent.unit),
#                              obj_label(ttrow)), collapse = "")
#                  } else {
#                      ""
#                  }
#     if (length(leaf_values(ttrow)) == 0) {
#         row.name
#     } else {
#         rowfmt = obj_fmt(ttrow)
#         cells <- lapply(leaf_values(ttrow), function(val, rwfmt) {
#             ## XXX TODO do this for realg
#             fmt = obj_fmt(val)
#             if(is.null(fmt) && !is.null(rowfmt))
#                 fmt = rowfmt
#             ## compatibility layer hack. do this right eventually
#             cell = old_rcell(val, fmt)
#             unlist(strsplit(format_rcell(cell, output = "ascii"), "\n", fixed = TRUE))
#     }, rwfmt = rowfmt)
#     
#     nlines <- max(vapply(cells,length, numeric(1)))
#     
#     cells_same_length <- lapply(cells, function(x) {
#       if (length(x) == nlines) {
#         x
#       } else {
#         c(x, rep(NA_character_, nlines -length(x)))
#       }
#     })
# 
#         ##XXX TODO do this for real
#     colspans <- rep(1L, length(cells)) ##vapply(ttrow, function(cell) attr(cell, "colspan"), numeric(1))
#     lines <- as.matrix(Reduce(cbind, cells_same_length))
#     
#     row_char <- paste0(
#       padstr(row.name, nchar_rownames, "left"),
#       paste(unlist(Map(function(cell, colspan) {
#         list(spaces(gap), padstr(cell, colspan*nchar_col+ (colspan-1)*gap))
#       }, lines[1,], colspans)), collapse = "")
#     )
#     
#     if (nrow(lines) > 1) {
#       additional_rows <- apply(lines[-1, , drop=FALSE], 1, function(r) {
#         paste0(
#           spaces(nchar_rownames),
#           paste(unlist(Map(function(cell, colspan) {
#             list(spaces(gap), padstr(cell, colspan*nchar_col + (colspan-1)*gap))
#           }, r, colspans)), collapse = "")
#         )
#       })
#       row_char <- paste(c(row_char, additional_rows), collapse = "\n")
#     }
#     row_char
#   } 
# 
# 
# 
# }
# setMethod("toString", "TableRow",
#           function(x, ...) {
#     ttrow_to_str(x, ...)
# })
# 
# 

# 
# 
# 
# .make_s3_header = function(tt) {
#     clyt = coltree(tt)
#  
#     
#     leaves = collect_leaves(clyt, incl.cont = FALSE)
#     vals = lapply(seq_along(leaves),
#                   function(i) splv_rawvalues(pos_splvals(leaves[[i]])))
#     
#     nvals = length(vals[[1]])
#     numcols = length(vals)
# 
# 
#     breakpts = 1 ## first value is always a "new" value
#     rrows = vector("list",length = nvals + as.integer(dispcounts))
#     for(i in seq(1, nvals)) {
#         ret = .collapse_cspans(lapply(vals, function(vlst) vlst[[i]]), breakpts)
#         breakpts = ret$breakpts
#         args = c(list(row.name = NULL, format = NULL),
#                                         ret$cells)
#         rrows[[i]] = do.call(old_rrow, args)
#     }
#     if(dispcounts) {
#         ccounts = col_counts(tt)
#         countcells = lapply(ccounts, old_rcell,
#                             format = colcount_fmt(tt))                            
#         rrows[[nvals + 1L]] = do.call(old_rrow, c(countcells, row.name = NULL, format = NULL))
# 
#     }
#         
#     ## rrows = lapply(seq(1, nvals),
#     ##                function(i) {
#         
#     ##                 lst = c( row.name = NULL, format = NULL,
#     ##                         .collapse_cspans(lapply(vals, function(vlst) vlst[[i]])))
#     ##                 do.call(rrow, lst)
#     ##                 }) 
#     old_rheaderl(rrows)
# }
# 
# 

# ## this is currently implemented as a
# ## light-as-possible compatibility layer on top
# ## of the existing toString logic.
# ##
# ## That will likely eventually want to change, but for now
# ## it is sufficient.
# setMethod("toString", "TableTree",
#           function(x, ...) {
# 
#     header <- .make_header_rrows(x)
#     body <- x
#     
#     header_body <- c(header, body)
#     
#     nchar_rownames <- max_nchar_rownames(header_body, indent.unit = indent.unit)
#     
#     nchar_col <- max_nchar_cols(header_body)
#     
#     txt_header <- lapply(header, function(row) {
#         row_to_str(row, nchar_rownames, nchar_col, gap, indent.unit)
#     })
#     
#     txt_body <- lapply(x, function(row) {
#         row_to_str(row, nchar_rownames, nchar_col, gap, indent.unit)
#     })
#     
#     
#     paste(
#         c(
#             txt_header,
#             paste(rep("-", nchar_rownames + ncol(x)*(nchar_col + gap)), collapse = ""),
#             txt_body
#         ),
#         collapse = "\n"
#     )  
# })
# 