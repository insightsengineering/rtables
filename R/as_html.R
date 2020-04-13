
#' Convert an \code{\link{rtable}} object to an \code{shiny.tag} html
#' representation of the \code{\link{rtable}}
#' 
#' The returned html object can be immediately used in shiny and rmarkdown
#' 
#' @param x object
#' @param ... additional arguments currently not used
#' @rdname as_html
#' @examples 
#' 
#' tbl <- rtable(
#'   header = LETTERS[1:3],
#'   format = "xx",
#'   rrow("r1", 1,2,3),
#'   rrow("r2", 4,3,2, indent = 1),
#'   rrow("r3", indent = 2)
#' )
#' 
#' as_html(tbl)
#' 
#' as_html(tbl, class.table = "table", class.tr = "row")
#' 
#' as_html(tbl, class.td = "aaa")
#' @export
setGeneric("as_html", function(x, ...) standardGeneric("as_html"))

## #' Convert an rtable object to html
## #' 
## #' @inheritParams as_html
## #' @param class.table class attributes for \code{<table>} html object
## #' 
## #' @return an object of class \code{shinyTag}
## #' 
#' @exportMethod as_html
setMethod("as_html", "rtable",

as_html.rtable <- function(x, class.table = "table table-condensed table-hover",
                           ...) {
  
    ncol <- ncol(x)
    
    header <- attr(x, "header")
    body <- x
    
    tags$table(
             class = class.table,
             lapply(header, as_html, ncol = ncol, is_header = TRUE, ...),
             lapply(body, as_html, ncol = ncol, is_header = FALSE, ...)
         )
    
})

#' @exportMethod as_html
setMethod("as_html", "rrow", 
as_html.rrow <- function(x, ncol, is_header, 
                         class.tr = NULL, class.td = NULL, class.th = NULL, ...) {
  
  (is.logical(is_header) && length(is_header) == 1) || stop("is_header is supposed to be a boolean")
  
  cell_tag <- if (is_header) {
    function(...) tags$th(class = class.th, ...)
  } else {
    function(...) tags$td(class = class.td, ...)
  }
  
  indent <- attr(x, "indent")
  row.name <- attr(x, "row.name")
  
  cells <- if (length(x) == 0) {
    cell_tag(row.name, class=paste("rowname", "text-left"), colspan = as.character(ncol+1))
  } else {
    tagList(
      cell_tag(row.name, class=paste("rowname", "text-left")),
      lapply(x, function(xi) {
        
        colspan <- attr(xi, "colspan")
        if (is.null(colspan)) stop("colspan for rcell is NULL")
        
        cell_content <- format_rcell(xi, output="html")
        if (colspan == 1) {
          cell_tag(cell_content, class = "text-center")
        } else {
          cell_tag(cell_content, colspan = as.character(colspan), class = "text-center")
        }
      }),
      replicate(ncol - ncell(x), cell_tag(), simplify = FALSE)
    )
  }
  
  if (indent>0) {
    if (length(x) == 0) {
      cells$attribs <- c(cells$attribs, list(style=paste0("padding-left: ", indent*3, "ch")))
    } else {
      cells[[1]]$attribs <- c(cells[[1]]$attribs, list(style=paste0("padding-left: ", indent*3, "ch")))
    }
  }
  
  tags$tr(cells, class = class.tr)
})

extract_rowobj = function(tt) {
    stopifnot(nrow(tt) == 1)
    
}

#' @param class.tr class for tr tag
#' @param class.tr class for td tag
#' @param class.tr class for th tag
#' @exportMethod as_html
#' @rdname as_html
setMethod("as_html", "VTableTree",
          function(x,
                   class.table = "table table-condensed table-hover",
                   class.tr = NULL,
                   class.td = NULL,
                   class.th = NULL, ...)  {

    hmat = .tbl_header_mat(x)
    hrows = lapply(seq_len(nrow(hmat$body)),
                   function(i) {
        tags$tr(class= class.tr,
                mapply(function(v, sp) {
                    tags$th(v, class = class.th, colspan = sp)
                }, v = hmat$body[i,],
                sp = hmat$span[i,],
                SIMPLIFY= FALSE))
    })

    pagdf = make_pagdf(x)
    brows = mapply(function(robj, ind) {
        as_html(robj, indent = ind)
    }, robj = collect_leaves(x, add.labrows = TRUE),
    ind = pagdf$indent,
    SIMPLIFY=FALSE)

    tags$table(class = class.table,
               hrows,
               brows)
})


#' @param indent amount it should be indented
#' @exportMethod as_html
#' @rdname as_html
setMethod("as_html", "TableRow",
          function(x, class.tr = NULL,
                   class.td = NULL,
                   class.th = NULL,
                   indent, ...) {
    ncol <- ncol(x)
    is_header <- FALSE ## we can just get rid of this.
  (is.logical(is_header) && length(is_header) == 1) || stop("is_header is supposed to be a boolean")
  
  cell_tag <- if (is_header) {
    function(...) tags$th(class = class.th, ...)
  } else {
    function(...) tags$td(class = class.td, ...)
  }
  
  row.name <- obj_label(x)
  
  cells <- if (length(row_values(x)) == 0) {
    cell_tag(row.name, class=paste("rowname", "text-left"), colspan = as.character(ncol+1))
  } else {
    tagList(
      cell_tag(row.name, class=paste("rowname", "text-left")),
      lapply(row_values(x), function(xi) {
        
        colspan <- 1 # XXXX
        if (is.null(colspan)) stop("colspan for rcell is NULL")
        
        cell_content <- format_rcell(xi, output="html")
        if (colspan == 1) {
          cell_tag(cell_content, class = "text-center")
        } else {
          cell_tag(cell_content, colspan = as.character(colspan), class = "text-center")
        }
      }),
      replicate(ncol - length(row_values(x)), cell_tag(), simplify = FALSE)
    )
  }
  
  if (indent>0) {
#    if (length(x) == 0) {
      cells$attribs <- c(cells$attribs, list(style=paste0("padding-left: ", indent*3, "ch")))
    ## } else {
    ##   cells[[1]]$attribs <- c(cells[[1]]$attribs, list(style=paste0("padding-left: ", indent*3, "ch")))
    ## }
  }
  
  tags$tr(cells, class = class.tr)
})
