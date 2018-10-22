#' Convert an \code{\link{rtable}} object to an \code{shiny.tag} html
#' representation of the \code{\link{rtable}}
#' 
#' The returned html object can be immediately used in shiny and rmarkdown
#' 
#' @inheritParams dim.rtable
#' @param ... arguments passed as attributes to the table html objet
#' 
#' @return an object of class \code{shiny.tag}
#' 
#' 
#' @export
#' 
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
#' 
as_html <- function(x, ...) {
  UseMethod("as_html", x)  
}

#' Generic for as_html
#' 
#' Generic method for as_html
#' 
#' @param x object
#' @param ... additional arguments currently not used
#' 
#' @export
#' 
as_html.default <- function(x, ...) {
  stop("no as_html method for class ", class(x))
}

# Convert an rtable object to html
# 
# @param x an object of class \code{\link{rtable}}
# @param class.table class attributes for the table in html
# @param ... arguments passed on to methods
# 
# @return an object of class \code{shinyTag}

#' @export
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
  
}

#' @export
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
}