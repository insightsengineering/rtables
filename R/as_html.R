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
#'   col.names = LETTERS[1:3],
#'   format = "xx",
#'   rrow("r1", 1,2,3),
#'   rrow("r2", 4,3,2, indent = 1),
#'   rrow("r3", indent = 2)
#' )
#' 
#' as_html(tbl)
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
# @param class class attributes for the table in html
# @param ... arguments passed on to \code{shiny::tags$table}
# 
# @return an object of class \code{shinyTag}

#' @export
as_html.rtable <- function(x, class = "table table-condensed table-hover",
                           body_cell_class = NULL,
                           header_cell_class = NULL,
                           ...) {
  
  ncol <- ncol(x)
  
  header <- attr(x, "header")
  body <- x


  
  tags$table(
    class = class,
  #  tags$tr(tagList(tags$th(""), lapply(col_headers, tags$th, align="center", class="text-center"))), 
    lapply(header, as_html, ncol = ncol, cell_tag = tags$th, ...),
    lapply(x, as_html, ncol = ncol, ...)
  )
  
}

#' @export
as_html.rrow <- function(x, ncol, cell_tag = tags$td, ...) {
  
  indent <- attr(x, "indent")
  row.name <- attr(x,"row.name")
  
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
  
  tags$tr(cells)
}