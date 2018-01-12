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
#'   rrow("r1", 1,2,3),
#'   rrow("r2", 4,3,2, indent = 1),
#'   rrow("r3", indent = 2),
#'   headers = LETTERS[1:3],
#'   format = "xx"
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
as_html.rtable <- function(x, class = "table table-condensed table-hover", ...) {
  
  ncol <- ncol(x)
  
  # split header into lines
  col_headers <- lapply(attr(x, "col.names"), function(colname) {
    els <- unlist(strsplit(colname, "\n", fixed = TRUE))
    Map(function(el, is.last) {
      tagList(el, if (!is.last) tags$br() else NULL)
    }, els, c(rep(FALSE, length(els) -1), TRUE))
  })
  
  tags$table(
    class = class,
    ...,
    tags$tr(tagList(tags$th(""), lapply(col_headers, tags$th, align="center", class="text-center"))), 
    lapply(x, as_html, ncol = ncol)
  )
  
}

#' @export
as_html.rrow <- function(x, ncol, ...) {
  
  indent <- attr(x, "indent")
  row.name <- attr(x,"row.name")
  
  cells <- if (length(x) == 0) {
    tags$td(colspan = as.character(ncol+1), class="rowname", align="left", row.name)
  } else {
    tagList(
      tags$td(class="rowname", align = "left", row.name),
      lapply(x, function(xi) {
        
        colspan <- attr(xi, "colspan")
        if (is.null(colspan)) stop("colspan for rcell is NULL")
        
        cell_content <- format_rcell(xi, output="html")
        if (colspan == 1) {
          tags$td(cell_content, align = "center")
        } else {
          tags$td(cell_content, colspan = as.character(colspan), align = "center")
        }
      }),
      replicate(ncol - ncells(x), tags$td(), simplify = FALSE)
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