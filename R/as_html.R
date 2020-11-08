

#' Convert an `rtable` object to a `shiny.tag` html object
#' 
#' The returned `html` object can be immediately used in shiny and rmarkdown.
#' 
#' @param x `rtable` object
#' @param class_table class for table tag
#' @param class_tr class for tr tag
#' @param class_td class for td tag
#' @param class_th class for th tag
#' @param width width
#' 
#' @importFrom htmltools tags
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
#' as_html(tbl, class_table = "table", class_tr = "row")
#' 
#' as_html(tbl, class_td = "aaa")
#' 
#' Viewer(tbl)
as_html <- function(x, 
                    width = NULL,
                    class_table = "table table-condensed table-hover",
                    class_tr = "",
                    class_td = "",
                    class_th = "") {
  
  stopifnot(is(x, "VTableTree"))
  
  mat <- matrix_form(x)
  
  nrh <- attr(mat, "nrow_header")
  nc <- ncol(x) + 1
  is_header <- matrix(rep(c(TRUE, FALSE), nc * c(nrh, nrow(mat$row_info))), ncol = nc, byrow = TRUE)
    
  cells <- matrix(mapply(function(str, spn, algn, dsp, hdr) {
    if (dsp) {
      args <- list(class = paste(class_th, paste0("text-", algn), collapse = " "), colspan = spn)
      do.call(ifelse(hdr, tags$th, tags$td), c(list(str), args))
    } else {
      NULL
    }
  }, mat$strings, mat$spans, mat$aligns, mat$display, is_header,  USE.NAMES = FALSE, SIMPLIFY = FALSE), ncol = ncol(x) + 1)


  # indent row names
  for (i in seq_len(nrow(x))) {
    indent <- mat$row_info$indent[i] 
    if (indent > 0) {
      cells[i + nrh, 1][[1]] <- htmltools::tagAppendAttributes(cells[i + nrh, 1][[1]], 
                                                          style = paste0("padding-left: ", indent * 3, "ch"))
    }
  }
  
  rows <- apply(cells, 1, function(row) {
    do.call(tags$tr, c(row, list(class = class_tr)))
  })
  
  do.call(tags$table, c(rows, list(class = class_table)))
  
}

