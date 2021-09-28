

insert_brs <- function(vec) {
    if(length(vec) == 1)
        ret <- list(vec)
    else {
        nout <- length(vec) * 2 - 1
        ret <- vector("list", nout)
        for(i in 1:length(vec)) {
            ret[[2*i - 1]] <- vec[i]
            if(2*i < nout) {
                ret[[2*i]] <- tags$br()
            }
        }
    }
    ret
}

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
#' @param caption_txt Caption text (not including label anchor) for the table
#' @param link_label link anchor label (not including \code{tab:} prefix) for the table.
#'
#' @return A \code{shiny.tag} object representing \code{x} in HTML.
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
#' \dontrun{
#' Viewer(tbl)
#' }
as_html <- function(x,
                    width = NULL,
                    class_table = "table table-condensed table-hover",
                    class_tr = NULL,
                    class_td = NULL,
                    class_th = NULL,
                    caption_txt = NULL,
                    link_label = NULL) {
  
  if (is.null(x)) {
    return(tags$p("Empty Table"))
  }
  
  stopifnot(is(x, "VTableTree"))
  
  mat <- matrix_form(x)
  
  nrh <- attr(mat, "nrow_header")
  nc <- ncol(x) + 1
  
  cells <- matrix(rep(list(list()), (nrh + nrow(x)) * (ncol(x) + 1)),
                  ncol = ncol(x) + 1)
  
  for(i in unique(mat$line_grouping)) {
    rows <- which(mat$line_grouping == i)
    for(j in 1:ncol(mat$strings)) {
      curstrs <- mat$strings[rows,j]
      curspans <- mat$spans[rows,j]
      curaligns <- mat$aligns[rows,j]
      
      curspn <- unique(curspans)
      stopifnot(length(curspn) == 1)
      inhdr <- i <= attr(mat, "nrow_header")
      tagfun <- if(inhdr) tags$th else tags$td
      algn <- unique(curaligns)
      stopifnot(length(algn) == 1)
      cells[i, j][[1]] <- tagfun(
        class = if (inhdr) class_th else class_tr,
        class = if(j > 1 || i > nrh) paste0("text-", algn),
        colspan = curspn,
        insert_brs(curstrs)
      )
    }
  }
  
  ## special casing hax for top_left. We probably want to do this better someday
  cells[1:nrh, 1] <- mapply(
    FUN = function(x, algn) {
      tags$th(x, class = class_th, style = "white-space:pre;")
    },
    x = mat$strings[1:nrh, 1],
    algn = mat$aligns[1:nrh, 1],
    SIMPLIFY = FALSE
  )
  
  # indent row names
  for (i in seq_len(nrow(x))) {
    indent <- mat$row_info$indent[i]
    if (indent > 0) {
      cells[i + nrh, 1][[1]] <- htmltools::tagAppendAttributes(cells[i + nrh, 1][[1]],
                                                               style = paste0("padding-left: ", indent * 3, "ch"))
    }
  }
  
  cells[!mat$display] <- NA_integer_
  
  rows <- apply(cells, 1, function(row) {
    tags$tr(
      class = class_tr,
      Filter(function(x) !identical(x, NA_integer_), row)
    )
  })

  if(!is.null(caption_txt)) {
    if(!is.null(link_label))
      labtxt <- sprintf("(#tab:%s)", link_label)
    else
      labtxt <- NULL
    captxt <- paste(labtxt, caption_txt)
    captag <- tags$caption(class = "Table Caption", captxt)
  } else {
    captag <- NULL
  }
  tags$table(class = class_table, rows, captag)
}
