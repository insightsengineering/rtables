

#' Convert a Table Tree to a String
#' 
#' 
#' @examples 
#' tbl <- tt_rtable(
#'   header = tt_rheader(tt_rrow("", "Treatement N=100", "Comparison N=300")),
#'   format = "xx (xx.xx%)",
#'   tt_rrow("A", c(104, .2), c(100, .4)),
#'   tt_rrow("B", c(23, .4), c(43, .5)),
#'   tt_rrow("this is a very long section header"),
#'   tt_rrow("estimate", tt_rcell(55.23, "xx.xx", colspan = 2)),
#'   tt_rrow("95% CI", indent = 1, tt_rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
#' )
#' 
#' tbl
#' 
#' 
#' mtbl <- tt_rtable(
#'     header = tt_rheader(
#'         tt_rrow(row.name = NULL, tt_rcell("Sepal.Length", colspan = 2),
#'                 tt_rcell("Petal.Length", colspan=2)),
#'         tt_rrow(NULL, "mean", "median", "mean", "median")
#'     ),
#'     tt_rrow(
#'         row.name = "Mean All Species",
#'         mean(iris$Sepal.Length), median(iris$Sepal.Length),
#'         mean(iris$Petal.Length), median(iris$Petal.Length),
#'         format = "xx.xx"
#'     ),
#'     tt_rrow(
#'         row.name = "SD All Species",
#'         sd(iris$Sepal.Length), sd(iris$Sepal.Length),
#'         sd(iris$Petal.Length), sd(iris$Petal.Length),
#'         format = "xx.xxx"
#'     )
#' )
#' 
#' mtbl
#' 
#' x <- mtbl 
#' 
setMethod("toString", "VTableTree", function(x, gap = 3, ...) {
  
  nr <- nrow(x)
  nc <- ncol(x)
  ne <- nr * nc

  body <- matrix(as.list(rep("", ne)), nrow = nr)
  spans <- matrix(rep(0, ne), nrow = nr)
  space <- matrix(rep(0, ne), nrow = nr)
  
  if (any(apply(body, c(1, 2), function(x) grepl("\n", x, fixed = TRUE))))
      stop("no \n allowed at the moment")

  cell_format <- value_fmts(x)
  
  if (ncol(cell_format) != nc || nrow(cell_format) != nr)
    stop("inconsistent dimensions")
  
  
  
  # Row Names
  lfs <- collect_leaves(x, TRUE, TRUE)
  depth <- vapply(lfs, tt_level, integer(1))
  lbls <- vapply(lfs, obj_label, character(1))
  
  
  depth <- c(0, 1, 4, 4, 2, 0, 0, 1 , 3, 0)

  
  
  # Body
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      cell_content <- x[i, j, drop = TRUE]
      
      body[i, j] <- format_rcell(cell_content, cell_format[[i, j]])
      spans[i, j] <-  1
      
      # there is some inefficiency here
      space[i, j] <- max(vapply(body[i, j], nchar, numeric(1))) / spans[i, j]    
    }
  }
  
  col_widths <- ceiling(apply(space, 2, max))
  
  apply(body, 2, function(row) {
    
    
  })
  
  
  
  
  
 
})



ttstring <- function(x, gap = 3) {
  ci <- x@col_info@tree_layout
}


setGeneric("square2nd", function(obj) standardGeneric("square2nd"))

## TableTree objects (can) have content Rows
## process the content, then the children by recursive call
setMethod("square2nd", "TableTree",
          function(obj) {
            ct = content_table(obj)
            if(nrow(ct))
              content_table(obj) = square2nd(ct)
            kids = tree_children(obj)
            if(length(kids)) { 
              newkids = lapply(kids, square2nd)
              names(newkids) = names(kids)
              tree_children(obj) = newkids
            }
            obj
          })


## this will hit all Content tables as well
## as any subtrees that happen to be
## Elementary
setMethod("square2nd", "ElementaryTable",
          function(obj) {
            kids = tree_children(obj)
            if(length(kids)) {
              newkids = lapply(kids, square2nd)
              names(newkids) = names(kids)
              tree_children(obj) = newkids
            }
            obj
          })

