

#' Trim Zero Rows
#' 
#' @param tbl table object
#' 
#' @return an rtable object
#' 
#' @export
#' 
trim_zero_rows <- function(tbl) {
  stopifnot(is(tbl, "VTableTree"))
  
  rows <- collect_leaves(tbl, TRUE, TRUE)
  torm <- vapply(rows, function(x) {
    identical(unname(unlist(row_values(x))), c(0L, 0L, 0L))
  }, NA, USE.NAMES = FALSE)
  tbl[!torm, ]
  
}