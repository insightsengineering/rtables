#' @examples 
#' 
#' library(magrittr)
#' 
#' l <- NULL %>% add_colby_varlevels("ARM") %>% 
#'     add_analyzed_var("AGE", afun = function(x) {
#'       setNames(as.list(fivenum(x)), c("minimum", "lower-hinge", "median", "upper-hinge", "maximum"))
#'     })
#' 
#' build_table(l, DM)
#' 