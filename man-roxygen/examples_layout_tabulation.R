#' 
#' library(magrittr)
#' 
#' l <- NULL %>% add_colby_varlevels("ARM") %>% 
#'     add_analyzed_vars("AGE", afun = function(x) {
#'       setNames(as.list(fivenum(x)), c("minimum", "lower-hinge", "median", "upper-hinge", "maximum"))
#'     })
#' 
#' l
#' 
#' build_table(l, DM)
#' 