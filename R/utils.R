

## is each object in a collection from a class
are <- function(object_collection, class2) {
  all(vapply(object_collection, is, logical(1), class2))
}

num_all_equal <- function(x, tol = .Machine$double.eps ^ 0.5) {
  stopifnot(is.numeric(x))
  
  if (length(x) == 1) return(TRUE)
  
  y <- range(x) / mean(x)
  isTRUE(all.equal(y[1], y[2], tolerance = tol))
}
