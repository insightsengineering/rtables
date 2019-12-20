

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

# copied over from utils.nest which is not open-source
all_true <- function(lst, fcn, ...) {
  all(vapply(lst, fcn, logical(1), ...))
}
is_logical_single <- function(x) {
  !is.null(x) &&
    is.logical(x) &&
    length(x) == 1 &&
    !is.na(x)
}
is_logical_vector_modif <- function(x, min_length = 1) {
  !is.null(x) &&
    is.logical(x) &&
    is.atomic(x) &&
    !anyNA(x) &&
    ifelse(min_length > 0, length(x) >= min_length, TRUE)
}