#' Convert an rtable to ascii
#' 
#' @param x rtable object
#' @param gap number of spaces between columns
#' @param indent.unit number of spaces used for indentation of row.names
#' @param ... currently not used
#' 
#' @method toString rtable
#' @export
#' 
toString.rtable <- function(x, gap = 8, indent.unit = 2, ...) {
  
  header <- attr(x, "header")
  body <- x
  
  header_body <- c(header, body)
  
  nchar_rownames <- max_nchar_rownames(header_body, indent.unit = indent.unit)

  nchar_col <- max_nchar_cols(header_body)
  
  txt_header <- lapply(header, function(row) {
    row_to_str(row, nchar_rownames, nchar_col, gap, indent.unit)
  })
  
  txt_body <- lapply(x, function(row) {
    row_to_str(row, nchar_rownames, nchar_col, gap, indent.unit)
  })
  
  
  paste(
    c(
      txt_header,
      paste(rep("-", nchar_rownames + ncol(x)*(nchar_col + gap)), collapse = ""),
      txt_body
    ),
    collapse = "\n"
  )  
}

# get the max 
# rows is a list of rows
max_nchar_cols <- function(rows) {
    if(length(rows) == 0)
        return(0)
  
  if (is(rows, "rrow")) rows <- list(rows)
  
  ceiling(max(unlist(lapply(rows, function(row) {
    lapply(row, function(cell) { 
      nc <- nchar(unlist(strsplit(format_rcell(cell, output = "ascii"), "\n", fixed = TRUE)))
      nc[is.na(nc)] <- 0
      nc / get_colspan(cell)
    })
  }))))
}


max_nchar_rownames <- function(rows, indent.unit) {
  
  if (is(rows, "rrow")) rows <- list(rows)
  
  if (!all(vapply(rows, is, logical(1), "rrow"))) stop("not all elements are of class rrow")
  
  nchar_rownames <- vapply(rows, function(row) {
    rn <- attr(row, "row.name")
    if (is.null(rn)) {
      0
    } else {
      nchar(rn) +  attr(row, "indent") * indent.unit
    }
  }, numeric(1))
  
  max(0, nchar_rownames)
  
}


#' @method print rtable
#' @export
print.rtable <- function(x, ...) {
  str <- toString(x, ...)
  cat(str)
  cat("\n")
}

#' @method print rcell
#' @export
print.rcell <- function(x, output = "ascii", ...) {
  cat(format_rcell(x, output = output))
  cat("\n")
}

#' @method print rrow
#' @export
print.rrow <- function(x, gap = 8, indent.unit = 2, ...) {
  txt <- row_to_str(x, max_nchar_rownames(x, indent.unit = indent.unit), max_nchar_cols(x), gap = gap, indent.unit = indent.unit)
  cat(txt)
  cat("\n")
}


row_to_str <- function(row, nchar_rownames, nchar_col, gap, indent.unit) {
  
  row.name <-  if (is.null(attr(row, "row.name"))) {
    ""
  } else {
    paste(c(rep(" ",  attr(row, "indent")*indent.unit),
            attr(row, "row.name")), collapse = "")    
  }
  
  if (length(row) == 0) {
    row.name
  } else {
    cells <- lapply(row, function(cell) {
      unlist(strsplit(format_rcell(cell, output = "ascii"), "\n", fixed = TRUE))
    })
    
    nlines <- max(vapply(cells,length, numeric(1)))
    
    cells_same_length <- lapply(cells, function(x) {
      if (length(x) == nlines) {
        x
      } else {
        c(x, rep(NA_character_, nlines -length(x)))
      }
    })
    
    colspans <- vapply(row, get_colspan, numeric(1))
    lines <- as.matrix(Reduce(cbind, cells_same_length))
    
    row_char <- paste0(
      padstr(row.name, nchar_rownames, "left"),
      paste(unlist(Map(function(cell, colspan) {
        list(spaces(gap), padstr(cell, colspan*nchar_col+ (colspan-1)*gap))
      }, lines[1,], colspans)), collapse = "")
    )
    
    if (nrow(lines) > 1) {
      additional_rows <- apply(lines[-1, , drop=FALSE], 1, function(r) {
        paste0(
          spaces(nchar_rownames),
          paste(unlist(Map(function(cell, colspan) {
            list(spaces(gap), padstr(cell, colspan*nchar_col + (colspan-1)*gap))
          }, r, colspans)), collapse = "")
        )
      })
      row_char <- paste(c(row_char, additional_rows), collapse = "\n")
    }
    row_char
  } 
}


padstr <- function(x, n, just = c("center", "left", "right")) {
  
  just <- match.arg(just)
  
  if (length(x) != 1) stop("length of x needs to be 1 and not", length(x))
  if (is.na(n) || !is.numeric(n) || n < 0) stop("n needs to be numeric and > 0")
  
  if (is.na(x)) x <- ""
  
  nc <- nchar(x)
  
  if (n < nc) stop(x, " has more than ", n, " characters")
  
  switch(
    just,
    center = {
      pad <- (n-nc)/2  
      paste0(spaces(floor(pad)), x, spaces(ceiling(pad)))
    },
    left = paste0(x, spaces(n-nc)),
    right = paste0(spaces(n-nc), x)
  )
}

spaces <- function(n) {
  paste(rep(" ", n), collapse = "")
}
